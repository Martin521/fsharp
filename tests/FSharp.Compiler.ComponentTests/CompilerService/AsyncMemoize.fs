module CompilerService.AsyncMemoize

open System
open System.Threading
open Internal.Utilities.Collections
open System.Threading.Tasks
open System.Diagnostics

open FSharp.Compiler.DiagnosticsLogger
open FSharp.Compiler.Diagnostics

open Xunit

let tap f x = f x; x

let internal record (cache: AsyncMemoize<_,_,_>) =

    let events = Collections.Concurrent.ConcurrentQueue()

    let waitForIdle() = SpinWait.SpinUntil(fun () -> not cache.Updating)

    waitForIdle()
    cache.Event
    |> Event.map (fun (e, (_, k, _)) -> e, k)
    |> Event.add events.Enqueue

    let getEvents () =
        waitForIdle()
        events |> List.ofSeq |> tap (printfn "events: %A")

    getEvents

let check getEvents assertFunction =
    let actual = getEvents()
    assertFunction actual

let waitUntil getEvents condition =
    while getEvents() |> condition |> not do ()

let recorded (expected: 't list) (actual: 't list) =
    Assert.Equal<'t>(expected, actual)

let countOf value count events =
    events |> Seq.filter (fst >> (=) value) |> Seq.length |> (=) count

let received value events =
    events |> List.tryLast |> Option.map (fst >> (=) value) |> Option.defaultValue false

[<Fact>]
let ``Basics``() =
    let computation key = async {
        do! Async.Sleep 1
        return key * 2
    }

    let memoize = AsyncMemoize<int, int, int>()
    let events = record memoize

    let result =
        seq {
            memoize.Get'(5, computation 5)
            memoize.Get'(5, computation 5)
            memoize.Get'(2, computation 2)
            memoize.Get'(5, computation 5)
            memoize.Get'(3, computation 3)
            memoize.Get'(2, computation 2)
        }
        |> Async.Parallel
        |> Async.RunSynchronously

    let expected = [| 10; 10; 4; 10; 6; 4|]

    Assert.Equal<int array>(expected, result)

    check events <| fun events ->
        let groups = events |> Seq.groupBy snd |> Seq.toList
        Assert.Equal(3, groups.Length)
        for key, events in groups do
            Assert.Equal<Set<(JobEvent * int)>>(Set [ Requested, key; Started, key; Finished, key ], Set events)

[<Fact>]
let ``We can cancel a job`` () =
    task {

        let jobStarted = new ManualResetEventSlim(false)
        let cts = new CancellationTokenSource()
        let ctsCancelled = new ManualResetEventSlim(false)

        let computation = async {
            use! _catch = Async.OnCancel ignore
            jobStarted.Set()
            ctsCancelled.Wait()
            do! async { }
            failwith "Should be canceled before it gets here"
        }

        let memoize = AsyncMemoize<_, int, _>()
        let events = record memoize

        let key = 1

        let _task1 = Async.StartAsTask( memoize.Get'(1, computation), cancellationToken = cts.Token)

        jobStarted.Wait()
        cts.Cancel()
        ctsCancelled.Set()

        check events recorded
             [ Requested, key
               Started, key
               Canceled, key ]
    }

[<Fact>]
let ``Job is restarted if first requestor cancels`` () =
        let jobStarted = new SemaphoreSlim(0)

        let jobCanComplete = new ManualResetEventSlim(false)

        let computation key = async {
            jobStarted.Release() |> ignore

            jobCanComplete.Wait()
            return key * 2
        }

        let memoize = AsyncMemoize<_, int, _>()
        let events = record memoize

        use cts1 = new CancellationTokenSource()

        let key = 1

        let _task1 = Async.StartAsTask( memoize.Get'(key, computation key), cancellationToken = cts1.Token)

        jobStarted.Wait()
        let task2 = Async.StartAsTask( memoize.Get'(key, computation key))
        let task3 = Async.StartAsTask( memoize.Get'(key, computation key))

        waitUntil events (countOf Requested 3)

        cts1.Cancel()

        jobCanComplete.Set() |> ignore

        jobStarted.Wait()

        Assert.Equal(2, task2.Result)
        Assert.Equal(2, task3.Result)

        check events recorded
              [ Requested, key
                Started, key
                Requested, key
                Requested, key
                Restarted, key
                Finished, key ]

[<Fact>]
let ``Job is restarted if first requestor cancels but keeps running if second requestor cancels`` () =
        let jobStarted = new ManualResetEventSlim(false)

        let jobCanComplete = new ManualResetEventSlim(false)

        let computation key = async {
            jobStarted.Set() |> ignore
            jobCanComplete.Wait()
            return key * 2
        }
        
        let memoize = AsyncMemoize<_, int, _>()
        let events = record memoize

        use cts1 = new CancellationTokenSource()
        use cts2 = new CancellationTokenSource()

        let key = 1

        let _task1 = Async.StartAsTask( memoize.Get'(key, computation key), cancellationToken = cts1.Token)

        jobStarted.Wait()
        jobStarted.Reset() |> ignore

        let _task2 = Async.StartAsTask( memoize.Get'(key, computation key), cancellationToken = cts2.Token)
        let task3 = Async.StartAsTask( memoize.Get'(key, computation key))

        waitUntil events (countOf Requested 3)

        cts1.Cancel()

        jobStarted.Wait()

        cts2.Cancel()

        jobCanComplete.Set() |> ignore

        Assert.Equal(2, task3.Result)

        check events recorded
          [ Requested, key
            Started, key
            Requested, key
            Requested, key
            Restarted, key
            Finished, key ]


type ExpectedException() =
    inherit Exception()

[<Fact>]
let ``Stress test`` () =

    let seed = System.Random().Next()

    let rng = System.Random seed
    let threads = 30
    let iterations = 30
    let maxDuration = 100
    let minTimeout = 0
    let maxTimeout = 500
    let exceptionProbability = 0.01
    let gcProbability = 0.1
    let stepMs = 10
    let keyCount = rng.Next(5, 200)
    let keys = [| 1 .. keyCount |]

    let testTimeoutMs = threads * iterations * maxDuration * 2

    let intenseComputation durationMs result =
        async {
            if rng.NextDouble() < exceptionProbability then
                raise (ExpectedException())
            let s = Stopwatch.StartNew()
            let mutable number = 0
            while (int s.ElapsedMilliseconds) < durationMs do
                number <- number + 1 % 12345
            return [result]
        }

    let rec sleepyComputation durationMs result =
        async {
            if rng.NextDouble() < (exceptionProbability / (float durationMs / float stepMs)) then
                raise (ExpectedException())
            if durationMs > 0 then
                do! Async.Sleep (min stepMs durationMs)
                return! sleepyComputation (durationMs - stepMs) result
            else
                return [result]
        }

    let rec mixedComputation durationMs result =
        async {
            if durationMs > 0 then
                if rng.NextDouble() < 0.5 then
                    let! _ = intenseComputation (min stepMs durationMs) ()
                    ()
                else
                    let! _ = sleepyComputation (min stepMs durationMs) ()
                    ()
                return! mixedComputation (durationMs - stepMs) result
            else
                return [result]
        }

    let computations = [|
        intenseComputation
        sleepyComputation
        mixedComputation
    |]

    let cache = AsyncMemoize<int, int, int list>(keepStrongly=5, keepWeakly=10)

    let mutable started = 0
    let mutable canceled = 0
    let mutable timeout = 0
    let mutable failed = 0
    let mutable completed = 0

    let test =
        seq {
            for _ in 1..threads do
                let rec loop iteration =
                    task {
                        if gcProbability > rng.NextDouble() then
                            GC.Collect(2, GCCollectionMode.Forced, false)

                        let computation = computations[rng.Next computations.Length]
                        let durationMs = rng.Next maxDuration
                        let timeoutMs = rng.Next(minTimeout, maxTimeout)
                        let key = keys[rng.Next keys.Length]
                        let result = key * 2
                        let job = cache.Get'(key, computation durationMs result)
                        let cts = new CancellationTokenSource()
                        let runningJob = Async.StartAsTask(job, cancellationToken = cts.Token)
                        cts.CancelAfter timeoutMs
                        Interlocked.Increment &started |> ignore
                        try
                            let! actual = runningJob
                            Assert.Equal(result, actual.Head)
                            Interlocked.Increment &completed |> ignore
                        with
                            | :? TaskCanceledException as _e ->
                                Interlocked.Increment &canceled |> ignore
                            | :? OperationCanceledException as _e ->
                                Interlocked.Increment &canceled |> ignore
                            | :? TimeoutException -> Interlocked.Increment &timeout |> ignore
                            | :? ExpectedException -> Interlocked.Increment &failed |> ignore
                            | :? AggregateException as ex when
                                ex.Flatten().InnerExceptions |> Seq.exists (fun e -> e :? ExpectedException) ->
                                Interlocked.Increment &failed |> ignore
                            | e ->
                                failwith $"Seed {seed} failed on iteration {iteration}: %A{e}"
                        if iteration < iterations then
                            return! loop (iteration + 1)
                        return ()
                    }
                loop 1
        }
        |> Task.WhenAll

    if not (test.Wait testTimeoutMs) then failwith "Test timed out - most likely deadlocked"
    
    Assert.Equal (threads * iterations, started)
    // Assert.Equal<int * int * int * int * int>((0,0,0,0,0),(started, completed, canceled, failed, timeout))
    Assert.Equal (started, completed + canceled + failed + timeout)

    Assert.True ((float completed) > ((float started) * 0.1), "Less than 10 % completed jobs")


[<Theory>]
[<InlineData(true, 1)>]
[<InlineData(false, 2)>]
let ``Cancel running jobs with the same key`` cancelDuplicate expectFinished =
    let cache = AsyncMemoize(cancelDuplicateRunningJobs=cancelDuplicate)

    let mutable started = 0
    let mutable finished = 0

    let job1started = new ManualResetEventSlim(false)
    let job1finished = new ManualResetEventSlim(false)

    let jobCanContinue = new ManualResetEventSlim(false)

    let job2started = new ManualResetEventSlim(false)
    let job2finished = new ManualResetEventSlim(false)

    let work onStart onFinish = async {
        Interlocked.Increment &started |> ignore
        onStart() |> ignore
        jobCanContinue.Wait()
        do! Async.Sleep 100
        Interlocked.Increment &finished |> ignore
        onFinish() |> ignore
    }

    let key1 =
        { new ICacheKey<_, _> with
                member _.GetKey() = 1
                member _.GetVersion() = 1
                member _.GetLabel() = "key1" }

    cache.Get(key1, work job1started.Set job1finished.Set) |> Async.Catch |> Async.Ignore |> Async.Start

    job1started.Wait()

    let key2 =
        { new ICacheKey<_, _> with
                member _.GetKey() = key1.GetKey()
                member _.GetVersion() = key1.GetVersion() + 1
                member _.GetLabel() = "key2" }

    cache.Get(key2, work job2started.Set job2finished.Set ) |> Async.Catch |> Async.Ignore |> Async.Start

    job2started.Wait()

    jobCanContinue.Set() |> ignore
        
    job2finished.Wait()
        
    if not cancelDuplicate then
        job1finished.Wait()

    Assert.Equal((2, expectFinished), (started, finished))

type DummyException(msg) =
    inherit Exception(msg)

[<Fact>]
let ``Preserve thread static diagnostics`` () = 

    let seed = System.Random().Next()

    let rng = System.Random seed
    
    let job1Cache = AsyncMemoize()
    let job2Cache = AsyncMemoize()

    let job1 (input: string) = async {
        let! _ = Async.Sleep (rng.Next(1, 30))
        let ex = DummyException("job1 error")
        DiagnosticsThreadStatics.DiagnosticsLogger.ErrorR(ex)
        return Ok input
    }

    let job2 (input: int) = async {
        
        DiagnosticsThreadStatics.DiagnosticsLogger.Warning(DummyException("job2 error 1"))

        let! _ = Async.Sleep (rng.Next(1, 30))

        let key = { new ICacheKey<_, _> with
                        member _.GetKey() = "job1"
                        member _.GetVersion() = input
                        member _.GetLabel() = "job1" }

        let! result = job1Cache.Get(key, job1 "${input}" )

        DiagnosticsThreadStatics.DiagnosticsLogger.Warning(DummyException("job2 error 2"))

        return input, result

    }

    let tasks = seq {
        for i in 1 .. 100 do

            task {
                let diagnosticsLogger =
                    CompilationDiagnosticLogger($"Testing task {i}", FSharpDiagnosticOptions.Default)

                use _ = new CompilationGlobalsScope(diagnosticsLogger, BuildPhase.Optimize)

                DiagnosticsThreadStatics.DiagnosticsLogger.Warning(DummyException("task error"))


                let key = { new ICacheKey<_, _> with
                                member _.GetKey() = "job2"
                                member _.GetVersion() = rng.Next(1, 10)
                                member _.GetLabel() = "job2" }

                let! result = job2Cache.Get(key, job2 (i % 10))

                let diagnostics = diagnosticsLogger.GetDiagnostics()

                Assert.Equal(4, diagnostics.Length)

                return result, diagnostics
            }
    }

    let results = (Task.WhenAll tasks).Result

    let diagnosticCounts = results |> Seq.map snd |> Seq.map Array.length |> Seq.groupBy id |> Seq.map (fun (k, v) -> k, v |> Seq.length) |> Seq.sortBy fst |> Seq.toList

    Assert.Equal<(int * int) list>([4, 100], diagnosticCounts)

    let diagnosticMessages = results |> Seq.map snd |> Seq.map (Array.map (fun (d, _) -> d.Exception.Message) >> Array.toList) |> Set

    Assert.Equal<Set<_>>(Set [["task error"; "job2 error 1"; "job1 error"; "job2 error 2"; ]], diagnosticMessages)


[<Fact>]
let ``Preserve thread static diagnostics already completed job`` () =

    let cache = AsyncMemoize()

    let key = { new ICacheKey<_, _> with
                    member _.GetKey() = "job1"
                    member _.GetVersion() = 1
                    member _.GetLabel() = "job1" }

    let job (input: string) = async {
        let ex = DummyException($"job {input} error")
        DiagnosticsThreadStatics.DiagnosticsLogger.ErrorR(ex)
        return Ok input
    }

    task {

        let diagnosticsLogger = CompilationDiagnosticLogger($"Testing", FSharpDiagnosticOptions.Default)

        use _ = new CompilationGlobalsScope(diagnosticsLogger, BuildPhase.Optimize)

        let! _ = cache.Get(key, job "1" )
        let! _ = cache.Get(key, job "2" )

        let diagnosticMessages = diagnosticsLogger.GetDiagnostics() |> Array.map (fun (d, _) -> d.Exception.Message) |> Array.toList

        Assert.Equal<_ list>(["job 1 error"; "job 1 error"], diagnosticMessages)

    }


[<Fact>]
let ``We get diagnostics from the job that failed`` () =

    let cache = AsyncMemoize()

    let key = { new ICacheKey<_, _> with
                    member _.GetKey() = "job1"
                    member _.GetVersion() = 1
                    member _.GetLabel() = "job1" }

    let job = async {
        let ex = DummyException($"job error")

        // no recovery
        DiagnosticsThreadStatics.DiagnosticsLogger.Error ex
        return 5
    }

    task {
        let logger = CapturingDiagnosticsLogger("AsyncMemoize diagnostics test")

        SetThreadDiagnosticsLoggerNoUnwind logger

        do! cache.Get(key, job ) |> Async.Catch |> Async.Ignore

        let messages = logger.Diagnostics |> List.map fst |> List.map _.Exception.Message

        Assert.Equal<_ list>(["job error"], messages)
    }
