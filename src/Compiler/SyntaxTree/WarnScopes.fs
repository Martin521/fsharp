// Copyright (c) Microsoft Corporation.  All Rights Reserved.  See License.txt in the project root for license information.

namespace FSharp.Compiler

open FSharp.Compiler.UnicodeLexing
open FSharp.Compiler.Text
open FSharp.Compiler.Text.Position
open FSharp.Compiler.Text.Range
open FSharp.Compiler.Diagnostics
open System.Text.RegularExpressions

[<RequireQualifiedAccess>]
module internal WarnScopes =

    // The key into the BufferLocalStore used to hold the warn scopes
    let private warnScopeKey = "WarnScopes"
    
    let private index(fileIndex, warningNumber) = (int64 fileIndex <<< 32) + int64 warningNumber
    
    [<RequireQualifiedAccess>]
    type private WarnDirective =
        | Nowarn of int * range
        | Warnon of int * range

    let private getWarningNumber (s: string) =
        let s = if s.StartsWith "\"" && s.EndsWith "\"" then s.Substring(1, s.Length - 2) else s
        let s = if s.StartsWith "FS" then s[2..] else s
        match System.Int32.TryParse s with
        | true, i -> Some i
        | false, _ -> None
        
    let private getWarnDirective (directiveId: string) (m: range) (c: Capture) =
        let argRange() = Range.withEnd (mkPos m.StartLine (c.Index + c.Length - 1)) (Range.shiftStart 0 c.Index m)
        match directiveId, getWarningNumber c.Value with
        | "nowarn", Some n -> Some (WarnDirective.Nowarn(n, argRange()))
        | "warnon", Some n -> Some (WarnDirective.Warnon(n, argRange()))
        | _ -> None

    let private regex =
        Regex(" *#(nowarn|warnon)(?: +([^ ]+))*(?:\n|\r\n)", RegexOptions.Compiled ||| RegexOptions.CultureInvariant)

    let private getDirectives text m =
        let mGroups = (regex.Match text).Groups
        let dIdent = mGroups[1].Value
        [for c in mGroups[2].Captures -> c] |> List.choose (getWarnDirective dIdent m)

    let private tryFindOpen idx (warnScopes: WarnScopes) =
        warnScopes.openEnded.TryFind idx
    
    let private getClosed idx (warnScopes: WarnScopes) =
        warnScopes.closed.TryFind idx |> Option.defaultValue []

    let private addClosed idx warnScopeList (warnScopes: WarnScopes) =
        let newScopesForIdx =
            match warnScopes.closed.TryFind idx with
            | Some wss -> warnScopeList @ wss
            | None -> warnScopeList
        {warnScopes with closed = warnScopes.closed.Add(idx, newScopesForIdx)}

    let private addOpen idx warnScope (warnScopes: WarnScopes) =
        if warnScopes.openEnded.ContainsKey idx then failwith "unexpected duplicate open warnScope"
        {warnScopes with openEnded = warnScopes.openEnded.Add(idx, warnScope)}

    let private removeOpen idx (warnScopes: WarnScopes) =
        {warnScopes with openEnded = warnScopes.openEnded.Remove idx}        

    let private mkScope (m1: range) (m2: range) = mkFileIndexRange m1.FileIndex m1.Start m2.End
        
    let private processWarnDirective (warnScopes: WarnScopes) (wd: WarnDirective) =
        match wd with
        | WarnDirective.Nowarn(n, m) ->
            let idx = index(m.FileIndex, n)
            match tryFindOpen idx warnScopes with
            | Some (WarnScope.Off _) -> warnScopes
            | Some (WarnScope.On m') ->
                warnScopes
                |> addClosed idx [WarnScope.On(mkScope m' m)]
                |> removeOpen idx
            | None -> warnScopes |> addOpen idx (WarnScope.Off(mkScope m m))
        | WarnDirective.Warnon(n, m) ->
            let idx = index(m.FileIndex, n)
            match tryFindOpen idx warnScopes with
            | Some (WarnScope.On _) -> warnScopes
            | Some (WarnScope.Off m') ->
                warnScopes
                |> addClosed idx [WarnScope.Off(mkScope m' m)]
                |> removeOpen idx
            | None -> warnScopes |> addOpen idx (WarnScope.On(mkScope m m))

    let FromLexbuf (lexbuf: Lexbuf) : WarnScopes =
        if not <| lexbuf.BufferLocalStore.ContainsKey warnScopeKey then
            let empty = {WarnScopes.closed = Map.empty; WarnScopes.openEnded = Map.empty}
            lexbuf.BufferLocalStore.Add(warnScopeKey, empty)
        lexbuf.BufferLocalStore[warnScopeKey] :?> WarnScopes
    
    /// true if m1 contains m2
    let private contains (m2: range) (m1: range) =
        m2.StartLine > m1.StartLine && m2.EndLine < m1.EndLine

    let ParseAndSaveWarnDirectiveLine (lexbuf: Lexbuf) =
        let convert (p: Internal.Utilities.Text.Lexing.Position) = mkPos p.Line p.Column
        let m = mkFileIndexRange lexbuf.StartPos.FileIndex (convert lexbuf.StartPos) (convert lexbuf.EndPos)
        let text = Lexbuf.LexemeString lexbuf
        let directives = getDirectives text m
        if not <| lexbuf.BufferLocalStore.ContainsKey warnScopeKey then
            let empty = {WarnScopes.closed = Map.empty; WarnScopes.openEnded = Map.empty}
            lexbuf.BufferLocalStore.Add(warnScopeKey, empty)
        let warnScopes = (FromLexbuf lexbuf, directives) ||> List.fold processWarnDirective
        lexbuf.BufferLocalStore[warnScopeKey] <- warnScopes
    
    let MergeInto (diagnosticOptions: FSharpDiagnosticOptions) (warnScopes: WarnScopes) =
        let withCombinedClosed =
            Map.fold (fun wss idx ws -> addClosed idx ws wss) diagnosticOptions.WarnScopes warnScopes.closed
        let withCombinedClosedAndOpen =
            Map.fold (fun wss idx ws -> addOpen idx ws wss) withCombinedClosed warnScopes.openEnded
        diagnosticOptions.WarnScopes <- withCombinedClosedAndOpen
    
    let Print header (warnScopes: WarnScopes) =
        let fIdx (idx: int64) = int (idx >>> 32)
        let nIdx (idx: int64) = int(idx - index(fIdx idx, 0))
        let fileName idx = idx |> fIdx |> FileIndex.fileOfFileIndex |> System.IO.Path.GetFileName
        let mutable lines = [header]
        let p s = lines <- s::lines
        p "warn scopes (closed):"
        warnScopes.closed |> Map.iter (fun idx wss ->
            p $"  {fileName idx} {nIdx idx}"
            wss |> List.iter (fun ws -> p $"      {ws}")
            )
        p "warn scopes (open):"
        warnScopes.openEnded |> Map.iter (fun idx ws -> p $"  {fileName idx} {nIdx idx} {ws}")
        System.IO.File.AppendAllLines("warnscopes.txt", List.rev lines)
        
    let IsWarnon (warnScopes: WarnScopes) warningNumber (mo: range option) =
        match mo with
        | None -> false
        | Some m ->
            let idx = index(m.FileIndex, warningNumber)
            match tryFindOpen idx warnScopes with
            | Some (WarnScope.On wm) when m.StartLine > wm.StartLine -> true
            | _ ->
                let closed = getClosed idx warnScopes
                let isEnclosingWarnonScope scope =
                    match scope with
                    | WarnScope.On wm when contains m wm -> true
                    | _ -> false
                List.exists isEnclosingWarnonScope closed

    /// compatible = compatible with earlier (< F#9.0) inconsistent interaction between #line and #nowarn
    let IsNowarn (warnScopes: WarnScopes) warningNumber (mo: range option) compatible =
        match mo with
        | None -> false
        | Some m ->
            let idx = index(m.FileIndex, warningNumber)
            match tryFindOpen idx warnScopes with
            | Some (WarnScope.Off wm) when compatible || m.StartLine > wm.StartLine -> true
            | _ ->
                let closed = getClosed idx warnScopes
                let isEnclosingNowarnScope scope =
                    match scope with
                    | WarnScope.Off wm when contains m wm -> true
                    | _ -> false
                List.exists isEnclosingNowarnScope closed
