module FSharp.Compiler.Service.Tests.WarnScopeTests

open Xunit
open FsUnit
open FSharp.Compiler.Service.Tests.Common

let sourceForWarnScopesInScript =
    """
module A
match None with None -> ()
#nowarn "25"
match None with None -> ()
#warnon "25"
match None with None -> ()
#nowarn "25"
match None with None -> ()
    """

[<Fact>]
let WarnScopesInScript () =
    let file = "WarnScopesInScript.fsx"
    let parseResult, typeCheckResults = parseAndCheckScript(file, sourceForWarnScopesInScript)
    printfn "parse errors:"
    for diag in parseResult.Diagnostics do
        printfn $"{diag.Range}: {diag.ErrorNumber}: {diag.Message}"
    printfn "type check errors:"
    for diag in typeCheckResults.Diagnostics do
        printfn $"{diag.Range}: {diag.ErrorNumber}: {diag.Message}"
    printfn "typeCheckResults.Diagnostics.Length = %d" typeCheckResults.Diagnostics.Length
    Assert.Equal(parseResult.Diagnostics.Length, 0)
    Assert.Equal(typeCheckResults.Diagnostics.Length, 2)
    Assert.Equal(typeCheckResults.Diagnostics[0].ErrorNumber, 25)
    Assert.Equal(typeCheckResults.Diagnostics[1].ErrorNumber, 25)
    Assert.Equal(typeCheckResults.Diagnostics[0].Range.StartLine, 3)
    Assert.Equal(typeCheckResults.Diagnostics[1].Range.StartLine, 7)
    


