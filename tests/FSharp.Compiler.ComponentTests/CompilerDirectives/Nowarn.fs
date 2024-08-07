namespace CompilerDirectives

open Xunit
open FSharp.Test.Compiler

module Nowarn =

    let private matchNoneErrorMessage = "Incomplete pattern matches on this expression. For example, the value 'Some (_)' may indicate a case not covered by the pattern(s)."
    
    let private sourceForWarningIsSuppressed = """
module A
match None with None -> ()
#nowarn 25
match None with None -> ()
#warnon 25
match None with None -> ()
#nowarn 25
match None with None -> ()
    """
    
    [<Fact>]
    let ``warning is suppressed between nowarn and warnon directives`` () =
        FSharp sourceForWarningIsSuppressed
        |> withLangVersionPreview
        |> compile
        |> withDiagnostics [
            (Warning 25, Line 3, Col 7, Line 3, Col 11, matchNoneErrorMessage)
            (Warning 25, Line 7, Col 7, Line 7, Col 11, matchNoneErrorMessage)
        ]