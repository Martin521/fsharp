// Copyright (c) Microsoft Corporation.  All Rights Reserved.  See License.txt in the project root for license information.
namespace CompilerDirectives

open Xunit
open FSharp.Test.Compiler

module Nowarn =

    let private warning25Text = "Incomplete pattern matches on this expression. For example, the value 'Some (_)' may indicate a case not covered by the pattern(s)."
    let private warning44Text = "This construct is deprecated"
    
    let private sourceForWarningIsSuppressed = """
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
    let warningIsSuppressedBetweenNowarnAndWarnonDirectives () =
        FSharp sourceForWarningIsSuppressed
        |> withLangVersionPreview
        |> compile
        |> withDiagnostics [
            Warning 25, Line 3, Col 7, Line 3, Col 11, warning25Text
            Warning 25, Line 7, Col 7, Line 7, Col 11, warning25Text
        ]

    let private sigSourceForWarningIsSuppressedInSigFile = """
module A
open System
[<Obsolete>]
type T = class end
type T2 = T
#nowarn "44"
type T3 = T
#warnon "44"
type T4 = T
#nowarn "44"
type T5 = T
    """
    
    let private sourceForWarningIsSuppressedInSigFile = """
module A
#nowarn "44"
open System
[<Obsolete>]
type T = class end
type T2 = T
type T3 = T
type T4 = T
type T5 = T
    """
    
    [<Fact>]
    let warningIsSuppressedBetweenNowarnAndWarnonDirectivesInASignatureFile () =
        Fsi sigSourceForWarningIsSuppressedInSigFile
        |> withAdditionalSourceFile (FsSource sourceForWarningIsSuppressedInSigFile)
        |> withLangVersionPreview
        |> compile
        |> withDiagnostics [
            Warning 44, Line 6, Col 11, Line 6, Col 12, warning44Text
            Warning 44, Line 10, Col 11, Line 10, Col 12, warning44Text
        ]

    let private scriptForWarningIsSuppressed = """

match None with None -> ()
#nowarn "25"
match None with None -> ()
#warnon "25"
match None with None -> ()
#nowarn "25"
match None with None -> ()
    """
    
    [<Fact>]
    let ``warning is suppressed between nowarn and warnon directives (in script)`` () =
        Fsx scriptForWarningIsSuppressed
        |> withLangVersionPreview
        |> compile
        |> withDiagnostics [
            // These warnings should appear if we make scripts spec-compliant
            // Warning 25, Line 3, Col 7, Line 3, Col 11, matchNoneErrorMessage
            // Warning 25, Line 7, Col 7, Line 7, Col 11, matchNoneErrorMessage
        ]

    [<InlineData("8.0")>]
    [<InlineData("9.0")>]
    [<Theory>]
    let ``#nowarn - errors`` (languageVersion) =

        FSharp """
#nowarn "988"
#nowarn FS
#nowarn FSBLAH
#nowarn ACME 
#nowarn "FS"
#nowarn "FSBLAH"
#nowarn "ACME"
        """
        |> withLangVersion languageVersion
        |> asExe
        |> compile
        |> shouldFail
        |> withDiagnostics [
            if languageVersion = "8.0" then
                (Warning 203, Line 6, Col 1, Line 6, Col 13, "Invalid warning number 'FS'")
                (Error 3350, Line 3, Col 9, Line 3, Col 11, "Feature '# directives with non-quoted string arguments' is not available in F# 8.0. Please use language version 9.0 or greater.")
                (Error 3350, Line 4, Col 9, Line 4, Col 15, "Feature '# directives with non-quoted string arguments' is not available in F# 8.0. Please use language version 9.0 or greater.")
                (Error 3350, Line 5, Col 9, Line 5, Col 13, "Feature '# directives with non-quoted string arguments' is not available in F# 8.0. Please use language version 9.0 or greater.")
            else
                (Warning 203, Line 3, Col 1, Line 3, Col 11, "Invalid warning number 'FS'")
                (Warning 203, Line 6, Col 1, Line 6, Col 13, "Invalid warning number 'FS'")
            ]

    [<InlineData("8.0")>]
    [<InlineData("9.0")>]
    [<Theory>]
    let ``#nowarn - errors - inline`` (languageVersion) =

        FSharp """
#nowarn "988"
#nowarn FS FSBLAH ACME "FS" "FSBLAH" "ACME"
        """
        |> withLangVersion languageVersion
        |> asExe
        |> compile
        |> shouldFail
        |> withDiagnostics [
            if languageVersion = "8.0" then
                (Warning 203, Line 3, Col 1, Line 3, Col 44, "Invalid warning number 'FS'")
                (Error 3350, Line 3, Col 9, Line 3, Col 11, "Feature '# directives with non-quoted string arguments' is not available in F# 8.0. Please use language version 9.0 or greater.")
                (Error 3350, Line 3, Col 12, Line 3, Col 18, "Feature '# directives with non-quoted string arguments' is not available in F# 8.0. Please use language version 9.0 or greater.")
                (Error 3350, Line 3, Col 19, Line 3, Col 23, "Feature '# directives with non-quoted string arguments' is not available in F# 8.0. Please use language version 9.0 or greater.")
            else
                (Warning 203, Line 3, Col 1, Line 3, Col 44, "Invalid warning number 'FS'")
            ]


    [<InlineData("8.0")>]
    [<InlineData("9.0")>]
    [<Theory>]
    let ``#nowarn - realcode`` (langVersion) =

        let compileResult =
            FSharp """
#nowarn 20 FS1104 "3391" "FS3221"

module Exception =
    exception ``Crazy@name.p`` of string

module Decimal =
    type T1 = { a : decimal }
    module M0 =
        type T1 = { a : int;}
    let x = { a = 10 }              // error - 'expecting decimal' (which means we are not seeing M0.T1)

module MismatchedYields =
    let collection () = [
        yield "Hello"
        "And this"
        ]
module DoBinding =
    let square x = x * x
    square 32
            """
            |> withLangVersion langVersion
            |> asExe
            |> compile

        if langVersion = "8.0" then
            compileResult
            |> shouldFail
            |> withDiagnostics [
                (Warning 1104, Line 5, Col 15, Line 5, Col 31, "Identifiers containing '@' are reserved for use in F# code generation")
                (Error 3350, Line 2, Col 9, Line 2, Col 11, "Feature '# directives with non-quoted string arguments' is not available in F# 8.0. Please use language version 9.0 or greater.")
                (Error 3350, Line 2, Col 12, Line 2, Col 18, "Feature '# directives with non-quoted string arguments' is not available in F# 8.0. Please use language version 9.0 or greater.")
                ]
        else
            compileResult
            |> shouldSucceed

