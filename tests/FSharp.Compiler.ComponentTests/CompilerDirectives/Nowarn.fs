// Copyright (c) Microsoft Corporation.  All Rights Reserved.  See License.txt in the project root for license information.
namespace CompilerDirectives

open Xunit
open FSharp.Test.Compiler

module Nowarn =

    let warning20Text = "The result of this expression has type 'string' and is implicitly ignored. Consider using 'ignore' to discard this value explicitly, e.g. 'expr |> ignore', or 'let' to bind the result to a name, e.g. 'let result = expr'."
    let private warning25Text = "Incomplete pattern matches on this expression. For example, the value 'Some (_)' may indicate a case not covered by the pattern(s)."
    let private warning44Text = "This construct is deprecated"
    

    let consistencySource1a = """
module A
#nowarn "20"
#line 5 "xyz1a.fs"
""
        """

    // need different file names here because of global table
    let consistencySource1b = """
module A
#nowarn "20"
#line 5 "xyz1b.fs"
""
        """

    let consistencySource2a = """
module A
#nowarn "20"
#line 1 "xyz2a.fs"
""
        """

    let consistencySource2b = """
module A
#nowarn "20"
#line 1 "xyz2b.fs"
""
        """

    let consistencySource2c = """
module A
#nowarn "20"
#line 1 "xyz2c.fs"
""
        """


    [<Fact>]
    let inconsistentInteractionBetweenLineAndNowarn1 () =

        FSharp consistencySource1a
        |> withLangVersion90
        |> compile
        |> shouldSucceed

    [<Fact>]
    let inconsistentInteractionBetweenLineAndNowarn2 () =

        FSharp consistencySource2a
        |> withLangVersion90
        |> compile
        |> shouldSucceed

    [<Fact>]
    let consistentInteractionBetweenLineAndNowarn1 () =

        FSharp consistencySource1b
        |> withLangVersionPreview
        |> compile
        |> shouldSucceed

    [<Fact>]
    let consistentInteractionBetweenLineAndNowarn2 () =

        FSharp consistencySource2b
        |> withLangVersionPreview
        |> compile
        |> shouldSucceed

    [<Fact>]
    let consistentInteractionBetweenLineAndNowarn2AsError () =

        FSharp consistencySource2c
        |> withLangVersionPreview
        |> withOptions ["--warnaserror+"]
        |> compile
        |> shouldSucceed


    let doubleSemiSource = """
module A
#nowarn "20";;
""
#warnon "20"  // comment
""
        """

    [<Fact>]
    let acceptDoubleSemicolonAfterDirective () =

        FSharp doubleSemiSource
        |> withLangVersionPreview
        |> compile
        |> withDiagnostics [
            Warning 20, Line 6, Col 1, Line 6, Col 3, warning20Text
        ]


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
    let warningIsSuppressedBetweenNowarnAndWarnonInScript () =
        Fsx scriptForWarningIsSuppressed
        |> withLangVersionPreview
        |> compile
        |> withDiagnostics [
            Warning 25, Line 3, Col 7, Line 3, Col 11, warning25Text
            Warning 25, Line 7, Col 7, Line 7, Col 11, warning25Text
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
        |> shouldSucceed

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
        |> shouldSucceed


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
            |> shouldSucceed
        else
            compileResult
            |> shouldSucceed

