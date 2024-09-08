// Copyright (c) Microsoft Corporation.  All Rights Reserved.  See License.txt in the project root for license information.

namespace FSharp.Compiler

open FSharp.Compiler.Diagnostics
open FSharp.Compiler.Text

module internal WarnScopes =

    val ParseAndSaveWarnDirectiveLine: lexbuf: UnicodeLexing.Lexbuf -> unit

    val FromLexbuf: lexbuf: UnicodeLexing.Lexbuf -> WarnScopes

    val MergeInto: FSharpDiagnosticOptions -> WarnScopes -> unit
    
    val Print: string -> WarnScopes -> unit

    val IsWarnon: WarnScopes -> warningNumber: int -> mo: range option -> bool

    val IsNowarn: WarnScopes -> warningNumber: int -> mo: range option -> bool -> bool

