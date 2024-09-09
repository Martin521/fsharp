// Copyright (c) Microsoft Corporation.  All Rights Reserved.  See License.txt in the project root for license information.

namespace FSharp.Compiler

open FSharp.Compiler.Diagnostics
open FSharp.Compiler.Text

module internal WarnScopes =

    val ParseAndSaveWarnDirectiveLine: lexbuf: UnicodeLexing.Lexbuf -> unit

    val FromLexbuf: lexbuf: UnicodeLexing.Lexbuf -> WarnScopeMap

    val MergeInto: FSharpDiagnosticOptions -> WarnScopeMap -> unit
    
    val Print: string -> WarnScopeMap -> unit

    val IsWarnon: WarnScopeMap -> warningNumber: int -> mo: range option -> bool

    val IsNowarn: WarnScopeMap -> warningNumber: int -> mo: range option -> bool -> bool

