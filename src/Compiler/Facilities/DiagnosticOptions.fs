// Copyright (c) Microsoft Corporation.  All Rights Reserved.  See License.txt in the project root for license information.

// This file contains simple types related to diagnostics that are made public in the
// FSharp.Compiler.Service API but which are also used throughout the
// F# compiler.
namespace FSharp.Compiler.Diagnostics

open FSharp.Compiler.Text

[<RequireQualifiedAccess>]
type WarnScope =
    | Off of range
    | On of range

[<RequireQualifiedAccess>]
type WarnScopes = {
    closed: Map<int64, WarnScope list>
    openEnded: Map<int64, WarnScope>
    }

[<RequireQualifiedAccess>]
type FSharpDiagnosticSeverity =
    | Hidden
    | Info
    | Warning
    | Error

type FSharpDiagnosticOptions =
    {
        WarnLevel: int
        GlobalWarnAsError: bool
        WarnOff: int list
        WarnOn: int list
        WarnAsError: int list
        WarnAsWarn: int list
        mutable WarnScopes: WarnScopes
    }

    static member Default =
        {
            WarnLevel = 3
            GlobalWarnAsError = false
            WarnOff = []
            WarnOn = []
            WarnAsError = []
            WarnAsWarn = []
            WarnScopes = {WarnScopes.closed = Map.empty; WarnScopes.openEnded = Map.empty}
        }

    member x.CheckXmlDocs =
        List.contains 3390 x.WarnOn && not (List.contains 3390 x.WarnOff)
