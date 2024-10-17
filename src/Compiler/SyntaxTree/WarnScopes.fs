// Copyright (c) Microsoft Corporation.  All Rights Reserved.  See License.txt in the project root for license information.

namespace FSharp.Compiler

open FSharp.Compiler.UnicodeLexing
open FSharp.Compiler.Text
open FSharp.Compiler.Text.Position
open FSharp.Compiler.Text.Range
open FSharp.Compiler.Diagnostics
open FSharp.Compiler.DiagnosticsLogger
open FSharp.Compiler.Features
open Internal.Utilities.Library
open System
open System.Text.RegularExpressions

[<RequireQualifiedAccess>]
module internal WarnScopes =

    // The keys into the BufferLocalStore used to hold the warn scopes and related data
    let private warnScopeKey = "WarnScopes"
    let private lineMapKey = "WarnScopes.LineMaps"

    // *************************************
    // Collect the line directives to correctly interact with them
    // *************************************

    let private getLineMap (lexbuf: Lexbuf) =
        if not <| lexbuf.BufferLocalStore.ContainsKey lineMapKey then
            lexbuf.BufferLocalStore.Add(lineMapKey, LineMap.Empty)

        lexbuf.BufferLocalStore[lineMapKey] :?> LineMap

    let private setLineMap (lexbuf: Lexbuf) lineMap =
        lexbuf.BufferLocalStore[lineMapKey] <- LineMap lineMap

    let RegisterLineDirective (lexbuf, previousFileIndex, fileIndex, line: int) =
        let (LineMap lineMap) = getLineMap lexbuf

        if not <| lineMap.ContainsKey fileIndex then
            lineMap
            |> Map.add fileIndex previousFileIndex
            |> Map.add previousFileIndex previousFileIndex // to flag that it contains a line directive
            |> setLineMap lexbuf

        ignore line // for now

    // *************************************
    // Collect the warn scopes during lexing
    // *************************************

    let private getWarnScopes (lexbuf: Lexbuf) =
        if not <| lexbuf.BufferLocalStore.ContainsKey warnScopeKey then
            lexbuf.BufferLocalStore.Add(warnScopeKey, WarnScopeMap Map.empty)

        lexbuf.BufferLocalStore[warnScopeKey] :?> WarnScopeMap

    [<RequireQualifiedAccess>]
    type private WarnDirective =
        | Nowarn of int * range
        | Warnon of int * range

    let private getNumber (langVersion: LanguageVersion) m (ns: string) =
        let argFeature = LanguageFeature.ParsedHashDirectiveArgumentNonQuotes

        let removeQuotes (s: string) =
            if s.StartsWithOrdinal "\"" && s.EndsWithOrdinal "\"" then
                if s.StartsWithOrdinal "\"\"\"" && s.EndsWithOrdinal "\"\"\"" then
                    Some(s.Substring(3, s.Length - 6))
                else
                    Some(s.Substring(1, s.Length - 2))
            elif tryCheckLanguageFeatureAndRecover langVersion argFeature m then
                Some s
            else
                None

        let removePrefix (s: string) =
            if (s.StartsWithOrdinal "FS" && langVersion.SupportsFeature argFeature) then
                Some(s.Substring 2, s)
            else
                Some(s, s)

        let parseInt (s: string, displ) =
            match System.Int32.TryParse s with
            | true, i -> Some i
            | false, _ ->
                warning (Error(FSComp.SR.buildInvalidWarningNumber displ, m))
                None

        ns |> removeQuotes |> Option.bind removePrefix |> Option.bind parseInt

    let private regex =
        Regex(" *#(nowarn|warnon)(?: +([^ \r\n/;]+))*(?: *(?:;;|\\/\\/).*)?", RegexOptions.Compiled ||| RegexOptions.CultureInvariant)

    let private getDirectives langVersion text m =
        let mkDirective (directiveId: string) (m: range) (c: Capture) =
            let argRange =
                withEnd (mkPos m.StartLine (c.Index + c.Length)) (shiftStart 0 c.Index m)

            match directiveId, getNumber langVersion argRange c.Value with
            | "nowarn", Some n -> Some(WarnDirective.Nowarn(n, argRange))
            | "warnon", Some n -> Some(WarnDirective.Warnon(n, argRange))
            | _, Some n -> failwith $"getDirectives: unexpected directive id {directiveId}"
            | _, None -> None

        let mGroups = (regex.Match text).Groups
        let dIdent = mGroups[1].Value
        let argCaptures = mGroups[2].Captures

        if dIdent = "warnon" then
            checkLanguageFeatureError langVersion LanguageFeature.ScopedNowarn m

        if argCaptures.Count = 0 then
            errorR (Error(FSComp.SR.lexWarnDirectiveMustHaveArgs (), m))

        [ for c in argCaptures -> c ] |> List.choose (mkDirective dIdent m)

    let private index (fileIndex, warningNumber) =
        (int64 fileIndex <<< 32) + int64 warningNumber

    let private warnNumFromIndex (idx: int64) = idx &&& 0xFFFFFFFFL

    let private getScopes idx warnScopes =
        Map.tryFind idx warnScopes |> Option.defaultValue []

    let private mkScope (m1: range) (m2: range) =
        mkFileIndexRange m1.FileIndex m1.Start m2.End

    let private processWarnDirective (langVersion: LanguageVersion) (WarnScopeMap warnScopes) (wd: WarnDirective) =
        match wd with
        | WarnDirective.Nowarn(n, m) ->
            let idx = index (m.FileIndex, n)

            match getScopes idx warnScopes with
            | WarnScope.OpenOn m' :: t -> warnScopes.Add(idx, WarnScope.On(mkScope m' m) :: t)
            | WarnScope.OpenOff m' :: _
            | WarnScope.On m' :: _ ->
                if langVersion.SupportsFeature LanguageFeature.ScopedNowarn then
                    informationalWarning (Error(FSComp.SR.lexWarnDirectivesMustMatch ("#nowarn", m'.StartLine), m))

                warnScopes
            | scopes -> warnScopes.Add(idx, WarnScope.OpenOff(mkScope m m) :: scopes)
        | WarnDirective.Warnon(n, m) ->
            let idx = index (m.FileIndex, n)

            match getScopes idx warnScopes with
            | WarnScope.OpenOff m' :: t -> warnScopes.Add(idx, WarnScope.Off(mkScope m' m) :: t)
            | WarnScope.OpenOn m' :: _
            | WarnScope.Off m' :: _ ->
                warning (Error(FSComp.SR.lexWarnDirectivesMustMatch ("#warnon", m'.EndLine), m))
                warnScopes
            | scopes -> warnScopes.Add(idx, WarnScope.OpenOn(mkScope m m) :: scopes)
        |> WarnScopeMap

    let ParseAndRegisterWarnDirective (lexbuf: Lexbuf) =
        let (LineMap lineMap) = getLineMap lexbuf
        let convert (p: Internal.Utilities.Text.Lexing.Position) = mkPos p.Line p.Column
        let idx = lexbuf.StartPos.FileIndex
        let idx = lineMap.TryFind idx |> Option.defaultValue idx
        let m = mkFileIndexRange idx (convert lexbuf.StartPos) (convert lexbuf.EndPos)
        let text = Lexbuf.LexemeString lexbuf
        let directives = getDirectives lexbuf.LanguageVersion text m

        let warnScopes =
            (getWarnScopes lexbuf, directives)
            ||> List.fold (processWarnDirective lexbuf.LanguageVersion)

        lexbuf.BufferLocalStore[warnScopeKey] <- warnScopes

    // *************************************
    // Move the warnscope data to diagnosticOptions
    // *************************************

    let MergeInto (diagnosticOptions: FSharpDiagnosticOptions) (lexbuf: Lexbuf) =
        let (WarnScopeMap warnScopes) = getWarnScopes lexbuf
        let (LineMap lineMap) = getLineMap lexbuf

        lock diagnosticOptions (fun () ->
            let (WarnScopeMap current) = diagnosticOptions.WarnScopes
            let warnScopes' = Map.fold (fun wss idx ws -> Map.add idx ws wss) current warnScopes
            diagnosticOptions.WarnScopes <- WarnScopeMap warnScopes'
            let (LineMap clm) = diagnosticOptions.LineMap
            let lineMap' = Map.fold (fun lms idx oidx -> Map.add idx oidx lms) clm lineMap
            diagnosticOptions.LineMap <- LineMap lineMap')

        lexbuf.BufferLocalStore.Remove warnScopeKey |> ignore
        lexbuf.BufferLocalStore.Remove lineMapKey |> ignore

    // *************************************
    // Apply the warn scopes after lexing
    // *************************************

    /// true if m1 contains the start of m2 (#line directives can appear in the middle of an error range)
    let private contains (m2: range) (m1: range) =
        m2.StartLine > m1.StartLine && m2.StartLine < m1.EndLine

    let private isEnclosingWarnonScope m scope =
        match scope with
        | WarnScope.On wm when contains m wm -> true
        | WarnScope.OpenOn wm when m.StartLine > wm.StartLine -> true
        | _ -> false

    let private isEnclosingNowarnScope m scope =
        match scope with
        | WarnScope.Off wm when contains m wm -> true
        | WarnScope.OpenOff wm when m.StartLine > wm.StartLine -> true
        | _ -> false

    let private isOffScope scope =
        match scope with
        | WarnScope.Off _
        | WarnScope.OpenOff _ -> true
        | _ -> false

    let IsWarnon (diagnosticOptions: FSharpDiagnosticOptions) warningNumber (mo: range option) =
        let (WarnScopeMap warnScopes) = diagnosticOptions.WarnScopes
        let (LineMap lineMap) = diagnosticOptions.LineMap

        match mo, diagnosticOptions.FSharp9CompatibleNowarn with
        | Some m, false ->
            if lineMap.ContainsKey m.FileIndex then
                false
            else
                let scopes = getScopes (index (m.FileIndex, warningNumber)) warnScopes
                List.exists (isEnclosingWarnonScope m) scopes
        | _ -> false

    let IsNowarn (diagnosticOptions: FSharpDiagnosticOptions) warningNumber (mo: range option) =
        let (WarnScopeMap warnScopes) = diagnosticOptions.WarnScopes
        let (LineMap lineMap) = diagnosticOptions.LineMap

        match mo, diagnosticOptions.FSharp9CompatibleNowarn with
        | Some m, false ->
            match lineMap.TryFind m.FileIndex with
            | None ->
                let scopes = getScopes (index (m.FileIndex, warningNumber)) warnScopes
                List.exists (isEnclosingNowarnScope m) scopes
            | Some fileIndex -> // file has #line directives
                let scopes = getScopes (index (fileIndex, warningNumber)) warnScopes
                List.exists isOffScope scopes
        | _ -> warnScopes |> Map.exists (fun idx _ -> warnNumFromIndex idx = warningNumber)