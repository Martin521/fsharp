// Copyright (c) Microsoft Corporation.  All Rights Reserved.  See License.txt in the project root for license information.

namespace Microsoft.FSharp.Core.CompilerServices

open Microsoft.FSharp.Collections
open Microsoft.FSharp.Core
open System
open System.Runtime.CompilerServices

/// Acts as a template for struct state machines introduced by __stateMachine, and also as a reflective implementation
[<Struct; NoComparison; NoEquality>]
type ResumableStateMachine<'Data> =

    /// When statically compiled, holds the data for the state machine
    [<DefaultValue(false)>]
    val mutable Data: 'Data

    /// When statically compiled, holds the continuation goto-label further execution of the state machine
    [<DefaultValue(false)>]
    val mutable ResumptionPoint: int

    /// <summary>Represents the delegated runtime continuation for a resumable state machine created dynamically</summary>
    /// <remarks>This field is removed from state machines generated using '__stateMachine'. Resumable code
    /// used in state machines which accesses this field will raise a runtime exception.</remarks>
    [<DefaultValue(false)>]
    val mutable ResumptionDynamicInfo: ResumptionDynamicInfo<'Data>

    interface IResumableStateMachine<'Data>

    interface IAsyncStateMachine

and 
    IResumableStateMachine<'Data> =
    /// Get the resumption point of the state machine
    abstract ResumptionPoint: int

    /// Copy-out or copy-in the data of the state machine
    abstract Data: 'Data with get, set

/// Represents the delegated runtime continuation of a resumable state machine created dynamically
and
    [<AbstractClass>]
    ResumptionDynamicInfo<'Data> =

    /// Create dynamic information for a state machine
    new: initial: ResumptionFunc<'Data> -> ResumptionDynamicInfo<'Data>
    
    /// The continuation of the state machine
    member ResumptionFunc: ResumptionFunc<'Data> with get, set 
    
    /// Additional data associated with the state machine
    member ResumptionData: objnull with get, set 

    /// Executes the MoveNext implementation of the state machine
    abstract MoveNext: machine: byref<ResumableStateMachine<'Data>> -> unit

    /// Executes the SetStateMachine implementation of the state machine
    abstract SetStateMachine: machine: byref<ResumableStateMachine<'Data>> * machineState: IAsyncStateMachine -> unit

/// Represents the runtime continuation of a resumable state machine created dynamically
and ResumptionFunc<'Data> = delegate of byref<ResumableStateMachine<'Data>> -> bool

/// A special compiler-recognised delegate type for specifying blocks of resumable code
/// with access to the state machine.
type ResumableCode<'Data, 'T> = delegate of byref<ResumableStateMachine<'Data>> -> bool

/// Contains functions for composing resumable code blocks
[<Microsoft.FSharp.Core.RequireQualifiedAccess>]
module ResumableCode =

    /// Sequences one section of resumable code after another
    val inline Combine: code1: ResumableCode<'Data, unit> * code2: ResumableCode<'Data, 'T> -> ResumableCode<'Data, 'T>

    /// Creates resumable code whose definition is a delayed function
    val inline Delay: f: (unit -> ResumableCode<'Data, 'T>) -> ResumableCode<'Data, 'T>

    /// Specifies resumable code which iterates an input sequence
    val inline For: sequence: seq<'T> * body: ('T -> ResumableCode<'Data, unit>) -> ResumableCode<'Data, unit>

    /// Specifies resumable code which iterates yields
    val inline Yield: unit -> ResumableCode<'Data, unit>

    /// Specifies resumable code which executes with try/finally semantics
    val inline TryFinally: body: ResumableCode<'Data, 'T> * compensation: ResumableCode<'Data,unit> -> ResumableCode<'Data, 'T>

    /// Specifies resumable code which executes with try/finally semantics
    val inline TryFinallyAsync: body: ResumableCode<'Data, 'T> * compensation: ResumableCode<'Data,unit> -> ResumableCode<'Data, 'T>

    /// Specifies resumable code which executes with try/with semantics
    val inline TryWith: body: ResumableCode<'Data, 'T> * catch: (exn -> ResumableCode<'Data, 'T>) -> ResumableCode<'Data, 'T>

    /// Specifies resumable code which executes with 'use' semantics
    val inline Using: resource: 'Resource * body: ('Resource -> ResumableCode<'Data, 'T>) -> ResumableCode<'Data, 'T> when 'Resource :> IDisposable|null

    /// Specifies resumable code which executes a loop
    val inline While: [<InlineIfLambda>] condition: (unit -> bool) * body: ResumableCode<'Data, unit> -> ResumableCode<'Data, unit>

    /// Specifies resumable code which does nothing
    val inline Zero: unit -> ResumableCode<'Data, unit>

    /// The dynamic implementation of the corresponding operation. This operation should not be used directly.
    val CombineDynamic: sm: byref<ResumableStateMachine<'Data>> * code1: ResumableCode<'Data, unit> * code2: ResumableCode<'Data, 'T> -> bool

    /// The dynamic implementation of the corresponding operation. This operation should not be used directly.
    val WhileDynamic: sm: byref<ResumableStateMachine<'Data>> * condition: (unit -> bool) * body: ResumableCode<'Data, unit> -> bool

    /// The dynamic implementation of the corresponding operation. This operation should not be used directly.
    val TryFinallyAsyncDynamic: sm: byref<ResumableStateMachine<'Data>> * body: ResumableCode<'Data, 'T> * compensation: ResumableCode<'Data,unit> -> bool

    /// The dynamic implementation of the corresponding operation. This operation should not be used directly.
    val TryWithDynamic: sm: byref<ResumableStateMachine<'Data>> * body: ResumableCode<'Data, 'T> * handler: (exn -> ResumableCode<'Data, 'T>) -> bool

    /// The dynamic implementation of the corresponding operation. This operation should not be used directly.
    val YieldDynamic: sm: byref<ResumableStateMachine<'Data>> -> bool

/// Defines the implementation of the MoveNext method for a struct state machine.
type MoveNextMethodImpl<'Data> = delegate of byref<ResumableStateMachine<'Data>> -> unit

/// Defines the implementation of the SetStateMachine method for a struct state machine.
type SetStateMachineMethodImpl<'Data> = delegate of byref<ResumableStateMachine<'Data>> * IAsyncStateMachine -> unit

/// Defines the implementation of the code run after the creation of a struct state machine.
type AfterCode<'Data, 'Result> = delegate of byref<ResumableStateMachine<'Data>> -> 'Result

/// Contains compiler intrinsics related to the definition of state machines.
module StateMachineHelpers = 

    /// <summary>
    /// Indicates a named debug point arising from the context of inlined code.
    /// </summary>
    /// <remarks>
    /// Only a limited range of debug point names are supported.
    ///
    /// If the debug point name is the empty string then the range used for the debug point will be
    /// the range of the outermost expression prior to inlining.
    ///
    /// If the debug point name is <c>ForLoop.InOrToKeyword</c> and the code was ultimately
    /// from a <c>for .. in .. do</c> or <c>for .. = .. to .. do</c> construct in a computation expression,
    /// de-sugared to an inlined <c>builder.For</c> call, then the name "ForLoop.InOrToKeyword" can be used.
    /// The range of the debug point will be precisely the range of the <c>in</c> or <c>to</c> keyword.
    ///
    /// If the name doesn't correspond to a known debug point arising from the original source context, then
    /// an opt-in warning 3514 is emitted, and the range used for the debug point will be
    /// the range of the root expression prior to inlining.
    /// </remarks>
    [<MethodImpl(MethodImplOptions.NoInlining)>]
    val __debugPoint: string -> unit

    /// <summary>
    /// When used in a conditional, statically determines whether the 'then' branch
    /// represents valid resumable code and provides an alternative implementation
    /// if not.
    /// </summary>
    [<MethodImpl(MethodImplOptions.NoInlining)>]
    val __useResumableCode<'T> : bool 

    /// <summary>
    /// Indicates a resumption point within resumable code
    /// </summary>
    [<MethodImpl(MethodImplOptions.NoInlining)>]
    val __resumableEntry: unit -> int option

    /// <summary>
    /// Indicates to jump to a resumption point within resumable code.
    /// This may be the first statement in a MoveNextMethodImpl.
    /// The integer must be a valid resumption point within this resumable code.
    /// </summary>
    /// <param name="programLabel"></param>
    [<MethodImpl(MethodImplOptions.NoInlining)>]
    val __resumeAt : programLabel: int -> 'T

    /// <summary>
    /// Statically generates a closure struct type based on ResumableStateMachine,
    /// At runtime an instance of the new struct type is populated and 'afterMethod' is called
    /// to consume it.
    /// </summary>
    ///
    /// <remarks>
    /// At compile-time, the ResumableStateMachine type guides the generation of a new struct type by the F# compiler
    /// with closure-capture fields in a way similar to an object expression. 
    /// Any mention of the ResumableStateMachine type in any the 'methods' is rewritten to this
    /// fresh struct type.  The 'methods' are used to implement the interfaces on ResumableStateMachine and are also rewritten.
    /// The 'after' method is then executed and must eliminate the ResumableStateMachine. For example,
    /// its return type must not include ResumableStateMachine.
    /// </remarks>
    /// <param name="moveNextMethod">Gives the implementation of the MoveNext method on IAsyncStateMachine.</param>
    /// <param name="setStateMachineMethod">Gives the implementation of the SetStateMachine method on IAsyncStateMachine.</param>
    /// <param name="afterCode">Gives code to execute after the generation of the state machine and to produce the final result.</param>
    [<MethodImpl(MethodImplOptions.NoInlining)>]
    val __stateMachine<'Data, 'Result> :
        moveNextMethod: MoveNextMethodImpl<'Data> -> 
        setStateMachineMethod: SetStateMachineMethodImpl<'Data> -> 
        afterCode: AfterCode<'Data, 'Result> 
            -> 'Result

/// <summary>Adding this attribute to the method adjusts the processing of some generic methods
/// during overload resolution.</summary>
///
/// <remarks>During overload resolution, caller arguments are matched with called arguments
/// to extract type information. By default, when the caller argument type is unconstrained (for example
/// a simple value <c>x</c> without known type information), and a method qualifies for
/// lambda constraint propagation, then member trait constraints from a method overload
/// are eagerly applied to the caller argument type. This causes that overload to be preferred,
/// regardless of other method overload resolution rules. Using this attribute suppresses this behaviour. 
/// </remarks>
///
/// <example>
/// Consider the following overloads:
/// <code>
/// type OverloadsWithSrtp() =
///     [&lt;NoEagerConstraintApplicationAttribute&gt;]
///     static member inline SomeMethod&lt; ^T when ^T : (member Number: int) &gt; (x: ^T, f: ^T -> int) = 1
///     static member SomeMethod(x: 'T list, f: 'T list -> int) = 2
/// 
/// let inline f x = 
///     OverloadsWithSrtp.SomeMethod (x, (fun a -> 1)) 
/// </code>
/// With the attribute, the overload resolution fails, because both members are applicable.
/// Without the attribute, the overload resolution succeeds, because the member constraint is
/// eagerly applied, making the second member non-applicable.  
/// </example>
/// <category>Attributes</category>
[<AttributeUsage (AttributeTargets.Method,AllowMultiple=false)>]  
[<Sealed>]
type NoEagerConstraintApplicationAttribute =
    inherit Attribute

    /// <summary>Creates an instance of the attribute</summary>
    /// <returns>NoEagerConstraintApplicationAttribute</returns>
    new : unit -> NoEagerConstraintApplicationAttribute
