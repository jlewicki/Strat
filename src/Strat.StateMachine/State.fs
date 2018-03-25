namespace rec Strat.StateMachine

type StateId = string

[<NoComparison; ReferenceEquality>]
type State<'Data,'Message> = 
   
   | Root of
      Id: StateId *
      Handlers: StateHandler<'Data,'Message> * 
      InitialTransition: InitialTransition<'Data>
   
   | Interior of
      Id: StateId * 
      ParentId: StateId *
      Handlers: StateHandler<'Data,'Message> * 
      InitialTransition: InitialTransition<'Data>
   
   | Leaf of 
      Id: StateId * 
      ParentId: StateId *
      Handlers: StateHandler<'Data,'Message>
   /// A terminal state. The state machine cannot transition out of this state. 
   | Terminal of 
      Parent: StateId * 
      FromState: StateId *
      Reason: option<StopReason>
with
   static member TerminalStateId =  
      "[TerminalState]"

   member this.Id = 
      match this with
      | Root (id, _, _) -> id
      | Interior (id, _, _, _) -> id
      | Leaf (id, _, _) -> id
      | Terminal _ -> State<'Data,'Message>.TerminalStateId

   member this.Handler = 
      match this with
      | Root (_, handler, _) -> handler
      | Interior (id, _, handler, _) -> handler
      | Leaf (id, _, handler) -> handler
      | Terminal _ -> Handlers.emptyHandler
      

type [<NoComparison; NoEquality>] MessageHandler<'D,'M> =
   | Sync of Handler: (MessageContext<'D,'M> -> MessageResult<'D,'M>) 
   | Async of Handler: (MessageContext<'D,'M> -> Async<MessageResult<'D,'M>>)


type [<NoComparison; NoEquality>] TransitionHandler<'D,'M> =
   | Sync of Handler: (TransitionContext<'D,'M> -> 'D) 
   | Async of Handler: (TransitionContext<'D,'M> -> Async<'D>)


type [<NoComparison; NoEquality>] InitialTransition<'D> =
   | Sync of Handler: ('D -> struct ('D * StateId))
   | Async of Handler: ('D -> Async<struct ('D * StateId)>)


type [<NoComparison; NoEquality>] StateHandler<'D,'M> = { 
   OnMessage: MessageHandler<'D,'M>
   OnEnter: TransitionHandler<'D,'M>
   OnExit: TransitionHandler<'D,'M> 
}


type [<Struct; NoComparison; NoEquality>] MessageContext<'D,'M> (message:'M, data: 'D) =
   
   member this.Message = message
   member this.Data = data
 
   member this.GoTo( nextState: StateId, ?nextData:'D, ?action: TransitionHandler<'D,'M>) =  
      let _nextData = defaultArg nextData this.Data
      MessageResult.Transition(nextState, _nextData, action)

   member this.Stay( ?nextData:'D ) : MessageResult<'D,'M> = 
      let _nextData = defaultArg nextData this.Data
      MessageResult.InternalTransition(_nextData)

   member this.GoToSelf(?nextData:'D, ?action: TransitionHandler<'D,'M>) =
      let _nextData = defaultArg nextData this.Data
      MessageResult.SelfTransition(_nextData, action)

   member this.Unhandled() : MessageResult<'D,'M> = 
      MessageResult.Unhandled

   member this.Stop(?reason: string, ?code: int) : MessageResult<'D,'M> = 
      let stopReason = 
         if reason.IsSome || code.IsSome then 
            let _reason = defaultArg reason ""
            let _code = defaultArg code -1
            Some (StopReason (_reason, _code))
         else None
      MessageResult.Stop(stopReason)


and [<Struct; NoComparison; NoEquality>] TransitionContext<'D,'M> = { 
   SourceState: State<'D,'M>
   SourceData: 'D
   TargetState: State<'D,'M> 
   TargetData: 'D
   HandlingState: State<'D,'M>
}


and [<NoComparison; NoEquality>] MessageResult<'D,'M> = 
   | Transition of NextState:StateId * NextData:'D * Action:option<TransitionHandler<'D,'M>>
   | InternalTransition of NextData:'D
   | SelfTransition of NextData:'D * Action:option<TransitionHandler<'D,'M>>
   | Unhandled
   | Stop of Reason: option<StopReason>
   | InvalidMessage of Reason:string * Code:option<int>


and [<Sealed>] StopReason (reason: string, code: int) =
   member this.Reason = reason
   member this.Code = code


module State = 

   let inline id (state: State<_,_>) = 
      state.Id

   let inline handler (state: State<_,_>) = 
      state.Handler 

   let initialTransition state =
      match state with
      | Root (_,_,initTransition) -> Some(initTransition)
      | Interior (_, _, _, initTransition) -> Some(initTransition)
      | _ -> None


 
module Handlers =
  
   [<GeneralizableValue>]
   let emptyMessageHandler = MessageHandler.Sync (fun _ -> MessageResult.Unhandled)
  
   [<GeneralizableValue>]
   let emptyTransitionHandler = TransitionHandler.Sync (fun transCtx -> transCtx.TargetData)
  
   [<GeneralizableValue>]
   let emptyHandler = 
      { OnMessage = emptyMessageHandler
        OnEnter = emptyTransitionHandler
        OnExit = emptyTransitionHandler } 
