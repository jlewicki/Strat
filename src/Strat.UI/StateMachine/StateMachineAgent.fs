namespace Strat.UI.StateMachine

open System
open System.Reactive.Subjects
open System.Reactive.Linq
open System.Threading.Tasks
open Strat.StateMachine

 
type IMessageDispatcher<'D,'M> =
   abstract PostMessage: message:'M -> unit
   abstract PostMessageAsync: message:'M -> Task<MessageProcessed<'D,'M>>


type IStateMachine<'D, 'M> =
   inherit IMessageDispatcher<'D, 'M>
   abstract Data: IObservable<'D>


module StateMachine =
   
   // Messages for the state machine agent 
   type private StateMachineMessage<'D, 'M> =
      // Post a message for processing by the state machine
      | Post of Message:'M
      // Post a message for processing by the state machine, and receive the result on the reply channel
      | PostAndReply of Message:'M * ReplyChannel: AsyncReplyChannel<MessageProcessed<'D,'M>>
   with 
      member this.Message = 
         match this with
         | Post message -> message
         | PostAndReply (message, _) -> message     


   // Simple state machine, wrapped in a mailbox processor to isolate message handling
   type private SimpleStateMachine<'D, 'M> (stateTree: StateTree<'D,'M>, initialData: 'D, ?initialState: StateId) =
      let initialContext = Machine.initializeContext stateTree initialData initialState |> Async.RunSynchronously
      let contextSubject = new BehaviorSubject<StateMachineContext<'D, 'M>>(initialContext)
      let currentData = contextSubject.Select(fun ctx -> ctx.Data)
      let agent : MailboxProcessor<StateMachineMessage<'D,'M>> = MailboxProcessor.Start (fun inbox ->
         let rec loop context =
            async {
               let! smMessage = inbox.Receive()
               let! msgProcessed = Machine.processMessage smMessage.Message context
               let nextContext = 
                  match msgProcessed with
                  | HandledMessage handled -> 
                     contextSubject.OnNext handled.NextContext
                     handled.NextContext
                  | _ -> context
               match smMessage with
               | PostAndReply (_, reply) -> reply.Reply msgProcessed
               | _ -> ()
               return! loop nextContext
            }
         loop initialContext)
    
      interface IStateMachine<'D,'M> with
         member this.Data : IObservable<'D> = 
            currentData
         member this.PostMessageAsync (message: 'M) =
            agent.PostAndAsyncReply (fun reply -> PostAndReply (message, reply))
            |> Async.StartAsTask
         member this.PostMessage(message: 'M) : unit =
            agent.Post (Post message)


   /// Creates a new state machine instance.
   let create 
      (stateTree: StateTree<'D,'M>) 
      (initialData: 'D) 
      (initialState: option<StateId>) : IStateMachine<'D, 'M> =
      upcast new SimpleStateMachine<'D,'M> (stateTree, initialData, ?initialState = initialState)