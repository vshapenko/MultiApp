// Copyright 2018-2019 Fabulous contributors. See LICENSE.md for license.
namespace MultiApp

open Fabulous
open Fabulous.XamarinForms
open Xamarin.Forms
open System
open System.Threading.Tasks

module App =
  
  type AppMessage =
      | Dummy of (unit->unit)
  
  type PageModel =
    { View: unit -> ViewElement
      Init: unit -> unit
      OnBackButtonPressed: (unit -> bool) option
      Dispose: unit-> unit
      SetDispatch :((AppMessage ->unit) -> unit)
    }

  module PageModel =
    type private Msg<'t> =
     |Msg of 't
     |Process of ((('t->unit)->unit)*('t->unit))
     
    let create<'T, 'TMessage>
      (defaultValue: 'T,
       init: ('TMessage -> unit) -> IDisposable list,
       update: 'TMessage -> 'T -> 'T * Cmd<'TMessage>,
       view: 'T -> ('TMessage -> unit) -> ViewElement)
      (onBackButtonPressed: ('T -> ('TMessage -> unit) -> bool) option) =

      let mutable parentDispatch = fun (x: AppMessage )-> printfn "fake dispatch"
      let mutable subs = List.empty<IDisposable>

      let mutable state = defaultValue

      let inbox = MailboxProcessor.Start(fun b->
          let rec loop ()= async {
              let! msg = b.Receive()
              match msg with
              | Msg msg ->
                 let dispatch msg =
                      b.Post (Msg msg)
                      
                 let f()=
                       let newState,cmd = update msg state
                       state<-newState
                       for c in cmd do
                          b.Post (Process (c,dispatch))
                          
                 parentDispatch (AppMessage.Dummy f)
                 return! loop ()

              | Process (f,arg) ->                 
                  f arg
                  return! loop ()
                  
              return! loop ()
          }
          loop ()
          )
      let disp msg = inbox.Post(Msg msg)

      let backPressed: (unit -> bool) option =
        
        match onBackButtonPressed with
        | Some x -> Some <| fun _ -> x state disp
        | None -> None

      { View = fun () -> view state disp
        Init = fun () ->
         subs |> List.iter (fun x -> x.Dispose())
         subs <- init disp
        OnBackButtonPressed = backPressed
        Dispose = fun ()->
           subs|> Seq.iter (fun x-> x.Dispose())
                
        SetDispatch  = fun d->parentDispatch<-d
      }

  module MainPage=
      type Model = 
          { Count : int
            Step : int
            TimerOn: bool }
          
      type Msg = 
            | Increment 
            | Decrement 
            | Reset
            | SetStep of int
            | TimerToggled of bool
            | TimedTick

      let initModel = { Count = 0; Step = 1; TimerOn=false }
      
      let fromAppMessage msg =
         None

      let init _ =
          []
                 
      
      let pageModel = PageModel.create 
      let timerCmd =
            async { do! Async.Sleep 200
                    return TimedTick }
            |> Cmd.ofAsyncMsg

      let update (msg:Msg) model =
            printfn "update %A" msg
            match msg with
            | Increment -> { model with Count = model.Count + model.Step }, Cmd.none
            | Decrement -> { model with Count = model.Count - model.Step }, Cmd.none
            | Reset -> initModel, [Increment |>Cmd.ofMsg;Decrement|>Cmd.ofMsg;Increment|>Cmd.ofMsg] |>Cmd.batch
            | SetStep n -> { model with Step = n }, Cmd.none
            | TimerToggled on -> { model with TimerOn = on }, (if on then timerCmd else Cmd.none)
            | TimedTick -> 
                if model.TimerOn then 
                    { model with Count = model.Count + model.Step }, timerCmd
                else 
                    model, Cmd.none

      let view (model: Model) dispatch =
            printfn "view %A" model
            View.ContentPage(
              content = View.StackLayout(padding = Thickness 20.0, verticalOptions = LayoutOptions.Center,
                children = [ 
                    View.Label(text = sprintf "%d" model.Count, horizontalOptions = LayoutOptions.Center, width=200.0, horizontalTextAlignment=TextAlignment.Center)
                    View.Button(text = "Increment", command = (fun () -> dispatch Increment), horizontalOptions = LayoutOptions.Center)
                    View.Button(text = "Decrement", command = (fun () -> dispatch Decrement), horizontalOptions = LayoutOptions.Center)
                    View.Label(text = "Timer", horizontalOptions = LayoutOptions.Center)
                    View.Switch(isToggled = model.TimerOn, toggled = (fun on -> dispatch (TimerToggled on.Value)), horizontalOptions = LayoutOptions.Center)
                    View.Slider(minimumMaximum = (0.0, 10.0), value = double model.Step, valueChanged = (fun args -> dispatch (SetStep (int (args.NewValue + 0.5)))), horizontalOptions = LayoutOptions.FillAndExpand)
                    View.Label(text = sprintf "Step size: %d" model.Step, horizontalOptions = LayoutOptions.Center) 
                    View.Button(text = "Reset", horizontalOptions = LayoutOptions.Center, command = (fun () -> dispatch Reset), commandCanExecute = (model <> initModel))
                ]))

  type MainModel = {Page :PageModel}
  let init() =
      let page = PageModel.create (MainPage.initModel,MainPage.init,MainPage.update,MainPage.view) None
      {Page = page},Cmd.none


  let init2() =  MainPage.initModel,Cmd.none
   
  let update msg (model:MainModel) =
      match msg with
      | AppMessage.Dummy f->f()
      model,Cmd.none
  
  let view model dispatch =
      model.Page.SetDispatch dispatch
      model.Page.View()
      
  let program = Program.mkProgram init update view
  //let program = Program.mkProgram init2 MainPage.update MainPage.view
  

type App () as app = 
    inherit Application ()

    let runner = 
        App.program
        |> XamarinFormsProgram.run app

#if DEBUG
    // Uncomment this line to enable live update in debug mode. 
    // See https://fsprojects.github.io/Fabulous/Fabulous.XamarinForms/tools.html#live-update for further  instructions.
    //
    //do runner.EnableLiveUpdate()
#endif    

    // Uncomment this code to save the application state to app.Properties using Newtonsoft.Json
    // See https://fsprojects.github.io/Fabulous/Fabulous.XamarinForms/models.html#saving-application-state for further  instructions.
#if APPSAVE
    let modelId = "model"
    override __.OnSleep() = 

        let json = Newtonsoft.Json.JsonConvert.SerializeObject(runner.CurrentModel)
        Console.WriteLine("OnSleep: saving model into app.Properties, json = {0}", json)

        app.Properties.[modelId] <- json

    override __.OnResume() = 
        Console.WriteLine "OnResume: checking for model in app.Properties"
        try 
            match app.Properties.TryGetValue modelId with
            | true, (:? string as json) -> 

                Console.WriteLine("OnResume: restoring model from app.Properties, json = {0}", json)
                let model = Newtonsoft.Json.JsonConvert.DeserializeObject<App.Model>(json)

                Console.WriteLine("OnResume: restoring model from app.Properties, model = {0}", (sprintf "%0A" model))
                runner.SetCurrentModel (model, Cmd.none)

            | _ -> ()
        with ex -> 
            App.program.onError("Error while restoring model found in app.Properties", ex)

    override this.OnStart() = 
        Console.WriteLine "OnStart: using same logic as OnResume()"
        this.OnResume()
#endif


