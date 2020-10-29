﻿module Renderer

open Elmish
open Elmish.React
open Elmish.Debug
open Elmish.HMR

open Fable.Core
open Fable.Core.JsInterop
open Browser.Types
open Electron
open Electron.Helpers
open MessageType

let isMac = Node.Api.``process``.platform = Node.Base.Darwin

(****************************************************************************************************
*
*                                  MENU HELPER FUNCTIONS
*
****************************************************************************************************)

let exitApp() =
    electron.ipcRenderer.send("exit-the-app",[||])

let menuSeparator =
   let sep = createEmpty<MenuItemOptions>
   sep.``type`` <- MenuItemType.Separator
   sep

/// Make action menu item from name, opt key to trigger, and action.
let makeItem (label : string) (accelerator : string option) (iAction : KeyboardEvent -> unit) =
   let handlerCaster f = System.Action<MenuItem, BrowserWindow, KeyboardEvent> f 
   let item = createEmpty<MenuItemOptions>
   item.label <- label
   match accelerator with | Some a -> item.accelerator <- a | _ -> ()
   item.click <- handlerCaster (fun _ _ keyEvent-> iAction keyEvent)
   item

/// Make role menu from name, opt key to trigger, and action.
let makeRoleItem label accelerator role =
   let item = makeItem label accelerator (fun _ -> ())
   item.role <- role
   item

/// make conditional menu item from condition, name, opt key to trigger, and role
let makeCondRoleItem cond label accelerator role =
   let item = makeItem label accelerator (fun _ -> ())
   item.role <- role
   item.visible <- cond
   item

/// make conditional menu item from condition, name, opt key to trigger, and action
let makeCondItem cond label accelerator action =
   let item = makeItem label accelerator action
   item.visible <- cond
   item


let makeElmItem (label:string) (accelerator : string) (action : unit -> unit) =
    jsOptions<MenuItemOptions> <| fun item ->
        item.label <- label
        item.accelerator <- accelerator
        item.click <- fun _ _ _ -> action()


/// Make a new menu from a a list of menu items
let makeMenu (topLevel: bool) (name : string) (table : MenuItemOptions list) =
   let subMenu = createEmpty<MenuItemOptions>
   subMenu.``type`` <- if topLevel then MenuItemType.Normal else MenuItemType.SubMenu
   subMenu.label <- name
   subMenu.submenu <- U2.Case1 (table |> Array.ofList)
   subMenu    


let fileMenu (dispatch) =
    makeMenu false "Sheet" [
        makeItem "New Sheet" (Some "CmdOrCtrl+N") (fun ev -> dispatch (MenuAction(MenuNewFile,dispatch)))
        makeItem "Save Sheet" (Some "CmdOrCtrl+S") (fun ev -> dispatch (MenuAction(MenuSaveFile,dispatch)))
        makeItem "Print Sheet" (Some "CmdOrCtrl+P") (fun ev -> dispatch (MenuAction(MenuPrint,dispatch)))
        makeItem "Exit Issie" None (fun ev -> exitApp())
        makeItem ("About Issie " + Version.VersionString) None (fun ev -> PopupView.viewInfoPopup dispatch)
        makeCondItem (JSHelpers.debugLevel <> 0 && not isMac) "Restart app" None (fun _ -> 
            let webContents = electron.remote.getCurrentWebContents()
            webContents.reload())


    ]

let viewMenu dispatch =
    let devToolsKey = if isMac then "Alt+Command+I" else "Ctrl+Shift+I"
    makeMenu false "View" [
        makeRoleItem "Toggle Fullscreen" (Some "F11") MenuItemRole.ToggleFullScreen
        menuSeparator
        makeRoleItem "Zoom In" (Some "CmdOrCtrl+Plus") MenuItemRole.ZoomIn
        makeRoleItem "Zoom Out" (Some "CmdOrCtrl+-") MenuItemRole.ZoomOut
        makeRoleItem "Reset Zoom" (Some "CmdOrCtrl+0") MenuItemRole.ResetZoom
        menuSeparator
        makeItem "Diagram Zoom In" (Some "CmdOrCtrl+z") (fun ev -> dispatch <| MenuAction(MenuZoom 1.25, dispatch))
        makeItem "Diagram Zoom Out" (Some "CmdOrCtrl+y") (fun ev -> dispatch <| MenuAction(MenuZoom (1. / 1.25), dispatch))
        menuSeparator
        makeCondItem (JSHelpers.debugLevel <> 0) "Toggle Dev Tools" (Some devToolsKey) (fun _ -> 
            let webContents = electron.remote.getCurrentWebContents()
            webContents.toggleDevTools())
    ]


// Editor Keybindings (also items on Edit menu)
// Use Elmish subscriptions to attach external source of events such as keyboard
// shortcuts. According to electron documentation, the way to configure keyboard
// shortcuts is by creating a menu.
let editMenu dispatch =
    let dispatch = MessageType.KeyboardShortcutMsg >> dispatch

    jsOptions<MenuItemOptions> <| fun invisibleMenu ->
        invisibleMenu.``type`` <- MenuItemType.SubMenu
        invisibleMenu.label <- "Edit"
        invisibleMenu.visible <- true
        invisibleMenu.submenu <-
            [| makeElmItem "Save Sheet" "CmdOrCtrl+S" (fun () -> dispatch MessageType.CtrlS)
               makeElmItem "Copy" "Alt+C" (fun () -> dispatch MessageType.AltC)
               makeElmItem "Paste" "Alt+V" (fun () -> dispatch MessageType.AltV)
               makeElmItem "Delete"  (if isMac then "Backspace" else "delete") (fun () -> dispatch MessageType.DEL)
               makeElmItem "Undo" "Alt+Z" (fun () -> dispatch MessageType.AltZ)
               makeElmItem "Redo" "Alt+Shift+Z" (fun () -> dispatch MessageType.AltShiftZ) |]
            |> U2.Case1

let attachMenusAndKeyShortcuts dispatch =
    let sub dispatch =
        let menu = 
            [|

                fileMenu dispatch

                editMenu dispatch 

                viewMenu dispatch
            |]          
            |> Array.map U2.Case1
            |> electron.remote.Menu.buildFromTemplate   
        menu.items.[0].visible <- Some true
        electron.remote.app.applicationMenu <- Some menu

    Cmd.ofSub sub    

// This setup is useful to add other pages, in case they are needed.

type Model = ModelType.Model

type Messages = MessageType.Msg

// -- Init Model

let init() = 
    JSHelpers.setDebugLevel()
    DiagramMainView.init(), Cmd.none

let init1() = (),Cmd.none

// -- Create View

let view1 (model:Unit) (dispatch:Msg->Unit) : Fable.React.ReactElement= Fable.React.Standard.div [] []
let view model dispatch = DiagramMainView.displayView model dispatch

// -- Update Model

let update1 msg model = model,Cmd.none
let update msg model = Update.update msg model

printfn "Starting renderer..."

Program.mkProgram init update view
|> Program.withReactBatched "app"
|> Program.withSubscription attachMenusAndKeyShortcuts
|> Program.run
