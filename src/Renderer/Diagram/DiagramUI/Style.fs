module DiagramStyle

open Fable.Helpers.React.Props

let headerHeight = "52px"
let rightSectionWidth = "400px"

let navbarStyle = Style [
    Width "100%"
    Height headerHeight
]

let rightSectionStyle = Style [
    Position "fixed"
    Right "0px"
    Height (sprintf "calc(100%s - %s)" "%" headerHeight) // WindowSize - headerHeight
    Width rightSectionWidth
    OverflowX "hidden"
    OverflowY "scroll"
    Border "2px solid gray"
]

let canvasHiddenStyle = Style [
    Display "none"
]

let canvasVisibleStyle = Style [
    Display "block"
    Position "absolute" // Required to work.
    Overflow "scroll"
    Top headerHeight // Placed just under the header.
    Left "0px"
    Bottom "0px"
    Right rightSectionWidth
    Border "2px solid gray"
]

let canvasSmallMenuStyle = Style [
    Display "block"
    Position "absolute" // Required to work.
    Overflow "hidden"
    Left "10px"
    Bottom "25px"
    Right (sprintf "calc(100%s - 300px)" "%")
]

let canvasSmallButtonStyle = Style [
    MarginRight "5px"
    BackgroundColor "white"
    BorderRadius "4px"
    BorderStyle "solid"
    Outline "none"
    Padding "4px"
    Opacity 0.7
]
