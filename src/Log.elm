module Log where

import Array
import Char
import Debug
import Html
import Html.Attributes
import Html.Lazy
import String

import Ansi
import Ansi.Log

type alias Model =
  { window : Ansi.Log.Window
  }

init : Model
init = Model Ansi.Log.init

update : String -> Model -> Model
update str model =
  { model | window <- Ansi.Log.update str model.window }

view : Model -> Html.Html
view model =
  Html.pre []
    (Array.toList (Array.map lazyLine model.window.lines))

lazyLine : Ansi.Log.Line -> Html.Html
lazyLine = Html.Lazy.lazy viewLine

viewLine : Ansi.Log.Line -> Html.Html
viewLine line =
  case line of
    [] -> Html.div [] [Html.text "\n"]
    _  -> Html.div [] (List.map viewChunk line)

viewChunk : Ansi.Log.Chunk -> Html.Html
viewChunk chunk =
  Html.span
    [Html.Attributes.style (styleStyle chunk.style)]
    [Html.text chunk.text]

styleStyle : Ansi.Log.Style -> List (String, String)
styleStyle style =
  [ ("font-weight", if style.bold then "bold" else "normal")
  , ("color", colorStyle (if not style.inverted then style.foreground else style.background))
  , ("background", colorStyle (if not style.inverted then style.background else style.foreground))
  ]

colorStyle : Maybe Ansi.Color -> String
colorStyle mc =
  case mc of
    Nothing -> "inherit"
    Just (Ansi.Black) -> "#000000"
    Just (Ansi.Red) -> "#C75646"
    Just (Ansi.Green) -> "#8EB33B"
    Just (Ansi.Yellow) -> "#D0B03C"
    Just (Ansi.Blue) -> "#72B3CC"
    Just (Ansi.Magenta) -> "#C8A0D1"
    Just (Ansi.Cyan) -> "#218693"
    Just (Ansi.White) -> "#B0B0B0"
    Just (Ansi.BrightBlack) -> "#5D5D5D"
    Just (Ansi.BrightRed) -> "#E09690"
    Just (Ansi.BrightGreen) -> "#CDEE69"
    Just (Ansi.BrightYellow) -> "#FFE377"
    Just (Ansi.BrightBlue) -> "#9CD9F0"
    Just (Ansi.BrightMagenta) -> "#FBB1F9"
    Just (Ansi.BrightCyan) -> "#77DFD8"
    Just (Ansi.BrightWhite) -> "#F7F7F7"
