module Log where

import Char
import Debug
import Html
import Html.Attributes
import Html.Lazy
import String

import Ansi

type alias Model =
  { previousLines : List Line
  , currentLine : Line
  , linePosition : Int
  , currentStyle : Style
  , remainder : String
  }

type alias Line = List Chunk

type alias Chunk =
  { text : String
  , style : Style
  }

type alias Style =
  { foreground : Maybe Ansi.Color
  , background : Maybe Ansi.Color
  , bold : Bool
  , faint : Bool
  , italic : Bool
  , underline : Bool
  , inverted : Bool
  }

init : Model
init = Model [] [] 0 (Style Nothing Nothing False False False False False) ""

update : String -> Model -> Model
update str model = List.foldl handleAnsiAction model (Ansi.parse (model.remainder ++ str))

handleAnsiAction : Ansi.Action -> Model -> Model
handleAnsiAction action model =
  case action of
    Ansi.Print s ->
      let
        chunk = Chunk s model.currentStyle
        line = writeChunk model.linePosition chunk model.currentLine
      in
        { model | currentLine <- line
                , linePosition <- model.linePosition + String.length s }

    Ansi.CarriageReturn ->
      { model | linePosition <- 0 }

    Ansi.Linebreak ->
      { model | previousLines <- model.previousLines ++ [model.currentLine]
              , currentLine <- [] }

    Ansi.Remainder s ->
      { model | remainder <- s }

    _ ->
      { model | currentStyle <- updateStyle action model.currentStyle }

updateStyle : Ansi.Action -> Style -> Style
updateStyle action style =
  case action of
    Ansi.SetForeground mc ->
      { style | foreground <- mc }

    Ansi.SetBackground mc ->
      { style | background <- mc }

    Ansi.SetInverted b ->
      { style | inverted <- b }

    Ansi.SetBold b ->
      { style | bold <- b }

    Ansi.SetFaint b ->
      { style | faint <- b }

    Ansi.SetItalic b ->
      { style | italic <- b }

    Ansi.SetUnderline b ->
      { style | underline <- b }

view : Model -> Html.Html
view model =
  Html.pre []
    (List.map lazyLine model.previousLines ++ [lazyLine model.currentLine])

lazyLine : Line -> Html.Html
lazyLine = Html.Lazy.lazy viewLine

viewLine : Line -> Html.Html
viewLine line =
  case line of
    [] -> Html.div [] [Html.text "\n"]
    _  -> Html.div [] (List.map viewChunk line)

viewChunk : Chunk -> Html.Html
viewChunk chunk =
  Html.span
    [Html.Attributes.style (styleStyle chunk.style)]
    [Html.text chunk.text]

styleStyle : Style -> List (String, String)
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

writeChunk : Int -> Chunk -> Line -> Line
writeChunk pos chunk line =
  let
    before = takeLen pos line
    after = dropLen (pos + String.length chunk.text) line
  in
    before ++ [chunk] ++ after

dropLen : Int -> Line -> Line
dropLen len line =
  case line of
    lc :: lcs ->
      let
          chunkLen = String.length lc.text
      in
        if chunkLen > len
           then { lc | text <- String.dropLeft len lc.text } :: lcs
           else dropLen (len - chunkLen) lcs

    [] -> []

takeLen : Int -> Line -> Line
takeLen len line =
  if len == 0 then
    []
  else
    case line of
      lc :: lcs ->
        let
            chunkLen = String.length lc.text
        in
          if chunkLen < len
             then lc :: takeLen (len - chunkLen) lcs
             else [{ lc | text <- String.left len lc.text }]

      [] -> []
