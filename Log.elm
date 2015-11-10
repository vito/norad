module Log where

import Debug
import Html
import Html.Attributes
import String

type Color
  = Black
  | Red
  | Green
  | Yellow
  | Blue
  | Magenta
  | Cyan
  | White
  | BrightBlack
  | BrightRed
  | BrightGreen
  | BrightYellow
  | BrightBlue
  | BrightMagenta
  | BrightCyan
  | BrightWhite

type alias Style =
  { foreground : Maybe Color
  , background : Maybe Color
  , bold : Bool
  , faint : Bool
  , italic : Bool
  , underline : Bool
  }

type alias Chunk =
  { text : String
  , style : Style
  }

type alias Line = List Chunk

type alias Model =
  { previousLines : List Line
  , currentLine : Line
  , linePosition : Int
  , currentStyle : Style
  , remainder : List Char
  }

type AnsiAction
  = SetForeground (Maybe Color)
  | SetBackground (Maybe Color)
  | SetBold Bool
  | SetFaint Bool
  | SetItalic Bool
  | SetUnderline Bool
  | Linebreak
  | CarriageReturn
  | Print String
  | Remainder (List Char)

init : Model
init = Model [] [] 0 (Style Nothing Nothing False False False False) []

update : String -> Model -> Model
update str model = List.foldl handleAction model (parseAnsi (model.remainder ++ (String.toList str)))

reset : List AnsiAction
reset = [SetForeground Nothing, SetBackground Nothing, SetBold False, SetItalic False, SetUnderline False]
parseAnsi : List Char -> List AnsiAction
parseAnsi str =
  case str of
    '\r' :: cs ->
      CarriageReturn :: parseAnsi cs

    '\n' :: cs ->
      Linebreak :: parseAnsi cs

    '\x1b' :: cs ->
      case cs of
        '[' :: '0' :: 'm' :: rest -> reset ++ parseAnsi rest
        '[' :: '1' :: 'm' :: rest -> SetBold True :: parseAnsi rest
        '[' :: '2' :: 'm' :: rest -> SetFaint True :: parseAnsi rest
        '[' :: '3' :: 'm' :: rest -> SetItalic True :: parseAnsi rest
        '[' :: '4' :: 'm' :: rest -> SetUnderline True :: parseAnsi rest
        '[' :: '3' :: '0' :: 'm' :: rest -> SetForeground (Just Black) :: parseAnsi rest
        '[' :: '3' :: '1' :: 'm' :: rest -> SetForeground (Just Red) :: parseAnsi rest
        '[' :: '3' :: '2' :: 'm' :: rest -> SetForeground (Just Green) :: parseAnsi rest
        '[' :: '3' :: '3' :: 'm' :: rest -> SetForeground (Just Yellow) :: parseAnsi rest
        '[' :: '3' :: '4' :: 'm' :: rest -> SetForeground (Just Blue) :: parseAnsi rest
        '[' :: '3' :: '5' :: 'm' :: rest -> SetForeground (Just Magenta) :: parseAnsi rest
        '[' :: '3' :: '6' :: 'm' :: rest -> SetForeground (Just Cyan) :: parseAnsi rest
        '[' :: '3' :: '7' :: 'm' :: rest -> SetForeground (Just White) :: parseAnsi rest
        '[' :: '4' :: '0' :: 'm' :: rest -> SetBackground (Just Black) :: parseAnsi rest
        '[' :: '4' :: '1' :: 'm' :: rest -> SetBackground (Just Red) :: parseAnsi rest
        '[' :: '4' :: '2' :: 'm' :: rest -> SetBackground (Just Green) :: parseAnsi rest
        '[' :: '4' :: '3' :: 'm' :: rest -> SetBackground (Just Yellow) :: parseAnsi rest
        '[' :: '4' :: '4' :: 'm' :: rest -> SetBackground (Just Blue) :: parseAnsi rest
        '[' :: '4' :: '5' :: 'm' :: rest -> SetBackground (Just Magenta) :: parseAnsi rest
        '[' :: '4' :: '6' :: 'm' :: rest -> SetBackground (Just Cyan) :: parseAnsi rest
        '[' :: '4' :: '7' :: 'm' :: rest -> SetBackground (Just White) :: parseAnsi rest
        '[' :: '9' :: '0' :: 'm' :: rest -> SetForeground (Just BrightBlack) :: parseAnsi rest
        '[' :: '9' :: '1' :: 'm' :: rest -> SetForeground (Just BrightRed) :: parseAnsi rest
        '[' :: '9' :: '2' :: 'm' :: rest -> SetForeground (Just BrightGreen) :: parseAnsi rest
        '[' :: '9' :: '3' :: 'm' :: rest -> SetForeground (Just BrightYellow) :: parseAnsi rest
        '[' :: '9' :: '4' :: 'm' :: rest -> SetForeground (Just BrightBlue) :: parseAnsi rest
        '[' :: '9' :: '5' :: 'm' :: rest -> SetForeground (Just BrightMagenta) :: parseAnsi rest
        '[' :: '9' :: '6' :: 'm' :: rest -> SetForeground (Just BrightCyan) :: parseAnsi rest
        '[' :: '9' :: '7' :: 'm' :: rest -> SetForeground (Just BrightWhite) :: parseAnsi rest

        -- partial ansi codes
        ['[']              -> [Remainder str]
        ['[', '0']         -> [Remainder str]
        ['[', '1']         -> [Remainder str]
        ['[', '2']         -> [Remainder str]
        ['[', '3']         -> [Remainder str]
        ['[', '3', _]      -> [Remainder str]
        ['[', '4']         -> [Remainder str]
        ['[', '4', _]      -> [Remainder str]
        ['[', '9']         -> [Remainder str]
        ['[', '9', _]      -> [Remainder str]
        ['[', '1', '0']    -> [Remainder str]
        ['[', '1', '0', _] -> [Remainder str]

        -- invalid; skip
        _ :: cs -> parseAnsi cs

    c :: cs ->
      let
        rest = parseAnsi cs
      in
        case rest of
          Print s :: actions ->
            Print (String.cons c s) :: actions

          actions ->
            Print (String.fromChar c) :: actions

    [] ->
      []

handleAction : AnsiAction -> Model -> Model
handleAction action model =
  case action of
    SetForeground mc ->
      let
        cs = model.currentStyle
      in
        { model | currentStyle <- { cs | foreground <- mc } }

    SetBackground mc ->
      let
        cs = model.currentStyle
      in
        { model | currentStyle <- { cs | background <- mc } }

    SetBold b ->
      let
        cs = model.currentStyle
      in
        { model | currentStyle <- { cs | bold <- b } }

    SetFaint b ->
      let
        cs = model.currentStyle
      in
        { model | currentStyle <- { cs | faint <- b } }

    SetItalic b ->
      let
        cs = model.currentStyle
      in
        { model | currentStyle <- { cs | italic <- b } }

    SetUnderline b ->
      let
        cs = model.currentStyle
      in
        { model | currentStyle <- { cs | underline <- b } }

    Print s ->
      let
          chunk = Chunk s model.currentStyle
      in
        { model |
          currentLine <- writeChunk model.linePosition chunk model.currentLine
        , linePosition <- model.linePosition + String.length s
        }

    CarriageReturn ->
      { model | linePosition <- 0 }

    Linebreak ->
      { model |
        previousLines <- model.previousLines ++ [model.currentLine]
      , currentLine <- []
      }

    Remainder s ->
      { model | remainder <- s }

view : Model -> Html.Html
view model =
  Html.pre []
    (List.map viewLine model.previousLines ++ [viewLine model.currentLine])

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
  , ("color", colorStyle style.foreground)
  , ("background", colorStyle style.background)
  ]

colorStyle : Maybe Color -> String
colorStyle mc =
  case mc of
    Nothing -> "inherit"
    Just Black -> "#000000"
    Just Red -> "#C75646"
    Just Green -> "#8EB33B"
    Just Yellow -> "#D0B03C"
    Just Blue -> "#72B3CC"
    Just Magenta -> "#C8A0D1"
    Just Cyan -> "#218693"
    Just White -> "#B0B0B0"
    Just BrightBlack -> "#5D5D5D"
    Just BrightRed -> "#E09690"
    Just BrightGreen -> "#CDEE69"
    Just BrightYellow -> "#FFE377"
    Just BrightBlue -> "#9CD9F0"
    Just BrightMagenta -> "#FBB1F9"
    Just BrightCyan -> "#77DFD8"
    Just BrightWhite -> "#F7F7F7"

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
