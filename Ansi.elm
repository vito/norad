module Ansi (Color(..), Action(..), parse) where

import Char
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

type Action
  = SetForeground (Maybe Color)
  | SetBackground (Maybe Color)
  | SetBold Bool
  | SetFaint Bool
  | SetItalic Bool
  | SetUnderline Bool
  | SetInverted Bool
  | Linebreak
  | CarriageReturn
  | Print String
  | Remainder String

parse : String -> List Action
parse = parseChars << String.toList

parseChars : List Char -> List Action
parseChars seq =
  case seq of
    '\r' :: cs ->
      CarriageReturn :: parseChars cs

    '\n' :: cs ->
      Linebreak :: parseChars cs

    '\x1b' :: '[' :: cs ->
      case collectCodes cs of
        Incomplete ->
          [Remainder (String.fromList seq)]

        Invalid ->
          parseChars cs

        Complete codes rest ->
          (List.concatMap codeActions codes) ++ parseChars rest

    c :: cs ->
      let
        rest = parseChars cs
      in
        case rest of
          Print s :: actions ->
            Print (String.cons c s) :: actions

          actions ->
            Print (String.fromChar c) :: actions

    [] ->
      []

type CodeParseResult
  = Incomplete
  | Complete (List Int) (List Char)
  | Invalid

collectCodes : List Char -> CodeParseResult
collectCodes seq = collectCodesMemo seq [] ""

collectCodesMemo : List Char -> (List Int) -> String -> CodeParseResult
collectCodesMemo seq codes currentNum =
  case seq of
    'm' :: cs ->
      case String.toInt currentNum of
        Ok num -> Complete (codes ++ [num]) cs
        Err _ -> Invalid

    ';' :: cs ->
      case String.toInt currentNum of
        Ok num -> collectCodesMemo cs (codes ++ [num]) ""
        Err _ -> Invalid

    c :: cs ->
      if Char.isDigit c
         then collectCodesMemo cs codes (currentNum ++ String.fromChar c)
         else Invalid

    [] ->
      Incomplete

codeActions : Int -> List Action
codeActions code =
  case code of
    0 -> reset
    1 -> [SetBold True]
    2 -> [SetFaint True]
    3 -> [SetItalic True]
    4 -> [SetUnderline True]
    7 -> [SetInverted True]
    30 -> [SetForeground (Just Black)]
    31 -> [SetForeground (Just Red)]
    32 -> [SetForeground (Just Green)]
    33 -> [SetForeground (Just Yellow)]
    34 -> [SetForeground (Just Blue)]
    35 -> [SetForeground (Just Magenta)]
    36 -> [SetForeground (Just Cyan)]
    37 -> [SetForeground (Just White)]
    40 -> [SetBackground (Just Black)]
    41 -> [SetBackground (Just Red)]
    42 -> [SetBackground (Just Green)]
    43 -> [SetBackground (Just Yellow)]
    44 -> [SetBackground (Just Blue)]
    45 -> [SetBackground (Just Magenta)]
    46 -> [SetBackground (Just Cyan)]
    47 -> [SetBackground (Just White)]
    90 -> [SetForeground (Just BrightBlack)]
    91 -> [SetForeground (Just BrightRed)]
    92 -> [SetForeground (Just BrightGreen)]
    93 -> [SetForeground (Just BrightYellow)]
    94 -> [SetForeground (Just BrightBlue)]
    95 -> [SetForeground (Just BrightMagenta)]
    96 -> [SetForeground (Just BrightCyan)]
    97 -> [SetForeground (Just BrightWhite)]
    100 -> [SetBackground (Just BrightBlack)]
    101 -> [SetBackground (Just BrightRed)]
    102 -> [SetBackground (Just BrightGreen)]
    103 -> [SetBackground (Just BrightYellow)]
    104 -> [SetBackground (Just BrightBlue)]
    105 -> [SetBackground (Just BrightMagenta)]
    106 -> [SetBackground (Just BrightCyan)]
    107 -> [SetBackground (Just BrightWhite)]
    _ -> []

reset : List Action
reset =
  [ SetForeground Nothing
  , SetBackground Nothing
  , SetBold False
  , SetItalic False
  , SetUnderline False
  , SetInverted False
  ]
