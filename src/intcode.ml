open Core
open Aoc_lib
type op = Add | Mul | Break | Input | Output
type mode = Position | Immediate
type operation =
  | Nullary of op
  | Unary of (op * mode)
  | Binary of (op * mode * mode)
  | Trinary of (op * mode * mode * mode)

exception Bad_argument of int
exception Bad_operator of op

let get_mode = function
  | 0 -> Position
  | 1 -> Immediate
  | n -> raise (Bad_argument n)

let get_operator instruction =
  let operation = instruction mod 100 in
  let mode_left = (instruction / 100) mod 10 |> get_mode in
  let mode_middle = (instruction / 1000) mod 10 |> get_mode in
  let mode_right = (instruction / 10000) |> get_mode in
  match operation with
  | 1 -> Trinary (Add, mode_left, mode_middle, mode_right)
  | 2 -> Trinary (Mul, mode_left, mode_middle, mode_right)
  | 99 -> Unary (Break, Position)
  | n -> raise (Bad_argument n)

let op_to_fun = function
  | Add -> ( + )
  | Mul -> ( * )
  | op -> raise (Bad_operator op)

let get_value state mode arg =
  match mode with
  | Position -> state.(state.(arg))
  | Immediate -> arg

let rec execute ~state ~pos =
  match get_operator state.(pos) with
  | Unary (Break, Position) -> state.(0)
  | Unary (Input, _) -> raise (Bad_operator Input)
  | Unary (Output, _) -> raise (Bad_operator Output)
  | Trinary (op, mode_left, mode_middle, mode_right) ->
     let val_a = get_value state mode_left (pos + 1) in
     let val_b = get_value state mode_middle (pos + 2) in
     let target_pos = state.(pos + 3) in
     state.(target_pos) <- ((op_to_fun op) val_a val_b);
     execute ~state:state ~pos:(pos + 4)

let adjust_state ~noun ~verb ~state =
  state.(1) <- noun;
  state.(2) <- verb

let run ~noun ~verb input =
  adjust_state ~noun:noun ~verb:verb ~state:input;
  execute ~state:input ~pos:0
