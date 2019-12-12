open Core
open Aoc_lib
type op = Add | Mul | Break | Input | Output | JIT | JIF | LT | Eq
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
  | 1 -> Trinary (Add, mode_left, mode_middle, Immediate)
  | 2 -> Trinary (Mul, mode_left, mode_middle, Immediate)
  | 3 -> Unary (Input, Immediate)
  | 4 -> Unary (Output, mode_left)
  | 5 -> Binary (JIT, mode_left, mode_middle)
  | 6 -> Binary (JIF, mode_left, mode_middle)
  | 7 -> Trinary (LT, mode_left, mode_middle, Immediate)
  | 8 -> Trinary (Eq, mode_left, mode_middle, Immediate)
  | 99 -> Unary (Break, Position)
  | n -> raise (Bad_argument n)

let op_to_fun = function
  | Add -> ( + )
  | Mul -> ( * )
  | LT -> fun x y -> if x < y then 1 else 0
  | Eq -> fun x y -> if x = y then 1 else 0
  | op -> raise (Bad_operator op)

let get_value state mode arg =
  match mode with
  | Position -> state.(state.(arg))
  | Immediate -> state.(arg)

let rec execute ~state ~pos ~input =
  match get_operator state.(pos) with
  | Unary (Break, Position) -> state.(0)
  | Unary (Input, mode) ->
     let target_pos = get_value state mode (pos + 1) in
     state.(target_pos) <- input;
     execute ~state:state ~pos:(pos + 2) ~input:input
  | Unary (Output, mode) ->
     let v = get_value state mode (pos + 1) in
     Printf.printf "Diagnostics code: %d\n" v;
     execute ~state:state ~pos:(pos + 2) ~input:input
  | Trinary (op, mode_left, mode_middle, mode_right) ->
     let val_a = get_value state mode_left (pos + 1) in
     let val_b = get_value state mode_middle (pos + 2) in
     let target_pos = get_value state mode_right (pos + 3) in
     state.(target_pos) <- ((op_to_fun op) val_a val_b);
     execute ~state:state ~pos:(pos + 4) ~input:input
  | Binary (op, mode_left, mode_right) ->
     let val_a = get_value state mode_left (pos + 1) in
     let val_b = get_value state mode_right (pos + 2) in
     let p = match op with | JIT -> ((!=) 0) | JIF -> ((=) 0) in
     let next_pos = if (p val_a) then val_b else pos + 3 in
     execute ~state:state ~pos:next_pos ~input:input

let adjust_state ~noun ~verb ~state =
  state.(1) <- noun;
  state.(2) <- verb

let run ~noun ~verb ~input state =
  adjust_state ~noun:noun ~verb:verb ~state:state;
  execute ~state:state ~pos:0 ~input:input
