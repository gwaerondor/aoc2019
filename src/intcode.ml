open Core
open Aoc_lib
type op = Add | Mul | Break
exception Bad_argument

let get_operator params pos =
  match params.(pos) with
  | 1 -> Add
  | 2 -> Mul
  | 99 -> Break
  | _ -> raise Bad_argument

let op_to_fun = function
  | Add -> ( + )
  | Mul -> ( * )
  | Break -> raise Bad_argument

let rec execute ~state ~pos =
  match get_operator state pos with
  | Break -> state.(0)
  | op ->
     let val_a = state.(state.(pos + 1)) in
     let val_b = state.(state.(pos + 2)) in
     let target_pos = state.(pos + 3) in
     state.(target_pos) <- ((op_to_fun op) val_a val_b);
     execute ~state:state ~pos:(pos + 4)

let adjust_state ~noun ~verb ~state =
  state.(1) <- noun;
  state.(2) <- verb

let run ~noun ~verb input =
  adjust_state ~noun:noun ~verb:verb ~state:input;
  execute ~state:input ~pos:0
