open Core
open Aoc_lib
type op = Add | Mul | Break | Input | Output
type parameter_mode = Position | Immediate
exception Bad_argument

let pad_to_two = function
  | [c] -> ['0'; c]
  | cs -> cs

let parse_operator data =
  char_list_of_int data |> pad_to_two |> String.of_char_list

let get_operator params pos =
  match params.(pos) |> parse_operator with
  | "01" -> (Position, Add)
  | "02" -> (Position, Mul)
  | "99" -> (Position, Break)
  | _ -> raise Bad_argument

let op_to_fun = function
  | Add -> ( + )
  | Mul -> ( * )
  | _ -> raise Bad_argument

let get_value state mode arg =
  match mode with
  | Position -> state.(state.(arg))
  | Immediate -> arg

let rec execute ~state ~pos =
  match get_operator state pos with
  | (_, Break) -> state.(0)
  | (_, Input) -> raise Bad_argument
  | (_, Output) -> raise Bad_argument
  | (mode, op) ->
     let val_a = get_value state mode (pos + 1) in
     let val_b = get_value state mode (pos + 2) in
     let target_pos = state.(pos + 3) in
     state.(target_pos) <- ((op_to_fun op) val_a val_b);
     execute ~state:state ~pos:(pos + 4)

let adjust_state ~noun ~verb ~state =
  state.(1) <- noun;
  state.(2) <- verb

let run ~noun ~verb input =
  adjust_state ~noun:noun ~verb:verb ~state:input;
  execute ~state:input ~pos:0
