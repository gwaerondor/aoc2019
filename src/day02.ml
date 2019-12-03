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

let run ~noun ~verb =
  let input =
    Aoc_lib.array_of_csv_of_file "input02.txt"
    |> Array.map ~f:int_of_string
  in
  adjust_state ~noun:noun ~verb:verb ~state:input;
  execute ~state:input ~pos:0

let day02_1 = run ~noun:12 ~verb:2

let day02_2 =
  let rec iterative_run n v =
    let res = run ~noun:n ~verb:v in
    match res with
    | 19690720 -> (n, v)
    | _ -> match n with
           | 99 -> iterative_run 0 (v + 1)
           | _ -> iterative_run (n + 1) v
  in
  let (res_noun, res_verb) = iterative_run 0 0 in
  100 * res_noun + res_verb

let () =
  Printf.printf "Part 1: %d\n" day02_1;
  Printf.printf "Part 2: %d\n" day02_2
