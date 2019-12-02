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

let rec execute ~params ~pos =
  let operator = get_operator params pos in
  match operator with
  | Break -> params.(0)
  | op ->
     let val_a = params.(params.(pos + 1)) in
     let val_b = params.(params.(pos + 2)) in
     let target_pos = params.(pos + 3) in
     params.(target_pos) <- ((op_to_fun op) val_a val_b);
     execute ~params:params ~pos:(pos + 4)

let adjust_input ~noun ~verb ~input =
  input.(1) <- noun;
  input.(2) <- verb

let run ~noun ~verb =
  let input =
    Aoc_lib.csv_of_file "input02.txt"
    |> List.map ~f:int_of_string
    |> List.to_array
  in
  adjust_input ~noun:noun ~verb:verb ~input:input;
  execute ~params:input ~pos:0

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
  Printf.printf "Part 1: %d\n" (day02_1);
  Printf.printf "Part 2: %d\n" (day02_2)
