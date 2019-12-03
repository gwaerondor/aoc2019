open Core
open Aoc_lib
open Intcode

let day02_1 =
  Aoc_lib.array_of_csv_of_file "input02.txt"
  |> Array.map ~f:int_of_string
  |> Intcode.run ~noun:12 ~verb:2

let day02_2 =
  let rec iterative_run n v =
    let input = Aoc_lib.array_of_csv_of_file "input02.txt"
                |> Array.map ~f:int_of_string
    in
    let res = Intcode.run ~noun:n ~verb:v input in
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
