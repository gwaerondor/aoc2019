open Core
open Aoc_lib
open Intcode

let run ~input =
  let state = Aoc_lib.array_of_csv_of_file "input07.txt"
              |> Array.map ~f:int_of_string
  in
  let noun = state.(1) in
  let verb = state.(2) in
  Intcode.run_and_get_output ~noun:noun ~verb:verb ~input:input state

let ins_all_positions x l =
  let rec aux prev acc = function
    | [] -> (prev @ [x]) :: acc |> List.rev
    | hd::tl as l -> aux (prev @ [hd]) ((prev @ [x] @ l) :: acc) tl
  in
  aux [] [] l

let rec permutations = function
  | [] -> []
  | x::[] ->
     [[x]]
  | x::xs ->
     List.fold_left ~f:(fun acc p -> acc @ ins_all_positions x p ) ~init:[] (permutations xs)

let day07_1 =
  let do_run [a; b; c; d; e] =
    let in_b = run ~input:[0; a] in
    let in_c = run ~input:[in_b; b] in
    let in_d = run ~input:[in_c; c] in
    let in_e = run ~input:[in_d; d] in
    run ~input:[in_e; e]
  in
  let sequences = permutations [1; 2; 3; 4; 5] in
  map ~f:do_run sequences |> Aoc_lib.max

let () =
  Printf.printf "Part 1: %d\n" day07_1
