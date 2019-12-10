open Core
open Aoc_lib
open Intcode

let run ~input =
  let state = Aoc_lib.array_of_csv_of_file "input05.txt"
              |> Array.map ~f:int_of_string
  in
  let noun = state.(1) in
  let verb = state.(2) in
  Intcode.run ~noun:noun ~verb:verb ~input:input state

let day05_1 = run ~input:1
let day05_2 = run ~input:5

let () =
  Printf.printf "Part 1: %d\n" day05_1;
  Printf.printf "Part 2: %d\n" day05_2
