open Core
open Aoc_lib
open Intcode

let day05_1 =
  let state = Aoc_lib.array_of_csv_of_file "input05.txt"
              |> Array.map ~f:int_of_string
  in
  let noun = state.(1) in
  let verb = state.(2) in
  Intcode.run ~noun:noun ~verb:verb ~input:1 state

let () =
  Printf.printf "Part 1: %d\n" day05_1
