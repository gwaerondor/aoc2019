open Aoc_lib
open Core

let day01_a =
  let to_numbers = List.map ~f:int_of_string in
  let masses = to_numbers (Aoc_lib.lines_of_file "input01.txt") in
  let thirds = List.map masses ~f:((Fn.flip (/)) 3) in
  let finals = List.map thirds ~f:((+) (-2)) in
  List.fold_left ~f:(+) ~init:0 finals

let () =
  print_int day01_a;
  print_newline ()
