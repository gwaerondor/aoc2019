open Aoc_lib
open Core

let to_numbers = List.map ~f:int_of_string
let inputs = to_numbers (Aoc_lib.lines_of_file "input01.txt")

let fuel_consumption mass = (mass / 3) - 2
let consumption_per_module = List.map ~f:fuel_consumption inputs
let sum = List.fold_left ~f:(+) ~init:0

let day01_a = sum consumption_per_module

let rec calculate_mass_with_added_fuel mass ~acc:sum =
    match fuel_consumption mass with
    | fc when fc > 0 -> calculate_mass_with_added_fuel fc ~acc:(sum + fc)
    | _ -> sum

let day01_b =
  List.map ~f:(calculate_mass_with_added_fuel ~acc:0) consumption_per_module
  |> sum

let () =
  let module_fuel = day01_a in
  Printf.printf "Part 1: %d\n" module_fuel;
  Printf.printf "Part 2: %d\n" (module_fuel + day01_b)
