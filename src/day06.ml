open Core
open Aoc_lib
type node = Node of string
type orbit = Orbit of node * node
exception Bad_argument

let input = lines_of_file "input06.txt"

let parse_orbit = function
  | [orbiter; orbitee] -> Orbit ((Node orbiter), (Node orbitee))
  | _ -> raise Bad_argument

let orbits = List.map ~f:(String.split_on_chars ~on:[')']) input
            |> List.map ~f:parse_orbit

let day06_1 = List.length orbits

let () =
  Printf.printf "Part 1: %d\n" day06_1
