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

let nodes_in_orbit (Orbit (n1, n2)) = [n1; n2]

let all_nodes =
  let rec an o =
    match o with
    | [] -> []
    | ((Orbit (n1, n2))::r) -> n1 :: n2 :: (an r)
  in
  let node_list = an orbits in
  Aoc_lib.remove_duplicates node_list ~equal:(=)

let nodes_are_equal (Node n1) (Node n2) = n1 = n2

let rec find_parent node orbits =
  match orbits with
  | [] -> None
  | (Orbit (parent, n))::os ->
     if nodes_are_equal node n
     then Some parent
     else find_parent node os

let rec steps ~orbits node =
  match find_parent node orbits with
  | Some parent -> 1 + (steps ~orbits:orbits parent)
  | None -> 0

let day06_1 = List.map ~f:(steps ~orbits:orbits) all_nodes |> Aoc_lib.sum

let () =
  Printf.printf "Part 1: %d\n" day06_1
