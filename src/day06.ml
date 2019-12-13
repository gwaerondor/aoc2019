open Core
open Aoc_lib
type node = Node of string
type orbit = Orbit of node * node
exception Bad_argument
exception Target_not_found

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

let rec path_to_root ~orbits node =
  match find_parent node orbits with
  | None -> [node]
  | Some parent -> node :: (path_to_root ~orbits parent)

let steps ~orbits node =
  List.length (path_to_root ~orbits:orbits node) - 1

(* let rec steps ~orbits node =
 *   match find_parent node orbits with
 *   | Some parent -> 1 + (steps ~orbits:orbits parent)
 *   | None -> 0 *)

let rec steps_to_target ~orbits ~target node =
  match find_parent node orbits with
  | None -> raise Target_not_found
  | Some parent when nodes_are_equal parent target -> 0
  | Some parent -> 1 + (steps_to_target ~orbits:orbits ~target:target parent)

let day06_1 =
  List.map ~f:(steps ~orbits:orbits) all_nodes
  |> Aoc_lib.sum

let find_first_common_ancestor ~orbits n1 n2 =
  let n1_path = path_to_root ~orbits:orbits n1 in
  let rec find_first_match path node =
    if List.mem ~equal:nodes_are_equal n1_path node
    then node
    else
      match (find_parent node orbits) with
      | Some next -> find_first_match path next
      | None -> raise Target_not_found
  in find_first_match n1_path n2

let day06_2 =
  let santa = Node "SAN" in
  let me = Node "YOU" in
  let first_ancestor = find_first_common_ancestor ~orbits:orbits santa me in
  let santa_s = steps_to_target ~orbits:orbits ~target:first_ancestor santa in
  let me_s = steps_to_target ~orbits:orbits ~target:first_ancestor me in
  santa_s + me_s

let () =
  Printf.printf "Part 1: %d\n" day06_1;
  Printf.printf "Part 2: %d\n" day06_2
