open Core
open Aoc_lib
type dir = Up | Down | Left | Right
type steps = Steps of dir * int

let rec range a b =
  if a < b then a :: (range (a + 1) b)
  else if a > b then a :: (range (a - 1) b)
  else [a]

let make_pos x y = (x, y)

let parse_dir = function
  | 'U' -> Up
  | 'D' -> Down
  | 'L' -> Left
  | 'R' -> Right

let parse_steps instruction =
  let (dir :: amount) = char_list_of_string instruction in
  Steps ((parse_dir dir), (int_of_char_list amount))

let move_one dir (x, y) =
  match dir with
  | Up -> (x, y - 1)
  | Down -> (x, y + 1)
  | Left -> (x - 1, y)
  | Right -> (x + 1, y)

let rec step_to_coords steps ~pos =
  match steps with
  | Steps (_, 0) -> [pos]
  | Steps (dir, n) ->
     pos :: step_to_coords (Steps (dir, n - 1)) ~pos:(move_one dir pos)

let hd list =
  match List.hd list with
  | Some e -> e
  | None -> raise Bad_argument

let nth list n =
  match List.nth list n with
  | Some e -> e
  | None -> raise Bad_argument

let tail (_::t) = t

let last list =
  match List.last list with
  | Some e -> e
  | None -> raise Bad_argument

let record_path path =
  let step_list = List.map ~f:parse_steps path in
  let rec rp s (x, y) =
    match s with
    | [] -> []
    | (next::remainder) ->
       let stepped_coords = step_to_coords next ~pos:(x, y) in
       let updated_coords = last stepped_coords in
       stepped_coords :: (rp remainder updated_coords)
  in
  rp step_list (0, 0) |> List.concat

let print_coord (x, y) =
  Printf.printf "(%d,%d), " x y

let print_visited coords =
  List.iter ~f:print_coord coords;
  Printf.printf "\n"

let compare_tuples (x1, y1) (x2, y2) =
  x1 = x2 && y1 = y2

let remove_duplicates list =
  let rec rd list acc =
    match list with
    | [] -> acc
    | (e :: r) -> if List.mem acc ~equal:compare_tuples e
                  then rd r acc
                  else rd r (e::acc)
  in rd list []

let intersect left_list right_list =
  List.filter ~f:(List.mem right_list ~equal:compare_tuples) left_list

let distance_to_origo (x, y) = (abs x) + (abs y)

let big_number = 10000000
let min = List.fold_left ~init:big_number ~f:min

let without_origo list =
  List.filter ~f:(fun e -> not (compare_tuples (0, 0) e)) list

let day03_1 =
  let input = multiline_csv_of_file "input03.txt" in
  let first_wire = hd input in
  let second_wire = nth input 1 in
  let visited_by_first_wire = record_path first_wire |> without_origo in
  let visited_by_second_wire = record_path second_wire |> without_origo in
  let intersections = intersect visited_by_first_wire visited_by_second_wire in
  let closest = List.map ~f:distance_to_origo intersections |> min in
  Printf.printf "Part 1: %d\n" closest
