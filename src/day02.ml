open Core
open Aoc_lib
type op = Add | Mul | Break

let input =
  Aoc_lib.csv_of_file "input02.txt"
  |> List.map ~f:int_of_string
  |> List.to_array

let get_operator params pos =
  match params.(pos) with
  | 1 -> Some Add
  | 2 -> Some Mul
  | 99 -> Some Break
  | _ -> None

let op_to_fun = function
  | Add -> ( + )
  | Mul -> ( * )
  | Break -> (fun _ _ -> -1)

let rec execute ~params ~pos =
  let operator = get_operator params pos in
  match operator with
  | Some Break -> params.(0)
  | Some op ->
     let val_a = params.(params.(pos + 1)) in
     let val_b = params.(params.(pos + 2)) in
     let target_pos = params.(pos + 3) in
     params.(target_pos) <- ((op_to_fun op) val_a val_b);
     execute ~params:params ~pos:(pos + 4)
  | None -> raise Division_by_zero

(* let print_array arr =
 *   String.concat ~sep:"; " (Array.to_list (Array.map ~f:string_of_int arr))
 *   |> print_string;
 *   print_newline () *)

let adjust_input =
  input.(1) <- 12;
  input.(2) <- 2

let day02_1 =
  adjust_input;
  execute ~params:input ~pos:0

let () =
  Printf.printf "Part 1: %d\n" (day02_1)

