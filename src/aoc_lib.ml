open Core
let input_line_opt ic =
  try Some (input_line ic)
  with End_of_file -> None

let read_lines ic =
  let rec aux acc =
    match input_line_opt ic with
    | Some line -> aux (line::acc)
    | None -> (List.rev acc)
  in
  aux []

let lines_of_file filename =
  let ic = open_in filename in
  let lines = read_lines ic in
  close_in ic;
  (lines)

let csv_of_file filename =
  match lines_of_file filename with
  | [line] -> String.split_on_chars ~on:[','] line
  | _ -> []

let (>.) = Fn.compose

let array_of_lines_of_file = List.to_array >. lines_of_file
let array_of_csv_of_file = List.to_array >. csv_of_file
