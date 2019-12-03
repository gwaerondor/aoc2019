open Core
exception Bad_argument
            
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

let csv_of_line = String.split_on_chars ~on:[',']

let csv_of_file filename =
  match lines_of_file filename with
  | [line] -> csv_of_line line
  | _ -> raise Bad_argument

let (<.) = Fn.compose
let multiline_csv_of_file = (List.map ~f:csv_of_line) <. lines_of_file
let array_of_lines_of_file = List.to_array <. lines_of_file
let array_of_csv_of_file = List.to_array <. csv_of_file

let char_list_of_string = String.to_list
let string_of_char_list = String.concat <. List.map ~f:String.of_char
let int_of_char_list = int_of_string <. string_of_char_list
