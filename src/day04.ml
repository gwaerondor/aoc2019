open Core
open Aoc_lib

let rec is_monotonically_increasing = function
  | [_] -> true
  | a::b::_ when a > b -> false
  | _::b::r -> is_monotonically_increasing (b::r)

let rec has_double = function
  | [_] -> false
  | a::b::_ when a = b -> true
  | _::b::r -> has_double (b::r)

let rec has_strictly_double = function
  | [] -> false
  | a::r -> let pred = (=) a in
            if List.take_while ~f:pred r |> List.length |> (=) 1
            then true
            else has_strictly_double (List.drop_while ~f:pred r)

let rec password_rule_matches lower upper current rule =
  if current > upper
  then []
  else
    let pw = Aoc_lib.char_list_of_int current in
    if rule pw
    then current :: (password_rule_matches lower upper (current + 1) rule)
    else (password_rule_matches lower upper (current + 1) rule)

let lax_rule pw =
  (is_monotonically_increasing pw) && (has_double pw)

let strict_rule pw =
  (is_monotonically_increasing pw) && (has_strictly_double pw)

let day04_1 =
  password_rule_matches 245182 790572 245182 lax_rule
  |> List.length
  |> Printf.printf "Part 1: %d\n";
  password_rule_matches 245182 790572 245182 strict_rule
  |> List.length
  |> Printf.printf "Part 2: %d\n"
