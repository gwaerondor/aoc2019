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

let rec password_rule_matches lower upper current =
  if current > upper
  then []
  else
    let pw = Aoc_lib.char_list_of_int current in
    if (is_monotonically_increasing pw) && (has_double pw)
    then current :: (password_rule_matches lower upper (current + 1))
    else (password_rule_matches lower upper (current + 1))

let day04_1 =
  password_rule_matches 245182 790572 245182
  |> List.length
  |> Printf.printf "Part 1: %d\n"
