open Core

let lo = 124075

let hi = 580769

let num_to_list n =
  string_of_int n
  |> String.to_list
  |> List.map ~f:(fun x -> int_of_char x - 48)

let rec same_adjacent = function
  | [] -> false
  | [x] -> false
  | x1 :: x2 :: xs when x1 = x2 -> true
  |  _ :: x2 :: xs -> same_adjacent (x2 :: xs)

let rec non_dec = function
  | [] -> true
  | [x] -> true
  | x1 :: x2 :: xs when x1 > x2 -> false
  |  _ :: x2 :: xs -> non_dec (x2 :: xs)

let part_1 lo hi =
  List.init (hi - lo + 1) (fun x -> x + lo)
  |> List.map ~f:num_to_list
  |> List.filter ~f:same_adjacent
  |> List.filter ~f:non_dec
  |> List.length

let run () =
  Printf.printf "Answer for Part 1 = %d" (part_1 lo hi)
