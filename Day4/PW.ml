open Core

let lo = 124075

let hi = 580769

let num_to_list n =
  string_of_int n
  |> String.to_list
  |> List.map ~f:(fun x -> int_of_char x - 48)

let rec non_dec = function
  | [] -> true
  | [_] -> true
  | x1 :: x2 :: xs when x1 > x2 -> false
  |  _ :: x2 :: xs -> non_dec (x2 :: xs)

let rec same_adjacent = function
  | [] -> false
  | [_] -> false
  | x1 :: x2 :: xs when x1 = x2 -> true
  |  _ :: x2 :: xs -> same_adjacent (x2 :: xs)

let rec same_adjacent_2 = function
  | [] -> false
  | [_] -> false
  | x :: xs -> let same x' = x' = x  in
               if List.take_while ~f:same xs
                  |> List.length
                  |> (=) 1
               then true
               else same_adjacent_2 (List.drop_while ~f:same xs)

let part lo hi rule =
  List.init (hi - lo + 1) (fun x -> x + lo)
  |> List.map ~f:num_to_list
  |> List.filter ~f:non_dec
  |> List.filter ~f:rule
  |> List.length

let run () =
  Printf.printf "Answer for Part 1 = %d\n" (part lo hi same_adjacent);
  Printf.printf "Answer for Part 2 = %d\n" (part lo hi same_adjacent_2)
