open Core

let calc_fuel n = n / 3 - 2

let rec calc_fuel_2 n =
  let sum = max 0 (calc_fuel n) in
  if sum <= 0 then 0 else sum + calc_fuel_2 sum

let day_1 (fpath : string) (f : int -> int) =
  In_channel.read_lines fpath
  |> List.fold_left ~init:0 ~f:(fun acc ele -> acc + f (Int.of_string ele)
