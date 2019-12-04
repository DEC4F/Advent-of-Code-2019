open Core

type points = {x : int ; y : int}

let origin = {x = 0 ; y = 0}

let manhattan_dist p1 p2 =
  (abs p2.x - p1.x) + (abs p2.y - p1.y)

let new_point p move =
  let norm = String.sub move 1 ((String.length move) - 1)
             |> int_of_string
  in
  match move.[0] with
    'U' -> {x = p.x ; y = p.y + norm}
  | 'D' -> {x = p.x ; y = p.y - norm}
  | 'L' -> {x = p.x - norm ; y = p.y}
  | 'R' -> {x = p.x + norm ; y = p.y}
  |  _  -> failwith "The direction is limited to U, D, L and R"

let calc_path moves =
  let rec walk p moves =
    match moves with
      []      -> []
    | [x]     -> [new_point p x]
    | x :: xs -> let new_p = (new_point p x) in
                 new_p :: (walk new_p xs)
  in
  walk origin moves

let parse fpath =
  In_channel.read_lines fpath
  |> List.map ~f:(fun x -> String.split_on_chars ~on:[','] x)
  |> List.map ~f:(calc_path)
