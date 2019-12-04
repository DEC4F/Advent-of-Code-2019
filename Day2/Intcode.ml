open Core

let read fpath =
  In_channel.read_all fpath
  |> String.split_on_chars ~on:[',' ; '\n' ; '\r']
  |> List.filter ~f:(fun ele -> ele <> "")
  |> Array.of_list_map ~f:Int.of_string

let initiate noun verb data =
  data.(1) <- noun;
  data.(2) <- verb;
  data

let execute data ic =
  let op1 = data.(data.(ic + 1)) in
  let op2 = data.(data.(ic + 2)) in
  match data.(ic) with
    1 -> data.(data.(ic + 3)) <- op1 + op2
  | 2 -> data.(data.(ic + 3)) <- op1 * op2
  | _ as i -> invalid_arg ("Halt at invalid input: " ^ (string_of_int i))

let rec run_until_halt ic data =
  match data.(ic) with
    99 -> data.(0)
  | _ -> execute data ic;
         run_until_halt (ic + 4) data

let rec find_verb noun data =
  let ans = initiate noun 0 data
            |> run_until_halt 0
  in
  let verb = 19690720 - ans in
  if verb < 100 then
    100 * noun + verb
  else
    find_verb (noun + 1) (read "input.txt")

let run () =
  Printf.printf "Answer for Part 1 = %d\n" (read "input.txt" |> initiate 12 2 |> run_until_halt 0);
  Printf.printf "Answer for Part 2 = %d\n" (read "input.txt" |> find_verb 0)
