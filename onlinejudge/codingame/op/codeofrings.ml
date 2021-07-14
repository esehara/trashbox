(* Auto-generated code below aims at helping you parse *)
(* the standard input according to the problem statement. *)

let bf_of_char chr =
  if ' ' = chr then 0 else (int_of_char chr) - 64;;
let pointer = ref 0;;
let memory = Array.make 30 0;;

let is_opt opt y = match opt with
  | None -> false
  | Some x -> x = y;;

let search_unused_memory char_int =
  let rec acc result =
    let memory_value = memory.(!pointer) in
    match memory_value with
    | 0 -> String.concat "" (List.rev result)
    | _ ->
      pointer := !pointer + 1;
      acc (">" :: result) in
  acc [];;

let set_letter char_int =
  memory.(!pointer) <- char_int;
  if char_int < 27 - char_int
  then String.make char_int '+'
  else String.make (27 - char_int) '-';;

let index_at char_int =
  let rec acc result =
    if memory.(result) = char_int then result
    else acc (result + 1) in
  acc 0;;

let move_string char_int at =
  let rec acc result =
    if at = !pointer then String.concat "" (List.rev result)
    else if at < !pointer
    then
      begin
        pointer := !pointer - 1;
        acc ("<" :: result)
      end
    else
      begin
        pointer := !pointer + 1;
        acc (">" :: result)
      end in
  acc [];;

let gen_letter char_int =
  let has_letter_in_memory =
    Array.exists (fun x -> x = char_int) memory in
  String.concat "" begin
    match has_letter_in_memory with
    | true  ->
      let order1 = move_string char_int (index_at char_int) in
      [order1; "."]
    | false ->
      let order1 = search_unused_memory char_int in
      let order2 = set_letter char_int in
      [order1; order2; "."]
  end |> print_string;;

let rec gen_bf_code = function
  | [] -> ();
  | hd::tl ->
    gen_letter hd;
    gen_bf_code tl;;

let solve str = 
  String.to_seq str
  |> List.of_seq
  |> List.map bf_of_char
  |> gen_bf_code;;

let () =
  let magicphrase = input_line stdin in
  solve magicphrase;
  print_newline ();;
