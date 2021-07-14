(* Bring data on patient samples from the diagnosis machine to the laboratory with enough molecules to produce medicine! *)

let projectcount = int_of_string (input_line stdin);;

(* --- current league ignore --- *)
for i = 0 to projectcount - 1 do
  let a, b, c, d, e = Scanf.sscanf (input_line stdin) " %d  %d  %d  %d  %d" (fun a b c d e -> (a, b, c, d, e)) in
  ();
done;;
(* ----- *)

type place = Diagnosis | Molecules | Laboratory;;
type life = {a: int; b: int; c: int; d: int; e: int};;
type player = {where: place;
               storage: life;
               expart: life};;

let player_parser_line () = 
  let target, eta, score,
      storagea, storageb, storagec, storaged, storagee,
      expertisea, expertiseb, expertisec, expertised, expertisee =
    Scanf.sscanf (input_line stdin) " %s  %d  %d  %d  %d  %d  %d  %d  %d  %d  %d  %d  %d"
      (fun target eta score
        storagea storageb storagec storaged storagee
        expertisea expertiseb expertisec expertised expertisee ->
        (target, eta, score,
         storagea, storageb, storagec, storaged, storagee,
         expertisea, expertiseb, expertisec, expertised, expertisee)) in
  {where = target;
   storage = {a = storagea; b = storageb; c = storagec; d = storaged; e = storagee};
   expart = {a = expertisea; b = expertiseb; c = expertisec; d = expertised; e = expertisee}};;

let available_parser () =
  let availablea, availableb, availablec, availabled, availablee = Scanf.sscanf (input_line stdin) " %d  %d  %d  %d  %d" (fun availablea availableb availablec availabled availablee -> (availablea, availableb, availablec, availabled, availablee)) in
  {a = availablea; b = availableb; c = availablec; d = availabled; e = availablee};;

let player_parser () =
  let rec acc n result = match n with
    | 0 -> List.rev result
    | _ -> acc (n - 1) (player_parser_line () :: result) in
  acc 2 [];;

type who = Player | Opponent;;
type sample = {
  id: int; carry: who;
  rank: int; expert_gain: string; health: int;
  cost: life};;

let sample_parser_line () =
  let sampleid, carriedby, rank, expertisegain, health, costa, costb, costc, costd, coste = Scanf.sscanf (input_line stdin) " %d  %d  %d  %s  %d  %d  %d  %d  %d  %d" (fun sampleid carriedby rank expertisegain health costa costb costc costd coste -> (sampleid, carriedby, rank, expertisegain, health, costa, costb, costc, costd, coste)) in
  {id = sampleid; carry = carriedby;
   rank = rank; expert_gain = expertisegain;
  health = health; cost = {a = costa; b = costb; c = costc; d = costd; e = coste}};;

let sample_parser c =
  let rec acc c result = match c with
    | 0 -> result
    | _ -> acc c (sample_parser_line () :: result) in
  acc c [];;

(* game loop *)
while true do
  let players = player_parser () in
  let available = available_parser () in
  let samplecount = int_of_string (input_line stdin) in
  let samples = sample_parser samplecount in
  (* Write an action using print_endline *)
  (* To debug: prerr_endline "Debug message"; *)
  Printf.printf "GOTO DIAGNOSIS";
  print_endline "";
  ();
done;
