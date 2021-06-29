(* Auto-generated code below aims at helping you parse *)
(* the standard input according to the problem statement. *)

let numsites = int_of_string (input_line stdin);;
type pos = {id: int; x: int; y: int; radius: int};;
let rec site_parser n result = match n with
  | 0 -> result
  | _ ->
    let siteid, x, y, radius = Scanf.sscanf (input_line stdin) " %d  %d  %d  %d" (fun siteid x y radius -> (siteid, x, y, radius)) in
    let site = {id = siteid; x = x; y = y; radius = radius } in
    site_parser (n - 1) (site :: result);;
let sites = site_parser numsites [];;

(* structuretype: -1 = No structure, 2 = Barracks *)
type structure = None | Barrack | Tower;;

(* owner: -1 = No structure, 0 = Friendly, 1 = Enemy *)
type owner = None | Friendly | Enemy;;
let owner_of_int owner = match owner with
  | 0 -> Friendly
  | 1 -> Enemy
  | -1 -> None
  | _ -> failwith "Invlid Owner";;

(* unittype: -1 = QUEEN, 0 = KNIGHT, 1 = ARCHER, 2 = GIANT*)
type unittype = Queen | Knight | Archer | Giant | Tower of int;;
type tower = {siteid: int; range: int; pos: pos};;
let unittype_of_int unittype = match unittype with
  | -1 -> Queen
  | 0 -> Knight
  | 1 -> Archer
  | 2 -> Giant
  | _ -> Tower unittype;;

type info = {structure: structure; owner: owner; unittype: unittype};;
type site = {siteid: int; pos: pos; info: info};;
type unitinfo = {unittype: unittype; owner: owner; x: int; y: int};;

let build_site siteid structuretype owner unittype =
  let structure = match structuretype with
    | 2 -> Barrack
    | 1 -> Tower
    | -1 -> None
    | _ -> failwith "Invalid Structure type" in
  let owner = owner_of_int owner in
  let pos = List.find (fun x -> x.id = siteid ) sites in
  let unittype = unittype_of_int unittype in
  {siteid = siteid; pos = pos; info = {structure = structure; owner = owner; unittype = unittype}};;

(* --- parser --- *)
let rec siteinfo_parser n result = match n with
  | 0 -> result
  | _ ->
    let siteid, ignore1, ignore2, structuretype, owner, param1, param2 =
      Scanf.sscanf (input_line stdin) " %d  %d  %d  %d  %d  %d  %d"
        (fun siteid ignore1 ignore2 structuretype owner param1 param2 ->
           (siteid, ignore1, ignore2, structuretype, owner, param1, param2)) in
    let siteinfo = build_site siteid structuretype owner param2 in
    siteinfo_parser (n - 1) (siteinfo :: result);;

let rec unit_parser n result = match n with
  | 0 -> result
  | _ ->
    let x, y, owner, unittype, health =
      Scanf.sscanf (input_line stdin) " %d  %d  %d  %d  %d"
        (fun x y owner unittype health -> (x, y, owner, unittype, health)) in
    let unittype = unittype_of_int unittype in
    let owner = owner_of_int owner in
    let unitinfo = {unittype = unittype; owner = owner; x = x; y = y} in
    unit_parser (n - 1) (unitinfo :: result);;
(* -------------- *)

(* - get function --- *)
let site_info () = siteinfo_parser numsites [];;
let my_units (units: unitinfo list) = List.filter (fun x -> x.owner = Friendly) units;;
let enemy_units (units: unitinfo list) = List.filter (fun x -> x.owner = Enemy) units;;
let find_enemy_queen units = enemy_units units |> List.find (fun x -> x.unittype = Queen);;
let find_my_queen units = my_units units |> List.find (fun x -> x.unittype = Queen);;
(* - game strategy --- *)
(*
   -- Queen Strategy --
   Queenがやること -> 一行目:
     -> 移動する = MOVE
     -> 建物を立てる = BUILD siteid (BALLACKS-KNIGHT | BALLACKES-ARCHER)
     -> 待つ = WAIT

   Wood League ->
     -> とりあえず一番近くの空いているsiteにバラックを建てる
*)
let dist queen (site_pos: pos) =
  let x = (queen.x - site_pos.x) in
  let y = (queen.y - site_pos.y) in
  let pre_sqrt = float_of_int ((x * x) + (y * y)) in
  sqrt pre_sqrt;;

let no_owner_site (sites: site list) = List.filter (fun x -> x.info.owner = None) sites;;
let near_site queen (sites: site list) = List.sort (fun x y -> compare (dist queen x.pos) (dist queen y.pos)) sites;;
let my_site_of_sites sites = List.filter (fun x -> x.info.owner = Friendly) sites;;
let barrack_of_sites sites = List.filter (fun x -> x.info.structure = Barrack) sites;;
let tower_of_sites sites = List.filter (fun x -> x.info.structure = Tower) sites;;

(* --- Barracks ---- *)
let any_of_barracks sites unittype = List.filter (fun x -> x.info.unittype = unittype) sites;;
let archer_of_barrack sites = any_of_barracks sites Archer;;
let knight_of_barrack sites = any_of_barracks sites Knight;;
let giant_of_barrack sites = any_of_barracks sites Giant;;
(* ----------------- *)

let build_check sites build_string (option: string option) = match option with
  | None when (List.length sites) < 1 -> Some build_string
  | _ -> option;;

let build_check_for_tower site build_string (option: string option) = match option with
  | None when (List.length sites) < 3 -> Some build_string
  | _ -> option;;

let build_check_finally new_build_target touched (option: string option): string option  = match option with
  | Some x ->
    let target = List.hd new_build_target in

    (* Debug *) Printf.sprintf "Target Site: %d" target.siteid |> prerr_endline;
    if target.siteid = touched then Some (Printf.sprintf "BUILD %d %s" touched x)
    else Some (Printf.sprintf "MOVE %d %d" target.pos.x target.pos.y)
  | None -> None;;

let tower_of_site (site: site): tower = match site.info.unittype with
  | Tower x -> {siteid = site.siteid; range = x; pos = site.pos}
  | _ -> failwith "cannot convate 'sites' to 'tower'";;

let my_tower_of_sites queen sites: tower list =
  my_site_of_sites sites |> tower_of_sites |> List.map tower_of_site
  |> List.sort (fun (x: tower) (y: tower) -> compare (dist queen x.pos) (dist queen y.pos));;

let grow_tower_if_build_check_none queen touched sites option: string = match option with
  | Some x -> x
  | None ->
    let t = my_tower_of_sites queen sites |> List.hd in
    if touched = t.siteid
    then Printf.sprintf "BUILD %d TOWER" touched
    else Printf.sprintf "MOVE %d %d" t.pos.x t.pos.y;;

let queen_stategy queen touched (sites: site list) =
  (* Debug *) Printf.sprintf "Queen Touch: %d" touched |> prerr_endline;
  let new_build_target = no_owner_site sites |> near_site queen in
  if List.length new_build_target > 0 then
        let sites = my_site_of_sites sites in
        build_check (tower_of_sites sites) "TOWER" None
        |> build_check (knight_of_barrack sites) "BARRACKS-KNIGHT"
        |> build_check (archer_of_barrack sites) "BARRACKS-ARCHER"
        |> build_check (giant_of_barrack sites) "BARRACKS-GIANT"
        |> build_check_for_tower (tower_of_sites sites) "TOWER"
        |> build_check_finally new_build_target touched
        |> grow_tower_if_build_check_none queen touched sites
  else "WAIT";;

(*
   -- Train Starategy --
   Trainの後ろに羅列していけば、その合計金額を払って訓練を開始する
   KNIGHT -> 80
   ARCHER -> 100 (ただしクイーンを攻撃しない)

   ARCHERが3体以上いるならKNIGHTだけを作る
   ARCHERが3体以上いないならARCHERだけを作る

   -> どうせなので敵の一番近くから出したほうがよい
*)


let archers_of_units units = List.filter (fun x -> x.unittype = Archer) units;;
let giants_of_units units = List.filter (fun x -> x.unittype = Giant) units;;

let rec can_train gold units sites =
  let my_queen = find_my_queen units in
  let enemy_queen = find_enemy_queen units in
  let my_units = my_units units in
  let archers = archers_of_units my_units in
  let giants = giants_of_units my_units in
  let archer_barracks = archer_of_barrack sites in
  let knight_barracks = knight_of_barrack sites in
  let giant_barracks = giant_of_barrack sites in
  if (List.length giants < 1) && (List.length giant_barracks > 0) then
    if (gold < 140) then []
    else [near_site my_queen giant_barracks |> List.hd]
  else if (List.length archers < 2) && (List.length archer_barracks > 0) then
    begin
      if (gold < 100) then []
      else
        [near_site my_queen archer_barracks |> List.hd]
    end
  else if (List.length knight_barracks > 0) then
    begin
      if (gold < 80) then []
      else [near_site enemy_queen knight_barracks |> List.hd]
    end
  else [];;

let train_strategy gold sites units =
  let my_barrack_sites = my_site_of_sites sites |> barrack_of_sites in
  (* Debug *) Printf.sprintf "My site barracks: %d" (List.length (my_site_of_sites sites |> barrack_of_sites))
                         |> prerr_endline;

  let barrack_sites = can_train gold units my_barrack_sites in
  if (List.length barrack_sites) = 0 then "TRAIN"
  else
    List.map (fun x -> x.siteid |> string_of_int) barrack_sites |>
    String.concat " " |> Printf.sprintf "TRAIN %s";;
(* ------------------- *)

(* game loop *)
while true do
    (* touchedsite: -1 if none *)
    let gold, touchedsite = Scanf.sscanf (input_line stdin) " %d  %d" (fun gold touchedsite -> (gold, touchedsite)) in

    let sites = site_info () in

    let numunits = int_of_string (input_line stdin) in
    let units = unit_parser numunits [] in
    let queen_unit =
      my_units units
      |> List.find (fun x -> x.unittype = Queen) in
    (* Write an action using print_endline *)
    (* To debug: prerr_endline "Debug message"; *)

    (* First line: A valid queen action *)
    (* Second line: A set of training instructions *)
    queen_stategy queen_unit touchedsite sites |> print_endline;
    train_strategy gold sites units |> print_endline;
done;
