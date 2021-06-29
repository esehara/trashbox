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
let field = site_parser numsites [];;

(* structuretype: -1 = No structure, 2 = Barracks *)
type structure = None | Barrack | Tower | GoldMine;;

(* owner: -1 = No structure, 0 = Friendly, 1 = Enemy *)
type owner = None | Friendly | Enemy;;
let owner_of_int owner = match owner with
  | 0 -> Friendly
  | 1 -> Enemy
  | -1 -> None
  | _ -> failwith "Invlid Owner";;

(* unittype: -1 = QUEEN, 0 = KNIGHT, 1 = ARCHER, 2 = GIANT*)
type unittype = Queen | Knight | Archer | Giant | Value of int ;;
type tower = {siteid: int; range: int; pos: pos};;
let unittype_of_int unittype = match unittype with
  | -1 -> Queen
  | 0 -> Knight
  | 1 -> Archer
  | 2 -> Giant
  | _ -> Value unittype;;

type info = {structure: structure; owner: owner; unittype: unittype; rank: int};;
type site = {siteid: int; pos: pos; info: info};;
type unitinfo = {unittype: unittype; owner: owner; x: int; y: int};;

let build_site siteid structuretype owner unittype rank =
  let structure = match structuretype with
    | 2 -> Barrack
    | 1 -> Tower
    | 0 -> GoldMine
    | -1 -> None
    | _ -> failwith "Invalid Structure type" in
  let owner = owner_of_int owner in
  let pos = List.find (fun x -> x.id = siteid ) field in
  let unittype = unittype_of_int unittype in
  {siteid = siteid; pos = pos; info = {structure = structure; owner = owner; unittype = unittype; rank = rank}};;

(* --- parser --- *)
let rec siteinfo_parser n result = match n with
  | 0 -> result
  | _ ->
    let siteid, ignore1, ignore2, structuretype, owner, param1, param2 =
      Scanf.sscanf (input_line stdin) " %d  %d  %d  %d  %d  %d  %d"
        (fun siteid ignore1 ignore2 structuretype owner param1 param2 ->
           (siteid, ignore1, ignore2, structuretype, owner, param1, param2)) in
    let siteinfo = build_site siteid structuretype owner param2 param1 in
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

*)
let dist queen (site_pos: pos) =
  let x = (queen.x - site_pos.x) in
  let y = (queen.y - site_pos.y) in
  let pre_sqrt = float_of_int ((x * x) + (y * y)) in
  sqrt pre_sqrt;;

let no_owner_site (sites: site list) = List.filter (fun x -> x.info.owner = None) sites;;
let near_site queen (sites: site list) =
  List.sort (fun x y -> compare (dist queen x.pos) (dist queen y.pos)) sites;;

let so_far_site queen (sites: site list) =
  List.sort (fun x y -> (compare (dist queen x.pos) (dist queen y.pos)) * -1) sites;;
(* --- Site Utility -- *)
let my_site_of_sites sites = List.filter (fun x -> x.info.owner = Friendly) sites;;

let any_of_sites sites structure_type = List.filter (fun x -> x.info.structure = structure_type) sites;;
let barrack_of_sites sites = any_of_sites sites Barrack;;
let tower_of_sites sites = any_of_sites sites Tower;;
let mine_of_sites sites = any_of_sites sites GoldMine;;
(* --- Barracks ---- *)
let any_of_barracks sites unittype = List.filter (fun x -> x.info.unittype = unittype) sites;;
let archer_of_barrack sites = any_of_barracks sites Archer;;
let knight_of_barrack sites = any_of_barracks sites Knight;;
let giant_of_barrack sites = any_of_barracks sites Giant;;

type order =
    Build of string
  | Grow of string

(* ----------------- *)
type mine = Work of int | Done;;
let mine_hash = Hashtbl.create 50;;
let grow_target_mine mine = mine.info.rank = 1;;
let growable_target_mine mine =
  match (Hashtbl.find_opt mine_hash mine.siteid) with
  | None -> true
  | Some x -> match x with
    | Done -> false
    | Work _ -> true;;

let prerr_option (option: order option) = Printf.sprintf "Option Value: %s"
    begin
      match option with
      | None -> "None"
      | Some x -> match x with
        | Build _ -> "Build"
        | Grow _ -> "Grow"
    end |> prerr_endline;;

let build_check sites build_string (option: order option) = prerr_option option;
  match option with
  | None -> begin
      let mine_size = List.length (mine_of_sites sites) in
      let tower_size = List.length (tower_of_sites sites) in
      match build_string with
      | "MINE" when mine_size < tower_size -> Some (Build "MINE")
      | "TOWER" when tower_size <= mine_size -> Some (Build "TOWER")
      | _ when (List.length sites) < 1 -> Some (Build build_string)
      | _ -> None end
  | _ -> option;;

let build_check_finally new_build_near new_build_far touched (option: order option): string option  = match option with
  | None -> None
  | Some x ->
    match x with
    | Grow x -> Some x
    | Build x ->
      let target = match x with
        | "MINE" ->
          List.hd new_build_far
        | _ -> List.hd new_build_near in
      if target.siteid = touched
      then Some (Printf.sprintf "BUILD %d %s" touched x)
      else Some (Printf.sprintf "MOVE %d %d" target.pos.x target.pos.y);;

let tower_of_site (site: site): tower = match site.info.unittype with
  | Value x -> {siteid = site.siteid; range = x; pos = site.pos}
  | _ -> failwith "cannot convate 'sites' to 'tower'";;

let my_tower_of_sites queen sites: tower list =
  my_site_of_sites sites |> tower_of_sites |> List.map tower_of_site;;

let unwrap_string_option option = match option with
  | Some x -> x
  | None -> "WAIT";;

type delay = {order: string option; time: int};;
let grow_tower_delay = ref {order = None; time = 0};;
let grow_tower queen touched sites option: order option = match option with
  | Some _ -> option
  | None ->
    grow_tower_delay :=
      begin
        match !grow_tower_delay.order with
        | Some x when !grow_tower_delay.time = 0 -> {order = None; time = 0}
        | Some x -> {order = Some x; time = !grow_tower_delay.time - 1}
        | None -> {order = None; time = 0}
      end;
    match !grow_tower_delay.order with
    | Some x -> Some (Grow x)
    | None ->
      let towers = my_tower_of_sites queen sites |> List.filter (fun x -> x.range < 300) in
      (* Debug *) Printf.sprintf "Non grow tower: %d" (List.length towers) |> prerr_endline;
      match towers with
      | [] -> None
      | hd::tl ->
        (* Debug *) hd.range |> Printf.sprintf "Tower Range: %d" |> prerr_endline;
        let order =
          begin
            if touched = hd.siteid
            then Printf.sprintf "BUILD %d TOWER" touched
            else Printf.sprintf "MOVE %d %d" hd.pos.x hd.pos.y
          end in
        grow_tower_delay := {order = Some order; time = 5};
        Some (Grow order);;

let order_grow_mine sites siteid =
  let order_string siteid = Some (Grow (Printf.sprintf "BUILD %d MINE" siteid)) in
  match Hashtbl.find_opt mine_hash siteid with
  | None -> Hashtbl.add mine_hash siteid (Work 1); order_string siteid
  | Some x -> match x with
    | Done -> failwith "Done is filltering, but it is done..."
    | Work previous_rank ->
      let current_mine = List.find (fun x -> x.siteid = siteid) sites in
      if current_mine.siteid = previous_rank
      then (Hashtbl.replace mine_hash siteid Done; None)
      else (Hashtbl.replace mine_hash siteid (Work current_mine.siteid); order_string siteid);;


let grow_mine queen touched sites option: order option = match option with
  | Some _ -> option
  | None ->
    let mines = mine_of_sites sites |> List.filter grow_target_mine
                |> List.filter growable_target_mine
    in
    match mines with
    | [] -> None
    | hd::tl ->
      if touched = hd.siteid
      then order_grow_mine mines hd.siteid
      else Some (Grow (Printf.sprintf "MOVE %d %d" hd.pos.x hd.pos.y))

let queen_stategy units touched (sites: site list) =
  (* Debug *) Printf.sprintf "Queen Touch: %d" touched |> prerr_endline;
  let my_queen = find_my_queen units in
  let enemy_queen = find_enemy_queen units in
  let new_build_near = no_owner_site sites |> near_site my_queen in
  let new_build_far = no_owner_site sites |> so_far_site enemy_queen in
  if List.length new_build_near > 0 then
    let sites = my_site_of_sites sites in
    build_check (archer_of_barrack sites) "BARRACKS-ARCHER" None
    |> grow_tower my_queen touched sites
    |> grow_mine my_queen touched sites
    |> build_check sites "MINE"
    |> build_check (knight_of_barrack sites) "BARRACKS-KNIGHT"
    |> build_check (giant_of_barrack sites) "BARRACKS-GIANT"
    |> build_check sites "TOWER"
    |> build_check_finally new_build_near new_build_far touched
    |> unwrap_string_option
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
    (* Write an action using print_endline *)
    (* To debug: prerr_endline "Debug message"; *)

    (* First line: A valid queen action *)
    (* Second line: A set of training instructions *)
    queen_stategy units touchedsite sites |> print_endline;
    train_strategy gold sites units |> print_endline;
done;
