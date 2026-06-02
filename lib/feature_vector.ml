open Types

let trie_depth = 16
let vector_length = 128

type feature_vector = int array

module IntMap = Map.Make(Int)

type node = {
  mutable children : node IntMap.t;
  mutable ids : int list;
}

type t = {
  root : node;
  vectors : (int, feature_vector) Hashtbl.t;
}

let symbol_to_id = Hashtbl.create 500
let next_symbol_id = ref 3

let get_symbol_id s =
  try Hashtbl.find symbol_to_id s
  with Not_found ->
    if !next_symbol_id < vector_length then (
      let id = !next_symbol_id in
      Hashtbl.add symbol_to_id s id;
      incr next_symbol_id;
      id
    ) else -1

let compute_vector (c : clause) =
  let v = Array.make vector_length 0 in
  let n_pos = ref 0 in
  let n_neg = ref 0 in
  
  let rec traverse_term = function
    | Var _ -> ()
    | Fun (f, args) ->
        let id = get_symbol_id f in
        if id <> -1 then v.(id) <- v.(id) + 1;
        List.iter traverse_term args
  in

  let traverse_lit = function
    | Pos { pred; args } ->
        incr n_pos;
        let id = get_symbol_id pred in
        if id <> -1 then v.(id) <- v.(id) + 1;
        List.iter traverse_term args
    | Neg { pred; args } ->
        incr n_neg;
        let id = get_symbol_id pred in
        if id <> -1 then v.(id) <- v.(id) + 1;
        List.iter traverse_term args
  in

  List.iter traverse_lit c;
  v.(0) <- List.length c;
  v.(1) <- !n_pos;
  v.(2) <- !n_neg;
  v

let create_node () = { children = IntMap.empty; ids = [] }

let create () = {
  root = create_node ();
  vectors = Hashtbl.create 1000;
}

let add index c id =
  let vec = compute_vector c in
  Hashtbl.add index.vectors id vec;
  let rec insert curr idx =
    if idx = trie_depth then
      curr.ids <- id :: curr.ids
    else
      let v = vec.(idx) in
      let next =
        match IntMap.find_opt v curr.children with
        | Some n -> n
        | None ->
            let n = create_node () in
            curr.children <- IntMap.add v n curr.children;
            n
      in
      insert next (idx + 1)
  in
  insert index.root 0

let remove index id =
  Hashtbl.remove index.vectors id

let check_subsumed v_c v_d =
  let ok = ref true in
  for i = 0 to vector_length - 1 do
    if v_c.(i) > v_d.(i) then ok := false
  done;
  !ok

let check_subsuming v_c v_d =
  let ok = ref true in
  for i = 0 to vector_length - 1 do
    if v_d.(i) > v_c.(i) then ok := false
  done;
  !ok

let find_subsumed_candidates index c =
  let v_c = compute_vector c in
  let results = ref [] in
  
  let rec search curr idx =
    if idx = trie_depth then
      List.iter (fun id -> 
        match Hashtbl.find_opt index.vectors id with
        | Some v_d -> if check_subsumed v_c v_d then results := id :: !results
        | None -> ()
      ) curr.ids
    else
      let val_c = v_c.(idx) in
      IntMap.iter (fun val_d child ->
        if val_d >= val_c then search child (idx + 1)
      ) curr.children
  in
  search index.root 0;
  !results

let find_subsuming_candidates index c =
  let v_c = compute_vector c in
  let results = ref [] in

  let rec search curr idx =
    if idx = trie_depth then
      List.iter (fun id -> 
        match Hashtbl.find_opt index.vectors id with
        | Some v_d -> if check_subsuming v_c v_d then results := id :: !results
        | None -> ()
      ) curr.ids
    else
      let val_c = v_c.(idx) in
      IntMap.iter (fun val_d child ->
        if val_d <= val_c then search child (idx + 1)
      ) curr.children
  in
  search index.root 0;
  !results
