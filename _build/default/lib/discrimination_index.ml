open Types

type sign =
  | Positive
  | Negative

type token =
  | TSign of sign
  | TPred of string * int
  | TFun of string * int
  | TVar

type entry = {
  clause_id : int;
  lit_index : int;
  literal : literal;
}

type node = {
  mutable entries : entry list;
  children : (token, node) Hashtbl.t;
}

type t = {
  root : node;
  buckets : ((sign * string * int), entry list ref) Hashtbl.t;
}

let create_node () =
  { entries = []; children = Hashtbl.create 17 }

let create () =
  { root = create_node (); buckets = Hashtbl.create 251 }

let sign_of_literal = function
  | Pos _ -> Positive
  | Neg _ -> Negative

let opposite_sign = function
  | Positive -> Negative
  | Negative -> Positive

let atom_of_literal = function
  | Pos a | Neg a -> a

let rec term_has_var = function
  | Var _ -> true
  | Fun (_, args) -> List.exists term_has_var args

let literal_has_var lit =
  let a = atom_of_literal lit in
  List.exists term_has_var a.args

let rec tokens_of_term = function
  | Var _ -> [TVar]
  | Fun (f, args) ->
      TFun (f, List.length args)
      :: List.concat (List.map tokens_of_term args)

let tokens_of_literal lit =
  let s = sign_of_literal lit in
  let a = atom_of_literal lit in
  TSign s
  :: TPred (a.pred, List.length a.args)
  :: List.concat (List.map tokens_of_term a.args)

let bucket_key lit =
  let s = sign_of_literal lit in
  let a = atom_of_literal lit in
  (s, a.pred, List.length a.args)

let get_bucket idx key =
  match Hashtbl.find_opt idx.buckets key with
  | Some r -> r
  | None ->
      let r = ref [] in
      Hashtbl.add idx.buckets key r;
      r

let rec insert node toks entry =
  match toks with
  | [] ->
      node.entries <- entry :: node.entries
  | tok :: rest ->
      let child =
        match Hashtbl.find_opt node.children tok with
        | Some n -> n
        | None ->
            let n = create_node () in
            Hashtbl.add node.children tok n;
            n
      in
      insert child rest entry

let add_literal idx entry =
  let toks = tokens_of_literal entry.literal in
  insert idx.root toks entry;
  let b = get_bucket idx (bucket_key entry.literal) in
  b := entry :: !b

let add_clause idx ~clause_id clause =
  List.iteri
    (fun lit_index literal ->
      add_literal idx { clause_id; lit_index; literal })
    clause

let rec collect_all node acc =
  let acc = List.rev_append node.entries acc in
  Hashtbl.fold
    (fun _ child acc -> collect_all child acc)
    node.children
    acc

let rec query node toks =
  match toks with
  | [] ->
      collect_all node []
  | TVar :: _ ->
      collect_all node []
  | tok :: rest ->
      let exact =
        match Hashtbl.find_opt node.children tok with
        | None -> []
        | Some child -> query child rest
      in
      let wildcard =
        match Hashtbl.find_opt node.children TVar with
        | None -> []
        | Some child -> query child rest
      in
      exact @ wildcard

let find_by_sign idx wanted_sign lit =
  let a = atom_of_literal lit in
  let key = (wanted_sign, a.pred, List.length a.args) in
  match Hashtbl.find_opt idx.buckets key with
  | None -> []
  | Some bucket ->
      if literal_has_var lit then
        !bucket
      else
        let toks =
          TSign wanted_sign
          :: TPred (a.pred, List.length a.args)
          :: List.concat (List.map tokens_of_term a.args)
        in
        query idx.root toks

let find_complementary idx lit =
  find_by_sign idx (opposite_sign (sign_of_literal lit)) lit

let find_same_sign_unifiable idx lit =
  find_by_sign idx (sign_of_literal lit) lit
