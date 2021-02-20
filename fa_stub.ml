
(* ******************************************** *)
(** Basic functions on finite automata *)
(* ******************************************** *)
(**
   Code stub for assignment 1
*)

type symbol = char
type input = char list

type state = string

(* transition function *)
type tf = (state * symbol * state) list

(* initial state * transition function * end state *)
type fa = { states: state list; start:state; tf: tf; final: state list}


(* ******************************************** *)
(* Examples of automata *)
(* ******************************************** *)

let a = {states = ["q0";"q1";"q2"];
         start = "q0";
         tf = [("q0",'a',"q1"); ("q1",'b',"q1"); ("q1",'c',"q2")];
         final = ["q2"]}

let a2 = {states = ["q0";"q1";"q2";"q3";"q4"];
          start = "q0";
          tf = [("q0",'a',"q1"); ("q1",'b',"q1")
               ; ("q1",'c',"q2");  ("q3",'a',"q4")];
          final= ["q2"]
         }

let a3 = {states = ["q0";"q1";"q2";"q3"];
         start = "q0";
         tf = [("q0",'a',"q1"); ("q1",'b',"q1"); ("q1",'c',"q2"); ("q2", 'd', "q3")];
         final = ["q2"; "q3"]}

let a4 = {states = ["q0"];
         start = "q0";
         tf = [("q0",'a',"q0")];
         final = ["q0"]}

let deadA = {states = ["q0";"q1";"q2";"q3";"q4"];
          start = "q0";
          tf = [("q0",'a',"q1"); ("q1",'b',"q1")
               ; ("q1",'c',"q2");  ("q3",'a',"q4")];
          final= ["q2";"q4"]
         }

let a5 = {states = ["q0"; "q1"; "q2"; "q3";"q4"];
         start = "q0";
         tf = [("q0",'a',"q1"); ("q2",'b',"q3")
         ; ("q3",'c',"q2"); ("q0",'a',"q3"); ("q3",'e',"q4")];
         final = ["q2"]}

let a6 = {states = ["q0"; "q1"; "q2"; "q3"];
         start = "q0";
         tf = [("q0",'a',"q1"); ("q2",'b',"q3")
         ; ("q3",'c',"q2")];
         final = ["q1"]}

let a7 = {states = ["q0"; "q1"; "q2"; "q3"; "q4"; "q5"];
         start = "q0";
         tf = [("q0",'a',"q1"); ("q1",'b',"q1")
         ; ("q1",'c',"q2"); ("q2",'d',"q2"); ("q2",'e',"q3")
         ; ("q3",'f',"q3"); ("q3",'g',"q4"); ("q4",'h',"q4")
         ; ("q4",'i',"q5"); ("q5",'j',"q4")];
         final = ["q5"]}

let a8 = {states = ["q0"; "q1";"q2"];
         start = "q0";
         tf = [("q0",'a',"q1");("q1",'b',"q2"); ("q2",'c',"q0")];
         final = ["q1"]}
         
let tf_of_a = [("q0",'a',"q1"); ("q1",'b',"q1"); ("q1",'c',"q2")]

let nfa = {states = ["q0";"q1";"q2";"q3";"q4"];
         start = "q0";
         tf = [("q0",'a',"q1"); ("q0", 'a', "q3"); ("q1",'b',"q1"); ("q1",'c',"q2"); ("q2",'d',"q4")];
         final = ["q2";"q3"]}

let nfa2 = {states = ["q0";"q1";"q2";"q3";"q4"];
         start = "q0";
         tf = [("q0",'a',"q1"); ("q0", 'a', "q3"); ("q0", 'a', "q4"); ("q1",'b',"q1"); ("q1",'c',"q2"); ("q1",'b',"q3"); ("q1", 'e', "q4")];
         final = ["q2";"q3"; "q4"]}

let nfa3 = {states = ["q0";"q1";"q2";"q3";"q4"; "q5"; "q6"];
         start = "q0";
         tf = [("q0",'a',"q1"); ("q0", 'a', "q2"); ("q1", 'b', "q3"); ("q2",'c',"q4"); ("q3",'d',"q5"); ("q4",'e',"q6")];
         final = ["q5";"q6"]}


(* ******************************************** *)
(* Helper functions *)
(* ******************************************** *)

let input_of_string s =
  let rec exp i l =
    if i < 0 then l else exp (i - 1) (s.[i] :: l) in
  exp (String.length s - 1) []

let extract i =
  match i with
  | Some i -> i
  | None -> ""

let rec rem_extra l =
  match l with
  | [] -> []
  | h::tl ->
  if List.mem h tl
  then rem_extra tl
  else h::rem_extra tl

let rec list_diff l1 l2 =
  match l1 with 
  | [] -> []
  | h::t when List.mem h l2 -> list_diff t l2
  | h::t -> h:: list_diff t l2

let rec intersection l1 l2 =
  match l1 with
  | [] -> []
  | h::tl when List.mem h l2 -> h::intersection tl l2
  | h::tl -> intersection tl l2


(* ******************************************** *)
(* Simulating automata *)
(* ******************************************** *)

let get_origin_state (x,y,z) = x
let get_sym (x,y,z) = y
let get_end_state (x,y,z) = z


let rec apply_transition_function (f: tf) (st: state) (sym: symbol) : state option = 
  match f with
  | [] -> None
  | h::t -> 
  if (get_origin_state (h) = st) && (get_sym (h) = sym)
  then Some (get_end_state (h))
  else apply_transition_function t st sym

let accept (finAuto: fa) (word: input) : bool = 
  let rec help (finAuto: fa) (st: state) (word: input) : state list =
    match word with
    | [] -> []
    | h::t -> 
    let ex = extract (apply_transition_function finAuto.tf st h) in
    ex::[] @ help finAuto ex t
  in
  List.mem (List.hd finAuto.final) (help finAuto finAuto.start word)


let rec deterministic (finAuto: fa) : bool =
  let rec next (f: tf) (st: state) (sym: symbol) : state list =
    match f with
    | [] -> []
    | h::tl ->
    if apply_transition_function f st sym != None
    then extract (apply_transition_function f st sym)::[] @ next tl st sym
    else next tl st sym
  in
  let rec check (f: tf) =
    match f with
    | [] -> true
    | (q, s, _)::tl ->
    List.length (next f q s) <= 1 && check tl
  in
  check finAuto.tf

let valid (finAuto: fa) : bool =
  let rec subset l = 
    match l with
    | [] -> true
    | h::tl ->
    List.mem h finAuto.states && subset tl
  in
  subset (finAuto.start::finAuto.final) && deterministic finAuto

let reachable (finAuto: fa) : state list =
  let rec directReach (st: state) (l: tf) =
    match l with 
    | [] -> []
    | (q, _, f)::tl ->
    if q = st && q <> f
    then f::[] @ directReach st tl
    else directReach st tl
  in
  let rec search (visit: state list) (curr: state list) =
    match curr with
    | [] -> visit
    | h::tl ->
    search (h::visit) ((list_diff (directReach h finAuto.tf) visit) @ tl)
  in
  if (List.length finAuto.tf = 1) && (List.length finAuto.final = 1) && (finAuto.start = List.hd finAuto.final)
  then [finAuto.start]
  else
  rem_extra (search [] [finAuto.start])

let remove_dead_states (finAuto: fa) : fa =
  let all_reach = reachable finAuto in
  let rec clean_tf (t: tf) : (state * symbol * state) list =
    match t with
    | [] -> []
    | (q, s, f)::tl ->
    if List.mem q all_reach && List.mem f all_reach
    then (q, s, f)::[] @ clean_tf tl
    else clean_tf tl
  in
  
  let fa_cleaned = {
    states = intersection (all_reach) (finAuto.states);
    start = finAuto.start;
    tf = clean_tf finAuto.tf;
    final = intersection (all_reach) (finAuto.final)
  }
  in
  fa_cleaned
