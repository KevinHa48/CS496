
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
         
let tf_of_a = [("q0",'a',"q1"); ("q1",'b',"q1"); ("q1",'c',"q2")]

let nfa = {states = ["q0";"q1";"q2";"q3";"q4"];
         start = "q0";
         tf = [("q0",'a',"q1"); ("q0", 'a', "q3"); ("q1",'b',"q1"); ("q1",'c',"q2"); ("q2",'d',"q4")];
         final = ["q2";"q3"]}

let nfa2 = {states = ["q0";"q1";"q2";"q3";"q4"];
         start = "q0";
         tf = [("q0",'a',"q1"); ("q0", 'a', "q3"); ("q0", 'a', "q4"); ("q1",'b',"q1"); ("q1",'c',"q2"); ("q1",'b',"q3")];
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

let dead_states_helper l = 
  failwith "implement"


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
  let rec search (s: state list) =
    match s with
    | [] -> []
    | h::tl ->
    directReach h finAuto.tf @ search (directReach h finAuto.tf) @ search tl
  in
  let start = directReach finAuto.start finAuto.tf in
  rem_extra (start @ search start)

let remove_dead_states (finAuto: fa) : fa =
  let fa_cleaned = {
    states = dead_states_helper finAuto.states;
    start = finAuto.start;
    tf = dead_states_helper finAuto.tf;
    final = dead_states_helper finAuto.final;
  }
  in
  fa_cleaned



(*
The strategy for deterministic: 
1. Utilize apply trans_fun
2. With the return of the result append that to a list
3. Get the tail of tf and re-apply to trans_fun
4. Repeat until tf is empty
5. Next utilize maybe at_least_two for the list you created
6. If true -> true, else false.
*)

(*
The strat for valid:
1. Cons together the start and the final states
2. Build a subset helper function that will check if that appended list is a subset
of the automata's state list.
3. And that bool with determinstic.

*)
