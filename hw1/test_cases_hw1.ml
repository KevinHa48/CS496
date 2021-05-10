(* List of all test cases used for HW_1 *)

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

let cycles = {states = ["q0";"q1";"q2";"q3"];
         start = "q0";
         tf = [("q0",'a',"q1"); ("q1",'b',"q1"); ("q1",'c',"q2");
         ("q2",'d',"q3"); ("q3", 'e', "q1")];
         final = ["q1"]}


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

let singleDead = {states = ["q0"];
         start = "q0";
         tf = [("q0",'a',"q0")];
         final = []}

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
         final = ["q2"]}

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
         final = ["q2"]}
         
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

let dead_nfa = {states = ["q0";"q1";"q2";"q3";"q4"; "q5"; "q6"];
         start = "q0";
         tf = [("q0",'a',"q1"); ("q0", 'a', "q3"); ("q0", 'a', "q4"); ("q1",'b',"q1"); ("q1",'c',"q2")
         ; ("q1",'b',"q3"); ("q1", 'e', "q4") ; ("q5", 'p', "q6"); ("q6", 'g',"q1")];
         final = ["q2";"q3"; "q4"; "q5"]}

let dead_nfa2 = {states = ["q0";"q1";"q2";"q3";"q4"; "q5";"q6"];
         start = "q0";
         tf = [("q0",'a',"q1"); ("q1",'b',"q1"); ("q1",'c',"q2")
         ; ("q2",'b',"q3"); ("q1", 'e', "q4") ; ("q5", 'p', "q6"); ("q5", 'k',"q6")];
         final = ["q2";"q3"; "q4"; "q5"; "q7"]}

let nfa4 = {states = ["q0";"q1";"q2";"q3";"q4"];
         start = "q0";
         tf = [("q0",'a',"q1"); ("q1",'b',"q1"); ("q1",'c',"q2"); ("q2",'d',"q4"); ("q2", 'd', "q3")];
         final = ["q6"]}
