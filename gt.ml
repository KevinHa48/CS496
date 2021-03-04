(* 

   Stub for HW2 
   Please
   1. Rename to gt.ml
   2. Place the names of the group members here:

    Name1: Kevin Ha
    Name2: Jacob Roessler

    I pledge my honor that I have abided by the Stevens Honor System.
*)



type 'a gt = Node of 'a*('a gt) list

let mk_leaf (n:'a) : 'a gt =
  Node(n,[])
    
let t : int gt =
 Node (33,
       [Node (12,[]);
        Node (77, 
              [Node (37, 
                     [Node (14, [])]); 
               Node (48, []); 
               Node (103, [])])
       ])

let t_perfect : int gt =
 Node (33,
       [Node (12,[]);
        Node (77,[])
       ])

let t2 : int gt = 
 Node (33, [Node (27, [Node (14, [Node (22, [Node(25, [])])]); Node(18, [])]); Node(96, [Node(7, [])])]) 

let rec height t =
  match t with 
  | Node(_ ,[]) -> 1
  | Node(_, ct) -> 
  1 + (List.fold_right (fun x y -> max x y) 
      (List.map height ct) 0)

let rec size t =
  match t with
  | Node(_ ,[]) -> 1
  | Node(_, ct) -> 
  1 + (List.fold_right (fun x y -> x + y) 
      (List.map size ct) 0)

let rec paths_to_leaves t =
  match t with 
  | Node(d, []) -> [[]]
  | Node(d, ct) -> List.flatten @@ 
  List.mapi (fun i ptl -> 
             List.map (fun l -> i::l) ptl)            
  (List.map paths_to_leaves ct)


let rec is_perfect t =
  let rec perfect_helper l =
  match l with
  | [ ] | [_] -> true
  | n::m::t -> n=m && perfect_helper (m::t)
  in
  perfect_helper (List.map List.length (paths_to_leaves t))
 
let rec preorder (Node(d,ch)) =
  d :: List.flatten (List.map preorder ch)
                        
let rec mirror (Node(d,ch)) =
  Node(d, List.map mirror (List.rev ch))

let rec mapt f (Node(d,ch)) =
  Node(f d, List.map (mapt f) ch)
  
let rec foldt : ('a -> 'b list -> 'b) -> 'a gt -> 'b =
  fun f (Node(d,ch)) ->
  f d (List.map (foldt f) ch)

let sumt t =
  foldt (fun i rs -> i + List.fold_left (fun i j -> i+j) 0 rs) t

let memt t e = 
  foldt (fun i rs -> i=e || List.exists (fun i -> i) rs) t

let mirror' t  = 
  foldt (fun i rs -> Node(i, List.rev rs)) t
