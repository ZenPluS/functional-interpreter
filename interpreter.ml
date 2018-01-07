open Environment 

type exp =
  | EInt of int
  | EBool of bool
  | Ide of ide
  | Sum of exp * exp
  | Diff of exp * exp
  | Times of exp * exp
  | Div of exp * exp
  | Minus of exp
  | Equals of exp * exp
  | IsZero of exp
  | Or of exp * exp
  | And of exp * exp
  | Not of exp
  | IfThenElse of exp * exp * exp
  | Let of ide * exp * exp
  | LetRec of ide * ide * exp * exp
  | Fun of ide * exp
  | FunCall of exp * exp
  | ETuple of tuple 
  | EPipe of tuple
  | ManyTimes of int * exp
  | Head of exp
  | Tail of exp
  | Cons of exp*exp (* Implementation of the cons operator over Tuple ADT *)

and tuple =
  | Nil
  | Seq of exp * tuple

type etype = 
  | Int of int
  | Bool of bool
  | Closure of closure
  | Recursive of recursive
  | Tuple of etype list (* a Tuple is an expressible type *)
  | Pipe of closure list (* a Pipe is an expressible type, and it is conceptually different than a Tuple*)
  | Unbound 
and closure = ide * exp * (etype env) (* non recursive closure *)
and recursive = ide * ide * exp * (etype env) (* recursive closure *)

(* right fold implementation over Tuple ADT *)
let rec fold_right (f : etype -> exp -> etype) (acc : etype) (pipe : tuple) : etype =
  match pipe with
  | Nil -> acc
  | Seq (head, tail) -> f (fold_right f acc tail) head;;

(* not really necessary, but implemented for sake of completeness *)
let rec fold_left (f : etype -> exp -> etype) (acc : etype) (pipe : tuple) : etype  = 
  match pipe with 
  | Nil -> acc
  | Seq (head, tail) -> fold_left f (f acc head) tail;;

(* interpreter *)
let rec sem (exp : exp)  (env : etype env) : etype = 

  (* auxiliary function used to fold over a tuple *)
  let evalTuple (acc : etype) (el : exp) : etype = match (sem el env, acc) with
    | (Closure closure, Pipe list) -> Pipe (closure :: list)
    | (Pipe closures, Pipe list) -> Pipe (list @ closures)
    | (x, Tuple list) -> Tuple (x :: list)
    | _ -> failwith "Not a functional pipe"
  in 

  match exp with
  | ETuple tuple -> fold_right evalTuple (Tuple []) tuple
  | EPipe pipe -> fold_right evalTuple (Pipe []) pipe
  (* ManyTimes gets transformed into a Pipe *)
  | ManyTimes (0, _) -> Pipe []
  | ManyTimes (n, f) -> (match (sem f env, sem (ManyTimes (n-1, f)) env) with
      | (Closure closure, Pipe list) -> Pipe (closure :: list)
      | (Pipe pipe, Pipe list) -> Pipe (pipe @ list)
      | _ -> failwith "ManyTimes operator requires Tuple of functionals")
  (* Takes the head of a Tuple and returns it *)
  | Head e -> (match sem e env with
      | Tuple [] -> failwith "Empty tuple"
      | Tuple (head::tail) -> head
      | _ -> failwith "Head operator expected 'Tuple'")
  (* Takes the tail of a Tuple and returns it *)
  | Tail e -> (match sem e env with
      | Tuple [] -> failwith "Empty tuple"
      | Tuple (head::tail) -> Tuple tail
      | _ -> failwith "Tail operator expected 'Tuple'")
  (* Takes an element and puts it on top of a Tuple *)
  | Cons (e1, e2) -> (match (sem e1 env, sem e2 env) with
      | (head, Tuple tuple) -> Tuple (head :: tuple)
      | _ -> failwith "Cons operator expected 'etype*Tuple'")
  | EInt x -> Int x
  | EBool x -> Bool x
  | Ide x -> applyEnv env x
  | Sum (e1, e2) -> (match (sem e1 env, sem e2 env) with
      | (Int a, Int b) -> Int (a + b)
      | _ -> failwith "Sum operator expected 'Int*Int'")
  | Diff (e1, e2) -> (match (sem e1 env, sem e2 env) with
      | (Int a, Int b) -> Int (a - b)
      | _ -> failwith "Diff operator expected 'Int*Int'")
  | Times  (e1, e2)  -> (match (sem e1 env, sem e2 env) with
      | (Int a, Int b) -> Int (a * b)
      | _ -> failwith "Times operator expected 'Int*Int'")
  | Div (e1, e2) -> (match (sem e1 env, sem e2 env) with
      | (_ , Int 0) -> failwith "Cannot divide by zero"
      | (Int a, Int b) -> Int (a / b)
      | _ -> failwith "Div operator expected 'Int*Int'")
  | Minus e -> (match (sem e env) with
      | Int a -> Int (-a)
      | _ -> failwith "Minus operator expected 'Int'")
  | Equals (e1, e2) -> Bool (sem e1 env = sem e2 env)
  | IsZero e1 -> Bool (sem e1 env = Int 0)
  | And (e1, e2) -> (match (sem e1 env, sem e2 env) with
      | (Bool a, Bool b) -> Bool (a && b)
      | _ -> failwith "And operator expected 'Bool*Bool'")
  | Or (e1, e2) -> (match (sem e1 env, sem e2 env) with
      | (Bool a, Bool b) -> Bool (a || b)
      | _ -> failwith "Or operator expected 'Bool*Bool'")
  | Not e -> (match (sem e env) with
      | Bool a -> Bool (not a)
      | _ -> failwith "Not operator expected 'Bool*Bool'")
  | IfThenElse (cond, thenb, elseb) -> (match (sem cond env) with
      | Bool true -> sem thenb env
      | Bool false -> sem elseb env
      | _ -> failwith "If statement expected 'Bool'")
  | Let (id, e1, e2) -> let env' = bind id (sem e1 env) env in
    sem e2 env'
  | LetRec (f, id, e1, e2) -> let env' = bind f (Recursive(f, id, e1, env)) env in 
    sem e2 env'
  | Fun (x, body) -> Closure (x, body, env)
  | FunCall (f, param) -> (match (sem f env, sem param env) with
      | (Closure(x, body, env'), v) -> sem body (bind x v env')
      | (Recursive(f, x, body, env') as closure, v) -> 
        let env'' = bind f closure env' in
        sem body (bind x v env'')
      | (Pipe closures, v) -> (* every closure in the Pipe gets evaluated 
                                 and passed as argument to the next function in the Pipe *)
        List.fold_left (fun acc (x, body, env') -> sem body (bind x acc env')) v closures  
      | _ -> failwith "Calling non functional value")
  | _ -> failwith "type error"

