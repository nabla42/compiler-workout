(* Opening a library for generic programming (https://github.com/dboulytchev/GT).
   The library provides "@type ..." syntax extension and plugins like show, etc.
*)
open GT 
open List

(* Opening a library for combinator-based syntax analysis *)
open Ostap.Combinators
       
(* Simple expressions: syntax and semantics *)
module Expr =
  struct
    
    (* The type for expressions. Note, in regular OCaml there is no "@type..." 
       notation, it came from GT. 
    *)
    @type t =
    (* integer constant *) | Const of int
    (* variable         *) | Var   of string
    (* binary operator  *) | Binop of string * t * t with show

    (* Available binary operators:
        !!                   --- disjunction
        &&                   --- conjunction
        ==, !=, <=, <, >=, > --- comparisons
        +, -                 --- addition, subtraction
        *, /, %              --- multiplication, division, reminder
    *)
                                                            
    (* State: a partial map from variables to integer values. *)
    type state = string -> int 

    (* Empty state: maps every variable into nothing. *)
    let empty = fun x -> failwith (Printf.sprintf "Undefined variable %s" x)

    (* Update: non-destructively "modifies" the state s by binding the variable x 
      to value v and returns the new state.
    *)
    let update x v s = fun y -> if x = y then v else s y

    (* Expression evaluator

          val eval : state -> t -> int
 
       Takes a state and an expression, and returns the value of the expression in 
       the given state.
   
    let eval _ = failwith "Not implemented yet" *)
    
    let i2b i = i != 0
    let b2i b = if b then 1 else 0

    let operator op left right = match op with
        | "+" -> left + right
        | "-" -> left - right
        | "*" -> left * right
        | "/" -> left / right
        | "%" -> left mod right
        | "<" -> b2i(left < right)
        | ">" -> b2i(left > right)
        | "<=" -> b2i(left <= right)
        | ">=" -> b2i(left >= right)
        | "==" -> b2i(left == right)
        | "!=" -> b2i(left != right)
        | "&&" -> b2i((i2b left) && (i2b right))
        | "!!" -> b2i((i2b left) || (i2b right))
        | _    -> failwith "Fail with operation"
        

	let rec eval state expr = match expr with
		| Const const -> const
		| Var var -> state var
		| Binop (op,left, right) -> (operator op) (eval state left) (eval state right)

    (* Expression parser. You can use the following terminals:

         IDENT   --- a non-empty identifier a-zA-Z[a-zA-Z0-9_]* as a string
         DECIMAL --- a decimal constant [0-9]+ as a string
                                                                                                                  
    *)

    let parse_bin op = ostap(- $(op)), (fun x y -> Binop (op, x, y))
    
    ostap (
      expr:
		!(Ostap.Util.expr
           (fun x -> x)
           (Array.map (fun (a, ops) -> a, List.map parse_bin ops)
           [|
             `Lefta, ["!!"];
             `Lefta, ["&&"];
             `Nona, ["<="; "<"; ">="; ">"; "=="; "!="];
             `Lefta, ["+";"-"];
             `Lefta, ["*"; "/"; "%"];             
           |])
           primary
         );
      
      primary: const:DECIMAL {Const const} | var:IDENT {Var var} | - "(" expr -")"
      )
    
  end
                    
(* Simple statements: syntax and sematics *)
module Stmt =
  struct

    (* The type for statements *)
    @type t =
    (* read into the variable           *) | Read   of string
    (* write the value of an expression *) | Write  of Expr.t
    (* assignment                       *) | Assign of string * Expr.t
    (* composition                      *) | Seq    of t * t with show

    (* The type of configuration: a state, an input stream, an output stream *)
    type config = Expr.state * int list * int list 

    (* Statement evaluator

         val eval : config -> t -> config

       Takes a configuration and a statement, and returns another configuration
    
    let eval _ = failwith "Not implemented yet" *)
    
    let rec eval (state, input, output) st = match st with
		| Assign (var, expr) -> (Expr.update var (Expr.eval state expr) state, input, output)
		| Write expr -> (state, input, output @ [Expr.eval state expr])
		| Read var -> (Expr.update var (List.hd input) state, List.tl input, output)
		| Seq (s1, s2) -> eval(eval(state, input, output)s1)s2;;


    (* Statement parser *)
    ostap (
      stmt:
		  x:IDENT   ":=" e:!(Expr.expr) {Assign (x, e)}
		| "read"    "(" x:IDENT ")" {Read x}
		| "write"   "(" e:!(Expr.expr) ")" {Write e};
		
			parse: s:stmt ";" rest:parse {Seq (s, rest)} | stmt      
			)
      
  end

(* The top-level definitions *)

(* The top-level syntax category is statement *)
type t = Stmt.t    

(* Top-level evaluator

     eval : t -> int list -> int list

   Takes a program and its input stream, and returns the output stream
*)
let eval p i =
  let _, _, o = Stmt.eval (Expr.empty, i, []) p in o

(* Top-level parser *)
let parse = Stmt.parse                                                     
