open GT
open Language
open List  

(* The type for the stack machine instructions *)
@type insn =
(* binary operator                 *) | BINOP of string
(* put a constant on the stack     *) | CONST of int                 
(* read to stack                   *) | READ
(* write from stack                *) | WRITE
(* load a variable to the stack    *) | LD    of string
(* store a variable from the stack *) | ST    of string with show

(* The type for the stack machine program *)                                                               
type prg = insn list

(* The type for the stack machine configuration: a stack and a configuration from statement
   interpreter
 *)
type config = int list * Stmt.config

(* Stack machine interpreter

     val eval : config -> prg -> config

   Takes a configuration and a program, and returns a configuration as a result
                         
let eval _ = failwith "Not yet implemented" *)

	let rec eval_bl config prg = 
		let (stack, prime) = config in
		let (state, input, output) = prime in
		match prg with
	
		| BINOP op -> (Language.Expr.operator op (List.hd (List.tl stack)) (List.hd stack) :: (List.tl (List.tl stack)), prime)
		| CONST const -> (const :: stack, prime)
		| READ -> (List.hd input :: stack, (state, List.tl input, output))
		| WRITE -> (List.tl stack, (state, input, output @ [List.hd stack]))
		| LD var -> (state var :: stack, prime)
		| ST var -> (List.tl stack, (Language.Expr.update var (List.hd stack) state, input, output))
		
		let eval config prg = List.fold_left eval_bl config prg

(* Top-level evaluation

     val run : prg -> int list -> int list

   Takes a program, an input stream, and returns an output stream this program calculates
*)
let run p i = let (_, (_, _, o)) = eval ([], (Expr.empty, i, [])) p in o

(* Stack machine compiler

     val compile : Language.Stmt.t -> prg

   Takes a program in the source language and returns an equivalent program for the
   stack machine
*)

let rec compile =
  let rec expr = function
  | Expr.Var   x          -> [LD x]
  | Expr.Const n          -> [CONST n]
  | Expr.Binop (op, x, y) -> expr x @ expr y @ [BINOP op]
  in
  function
  | Stmt.Seq (s1, s2)  -> compile s1 @ compile s2
  | Stmt.Read x        -> [READ; ST x]
  | Stmt.Write e       -> expr e @ [WRITE]
  | Stmt.Assign (x, e) -> expr e @ [ST x]
