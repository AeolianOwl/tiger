type id = string

type env = (id * int) list

datatype binop = Plus | Minus | Times | Div

datatype stm = CompoundStm of stm * stm
             | AssignStm of id * exp
             | PrintStm of exp list

     and exp = IdExp of id
             | NumExp of int
             | OpExp of exp * binop * exp
             | EseqExp of stm * exp

val prog =
 CompoundStm (AssignStm("a", OpExp(NumExp 5, Plus, NumExp 3)),
  CompoundStm (AssignStm("b",
      EseqExp (PrintStm [IdExp "a", OpExp(IdExp "a", Minus, NumExp 1)],
           OpExp (NumExp 10, Times, IdExp "a"))),
   PrintStm [IdExp "b"]))

fun lookup (env : env, id : id) : int =
  (fn (k, v) => v) (valOf (List.find (fn (k, v) => k = id) env))

fun maxargs (CompoundStm (a, b)) = Int.max (maxargs a, maxargs b)
  | maxargs (AssignStm (a, b)) = maxargsExp b
  | maxargs (PrintStm []) = 0
  | maxargs (PrintStm (x::xs)) = Int.max (List.length (x::xs), Int.max (maxargsExp x, maxargs (PrintStm xs)))
and maxargsExp (IdExp a) = 0
  | maxargsExp (NumExp a) = 0
  | maxargsExp (OpExp (a, b, c)) = Int.max (maxargsExp a, maxargsExp c)
  | maxargsExp (EseqExp (a, b)) = Int.max (maxargs a, maxargsExp b)

fun interpStm (CompoundStm (a, b), env) =
    let val env' = interpStm (a, env)
    in interpStm (b, env')
    end
  | interpStm (AssignStm (a, b), env) =
    let val (value, env') = interpExp (b, env)
    in (a, value) :: env'
    end
  | interpStm (PrintStm [], env) = env
  | interpStm (PrintStm (x::xs), env) =
      let val (left, env') = interpExp (x, env)
          val _ = print ((Int.toString left) ^ " ")
      in interpStm (PrintStm xs, env')
      end
and interpExp (IdExp a, env) = (lookup (env, a), env)
  | interpExp (NumExp a, env) = (a, env)
  | interpExp (OpExp (a, b, c), env) =
    let val (left, env') = interpExp (a, env)
        val (right, env'') = interpExp (c, env')
    in case b of
          Plus  => (left + right, env'')
        | Minus => (left - right, env'')
        | Times => (left * right, env'')
        | Div   => (left div right, env'')
    end
  | interpExp (EseqExp (a, b), env) =
    let val env' = interpStm (a, env)
    in interpExp (b, env')
    end

fun interp (x : stm) : unit =
    let val _ = interpStm (x, [])
    in ()
    end
