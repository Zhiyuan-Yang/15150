(* qatar Id: _________________________________ *)

(* Left and right parentheses *)
datatype par = LPAR | RPAR
type pList = par list

(* Parse tree for parentheses *)
datatype pTree = empty                (* no parentheses *)
               | nested of pTree      (* nested parentheses *)
               | sbs of pTree * pTree (* side by side *)

(* Stack of either open parentheses or parse trees *)
datatype stackItem = OPEN
                   | T of pTree
type stack = stackItem list


(********************* Begin utility functions *********************)

(* pList_fromString s ==> ps
 * ENSURES: convert the string of parentheses s into the corresponding
            entity ps of type pList; raises an exception if s
            contains characters other than "(" and ")".
 *)
fun pList_fromString (s: string): pList =
  let
    fun pts ([]: char list): pList = []
      | pts (#"("::l) = LPAR :: pts l
      | pts (#")"::l) = RPAR :: pts l
      | pts ps = raise Fail ("Bad parentheses string starting at " ^
                              String.implode ps)
  in
    pts (String.explode s)
  end

(* pList_toString ps ==> s
 * ENSURES: converts ps into the string s
 *)
fun pList_toString ([]: pList): string = ""
  | pList_toString (LPAR::ps) = "(" ^ pList_toString ps
  | pList_toString (RPAR::ps) = ")" ^ pList_toString ps

(* pTree_toString t ==> s
 * ENSURES: converts parse tree t into the string s
 *)
fun pTree_toString (empty: pTree): string = ""
  | pTree_toString (nested t) = "(" ^ pTree_toString t ^ ")"
  | pTree_toString (sbs (t1,t2)) = pTree_toString t1 ^
                                   pTree_toString t2

(* stack_toString S ==> s
 * ENSURES: converts stack S into the string s
 *)
fun stack_toString ([]: stack): string = "#"
  | stack_toString (OPEN::S) = "( | " ^ stack_toString S
  | stack_toString (T t::S)  = pTree_toString t ^ " | " ^
                               stack_toString S

(********************** End utility functions **********************)

(* whether l is a valid parentheses list *)
fun valid(l: pList): bool =
let 
  fun f(RPAR: par, n: int): int= n-1
    | f(LPAR, n) = if n<0 then n-1 else n+1
in
  (foldl f 0 l) = 0
end

val true = valid(pList_fromString("()"))
val true = valid(pList_fromString("(())()(())"))
val true = valid(pList_fromString("()()(())"))
val false = valid(pList_fromString(")(())"))
val false = valid(pList_fromString("(()()"))
val false = valid(pList_fromString("(()))"))

(* get corresponding pList of a pTree *)
fun flattenPTree(empty: pTree): pList = []
  | flattenPTree(nested(t)) = (LPAR::flattenPTree(t))@[RPAR]
  | flattenPTree(sbs(t1,t2)) = (flattenPTree t1)@(flattenPTree t2)

val "(()())" = pList_toString(flattenPTree(nested(sbs(nested(empty), nested(empty)))))

fun pp2(i: pTree, []: stack): stack = [T(i)]
  | pp2(i, T(t)::s) = pp2(sbs(t, i), s)
  | pp2(i, OPEN::s) = T(i)::OPEN::s

(* get corresponding pTree of a pList *)
fun pp([]: pList, []: stack): pTree = empty
  | pp([], T(t)::ss) = t
  | pp(LPAR::l, s) = pp(l, OPEN::s)
  | pp(RPAR::l, s) = 
    case hd s of
         OPEN=>pp(l, pp2(nested(empty), tl s))
       | T(t)=>pp(l, T(nested(t))::(tl(tl s)))

val nested(sbs(nested(empty), nested(empty))) = pp(pList_fromString("(()())"), [])

(* get pTree of a string *)
fun parsePar(ps: string): pTree = pp(pList_fromString(ps), [])

val nested(sbs(nested(empty), nested(empty))) = parsePar("(()())")
