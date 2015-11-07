(* qatar Id: _________________________________ *)

(* Left and right delimiters *)
datatype par = L of string | R of string
type pList = par list

(* Parse tree for parentheses *)
datatype pTree = empty                     (* no parentheses *)
               | nested of string * pTree  (* nested parentheses *)
               | sbs of pTree * pTree      (* side by side *)

(* Stack of either open parentheses or parse trees *)
datatype stackItem = OPEN of string
                   | T of pTree
type stack = stackItem list


(********************* Begin utility functions *********************)

(* pList_fromString s ==> ps
 * ENSURES: convert the string s of delimiters of the form "(s" or
            ")s" where s is any parenthesis-free string into the
            corresponding entity ps of type pList.
 *)
fun pList_fromString (s: string): pList =
  let
    datatype del = Ld (* left delimiter marker *)
                 | Rd (* right delimiter marker *)
    fun parsed (Ld, acc) = L (String.implode (rev acc))
      | parsed (Rd, acc) = R (String.implode (rev acc))
    fun scan ([]: char list, w: del * char list): pList = [parsed w]
      | scan (#"("::cs, w) = (parsed w) :: scan (cs, (Ld,[]))
      | scan (#")"::cs, w) = (parsed w) :: scan (cs, (Rd,[]))
      | scan (c::cs, (d,acc)) = scan (cs, (d, c::acc))
    fun pfs ([]: char list): pList = []
      | pfs (#"("::cs) = scan (cs, (Ld, []))
      | pfs (#")"::cs) = scan (cs, (Rd, []))
      | pfs cs = raise Fail ("Malformed delimiter string " ^ String.implode cs)
  in
    pfs (String.explode s)
  end

(* pList_toString ps ==> s
 * ENSURES: converts ps into the string s
 *)
fun pList_toString ([]: pList): string = ""
  | pList_toString ((L s)::ps) = "(" ^ s ^ pList_toString ps
  | pList_toString ((R s)::ps) = ")" ^ s ^ pList_toString ps

(* pTree_toString t ==> s
 * ENSURES: converts parse tree t into the string s
 *)
fun pTree_toString (empty: pTree): string = ""
  | pTree_toString (nested (s,t)) = "(" ^ s ^ pTree_toString t ^ ")" ^ s
  | pTree_toString (sbs (t1,t2)) = pTree_toString t1 ^
                                   pTree_toString t2

(* stack_toString S ==> s
 * ENSURES: converts stack S into the string s
 *)
fun stack_toString ([]: stack): string = "#"
  | stack_toString ((OPEN s)::S) = "(" ^ s^ " | " ^ stack_toString S
  | stack_toString (T t::S)  = pTree_toString t ^ " | " ^
                               stack_toString S

(********************** End utility functions **********************)

(* whether l is a valid parentheses list *)
fun valid(l: pList): bool =
let
  fun f(L(str): par, l: pList): pList = L(str)::l
    | f(R(str), []) = [R(str)]
    | f(R(str), L(str2)::l) = if str=str2 then l else R(str)::l
    | f(R(str), R(str2)::l) = R(str)::R(str2)::l

in
  (foldl f [] l) = []
end

val true = valid(pList_fromString("(h1)h1"))
val false = valid(pList_fromString("(h1"))
val true = valid(pList_fromString("(h1(h2)h2)h1"))
val false = valid(pList_fromString("(h1)h2"))

(* get corresponding pList of a pTree *)
fun flattenPTree(empty: pTree): pList = []
  | flattenPTree(nested(str, t)) = (L(str)::flattenPTree(t))@[R(str)]
  | flattenPTree(sbs(t1, t2)) = (flattenPTree t1)@(flattenPTree t2)

val "(h1(h2)h2)h1(h3)h3" = pList_toString(flattenPTree(sbs(nested("h1", nested("h2", empty)),nested("h3", empty))))

(* get corresponding pTree of a pList *)
fun pp2(i: pTree, []: stack): stack = [T(i)]
  | pp2(i, T(t)::s) = pp2(sbs(t, i), s)
  | pp2(i, OPEN(str)::s) = T(i)::OPEN(str)::s

fun pp([]: pList, []: stack): pTree = empty
  | pp([], [T(t)]) = t
  | pp(L(str)::l, s) = pp(l, OPEN(str)::s)
  | pp(R(str)::l, s) = 
    case hd s of
         OPEN(str2)=>pp(l, pp2(nested(str, empty), tl s))
       | T(t)=>pp(l, T(nested(str, t))::(tl(tl s)))

val "(h1(h2)h2)h1(h3)h3" = pTree_toString(pp([L("h1"),L("h2"),R("h2"),R("h1"),L("h3"),R("h3")], []))

(* get pTree of a string *)
fun parsePar(ps: string): pTree = pp(pList_fromString(ps), [])
val "(h1(h2)h2)h1(h3)h3" = pTree_toString(parsePar "(h1(h2)h2)h1(h3)h3")
