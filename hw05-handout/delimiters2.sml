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

fun valid        _ = raise Fail "Unimplemented"
fun flattenPTree _ = raise Fail "Unimplemented"
fun pp           _ = raise Fail "Unimplemented"
fun parsePar     _ = raise Fail "Unimplemented"
