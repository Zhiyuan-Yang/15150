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

fun valid        _ = raise Fail "Unimplemented"
fun flattenPTree _ = raise Fail "Unimplemented"
fun pp           _ = raise Fail "Unimplemented"
fun parsePar     _ = raise Fail "Unimplemented"
