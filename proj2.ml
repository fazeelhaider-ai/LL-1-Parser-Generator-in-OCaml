open Proj2_types;;

let getStartSymbol (g : grammar) : string =
  (* YOUR CODE GOES HERE *)
  match g with 
  | h,t -> h;;

let helpernt ls = 
  let rec aux acc = function 
    | [] -> acc
    | (h,s)::t -> aux (h :: acc) t in aux [] ls;;

let getNonterminals (g : grammar) : string list =
  (* YOUR CODE GOES HERE *)
  match g with 
  |h,t -> (helpernt t);;


let getInitFirstSets (g : grammar) : symbolMap =
  (* YOUR CODE GOES HERE *)
  let v = getNonterminals g in 
  List.fold_left (fun m t -> (SMap.add t (SymbolSet.empty) m)) SMap.empty v
;;

let getInitFollowSetHelper x = 
  let v = (getNonterminals x) in 
  List.fold_left (fun m t -> SMap.add t SymbolSet.empty m) SMap.empty v;;

let getInitFollowSets (g : grammar) : symbolMap =
  (* YOUR CODE GOES HERE *)
  SMap.add (getStartSymbol g) (SymbolSet.singleton "eof") (getInitFollowSetHelper g)
;;

let takeOuteps k =
  SymbolSet.remove "eps" k
;;

let rec computeFirstSet (first : symbolMap) (symbolSeq : string list) : SymbolSet.t =
  (* YOUR CODE GOES HERE *)
  match symbolSeq with 
    [] -> SymbolSet.singleton "eps"
  |s::t -> if SMap.mem s first then 
        let sfirst = (SMap.find s first) in 
        (if SymbolSet.mem "eps" sfirst 
         then 
           SymbolSet.union (takeOuteps sfirst) (computeFirstSet first t)
         else sfirst) 
      else SymbolSet.singleton s
;;

let rec recurseFirstSets (g : grammar) (first : symbolMap) firstFunc : symbolMap =
  (* YOUR CODE GOES HERE *)
    let (ss, rules) = g in 
    match rules with 
      [] -> first
    | h :: t -> let (lhs, rhs) = h in 
        let firstrhs = firstFunc first rhs in 
        let addinglhs = SMap.add lhs (SymbolSet.union firstrhs (SMap.find lhs first)) first in
        recurseFirstSets (ss, t) addinglhs firstFunc ;;

let equalcheckset x y =
  SymbolSet.equal x y;;

let equalcheckmap (x: symbolMap) (y: symbolMap) : bool = 
SMap.equal equalcheckset x y;;

let rec getFirstSets (g : grammar) (first : symbolMap) firstFunc : symbolMap =
  (* YOUR CODE GOES HERE *)
    let runitback = recurseFirstSets g first firstFunc in 
    if equalcheckmap runitback first then runitback 
    else (getFirstSets g runitback firstFunc) ;;


let rec updateFollowSet (first : symbolMap) (follow : symbolMap) (nt : string) (symbolSeq : string list) : symbolMap =
  (* YOUR CODE GOES HERE *)
  match symbolSeq with 
    [] -> follow
  | h::t -> if (SMap.mem h first) then
        let getFirsts = computeFirstSet first t in
       
        let followset = if (SymbolSet.mem "eps" getFirsts) then 
             let buu = (SymbolSet.union (SMap.find h follow) (SymbolSet.remove "eps" getFirsts)) in
             let majinbuu = (SymbolSet.union buu (SMap.find nt follow))  in
             SMap.add h majinbuu follow
          else SMap.add h (SymbolSet.union (SMap.find h follow) getFirsts) follow in
        updateFollowSet first followset nt t
      else
        updateFollowSet first follow nt t;;

let rec recurseFollowSets (g : grammar) (first : symbolMap) (follow : symbolMap) followFunc : symbolMap =
  (* YOUR CODE GOES HERE *)
    match g with 
    |(_ , []) -> follow 
    |(ss, (lhs, rhs) :: t) -> 
        let new_follow = followFunc first follow lhs rhs in 
        recurseFollowSets(ss, t) first new_follow followFunc;;

let rec getFollowSets (g : grammar) (first : symbolMap) (follow : symbolMap) followFunc : symbolMap =
  (* YOUR CODE GOES HERE *)
  let runitback = recurseFollowSets g first follow followFunc in
  if equalcheckmap runitback follow then runitback 
  else (getFollowSets g first runitback followFunc) ;;

let rec getPredictSets (g : grammar) (first : symbolMap) (follow : symbolMap) firstFunc : ((string * string list) * SymbolSet.t) list =
  (* YOUR CODE GOES HERE *)
  match g with 
  (_, []) -> []
|(ss, (lhs,rhs) :: t) -> 
    let rhs_first = firstFunc first rhs in 
    let insert = (
      if SymbolSet.mem "eps" rhs_first then SymbolSet.union (SymbolSet.remove "eps" rhs_first)
          (SMap.find lhs follow)
      else rhs_first 
    ) in ((lhs, rhs), insert) :: getPredictSets (ss,t) first follow firstFunc;;

let tryDerive (g : grammar) (inputStr : string list) : bool =
  (* YOUR CODE GOES HERE *)
 
false;;



let tryDeriveTree (g : grammar) (inputStr : string list) : parseTree =
  (* YOUR CODE GOES HERE *)
Terminal "empty";;

let genParser g = tryDerive g;;
let genTreeParser g = tryDeriveTree g;;
