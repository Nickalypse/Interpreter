(** ESPRESSIONI **********************************************************)

type ide = string;;
type set_t = SET_INT | SET_BOOL | SET_STRING;;

type exp =
	| CstInt		of	int
	| CstBool		of	bool
	| CstString		of	string
	| Den			of	ide
	
	| Minus			of	exp
	| IsZero		of	exp
	| Sum			of	exp * exp
	| Diff			of	exp * exp
	| Prod			of	exp * exp
	| Div			of	exp * exp
	| Mod			of	exp * exp
	
	| Not			of	exp
	| And			of	exp * exp
	| Or			of	exp * exp
	
	| Concat		of	exp * exp
	
	| Major			of	exp * exp
	| Eq			of	exp * exp
	
	| Empty			of	set_t
	| Singleton		of	exp * set_t
	| IsEmpty		of	exp
	| Contains		of	exp * exp
	| Push			of	exp * exp
	| Pop			of	exp * exp
	| Min			of	exp
	| Max			of	exp
	| IsSubset		of	exp * exp
	| Union			of	exp * exp
	| Inter			of	exp * exp
	| Subtract		of	exp * exp
	| ForAll		of	exp * exp
	| Exist			of	exp * exp
	| Filter		of	exp * exp
	| Map			of	exp * exp

	| IfThenElse	of	exp * exp * exp
	| Let			of	ide * exp * exp
	| Fun			of	ide * exp
	| FunCall		of	exp * exp
	| LetRec		of	ide * ide * exp * exp;;


(** AMBIENTE POLIMORFO [FUNZIONE] ******************************************)

type 't env = (ide -> 't);;

(* crea ambiente vuoto contenente valore di default *)
let emptyenv (value:'t) = (function(s:ide) -> value);;

(* applica la funzione-ambiente all'etichetta *)
let applyenv (f:('t env)) (s:ide) = f s;;

(* aggiunge la coppia (s',value) all'ambiente *)
let bind (f:('t env)) (s':ide) (value:'t) = function (s:ide) ->
	if s=s'
	then value
	else (applyenv f s);;


(** TIPI ESPRIMIBILI *******************************************************)

type evT =
	| Unbound
	| Int			of	int
	| Bool			of	bool
	| String		of	string
	| Set			of	(evT list) * set_t
	(* <par_form, corpo, env_dichiarazione> *)
	| Closure		of	ide * exp * (evT env)
	(* <nome_fun, par_form, corpo, env_dichiarazione> *)
	| RecClosure	of	ide * ide * exp * (evT env);;


(** TYPE CHECKING **********************************************************)

let typecheck (s:string) (t:evT) : bool =
	match s with
	| "int"		->	(match t with
					| Int(_)	->	true
					| _			->	false)
	| "bool"	->	(match t with
					| Bool(_)	->	true
					| _			->	false)
	| "string"	->	(match t with
					| String(_)	->	true
					| _			->	false)
	| "set"		->	(match t with
					| Set(_,_)	->	true
					| _			->	false)
	| "fun"		->	(match t with
					| Closure(_,_,_)	->	true
					| _					->	false)
	| _			->	failwith "Not Valid Type";;

(* verifica se l'elemento x è di tipo t *)
let set_typecheck (t:set_t) (x:evT) : bool =
	match (t,x) with
	| (SET_INT,Int(_))			->	true
	| (SET_BOOL,Bool(_))		->	true
	| (SET_STRING,String(_))	->	true
	| (_,_)						->	false;;


(** FUNZIONI PRIMITIVE *****************************************************)

let _int_minus x =
	match ((typecheck "int" x),x) with
	| (true,Int(a))		->	Int(-a)
	| (_,_)				->	failwith "Minus : Run-Time Error";;

let _int_iszero x =
	match ((typecheck "int" x),x) with
	| (true,Int(a))		->	Bool(a=0)
	| (_,_)				->	failwith "IsZero : Run-Time Error";;

let _int_sum x y =
	match ((typecheck "int" x),(typecheck "int" y),x,y) with
	| (true,true,Int(a),Int(b))	->	Int(a+b)
	| (_,_,_,_)					->	failwith "Sum : Run-Time Error";;
	
let _int_diff x y =
	match ((typecheck "int" x),(typecheck "int" y),x,y) with
	| (true,true,Int(a),Int(b))	->	Int(a-b)
	| (_,_,_,_)					->	failwith "Diff : Run-Time Error";;
	
let _int_prod x y =
	match ((typecheck "int" x),(typecheck "int" y),x,y) with
	| (true,true,Int(a),Int(b))	->	Int(a*b)
	| (_,_,_,_)					->	failwith "Prod : Run-Time Error";;

let _int_div x y =
	match ((typecheck "int" x),(typecheck "int" y),x,y) with
	| (true,true,Int(a),Int(b))	->	Int(a/b)
	| (_,_,_,_)					->	failwith "Div : Run-Time Error";;

let _int_mod x y =
	match ((typecheck "int" x),(typecheck "int" y),x,y) with
	| (true,true,Int(a),Int(b))	->	Int(a mod b)
	| (_,_,_,_)					->	failwith "Mod : Run-Time Error";;

let _bool_not x =
	match ((typecheck "bool" x),x) with
	| (true,Bool(a))	->	Bool(not a)
	| (_,_)				->	failwith "Not : Run-Time Error";;

let _bool_and x y =
	match ((typecheck "bool" x),(typecheck "bool" y),x,y) with
	| (true,true,Bool(a),Bool(b))	->	Bool(a && b)
	| (_,_,_,_)						->	failwith "And : Run-Time Error";;

let _bool_or x y =
	match ((typecheck "bool" x),(typecheck "bool" y),x,y) with
	| (true,true,Bool(a),Bool(b))	->	Bool(a || b)
	| (_,_,_,_)						->	failwith "Or : Run-Time Error";;

let _str_concat x y =
	match ((typecheck "string" x),(typecheck "string" y),x,y) with
	| (true,true,String(a),String(b))	->	String(a ^ b)
	| (_,_,_,_)							->	failwith "Concat : Run-Time Error";;

let _major x y =
	(* se sono entrambi interi *)
	match ((typecheck "int" x),(typecheck "int" y),x,y) with
	| (true,true,Int(a),Int(b)) -> Bool(a>b)
	| (_,_,_,_)	->
	(* se sono entrambi booleani *)
	match ((typecheck "bool" x),(typecheck "bool" y),x,y) with
	| (true,true,Bool(a),Bool(b)) -> Bool(a>b)
	| (_,_,_,_) ->
	(* se sono entrambe stringhe *)
	match ((typecheck "string" x),(typecheck "string" y),x,y) with
	| (true,true,String(a),String(b)) -> Bool(a>b)
	(* altrimenti *)
	| (_,_,_,_) -> failwith " Major : Run-Time Error";;

let _eq x y =
	(* se sono entrambi interi *)
	match ((typecheck "int" x),(typecheck "int" y),x,y) with
	| (true,true,Int(a),Int(b)) -> Bool(a=b)
	| (_,_,_,_)	->
	(* se sono entrambi booleani *)
	match ((typecheck "bool" x),(typecheck "bool" y),x,y) with
	| (true,true,Bool(a),Bool(b)) -> Bool(a=b)
	| (_,_,_,_) ->
	(* se sono entrambe stringhe *)
	match ((typecheck "string" x),(typecheck "string" y),x,y) with
	| (true,true,String(a),String(b)) -> Bool(a=b)
	| (_,_,_,_) ->
	(* se sono entrambi insiemi *)
	match ((typecheck "set" x),(typecheck "set" y),x,y) with
	| (true,true,Set(l1,t1),Set(l2,t2)) ->
		if(t1=t2) then
			(* devono avere la stessa cardinalità *)
			if ((List.length l1)=(List.length l2)) then
				(* tutti gli elementi del primo stanno nel secondo *)
				let rec eq l1 l2 =
					match l1 with
					| []	->	Bool(true)
					| h::t	->	if (List.mem h l2)
								then (eq t l2)
								else Bool(false)
				in (eq l1 l2)
			else Bool(false)
		else failwith " Eq : Different Set Types"
	(* altrimenti *)
	| (_,_,_,_) -> failwith " Eq : Run-Time Error";;


(** FUNZIONI PRIMITIVE SET *************************************************)

(* crea set vuoto per elementi di tipo t *)
let _set_empty (t:set_t) : evT = Set(([]: evT list), t);;

(* crea set con elemento x di tipo t *)
let _set_singleton (x:evT) (t:set_t) : evT =
	if (set_typecheck t x)
	then Set([x], t)
	else failwith "Singleton : Not Valid Set";;

(* verifica se insieme s è vuoto *)
let _set_isempty (s:evT) : evT =
	match ((typecheck "set" s),s) with
	| (true, Set(l,_))	->	Bool(l=[])
	| (_,_)				->	failwith "IsEmpty : Run-Time Error";;

(* verifica se insieme s contiene x *)
let _set_contains (s:evT) (x:evT) : evT =
	match ((typecheck "set" s),s) with
	| (true, Set(l,t))	->	if (set_typecheck t x)
							then Bool(List.mem x l)
							else failwith "Contains : Not Respected Set Type";
	| (_,_)				->	failwith "Contains : Run-Time Error";;

(* inserisci x in insieme s (se non presente) *)
let _set_push (s:evT) (x:evT) : evT =
	match ((typecheck "set" s),s) with
	| (true, Set(l,t))	->	(match ((set_typecheck t x),(List.mem x l)) with
							| (true, true)	->	s
							| (true, false)	->	Set(x::l,t)
							| (_,_)			->	failwith "Push : Not Respected Set Type")
	| (_,_)				->	failwith "Push : Run-Time Error";;

(* rimuovi x da insieme s (se presente) *)
let _set_pop (s:evT) (x:evT) : evT =
	match ((typecheck "set" s),s) with
	| (true, Set(l,t))	->	if (set_typecheck t x) then
								let rec pop l x =
									match l with
									| []	->	[]
									| h::t	->	if (h=x) then t else h::(pop t x)
								in Set((pop l x),t)
							else failwith "Pop : Not Respected Set Type"
	| (_,_)				->	failwith "Pop : Run-Time Error";;

(* trova elemento minimo in insieme s *)
let _set_min (s:evT) : evT =
	match ((typecheck "set" s),s) with
	| (true, Set([],_))		->	failwith "Min : Empty Set"
	| (true, Set(h::l,t))	->	let rec min l acc =
									match l with
									| []	->	acc
									| h::t	->	if (h<acc) then (min t h) else (min t acc)
								in (min l h)
	| (_,_)					->	failwith "Min : Run-Time Error";;

(* trova elemento massimo in insieme s *)
let _set_max (s:evT) : evT =
	match ((typecheck "set" s),s) with
	| (true, Set([],_))		->	failwith "Max : Empty Set"
	| (true, Set(h::l,t))	->	let rec max l acc =
									match l with
									| []	->	acc
									| h::t	->	if (h>acc) then (max t h) else (max t acc)
								in (max l h)
	| (_,_)					->	failwith "Max : Run-Time Error";;

(* verifica se s1 è sottoinsieme di s2 *)
let _set_issubset (s1:evT) (s2:evT) : evT =
	match ((typecheck "set" s1),(typecheck "set" s2),s1,s2) with
	| (true,true,Set(l1,t1),Set(l2,t2)) ->
		if (t1=t2) then
			(* tutti gli elementi del primo stanno nel secondo *)
			let rec issubset l1 l2 =
				match l1 with
				| []	->	Bool(true)
				| h::t	->	if (List.mem h l2) then (issubset t l2) else Bool(false)
			in (issubset l1 l2)
		else failwith "IsSubset : Different Set Types"
	| (_,_,_,_) -> failwith "IsSubset : Run-Time Error";;

(* unione di insiemi s1 s2 *)
let _set_union (s1:evT) (s2:evT) : evT =
	match ((typecheck "set" s1),(typecheck "set" s2),s1,s2) with
	| (true,true,Set(l1,t1),Set(l2,t2)) ->
		if (t1=t2) then
			(* espandi l2 con elementi che non contiene, presenti in l1 *)
			let rec union l1 l2 =
				match l1 with
				| []	->	l2
				| h::t	->	if (List.mem h l2)
							then (union t l2)
							else (union t (h::l2))
			in Set((union l1 l2),t1)
		else failwith "Union : Different Set Types"
	| (_,_,_,_) -> failwith "Union : Run-Time Error";;

(* intersezione di insiemi s1 s2 *)
let _set_inter (s1:evT) (s2:evT) : evT =
	match ((typecheck "set" s1),(typecheck "set" s2),s1,s2) with
	| (true,true,Set(l1,t1),Set(l2,t2)) ->
		if (t1=t2) then
			(* colleziona elementi comuni in lista acc vuota *)
			let rec inter l1 l2 acc =
				match l1 with
				| []	->	acc
				| h::t	->	if (List.mem h l2)
							then (inter t l2 (h::acc))
							else (inter t l2 acc)
			in Set((inter l1 l2 []),t1)
		else failwith "Inter : Different Set Types"
	| (_,_,_,_) -> failwith "Inter : Run-Time Error";;

(* sottrazione di insiemi s1 s2 *)
let _set_subtract (s1:evT) (s2:evT) : evT =
	match ((typecheck "set" s1),(typecheck "set" s2),s1,s2) with
	| (true,true,Set(l1,t1),Set(l2,t2)) ->
		if (t1=t2) then
			(* colleziona elementi solo in l1 in lista acc vuota *)
			let rec subtract l1 l2 acc =
				match l1 with
				| []	->	acc
				| h::t	->	if (List.mem h l2)
							then (subtract t l2 acc)
							else (subtract t l2 (h::acc))
			in Set((subtract l1 l2 []),t1)
		else failwith "Subtract : Different Set Types"
	| (_,_,_,_) -> failwith "Subtract : Run-Time Error";;


(** VALUTA ESPRESSIONI *****************************************************)

let rec eval (e:exp) (f:(evT env)) : evT =
	match e with
	| CstInt(x)				->	Int x
	| CstBool(x)			->	Bool x
	| CstString(x)			->	String x
	| Den(s)				->	(applyenv f s)
	
	| Minus(a)				->	_int_minus	(eval a f)
	| IsZero(a)				->	_int_iszero	(eval a f)
	| Sum(a,b)				->	_int_sum	(eval a f) (eval b f)
	| Diff(a,b)				->	_int_diff	(eval a f) (eval b f)
	| Prod(a,b)				->	_int_prod	(eval a f) (eval b f)
	| Div(a,b)				->	_int_div	(eval a f) (eval b f)
	| Mod(a,b)				->	_int_mod	(eval a f) (eval b f)
	
	| Not(a)				->	_bool_not	(eval a f)
	| And(a,b)				->	_bool_and	(eval a f) (eval b f)
	| Or(a,b)				->	_bool_or	(eval a f) (eval b f)
	
	| Concat(a,b)			->	_str_concat	(eval a f) (eval b f)
	
	| Major(a,b)			->	_major		(eval a f) (eval b f)
	| Eq(a,b)				->	_eq			(eval a f) (eval b f)
	
	| Empty(t)				->	_set_empty		(t)
	| Singleton(x,t)		->	_set_singleton	(eval x f) (t)
	| IsEmpty(s)			->	_set_isempty	(eval s f)
	| Contains(s,x)			->	_set_contains	(eval s f) (eval x f)
	| Push(s,x)				->	_set_push		(eval s f) (eval x f)
	| Pop(s,x)				->	_set_pop		(eval s f) (eval x f)
	| Min(s)				->	_set_min		(eval s f)
	| Max(s)				->	_set_max		(eval s f)
	| IsSubset(a,b)			->	_set_issubset	(eval a f) (eval b f)
	| Union(a,b)			->	_set_union		(eval a f) (eval b f)
	| Inter(a,b)			->	_set_inter		(eval a f) (eval b f)
	| Subtract(a,b)			->	_set_subtract	(eval a f) (eval b f)
	
	| ForAll(s,p) -> let s=(eval s f) in let p=(eval p f) in
		(match ((typecheck "set" s),(typecheck "fun" p),s,p) with
		| (true,true,Set(l,_),Closure(par_form,corpo,env)) ->
			let rec forall l p =
				(match l with
				| []	->	Bool(true)
				| h::t	->	let env' = (bind env par_form h) in
							let ris = (eval corpo env') in
							(match ((typecheck "bool" ris),ris) with
							| (true,Bool(true))		->	(forall t p)
							| (true,Bool(false))	->	Bool(false)
							| (_,_) -> failwith "ForAll : Not Bool Returned"))
			in (forall l p)
		| (false,_,_,_)	->	failwith "ForAll : Not Valid Set"
		| (_,false,_,_)	->	failwith "ForAll : Not Valid Closure"
		| (_,_,_,_)		->	failwith "ForAll : Run-Time Error")
	
	| Exist(s,p) -> let s=(eval s f) in let p=(eval p f) in
		(match ((typecheck "set" s),(typecheck "fun" p),s,p) with
		| (true,true,Set(l,_),Closure(par_form,corpo,env)) ->
			let rec exist l p =
				(match l with
				| []	->	Bool(false)
				| h::t	->	let env' = (bind env par_form h) in
							let ris = (eval corpo env') in
							(match ((typecheck "bool" ris),ris) with
							| (true,Bool(true))		->	Bool(true)
							| (true,Bool(false))	->	(exist t p)
							| (_,_) -> failwith "Exist : Not Bool Returned"))
			in (exist l p)
		| (false,_,_,_)	->	failwith "Exist : Not Valid Set"
		| (_,false,_,_)	->	failwith "Exist : Not Valid Closure"
		| (_,_,_,_)		->	failwith "Exist : Run-Time Error")
	
	| Filter(s,p) -> let s=(eval s f) in let p=(eval p f) in
		(match ((typecheck "set" s),(typecheck "fun" p),s,p) with
		| (true,true,Set(l,t),Closure(par_form,corpo,env)) ->
			(* colleziona elementi che rispettano condizione in lista acc vuota *)
			let rec filter l p acc =
				(match l with
				| []	->	acc
				| h::t	->	let env' = (bind env par_form h) in
							let ris = (eval corpo env') in
							(match ((typecheck "bool" ris),ris) with
							| (true,Bool(true))		->	(filter t p (h::acc))
							| (true,Bool(false))	->	(filter t p acc)
							| (_,_) -> failwith "Filter : Not Bool Returned"))
			in Set((filter l p []),t)
		| (false,_,_,_)	->	failwith "Filter : Not Valid Set"
		| (_,false,_,_)	->	failwith "Filter : Not Valid Closure"
		| (_,_,_,_)		->	failwith "Filter : Run-Time Error")
	
	| Map(s,p) -> let s=(eval s f) in let p=(eval p f) in
		(match ((typecheck "set" s),(typecheck "fun" p),s,p) with
		| (true,true,Set(l,typ),Closure(par_form,corpo,env)) ->
			let rec map l p =
				(match l with
				| []	->	[]
				| h::t	->	let env' = (bind env par_form h) in
							let ris = (eval corpo env') in
							(* elemento restituito deve avere stesso tipo dell'insieme *)
							if (set_typecheck typ ris)
							then ris::(map t p)
							else failwith "Map : Not Returned a Set Type Element")
			in Set((map l p),typ)
		| (false,_,_,_)	->	failwith "Map : Not Valid Set"
		| (_,false,_,_)	->	failwith "Map : Not Valid Closure"
		| (_,_,_,_)		->	failwith "Map : Run-Time Error")
	
	| IfThenElse(cond,a,b)	->	let v = (eval cond f) in
								(match ((typecheck "bool" v),v) with
								| (true,Bool(true))   ->  (eval a f)
								| (true,Bool(false))  ->  (eval b f)
								| (_,_) -> failwith "IfThenElse : Not Boolean Guard")
	
	| Let(s',e1,e2)			->	let v1 = (eval e1 f) in
								let f' = (bind f s' v1) in
								(eval e2 f')
	
	| Fun(par_form,corpo)	->	Closure(par_form,corpo,f)

	| LetRec(str_fun, par_form, corpo_fun, corpo_in) ->
		let chiusura = RecClosure(str_fun,par_form,corpo_fun,f) in
		let f' = (bind f str_fun chiusura) in
		(eval corpo_in f')
	
	| FunCall(str_fun,x) ->
		let chiusura = (eval str_fun f) in
		(match chiusura with
		| Closure(par_form, corpo, f0) ->
			let par_att = (eval x f) in
			let f' = (bind f0 par_form par_att) in
			(eval corpo f')
		| RecClosure(str_fun, par_form, corpo, f0) ->
			let par_att = (eval x f) in
			let f' = (bind f0 str_fun chiusura) in
			let f' = (bind f' par_form par_att) in
			(eval corpo f')
		| _ -> failwith "FunCall : Not Functional Value")
;;


(** TEST *******************************************************************)

let env = emptyenv Unbound;;

let pari    = Fun("x",IsZero(Mod(Den("x"),CstInt(2))));;
let dispari = Fun("x",Eq(Mod(Den("x"),CstInt(2)),CstInt(1)));;
let doppio  = Fun("x",Prod(Den("x"),CstInt(2)));;

let s0 = Empty(SET_INT);;
let s1 = Push(Push(Singleton(CstInt(2),SET_INT),CstInt(1)),CstInt(5));;
let s2 = Push(Singleton(CstInt(4),SET_INT),CstInt(2));;
let s3 = Push(Singleton(CstInt(5),SET_INT),CstInt(2));;
let s4 = Push(Push(Singleton(CstInt(1),SET_INT),CstInt(5)),CstInt(2));;

(* s0 := {}      *)
(* s1 := {5,1,2} *)
(* s2 := {2,4}   *)
(* s3 := {2,5}   *)
(* s4 := {2,5,1} *)

(*==[Eq]==================================================================*)
eval (Eq(s0,s0)) env;; (* Bool(true)  *)
eval (Eq(s1,s2)) env;; (* Bool(false) *)
eval (Eq(s1,s4)) env;; (* Bool(true)  *)

(*==[IsEmpty]=============================================================*)
eval (IsEmpty(s0)) env;; (* Bool(true)  *)
eval (IsEmpty(s1)) env;; (* Bool(false) *)
eval (IsEmpty(s2)) env;; (* Bool(false) *)

(*==[Contains]============================================================*)
eval (Contains(s0,CstInt(4))) env;; (* Bool(false) *)
eval (Contains(s1,CstInt(4))) env;; (* Bool(false) *)
eval (Contains(s2,CstInt(4))) env;; (* Bool(true)  *)

(*==[Push]================================================================*)
eval (Push(s0,CstInt(3))) env;; (* Set([Int 3],SET_INT) *)
eval (Push(s1,CstInt(1))) env;; (* Set([Int 5;Int 1;Int 2],SET_INT) *)
eval (Push(s2,CstInt(4))) env;; (* Set([Int 2;Int 4],SET_INT) *)

(*==[Pop]=================================================================*)
eval (Pop(s0,CstInt(1))) env;; (* Set([],SET_INT) *)
eval (Pop(s1,CstInt(2))) env;; (* Set([Int 5;Int 1],SET_INT) *)
eval (Pop(s2,CstInt(3))) env;; (* Set([Int 2;Int 4],SET_INT) *)

(*==[Min]=================================================================*)
eval (Min(s1)) env;; (* Int(1) *)
eval (Min(s2)) env;; (* Int(2) *)
eval (Min(s3)) env;; (* Int(2) *)

(*==[Max]=================================================================*)
eval (Max(s1)) env;; (* Int(5) *)
eval (Max(s2)) env;; (* Int(4) *)
eval (Max(s3)) env;; (* Int(5) *)

(*==[IsSubset]============================================================*)
eval (IsSubset(s0,s1)) env;; (* Bool(true)  *)
eval (IsSubset(s2,s1)) env;; (* Bool(false) *)
eval (IsSubset(s3,s1)) env;; (* Bool(true)  *)

(*==[Union]===============================================================*)
eval (Union(s0,s1)) env;; (* Set([Int 5;Int 1;Int 2],SET_INT) *)
eval (Union(s1,s2)) env;; (* Set([Int 1;Int 5;Int 2;Int 4],SET_INT) *)
eval (Union(s2,s3)) env;; (* Set([Int 4;Int 2;Int 5],SET_INT) *)

(*==[Inter]===============================================================*)
eval (Inter(s0,s1)) env;; (* Set([],SET_INT) *)
eval (Inter(s1,s2)) env;; (* Set([Int 2],SET_INT) *)
eval (Inter(s1,s3)) env;; (* Set([Int 2;Int 5],SET_INT) *)

(*==[Subtract]============================================================*)
eval (Subtract(s0,s1)) env;; (* Set([],SET_INT) *)
eval (Subtract(s1,s2)) env;; (* Set([Int 1;Int 5],SET_INT) *)
eval (Subtract(s1,s3)) env;; (* Set([Int 1],SET_INT) *)

(*==[ForAll]==============================================================*)
eval (ForAll(s0,pari)) env;; (* Bool(true)  *)
eval (ForAll(s1,pari)) env;; (* Bool(false) *)
eval (ForAll(s2,pari)) env;; (* Bool(true)  *)

(*==[Exist]===============================================================*)
eval (Exist(s0,dispari)) env;; (* Bool(false) *)
eval (Exist(s1,dispari)) env;; (* Bool(true)  *)
eval (Exist(s2,dispari)) env;; (* Bool(false) *)

(*==[Filter]==============================================================*)
eval (Filter(s0,dispari)) env;; (* Set([],SET_INT) *)
eval (Filter(s1,dispari)) env;; (* Set([Int 5;Int 1],SET_INT) *)
eval (Filter(s2,dispari)) env;; (* Set([],SET_INT) *)

(*==[Map]=================================================================*)
eval (Map(s0,doppio)) env;; (* Set([],SET_INT) *)
eval (Map(s1,doppio)) env;; (* Set([Int 10;Int 2;Int 4],SET_INT) *)
eval (Map(s2,doppio)) env;; (* Set([Int 4;Int 8],SET_INT) *)

(*==[ERRORI A RUN-TIME]===================================================*)
(*
let e = Singleton(CstString("hello"),SET_BOOL);;
eval e env;; (*Singleton : Not Valid Set*)

let e = Contains(s0,CstBool(true));; (*anche: Push/Pop*)
eval e env;; (*Contains : Not Respected Set Type*)

let e = Max(Empty(SET_INT));; (*anche: Min*)
eval e env;; (*Max : Empty Set*)

let e = Eq(s0,Empty(SET_BOOL));; (*anche: IsSubset/Union/Inter/Subtract*)
eval e env;; (*IsSubset : Different Set Types*)

let f = Fun("x",CstInt(0));;
let e = ForAll(s1,f);; (*anche: Exist/Filter*)
eval e env;; (*ForAll : Not Bool Returned*)

let f = Fun("x",CstString("hello"));;
let e = Map(s1,f);;
eval e env;; (*Map : Not Returned a Set Type Element*)
*)

