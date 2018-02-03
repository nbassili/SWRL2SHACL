/************************************************************************
 SWRL2SHACL
 A tool for translating SWRL rules into SPIN rules

 by Dr. Nick Bassiliades, Associate Professor
 Department of Informatics, Aristotle University of Thessaloniki, Greece

 email: nbassili@csd.auth.gr
 URL: http://tinyURL.com/bassiliades

 All right reserved - Please contact for details
*************************************************************************/

%:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdf11)).
:- rdf_register_prefix(swrl, 'http://www.w3.org/2003/11/swrl#').
:- rdf_register_prefix(spin, 'http://spinrdf.org/spin#').
:- rdf_register_prefix(sp, 'http://spinrdf.org/sp#').
:- rdf_register_prefix(spif, 'http://spinrdf.org/spif#').
:- rdf_register_prefix(xsd, 'http://www.w3.org/2001/XMLSchema#').
:- rdf_register_prefix(arg, 'http://spinrdf.org/arg#').
:- dynamic option/2.

%:- load_rdf('univ-with-SWRL.owl', Triples), writelist(Triples), nl.

%:- rdf_load('univ-with-SWRL.owl',[register_namespaces(true)]).



swrl2shacl(OntoFile) :-
	swrl2shacl(OntoFile,1,[]).

swrl2shacl(OntoFile,N) :-
	integer(N),
	swrl2shacl(OntoFile,N,[]).
swrl2shacl(OntoFile,Options) :-
	is_list(Options),
	swrl2shacl(OntoFile,1,Options).

swrl2shacl(OntoFile,N,Options) :-
	integer(N),
	is_list(Options), !,
	assert_options(Options),
	rdf_reset_db,
	rdf_load(OntoFile,[register_namespaces(true)]),
	findall(R, rdf(R,rdf:type,swrl:'Imp'), Rules),
	multiply_rules(Rules,N,RulesN),
	% Uncomment the line below (and comment the one right after) to measure total rule translation time
	%time(translate_rules(RulesN,_SPINRules,_TrRules)),
	translate_rules(RulesN,_SPINRules,_TrRules),
	delete_swrl_rules(RulesN),
	%writelist(SPINRules), nl,
	%writelist(TrRules),
	rdf(OntoURI,rdf:type,owl:'Ontology'),
	rdf_assert(OntoURI,owl:imports,'http://spinrdf.org/spif'),
	rdf_assert(OntoURI,owl:imports,'http://spinrdf.org/spin'),
	rdf_assert(OntoURI,owl:imports,'http://spinrdf.org/sp'),
	rdf_assert(OntoURI,owl:imports,'http://spinrdf.org/spl'),
	atom_concat('OUT-',OntoFile,OutOntoFile),
	rdf_save(OutOntoFile,[]),
	retract_options.
swrl2shacl(_,_,_) :-
	writeln('Wrong arguments!').

assert_options([]) :- !.
assert_options([Option|Rest]) :-
	Option =.. [Opt,Arg],
	assert(option(Opt,Arg)),
	assert_options(Rest).

retract_options :-
	retractall(option(_,_)).

multiply_rules(Rules,1,Rules) :- !.
multiply_rules(Rules,N,RulesN) :-
	N > 1,
	multiply_rules_aux(Rules,N,NewRules),
	N1 is N - 1,
	append(NewRules,RulesN1,RulesN),
	multiply_rules(Rules,N1,RulesN1).

multiply_rules_aux([],_,[]) :- !.
multiply_rules_aux([Rule|RestRules],N,[NewRule|RestNewRules]) :-
	rdf(Rule,swrl:head,Head),
	rdf(Rule,swrl:body,Body),
	atomic_list_concat([Rule,'-',N],NewRule),
	rdf_assert(NewRule,swrl:head,Head),
	rdf_assert(NewRule,swrl:body,Body),
	multiply_rules_aux(RestRules,N,RestNewRules).

translate_rules([],[],[]).
translate_rules([Rule|RestRules],AllSPINRules,AllTrRules) :-
	translate_one_rule(Rule,SPINRules,TrRuleStrs),
	append(SPINRules,RestSPINRules,AllSPINRules),
	append(TrRuleStrs,RestTrRules,AllTrRules),
	translate_rules(RestRules,RestSPINRules,RestTrRules).

translate_one_rule(Rule,SPINRules,TrRuleStrs) :-
	rdf(Rule,swrl:head,Head),
	rdf(Rule,swrl:body,Body),
	discover_this(Body,ListOfThis),
	translate_one_rule_many_this(ListOfThis,[],Head,Body,SPINRules,TrRuleStrs), !.
translate_one_rule(Rule,[],[]) :-
	write('The translation of SWRL rule '),
	rdf(Rule,rdfs:label,RL),
	(RL = Label ^^ _DataType; RL = Label),
	write(Label),
	%rdf_tree(Rule,swrl,Triples),
	write(' has failed! Rule is ignored.'), nl.
	%writelist(Triples), nl.


discover_this(Body,ListOfThis) :-
	rdf_list(Body,ListOfAtoms),
	setof(This,Atom^(member(Atom,ListOfAtoms),check_atom_type(Atom),rdf(Atom,swrl:argument1,This)),ListOfThis).

check_atom_type(Atom) :-
	rdf(Atom,rdf:type,swrl:'ClassAtom'), !.
check_atom_type(Atom) :-
	rdf(Atom,rdf:type,swrl:'DatavaluedPropertyAtom'), !.
check_atom_type(Atom) :-
	rdf(Atom,rdf:type,swrl:'IndividualPropertyAtom').

translate_one_rule_many_this([],_ClassThis,_Head,_Body,[],[]).
translate_one_rule_many_this([This|RestThis],ListOfThisClasses,Head,Body,[SPINRule|RestSPINRules],[TrRuleStr|RestTrRuleStrs]):-
	discover_this_class(Body,This,ThisClass),
	\+ member(ThisClass,ListOfThisClasses), !,
	translate_one_rule_one_this(This,ThisClass,Head,Body,SPINRule,TrRuleStr),
	translate_one_rule_many_this(RestThis,[ThisClass|ListOfThisClasses],Head,Body,RestSPINRules,RestTrRuleStrs).
translate_one_rule_many_this([_This|RestThis],ListOfThisClasses,Head,Body,RestSPINRules,RestTrRuleStrs):-
	translate_one_rule_many_this(RestThis,ListOfThisClasses,Head,Body,RestSPINRules,RestTrRuleStrs).

translate_one_rule_one_this(This,ThisClass,Head,Body,SPINRule,TrRuleStr) :-
	translate_head(Head,SPINHead,TrHead,This),
	translate_body(Body,SPINBody,TrBody,This),
	append(TrHead,TrBody,TrRule),
	rdf_create_bnode(SPINRule),
	rdf_assert(ThisClass,spin:rule,SPINRule),
	rdf_assert(SPINRule,rdf:type,sp:'Construct'),
	rdf_assert(SPINRule,sp:where,SPINBody),
	rdf_assert(SPINRule,sp:templates,SPINHead),
	flatten(TrRule,TrRule1),
	atomic_list_concat(TrRule1, ' ', TrRuleAtom),
	atom_string(TrRuleAtom,TrRuleStr),
	rdf_assert(SPINRule,sp:text,TrRuleStr).

translate_head(Head,SPINHead,TransHead,This) :-
	translate_atom_list(head,Head,This,SPINHead,TrAL),
	append([['CONSTRUCT','{'],TrAL,['}']],TransHead).

translate_body(Body,SPINBody,TransBody,This) :-
	translate_atom_list(body,Body,This,SPINBody,TrAL),
	append([['WHERE','{'],TrAL,['}']],TransBody).

discover_this_class(Body,This,ThisClass) :-
	rdf_list(Body,AtomList),
	member(Atom,AtomList),
	rdf(Atom,swrl:argument1,This),
	discover_var_class(Atom,ThisClass), !.
discover_this_class(Body,This,ThisClass) :-
	rdf_list(Body,AtomList),
	member(Atom,AtomList),
	rdf(Atom,swrl:argument2,This),
	discover_var_class2(Atom,ThisClass), !.

discover_var_class(Atom,ThisClass) :-
	rdf(Atom,rdf:type,swrl:'ClassAtom'), !,
	rdf(Atom,swrl:classPredicate,ThisClass).
discover_var_class(Atom,ThisClass) :-
	rdf(Atom,rdf:type,swrl:'IndividualPropertyAtom'), !,
	rdf(Atom,swrl:propertyPredicate,Property),
	rdf(Property,rdfs:domain,ThisClass).
discover_var_class(Atom,ThisClass) :-
	rdf(Atom,rdf:type,swrl:'DatavaluedPropertyAtom'), !,
	rdf(Atom,swrl:propertyPredicate,Property),
	rdf(Property,rdfs:domain,ThisClass).

discover_var_class2(Atom,ThisClass) :-
	rdf(Atom,rdf:type,swrl:'IndividualPropertyAtom'), !,
	rdf(Atom,swrl:propertyPredicate,Property),
	rdf(Property,rdfs:range,ThisClass).

translate_atom_list(Mode,AtomList,This,SPINAtomList,TrAtomList) :-
	rdf_list(AtomList, PrologAtomList),
	translate_atom_list_aux(Mode,PrologAtomList,This,SPINPrologAtomList1,TrAtomList1),
	/*
	(Mode == body ->
		(writeln('-----------------------------'),
		writeln('----------BEFORE-------------'),
		write('SPINPrologAtomList: '), writeln(SPINPrologAtomList1),
		write('TrAtomList: '), writeln(TrAtomList1));
		true),*/
	re_order_atom_list_all(Mode,SPINPrologAtomList1,TrAtomList1,SPINPrologAtomList,TrAtomList),
	/*(Mode == body ->
		(writeln('-------AFTER----------------'),
		write('SPINPrologAtomList: '), writeln(SPINPrologAtomList),
		write('TrAtomList: '), writeln(TrAtomList));
		true),*/
	%flatten(SPINPrologAtomList,SPINPrologAtomList1),
	rdf_assert_list(SPINPrologAtomList, SPINAtomList).

translate_atom_list_aux(_,[],_,[],[]).
translate_atom_list_aux(Mode,[Atom|RestAtoms],This,AllSPINAtoms,TrAllAtoms) :-
	translate_atom(Mode,Atom,This,SPINAtom,TrAtom),
	(SPINAtom == [] ->
		(AllSPINAtoms = RestSPINAtoms, TrAllAtoms = RestTrAtoms);
		(is_list(SPINAtom) ->
			(append(SPINAtom,RestSPINAtoms,AllSPINAtoms), TrAllAtoms = [TrAtom|RestTrAtoms]);
			(AllSPINAtoms = [SPINAtom|RestSPINAtoms], TrAllAtoms = [TrAtom|RestTrAtoms]))
	),
	%append(TrAtom,RestTrAtoms,TrAllAtoms),
	translate_atom_list_aux(Mode,RestAtoms,This,RestSPINAtoms,RestTrAtoms).

re_order_atom_list_all(Mode,SPINPrologAtomListIn,TrAtomListIn,SPINPrologAtomListOut,TrAtomListOut) :-
	re_order_atom_list(Mode,SPINPrologAtomListIn,TrAtomListIn,SPINPrologAtomListNext,TrAtomListNext),
	SPINPrologAtomListIn \= SPINPrologAtomListNext, !,
	re_order_atom_list_all(Mode,SPINPrologAtomListNext,TrAtomListNext,SPINPrologAtomListOut,TrAtomListOut).
re_order_atom_list_all(_Mode,SPINPrologAtomList,TrAtomList,SPINPrologAtomList,TrAtomList).

%% TODO: Check if optimization is performed in all cases
re_order_atom_list(body,SPINPrologAtomListIn,TrAtomListIn,SPINPrologAtomList,TrAtomList) :-
	nth1(Pos1,TrAtomListIn,[Arg11,_Pred1,Arg21,'.']),
	%(is_var(Arg11); is_var(Arg21)), Arg11\=='?this', Arg21\=='?this',
	is_var(Arg11), Arg11 \== '?this', Arg21 \== '?this',
	nth1(Pos2,TrAtomListIn,[Arg12,_Pred2,Arg22,'.']), Pos2 > Pos1,
	((Arg12 == Arg11, Arg22=='?this');(Arg12 == '?this', Arg22 == Arg11)), !,
	swap(Pos1,Pos2,TrAtomListIn,TrAtomList),
	swap(Pos1,Pos2,SPINPrologAtomListIn,SPINPrologAtomList).
re_order_atom_list(body,SPINPrologAtomListIn,TrAtomListIn,SPINPrologAtomList,TrAtomList) :-
	nth1(Pos1,TrAtomListIn,[Arg11,_Pred1,Arg21,'.']),
	%(is_var(Arg11); is_var(Arg21)), Arg11\=='?this', Arg21\=='?this',
	is_var(Arg21), Arg21 \== '?this', Arg11\=='?this',
	nth1(Pos2,TrAtomListIn,[Arg12,_Pred2,Arg22,'.']), Pos2 > Pos1,
	((Arg12 == Arg21, Arg22=='?this');(Arg12 == '?this', Arg22 == Arg21)), !,
	swap(Pos1,Pos2,TrAtomListIn,TrAtomList),
	swap(Pos1,Pos2,SPINPrologAtomListIn,SPINPrologAtomList).
/* TODO here*/
re_order_atom_list(body,SPINPrologAtomListIn,TrAtomListIn,SPINPrologAtomList,TrAtomList) :-
	rank_triples(TrAtomListIn,RankedTrAtomListIn,_UnRankedTrAtomListIn), !,
	%length(TrAtomListIn,Length1),
	%length(RankedTrAtomListIn,Length2),
	/*
	(Length1 \== Length2 ->
		(write('TrAtomListIn : '), write(Length1), nl,
		 writelist(TrAtomListIn), nl,
		 write('RankedTrAtomListIn : '), write(Length2), nl,
		 writelist(RankedTrAtomListIn), nl);
		true),
	(UnRankedTrAtomListIn \== [] ->
		(write('UnRankedTrAtomListIn: '), write(UnRankedTrAtomListIn), nl);
		true),
		*/
	length(SPINPrologAtomListIn,Length),
	length(SPINPrologAtomList,Length),
	length(TrAtomList,Length),
	re_order_atom_list_aux(1,RankedTrAtomListIn,SPINPrologAtomListIn,TrAtomListIn,SPINPrologAtomList,TrAtomList).
re_order_atom_list(_Mode,SPINPrologAtomList,TrAtomList,SPINPrologAtomList,TrAtomList).

rank_triples(Triples,RankedTriples,UnRankedTriples) :-
	findall(0-[Arg1,Pred,Arg2,'.'],((Arg1='?this';Arg2='?this'),member([Arg1,Pred,Arg2,'.'],Triples)),ThisTriples),
	my_subtract(Triples, ThisTriples, RestTriples),
	rank_triples_aux(1,ThisTriples,RestTriples,RankedTriples,UnRankedTriples).

rank_triples_aux(_,RankedTriples,[],RankedTriples,[]).
rank_triples_aux(N,PrevRankedTriples,RestTriples,AllRankedTriples,UnRankedTriples) :-
	N_1 is N - 1,
	findall(N-[Arg1,Pred,Arg2,'.'],
		(member([Arg1,Pred,Arg2,'.'],RestTriples),
		 Pred \= 'owl:differentFrom', Pred \= 'owl:sameAs',
		 member(N_1-[OldArg1,_OldPred11,OldArg2,'.'],PrevRankedTriples),
		 once((Arg2==OldArg2;Arg2==OldArg1;Arg1==OldArg2;Arg1==OldArg1))),
		NewTriples1),
	findall(N-[Arg1,Pred,Arg2,'.'],
		(member([Arg1,Pred,Arg2,'.'],RestTriples),
		 (Pred = 'owl:differentFrom'; Pred = 'owl:sameAs'),
		 member(N_1-[OldArg1,_OldPred12,OldArg2,'.'],PrevRankedTriples),
		 ((once((Arg2==OldArg2;Arg2==OldArg1)),
		   member(M-[OlderArg1,_OlderPred11,OlderArg2,'.'],PrevRankedTriples),
		   M =< N_1,
		   once((Arg1==OlderArg2;Arg1==OlderArg1)));
		  (once((Arg1==OldArg2;Arg1==OldArg1)),
		   member(M-[OlderArg1,_OlderPred12,OlderArg2,'.'],PrevRankedTriples),
		   M =< N_1,
		   once((Arg2==OlderArg2;Arg2==OlderArg1)))
		  )
	 	),
		NewTriples2),
	findall(N-[Expression],
		(member([Expression],RestTriples),
		 member(N_1-[OldArg1,_OldPred2,OldArg2,'.'],PrevRankedTriples),
		 once((sub_atom(Expression,_,_,_,OldArg1);sub_atom(Expression,_,_,_,OldArg2)))),
		NewTriples3),
	append([NewTriples1,NewTriples2,NewTriples3],NewTriplesAll),
	remove_duplicates(NewTriplesAll,NewTriples),
	NewTriples \= [], !,
	my_subtract(RestTriples, NewTriples, NextRestTriples),
	append(PrevRankedTriples,NewTriples,NextRankedTriples),
	N1 is N + 1,
	rank_triples_aux(N1,NextRankedTriples,NextRestTriples,AllRankedTriples,UnRankedTriples).
rank_triples_aux(_,RankedTriples,UnRankedTriples,RankedTriples,UnRankedTriples).

re_order_atom_list_aux(_Pos,[],_SPINPrologAtomListIn,_TrAtomListIn,_SPINPrologAtomList,_TrAtomList).
re_order_atom_list_aux(Pos,[_N-Triple|RankedTrAtomList],SPINPrologAtomListIn,TrAtomListIn,SPINPrologAtomList,TrAtomList) :-
	nth1(PosIn,TrAtomListIn,Triple),
	nth1(PosIn,SPINPrologAtomListIn,SPINPrologAtom),
	nth1(Pos,SPINPrologAtomList,SPINPrologAtom),
	nth1(Pos,TrAtomList,Triple),
	NextPos is Pos + 1,
	re_order_atom_list_aux(NextPos,RankedTrAtomList,SPINPrologAtomListIn,TrAtomListIn,SPINPrologAtomList,TrAtomList).



/*
ClassAtom
	classPredicate
	argument1
DatavaluedPropertyAtom
IndividualPropertyAtom
	swrl#propertyPredicate
	swrl#argument2
	swrl#argument1
BuiltinAtom
	swrl#builtin
	swrl#arguments
*/

translate_atom(body,Atom,This,[],[]) :-
	rdf(Atom,rdf:type,swrl:'ClassAtom'),
	rdf(Atom,swrl:argument1,This), !.
translate_atom(Mode,Atom,This,SPINAtom,[TrArg,a,Class,'.']) :-
	rdf(Atom,rdf:type,swrl:'ClassAtom'),
	(Mode = head ; (Mode = body, not(option(subclass,true)))),	!,
	rdf(Atom,swrl:classPredicate,Class),
	rdf(Atom,swrl:argument1,Arg),
	translate_argument(Arg,This,SPINArg,TrArg),
	rdf_create_bnode(SPINAtom),
	rdf_assert(SPINAtom,sp:subject,SPINArg),
	rdf_assert(SPINAtom,sp:predicate,rdf:type),
	rdf_assert(SPINAtom,sp:object,Class).
% Optionally translate classAtom to rdf:type/(rdfs:subClassOf)* in order to avoid using reasoner
% Use subclass(true) in the options
translate_atom(body,Atom,This,SPINAtom,[TrArg,'rdf:type/(rdfs:subClassOf)*',Class,'.']) :-
		rdf(Atom,rdf:type,swrl:'ClassAtom'), !,
		rdf(Atom,swrl:classPredicate,Class),
		rdf(Atom,swrl:argument1,Arg),
		translate_argument(Arg,This,SPINArg,TrArg),
		rdf_create_bnode(SPINAtom),
		rdf_global_id(sp:'TriplePath',ExprURI1),
		rdf_assert(SPINAtom,rdf:type,ExprURI1),
		rdf_assert(SPINAtom,sp:subject,SPINArg),
		rdf_assert(SPINAtom,sp:object,Class),
		rdf_create_bnode(PathExpr),
		rdf_assert(SPINAtom,sp:path,PathExpr),
		rdf_global_id(sp:'SeqPath',ExprURI2),
		rdf_assert(PathExpr,rdf:type,ExprURI2),
		rdf_assert(PathExpr,sp:path1,rdf:type),
		rdf_create_bnode(PathExpr1),
		rdf_assert(PathExpr,sp:path2,PathExpr1),
		rdf_global_id(sp:'ModPath',ExprURI3),
		rdf_assert(PathExpr1,rdf:type,ExprURI3),
		rdf_assert(PathExpr1,sp:modMax,-2),
		rdf_assert(PathExpr1,sp:modMin,0),
		rdf_assert(PathExpr1,sp:subPath,rdfs:subClassOf).
translate_atom(_Mode,Atom,This,SPINAtom,[TrArg1,Property,TrArg2,'.']) :-
	rdf(Atom,rdf:type,swrl:'IndividualPropertyAtom'), !,
	rdf(Atom,swrl:propertyPredicate,Property),
	rdf(Atom,swrl:argument1,Arg1),
	rdf(Atom,swrl:argument2,Arg2),
	translate_argument(Arg1,This,SPINArg1,TrArg1),
	translate_argument(Arg2,This,SPINArg2,TrArg2),
	rdf_create_bnode(SPINAtom),
	rdf_assert(SPINAtom,sp:subject,SPINArg1),
	rdf_assert(SPINAtom,sp:predicate,Property),
	rdf_assert(SPINAtom,sp:object,SPINArg2).
translate_atom(_Mode,Atom,This,SPINAtom,[TrArg1,'owl:sameAs',TrArg2,'.']) :-
	rdf(Atom,rdf:type,swrl:'SameIndividualAtom'), !,
	rdf(Atom,swrl:argument1,Arg1),
	rdf(Atom,swrl:argument2,Arg2),
	translate_argument(Arg1,This,SPINArg1,TrArg1),
	translate_argument(Arg2,This,SPINArg2,TrArg2),
	rdf_create_bnode(SPINAtom),
	rdf_assert(SPINAtom,sp:subject,SPINArg1),
	rdf_assert(SPINAtom,sp:predicate,owl:sameAs),
	rdf_assert(SPINAtom,sp:object,SPINArg2).
translate_atom(_Mode,Atom,This,SPINAtom,[TrArg1,'owl:differentFrom',TrArg2,'.']) :-
	rdf(Atom,rdf:type,swrl:'DifferentIndividualsAtom'), !,
	rdf(Atom,swrl:argument1,Arg1),
	rdf(Atom,swrl:argument2,Arg2),
	translate_argument(Arg1,This,SPINArg1,TrArg1),
	translate_argument(Arg2,This,SPINArg2,TrArg2),
	rdf_create_bnode(SPINAtom),
	rdf_assert(SPINAtom,sp:subject,SPINArg1),
	rdf_assert(SPINAtom,sp:predicate,owl:differentFrom),
	rdf_assert(SPINAtom,sp:object,SPINArg2).
translate_atom(_Mode,Atom,This,SPINAtom,[TrArg1,Property,TrArg2,'.']) :-
	rdf(Atom,rdf:type,swrl:'DatavaluedPropertyAtom'), !,
	rdf(Atom,swrl:propertyPredicate,Property),
	rdf(Atom,swrl:argument1,Arg1),
	rdf(Atom,swrl:argument2,Arg2),
	translate_argument(Arg1,This,SPINArg1,TrArg1),
	translate_argument(Arg2,This,SPINArg2,TrArg2),
	rdf_create_bnode(SPINAtom),
	rdf_assert(SPINAtom,sp:subject,SPINArg1),
	rdf_assert(SPINAtom,sp:predicate,Property),
	rdf_assert(SPINAtom,sp:object,SPINArg2).
translate_atom(_Mode,Atom,This,SPINAtom,TrBuiltIn) :-
	rdf(Atom,rdf:type,swrl:'BuiltinAtom'), !,
	rdf(Atom,swrl:builtin,SWRLB),
	rdf(Atom,swrl:arguments,ArgumentList),
	rdf_list(ArgumentList, PrologArgumentList),
	rdf_current_prefix(swrlb,SWRLB_ns),
	atom_concat(SWRLB_ns,Function,SWRLB),
	translate_built_in(Function,This,PrologArgumentList,SPINAtom,TrBuiltIn).

translate_built_in(SWRL_builtin,This,[Arg1,Arg2],SPINAtom,[TrExpr] ) :-
	binary_filter_builtin(SWRL_builtin,SPIN_builtin,SPARQL_operator), !,
	tr_binary_Math_expression(SPIN_builtin,SPARQL_operator,This,[Arg1,Arg2],SPINExpr,TrArgs),
	tr_filter_expression(SPINExpr,TrArgs,SPINAtom,TrExpr).
translate_built_in(SWRL_builtin,This,[Arg1,Arg2,Arg3],SPINAtom,[TrExpr] ) :-
	binary_infix_assign_builtin(SWRL_builtin,SPIN_builtin,SPARQL_operator), !,
	tr_binary_assign_Math_expression(SPIN_builtin,SPARQL_operator,This,Arg1,[Arg2,Arg3],SPINAtom,TrExpr).
translate_built_in(SWRL_builtin,This,[Arg1|Args],SPINAtom,[TrExpr] ) :-
	associative_infix_assign_builtin(SWRL_builtin,SPIN_builtin,SPARQL_operator), !,
	tr_associative_assign_Math_expression(SPIN_builtin,SPARQL_operator,This,Arg1,Args,SPINAtom,TrExpr).
translate_built_in(SWRL_builtin,This,[Arg1,Arg2],SPINAtom,[TrExpr] ) :-
	unary_assign_builtin(SWRL_builtin,SPIN_builtin,SPARQL_operator), !,
	tr_unary_assign_math_expression(SPIN_builtin,SPARQL_operator,This,Arg1,Arg2,SPINAtom,TrExpr).
translate_built_in(SWRL_builtin,This,[Arg1|Args],SPINAtom,[TrExpr] ) :-
	assign_function_builtin(SWRL_builtin,SPIN_builtin), !,
	tr_function_expression(SPIN_builtin,This,Args,SPINExpr1,TrArgs1),
	tr_bind_expression(Arg1,This,SPINExpr1,TrArgs1,SPINAtom,TrExpr).
translate_built_in(SWRL_builtin,This,Args,SPINAtom,[TrExpr] ) :-
	filter_function_builtin(SWRL_builtin,SPIN_builtin), !,
	tr_function_expression(SPIN_builtin,This,Args,SPINExpr,TrArgs),
	tr_filter_expression(SPINExpr,TrArgs,SPINAtom,TrExpr).
translate_built_in(SWRL_builtin,This,[Arg1|Args],SPINAtom,[TrExpr] ) :-
	complex_assign_builtin(SWRL_builtin), !,
	tr_complex_expression(SWRL_builtin,This,Args,SPINExpr1,TrArgs1),
	tr_bind_expression(Arg1,This,SPINExpr1,TrArgs1,SPINAtom,TrExpr).
translate_built_in(SWRL_builtin,This,Args,SPINAtom,[TrExpr] ) :-
	complex_filter_builtin(SWRL_builtin), !,
	tr_complex_expression(SWRL_builtin,This,Args,SPINExpr,TrArgs),
	tr_filter_expression(SPINExpr,TrArgs,SPINAtom,TrExpr).
translate_built_in(SWRL_builtin,This,Args,SPINExpr,[TrArgs] ) :-
	complex_expr_builtin(SWRL_builtin), !,
	tr_complex_expression(SWRL_builtin,This,Args,SPINExpr,TrArgs).
translate_built_in(SWRL_builtin,This,Args,SPINExpr,[TrArgs] ) :-
	magic_property_builtin(SWRL_builtin,SPINMagicProperty), !,
	tr_magic_property_expression(SPINMagicProperty,This,Args,SPINExpr,TrArgs).


binary_filter_builtin(greaterThan,sp:gt,'>').
binary_filter_builtin(greaterThanOrEqual,sp:ge,'>=').
binary_filter_builtin(lessThan,sp:lt,'<').
binary_filter_builtin(lessThanOrEqual,sp:le,'<=').
binary_filter_builtin(equal,sp:eq,'=').
binary_filter_builtin(notEqual,sp:ne,'!=').

associative_infix_assign_builtin(add,sp:add,'+').
associative_infix_assign_builtin(multiply,sp:mul,'*').

binary_infix_assign_builtin(subtract,sp:sub,'-').
binary_infix_assign_builtin(divide,sp:divide,'/').

unary_assign_builtin(unaryPlus,sp:unaryPlus,'+').
unary_assign_builtin(unaryMinus,sp:unaryMinus,'-').
unary_assign_builtin(abs,sp:abs,abs).
unary_assign_builtin(ceiling,sp:ceil,ceil).
unary_assign_builtin(floor,sp:floor,floor).
unary_assign_builtin(round,sp:round,round).

assign_function_builtin(mod,spif:mod).
assign_function_builtin(stringConcat,sp:concat).
assign_function_builtin(stringLength,sp:strlen).
assign_function_builtin(upperCase,sp:ucase).
assign_function_builtin(lowerCase,sp:lcase).
assign_function_builtin(substringBefore,sp:strbefore).
assign_function_builtin(substringAfter,sp:strafter).
assign_function_builtin(substring,sp:substr).
assign_function_builtin(replace,sp:replace).

filter_function_builtin(endsWith,sp:strends).
filter_function_builtin(startsWith,sp:strstarts).
filter_function_builtin(contains,sp:contains).
filter_function_builtin(matches,sp:regex).

complex_assign_builtin(integerDivide).
% complex_assign_builtin(mod).   % There is a direct translation to spif:mod
complex_assign_builtin(pow).
complex_assign_builtin(normalizeSpace).
complex_assign_builtin(date).

complex_filter_builtin(containsIgnoreCase).
complex_filter_builtin(stringEqualIgnoreCase).
complex_filter_builtin(empty).

complex_expr_builtin(member).
complex_expr_builtin(length).
complex_expr_builtin(first).
complex_expr_builtin(rest).

magic_property_builtin(tokenize,spif:split).


% More built-ins to come ...


tr_complex_expression(integerDivide,This,[Arg1,Arg2],SPINExpr1,TrArgs1) :- !,
	tr_binary_Math_expression(sp:divide,'/',This,[Arg1,Arg2],SPINExpr,TrArgs),
	tr_cast_expression(SPINExpr,TrArgs,xsd:integer,SPINExpr1,TrArgs1).
/* There is a direct translation to spif:mod
tr_complex_expression(mod,This,[Arg1,Arg2],SPINExpr3,TrArgs3) :- !,
	tr_complex_expression(integerDivide,This,[Arg1,Arg2],SPINExpr1,TrArgs1),
	binary_assign_builtin(multiply,SPIN_mul,OpMul),
	tr_binary_Math_expression(SPIN_mul,OpMul,This,[SPINExpr1-TrArgs1,Arg2],SPINExpr2,TrArgs2),
	binary_assign_builtin(subtract,SPIN_sub,OpSub),
	tr_binary_Math_expression(SPIN_sub,OpSub,This,[Arg1,SPINExpr2-TrArgs2],SPINExpr3,TrArgs3).
*/
tr_complex_expression(pow,This,[Arg1,Arg2],SPINExpr,TrArgs) :- !,
	Arg2=Arg2Value^^'http://www.w3.org/2001/XMLSchema#int',
	tr_multi_binary_Math_expression(sp:mul,'*',This,Arg1,Arg2Value,SPINExpr,TrArgs).
tr_complex_expression(containsIgnoreCase,This,[Arg1,Arg2],SPINExpr,TrArgs) :- !,
	tr_function_expression(sp:lcase,This,[Arg1],SPINExpr1,TrArgs1),
	tr_function_expression(sp:lcase,This,[Arg2],SPINExpr2,TrArgs2),
	tr_function_expression(sp:contains,This,[SPINExpr1-TrArgs1,SPINExpr2-TrArgs2],SPINExpr,TrArgs).
tr_complex_expression(stringEqualIgnoreCase,This,[Arg1,Arg2],SPINExpr,TrArgs) :- !,
	tr_function_expression(sp:lcase,This,[Arg1],SPINExpr1,TrArgs1),
	tr_function_expression(sp:lcase,This,[Arg2],SPINExpr2,TrArgs2),
	tr_binary_Math_expression(sp:eq,'=',This,[SPINExpr1-TrArgs1,SPINExpr2-TrArgs2],SPINExpr,TrArgs).
tr_complex_expression(normalizeSpace,This,[Arg2],SPINExpr,TrArgs) :- !,
%%%   (REPLACE(REPLACE(REPLACE(?y, "\\s+", " "), "^\\s+", ""), "\\s+$", "")
	rdf_global_id(xsd:string,StringURI),
	tr_function_expression(sp:replace,This,[Arg2,'\\s+'^^StringURI, ' '^^StringURI],SPINExpr1,TrArgs1),
	tr_function_expression(sp:replace,This,[SPINExpr1-TrArgs1,'^\\s+'^^StringURI, ''^^StringURI],SPINExpr2,TrArgs2),
	tr_function_expression(sp:replace,This,[SPINExpr2-TrArgs2,'\\s+$'^^StringURI, ''^^StringURI],SPINExpr,TrArgs).
tr_complex_expression(date,This,[YearArg,MonthArg,DayArg],SPINExpr,TrArgs) :- !,
	translate_argument(YearArg,This,SPINYearArg,YearTrArg),
	translate_argument(MonthArg,This,SPINMonthArg,MonthTrArg),
	translate_argument(DayArg,This,SPINDayArg,DayTrArg),
	tr_cast_expression(SPINYearArg,YearTrArg,xsd:string,SPINYearArg1,YearTrArg1),
	tr_cast_expression(SPINMonthArg,MonthTrArg,xsd:string,SPINMonthArg1,MonthTrArg1),
	tr_cast_expression(SPINDayArg,DayTrArg,xsd:string,SPINDayArg1,DayTrArg1),
	rdf_global_id(xsd:string,StringURI),
	tr_function_expression(sp:concat,This,[SPINYearArg1-YearTrArg1,'-'^^StringURI,SPINMonthArg1-MonthTrArg1,'-'^^StringURI,SPINDayArg1-DayTrArg1],SPINExpr1,TrArgs1),
	tr_cast_expression(SPINExpr1,TrArgs1,xsd:date,SPINExpr,TrArgs).
tr_complex_expression(empty,This,[Arg1],SPINExpr,TrArgs) :- !,
	tr_binary_Math_expression(sp:eq,'=',This,[Arg1,rdf:nil],SPINExpr,TrArgs).
tr_complex_expression(member,This,[Arg1,Arg2],SPINExpr,TrArgs) :- !,
	translate_argument(Arg1,This,SPINArg1,TrArg1),
	translate_argument(Arg2,This,SPINArg2,TrArg2),
	rdf_global_id(sp:'TriplePath',ExprURI1),
	rdf_create_bnode(SPINExpr),
	rdf_assert(SPINExpr,rdf:type,ExprURI1),
	rdf_assert(SPINExpr,sp:subject,SPINArg2),
	rdf_assert(SPINExpr,sp:object,SPINArg1),
	rdf_create_bnode(PathExpr),
	rdf_assert(SPINExpr,sp:path,PathExpr),
	rdf_global_id(sp:'SeqPath',ExprURI2),
	rdf_assert(PathExpr,rdf:type,ExprURI2),
	rdf_assert(PathExpr,sp:path2,rdf:first),
	rdf_create_bnode(PathExpr1),
	rdf_assert(PathExpr,sp:path1,PathExpr1),
	rdf_global_id(sp:'ModPath',ExprURI3),
	rdf_assert(PathExpr1,rdf:type,ExprURI3),
	rdf_assert(PathExpr1,sp:modMax,-2),
	rdf_assert(PathExpr1,sp:modMin,0),
	rdf_assert(PathExpr1,sp:subPath,rdf:rest),
	atomic_list_concat([TrArg2,'(rdf:rest)*/rdf:first',TrArg1,'.'],' ',TrArgs).
tr_complex_expression(first,This,[Arg1,Arg2],SPINExpr,TrArgs) :- !,
	translate_argument(Arg1,This,SPINArg1,TrArg1),
	translate_argument(Arg2,This,SPINArg2,TrArg2),
	rdf_create_bnode(SPINExpr),
	rdf_assert(SPINExpr,sp:subject,SPINArg2),
	rdf_assert(SPINExpr,sp:object,SPINArg1),
	rdf_assert(SPINExpr,sp:predicate,rdf:first),
	atomic_list_concat([TrArg2,'rdf:first',TrArg1,'.'],' ',TrArgs).
tr_complex_expression(rest,This,[Arg1,Arg2],SPINExpr,TrArgs) :- !,
	translate_argument(Arg1,This,SPINArg1,TrArg1),
	translate_argument(Arg2,This,SPINArg2,TrArg2),
	rdf_create_bnode(SPINExpr),
	rdf_assert(SPINExpr,sp:subject,SPINArg2),
	rdf_assert(SPINExpr,sp:object,SPINArg1),
	rdf_assert(SPINExpr,sp:predicate,rdf:first),
	atomic_list_concat([TrArg2,'rdf:rest',TrArg1,'.'],' ',TrArgs).
tr_complex_expression(length,This,[Arg1,Arg2],SPINExpr,TrArgs) :- !,
	translate_argument(Arg1,This,SPINArg1,TrArg1),
	translate_argument(Arg2,This,SPINArg2,TrArg2),
	create_var(e,SPINVar_e,StrVar_e),
	tr_complex_expression(member,This,[SPINVar_e-StrVar_e,SPINArg2-TrArg2],SPINPathExpr,PathTrArgs),
	rdf_global_id(sp:'SubQuery',ExprURI1),
	rdf_create_bnode(SPINExpr),
	rdf_assert(SPINExpr,rdf:type,ExprURI1),
	rdf_create_bnode(SPINQueryExpr),
	rdf_assert(SPINExpr,sp:query,SPINQueryExpr),
	rdf_global_id(sp:'Select',ExprURI2),
	rdf_assert(SPINQueryExpr,rdf:type,ExprURI2),
	rdf_global_id(spin:'_this', ThisURI),
	rdf_assert_list([ThisURI,SPINArg2],SPINGroupByVarsExpr),
	rdf_assert(SPINQueryExpr,sp:groupBy,SPINGroupByVarsExpr),
	%% Count expression
	rdf_create_bnode(SPINCountExpr),
	rdf_global_id(sp:'Count',ExprURI3),
	rdf_assert(SPINCountExpr,rdf:type,ExprURI3),
	rdf_assert(SPINCountExpr,sp:expression,SPINVar_e),
	rdf_assert(SPINArg1,sp:expression,SPINCountExpr),
	rdf_assert_list([ThisURI,SPINArg2,SPINArg1],SPINResultVarsExpr),
	rdf_assert(SPINQueryExpr,sp:resultVariables,SPINResultVarsExpr),
	%% Where expression
	rdf_assert_list([SPINPathExpr],SPINWhereExpr),
	rdf_assert(SPINQueryExpr,sp:where,SPINWhereExpr),
	atomic_list_concat(['{','SELECT','?this',TrArg2,'((COUNT(',StrVar_e,')) AS',TrArg1,')','WHERE','{',PathTrArgs,'}','GROUP BY','?this',TrArg2,'}','.'],' ',TrArgs).
/*
    {
          SELECT ?this ?arg2 ((COUNT(?e)) AS ?arg1)
          WHERE {
              ?arg2 (rdf:rest)* / rdf:first ?e .
          }
          GROUP BY ?this ?arg2
    } .
*/

tr_function_expression(Expression,This,Args,SPINExpr,TrArgs) :-
	rdf_global_id(Expression,ExprURI),
	translate_list_of_arguments(Args,This,SPINArgs,',',TrArgs1),
	term_to_atom(Expression,AtomFunc),
	atom_concat(AtomFunc,'(',FuncExpr),
	atomic_list_concat([FuncExpr,TrArgs1,')'],' ',TrArgs),
	rdf_create_bnode(SPINExpr),
	rdf_assert(SPINExpr,rdf:type,ExprURI),
	assert_math_expression_arguments(1,SPINExpr,SPINArgs).

/*
tr_binary_filter_expression(Expression,Operator,This,Arg1,Arg2,SPINAtom,TrExpr) :-
	tr_binary_Math_expression(Expression,Operator,This,[Arg1,Arg2],SPINExpr,TrArgs),
	atomic_list_concat(['FILTER','(',TrArgs,')'],TrExpr),
	rdf_create_bnode(SPINAtom),
	rdf_assert(SPINAtom,rdf:type,sp:'Filter'),
	rdf_assert(SPINAtom,sp:expression,SPINExpr).
*/

tr_filter_expression(SPINExpr,TrArgs,SPINAtom,TrExpr) :-
	atomic_list_concat(['FILTER','(',TrArgs,')'],TrExpr),
	rdf_create_bnode(SPINAtom),
	rdf_assert(SPINAtom,rdf:type,sp:'Filter'),
	rdf_assert(SPINAtom,sp:expression,SPINExpr).

tr_bind_expression(Arg1,This,SPINExpr,TrArgs,SPINAtom,TrExpr) :-
	translate_argument(Arg1,This,SPINArg1,TrArg1),
	atomic_list_concat(['BIND','(',TrArgs,'AS',TrArg1,')'],' ',TrExpr),
	rdf_create_bnode(SPINAtom),
	rdf_assert(SPINAtom,rdf:type,sp:'Bind'),
	rdf_assert(SPINAtom,sp:variable,SPINArg1),
	rdf_assert(SPINAtom,sp:expression,SPINExpr).

tr_binary_assign_Math_expression(Expression,Operator,This,Arg1,[Arg2,Arg3],SPINAtom,TrExpr) :-
	tr_binary_Math_expression(Expression,Operator,This,[Arg2,Arg3],SPINExpr,TrArgs),
	tr_bind_expression(Arg1,This,SPINExpr,TrArgs,SPINAtom,TrExpr).

tr_associative_assign_Math_expression(Expression,Operator,This,Arg1,Args,SPINAtom,TrExpr) :-
	tr_associative_Math_expression(Expression,Operator,This,Args,SPINExpr,TrArgs),
	tr_bind_expression(Arg1,This,SPINExpr,TrArgs,SPINAtom,TrExpr).

tr_unary_assign_math_expression(Expression,Operator,This,Arg1,Arg2,SPINAtom,TrExpr) :-
	tr_unary_Math_expression(Expression,Operator,This,Arg2,SPINExpr,TrArg2),
	tr_bind_expression(Arg1,This,SPINExpr,TrArg2,SPINAtom,TrExpr).

tr_binary_Math_expression(Expression,Operator,This,[Arg1,Arg2],SPINExpr,TrArgs) :-
	rdf_global_id(Expression,ExprURI),
	translate_argument(Arg1,This,SPINArg1,TrArg1),
	translate_argument(Arg2,This,SPINArg2,TrArg2),
	atomic_list_concat(['(',TrArg1,Operator,TrArg2,')'],' ',TrArgs),
	rdf_create_bnode(SPINExpr),
	rdf_assert(SPINExpr,rdf:type,ExprURI),
	assert_math_expression_arguments(1,SPINExpr,[SPINArg1,SPINArg2]).

tr_associative_Math_expression(Expression,Operator,This,[Arg1,Arg2],SPINExpr,TrArgs) :- !,
	tr_binary_Math_expression(Expression,Operator,This,[Arg1,Arg2],SPINExpr,TrArgs).
tr_associative_Math_expression(Expression,Operator,This,[FirstArg|RestArgs],SPINExpr,TrArgs) :-
	tr_associative_Math_expression(Expression,Operator,This,RestArgs,RestSPINExpr,RestTrArgs),
	tr_binary_Math_expression(Expression,Operator,This,[FirstArg,RestSPINExpr-RestTrArgs],SPINExpr,TrArgs).

tr_unary_Math_expression(Expression,Operator,This,Arg,SPINExpr,TrArg) :-
	rdf_global_id(Expression,ExprURI),
	translate_argument(Arg,This,SPINArg,TrArg1),
	atomic_list_concat([Operator,'(',TrArg1,')'],TrArg),
	rdf_create_bnode(SPINExpr),
	rdf_assert(SPINExpr,rdf:type,ExprURI),
	assert_math_expression_arguments(1,SPINExpr,[SPINArg]).

tr_multi_binary_Math_expression(sp:mul,'*',_This,_Arg,0,SPINExpr,1) :- !,
	rdf_canonical_literal(1,SPINExpr).
tr_multi_binary_Math_expression(sp:mul,'*',This,Arg,1,SPINArg,TrArg) :- !,
	translate_argument(Arg,This,SPINArg,TrArg).
tr_multi_binary_Math_expression(sp:mul,'*',This,Arg,2,SPINExpr,TrArgs) :- !,
	tr_binary_Math_expression(sp:mul,'*',This,[Arg,Arg],SPINExpr,TrArgs).
tr_multi_binary_Math_expression(sp:mul,'*',This,Arg,N,SPINExpr,TrArgs) :-
	N > 2, !,
	N1 is N - 1,
	tr_multi_binary_Math_expression(sp:mul,'*',This,Arg,N1,SPINExpr1,TrArgs1),
	tr_binary_Math_expression(sp:mul,'*',This,[SPINExpr1-TrArgs1,Arg],SPINExpr,TrArgs).

tr_cast_expression(SPINExpr,TrArgs,Datatype,SPINExpr1,TrArgs1) :-
	rdf_global_id(spif:cast,ExprURI),
	rdf_create_bnode(SPINExpr1),
	rdf_assert(SPINExpr1,rdf:type,ExprURI),
	rdf_assert(SPINExpr1,sp:arg1,SPINExpr),
	rdf_global_id(Datatype,DatatypeURI),
	rdf_assert(SPINExpr1,arg:datatype,DatatypeURI),
	term_to_atom(Datatype,DatatypeAtom),
	atomic_list_concat(['spif:cast(',TrArgs,',',DatatypeAtom,')'],' ',TrArgs1).

tr_magic_property_expression(spif:split,This,[Arg1,Arg2,Arg3],[SPINExpr1,SPINExpr2,SPINExpr3,SPINExpr4,SPINExpr5],TrArgs) :-
	translate_argument(Arg1,This,SPINArg1,TrArg1),
	translate_argument(Arg2,This,SPINArg2,TrArg2),
	translate_argument(Arg3,This,SPINArg3,TrArg3),
	rdf_global_id(spif:split,ExprURI),
	rdf_create_bnode(SPINExpr1),
	create_var('?0',SPINVar1,_TrArgVar1),
	rdf_assert(SPINExpr1,sp:subject,SPINArg1),
	rdf_assert(SPINExpr1,sp:predicate,ExprURI),
	rdf_assert(SPINExpr1,sp:object,SPINVar1),
	rdf_create_bnode(SPINExpr2),
	create_var('?0',SPINVar2,_TrArgVar2),
	rdf_assert(SPINExpr2,sp:subject,SPINVar2),
	rdf_assert(SPINExpr2,sp:predicate,rdf:first),
	rdf_assert(SPINExpr2,sp:object,SPINArg2),
	rdf_create_bnode(SPINExpr3),
	create_var('?0',SPINVar3,_TrArgVar3),
	create_var('?1',SPINVar4,_TrArgVar4),
	rdf_assert(SPINExpr3,sp:subject,SPINVar3),
	rdf_assert(SPINExpr3,sp:predicate,rdf:rest),
	rdf_assert(SPINExpr3,sp:object,SPINVar4),
	rdf_create_bnode(SPINExpr4),
	create_var('?1',SPINVar5,_TrArgVar5),
	rdf_assert(SPINExpr4,sp:subject,SPINVar5),
	rdf_assert(SPINExpr4,sp:predicate,rdf:first),
	rdf_assert(SPINExpr4,sp:object,SPINArg3),
	rdf_create_bnode(SPINExpr5),
	create_var('?1',SPINVar6,_TrArgVar6),
	rdf_assert(SPINExpr5,sp:subject,SPINVar6),
	rdf_assert(SPINExpr5,sp:predicate,rdf:rest),
	rdf_assert(SPINExpr5,sp:object,rdf:nil),
	%rdf_assert_list([SPINExpr1,SPINExpr2,SPINExpr3,SPINExpr4,SPINExpr5],SPINExpr),
	atomic_list_concat([TrArg1,'spif:split','(',TrArg2,TrArg3,')','.'],' ',TrArgs).


%assert_math_expression_arguments(1,SPINExpr,SPINArgs).
assert_math_expression_arguments(_,_,[]).
assert_math_expression_arguments(N,SPINExpr,[SPINArg|RestSPINArgs]) :-
	atom_concat(arg,N,ArgN),
	rdf_current_prefix(sp, SPURI),
	atom_concat(SPURI, ArgN, SPINArgNURI),
	rdf_global_id(SPINArgNPredicate, SPINArgNURI),
	rdf_assert(SPINExpr,SPINArgNPredicate,SPINArg),
	N1 is N + 1,
	assert_math_expression_arguments(N1,SPINExpr,RestSPINArgs).

translate_list_of_arguments(Args,This,SPINArgs,Operator,TrArgs) :-
	translate_list_of_arguments_aux(Args,This,SPINArgs,ListOfTrArgs),
	atom_concat(' ',Operator,Operator1),
	atom_concat(Operator1,' ',Operator2),
	atomic_list_concat(ListOfTrArgs,Operator2,TrArgs).

translate_list_of_arguments_aux([],_,[],[]).
translate_list_of_arguments_aux([Arg|RestArgs],This,[SPINArg|RestSPINArgs],[TrArg|RestTrArgs]) :-
	translate_argument(Arg,This,SPINArg,TrArg),
	translate_list_of_arguments_aux(RestArgs,This,RestSPINArgs,RestTrArgs).


translate_argument(SPINExpr-TrArg,_This,SPINExpr,TrArg) :- !.
translate_argument(Arg,_This,SPINData,Data) :-
	rdf_is_literal(Arg), !,
	Arg = Data ^^ DataType,
	rdf_lexical_form(Arg,Arg1),
	Arg1 = DataStr ^^ DataType,
	SPINData = DataStr ^^ DataType.
translate_argument(This,This,ThisURI,'?this') :- !,
	rdf_global_id(spin:'_this', ThisURI).
translate_argument(Arg,_This,SPINVar,TrArg) :-
	rdf(Arg,rdf:type,swrl:'Variable'), !,
	discover_var_name(Arg,Var),
	create_var(Var,SPINVar,TrArg).
translate_argument(Arg,_This,Arg,TrArg) :-
	rdf(Arg,rdf:type,Class),
	rdf(Class,rdf:type,owl:'Class'), !,
	rdf_global_id(Arg1,Arg),
	term_to_atom(Arg1,TrArg).

discover_var_name(Arg,Var) :-
	rdf(A,rdf:type,owl:'Ontology'),
	atom_concat(A,HashVar,Arg), !,
	atom_concat('#',Var,HashVar).
discover_var_name(Arg,Var) :-
	tokenize_atom(Arg,List),
	last(List,Var).

create_var(Var,SPINVar,TrArgVar) :-
	rdf_create_bnode(SPINVar),
	atom_string(Var,StrVar),
	rdf_assert(SPINVar,sp:varName,StrVar),
	atom_concat('?',Var,TrArgVar).

delete_swrl_rules([]).
delete_swrl_rules([R|T]) :-
	rdf_tree(R,swrl,Triples),
	delete_all_triples(Triples),
	delete_swrl_rules(T).

delete_all_triples([]).
delete_all_triples([S-P-O|Tail]) :-
	rdf_retractall(S, P, O),
	delete_all_triples(Tail).

% return all triples that originate from node
rdf_tree(Node,Prefix,Triples) :-
	rdf_tree_aux(Prefix,[Node],[],Triples1),
	reverse(Triples1,Triples).

rdf_tree_aux(_,[],Triples,Triples).
rdf_tree_aux(Prefix,[Node|Rest],TempTriples,Triples) :-
	rdf_current_prefix(Prefix,NS),
	mysetof(Node-Pred-Obj,(rdf(Node,Pred,Obj), \+ member(Node-Pred-Obj, TempTriples)),NewTriples),
	mysetof(NewNode, Length^After^Class^Pred^(
			      member(Node-Pred-NewNode,NewTriples),
			      \+ rdf_is_literal(NewNode),
			      ((rdf(NewNode,rdf:type,Class), sub_string(Class, 0, Length, After, NS));
			      rdf_is_bnode(NewNode))
		         ),
		NewNodes),
	append(NewNodes,Rest,NextNodes),
	append(NewTriples,TempTriples,NextTriples),
	rdf_tree_aux(Prefix,NextNodes,NextTriples,Triples).

duplicate_instance(_S,1) :- !.
duplicate_instance(S,N) :-
	N > 1,
	atomic_list_concat([S,'_',N],NewS),
	duplicate_all_properties(S,NewS),
	N1 is N - 1,
	duplicate_instance(S,N1).

duplicate_all_properties(S,NewS) :-
	rdf(S,P,O),
	rdf_assert(NewS,P,O),
	fail; true.


mysetof(X,Y,Z) :-
	setof(X,Y,Z), !.
mysetof(_X,_Y,[]).

swap(N1,N2,ListIn,ListOut) :-
	dissolve(ListIn,N1,Before1,E1,After1),
	NewN2 is N2 - N1,
	dissolve(After1,NewN2,Before2,E2,After2),
	append(Before2,[E1|After2],NewAfter1),
	append(Before1,[E2|NewAfter1],ListOut).

dissolve([E|After],1,[],E,After).
dissolve([H|T],N,[H|RestBefore],E,After) :-
	N > 1,
	N1 is N - 1,
	dissolve(T,N1,RestBefore,E,After).

is_var(Atom) :-
	atom(Atom),
	atom_prefix(Atom,'?').

writelist([]).
writelist([H|T]) :-
	write(H), nl,
	writelist(T).

my_subtract([],_,[]).
my_subtract([H|T],RankedTriples,Diff) :-
	member(_-H,RankedTriples), !,
	my_subtract(T,RankedTriples,Diff).
my_subtract([H|T],RankedTriples,[H|Diff]) :-
	my_subtract(T,RankedTriples,Diff).

remove_duplicates([],[]).
remove_duplicates([H|T],T1) :-
	member(H,T), !,
	remove_duplicates(T,T1).
remove_duplicates([H|T],[H|T1]) :-
	remove_duplicates(T,T1).


%:- trace, swrl2shacl('univ-with-SWRL.owl').

%stop_here.
