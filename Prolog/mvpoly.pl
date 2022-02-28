%%%% -*- Mode: Prolog -*-

%%%% mvpoly.pl


is_monomial(poly(_)) :- false.

is_monomial(m(_C, TD, VPs)) :-
    integer(TD),
    TD >= 0,
    is_list(VPs),
    foreach(member(V, VPs), is_varpower(V)),
	total_degree(VPs, TD).

is_polynomial(poly(Monomials)) :-
    is_list(Monomials), !,
    foreach(member(M, Monomials), is_monomial(M)).

is_polynomial(SingleMono) :-
  is_monomial(SingleMono), !.

is_varpower(v(Power, VarSymbol)) :-
    integer(Power), !,
    atom(VarSymbol).


%is_zero(X) è vero quanto X è una rappresentazione dello 0 
%(incluso, ovviamente il caso in cui sia proprio 0).

is_zero(m(0, _, _)).
is_zero(0).
is_zero([]).
is_zero(poly([])).


% coefficients(Poly, Coefficients) è vero quando Coefficients 
% è una lista dei coefficienti di Poly.

coefficients(m(C, TD, VPs), X) :-
    coefficients(poly([m(C, TD, VPs)]), X).

coefficients(poly([]), []).

coefficients(poly(m(C, _, _)), C).

coefficients(poly([m(C, _, _) | Rest]), [C | Cs]) :-
	coefficients(poly(Rest), Cs).



% degree_list(Poly, Degrees) è vero quando Degrees
% è una lista dei gradi di Poly.
degree_list(m(C, TD, VPs), X) :-
    degree_list(poly([m(C, TD, VPs)]), X).

degree_list(poly([]), []).

degree_list(poly(m(_, TD, _)), TD).

degree_list(poly([m(_, TD, _) | Rest]), [TD | TDs]) :-
	degree_list(poly(Rest), TDs).



% variables(Poly, Variables) è vero quando Variables 
% è una lista dei simboli di variabile che appaiono in Poly.

variables(m(_, _, Vp), VlistFlat):-
    variables(poly([m(_, _, Vp)]), VlistSorted),
    flat(VlistSorted, VlistFlat).

variables(poly([m(_, _, [])]), []).

variables(poly([m(_, _, []) | [Rest]]), Vs) :-
	variables(poly(Rest), Vs), !.

variables(poly([m(_, _, Vp)]), VlistSorted):-      
	vp_list(Vp, Vlist), !,
	sort(Vlist, VlistSorted).

variables(poly([m(_, _, Vp)]), VSorted) :-
	vp_list(Vp, VList), !,
	sort(VList, VSorted).

variables(poly([m(_, _, Vp) | Rest]), VSorted) :-
	vp_list(Vp, VList), !,
	variables(poly(Rest), Vs),
    append(VList, Vs, List),
    flat(List, FlatList),
	sort(FlatList, VSorted).

vp_list([], []) :- !.

vp_list([v(_, V)], [V]):- !.

vp_list([v(_, V) | Vps], [V, Vs]) :-
	vp_list(Vps, Vs).



% monomials(Poly, Monomials) è vero quando Monomials è la lista ordinata
% dei monomi che appaiono in Poly.
monomials(m(C, TD, VPs), X) :-
    monomials(poly([m(C, TD, VPs)]), X).

monomials(poly([]), []).

monomials(poly(Monomials), MonomialsSorted) :-
	sort_poly(Monomials, MonomialsSorted).



% max_degree(Poly, Degree) è vero quando Degree 
% è il massimo grado dei monomi che appaiono in Poly.

max_degree(poly([]), 0).

max_degree(Poly, Max) :-
	degree_list(Poly, Degree_list),
	max_list(Degree_list, Max).



% min_degree(Poly, Degree) è vero quando Degree 
% è il minimo grado dei monomi che appaiono in Poly.

min_degree(poly([]), 0).

min_degree(Poly, Min) :-
	degree_list(Poly, Degree_list),
	min_list(Degree_list, Min).



% poly_plus(Poly1, Poly2, Result)
% Il predicato poly_plus è vero quando Result è 
% il polinomio somma di Poly1 e Poly2

% Se sono tutti e due vuoti
poly_plus(poly([]), poly([]), poly([])).

% Se primo vuoto
poly_plus(poly([]), Result, Result).

% Se secondo vuoto
poly_plus(Result, poly([]), Result).

% somma con monomi
poly_plus(m(C, TD, VPs), poly(Poly2), Result) :-
    poly_plus(poly([m(C, TD, VPs)]), poly(Poly2), Result), !.

poly_plus(poly(Poly1), m(C, TD, VPs), Result) :-
    poly_plus(poly(Poly1), poly([m(C, TD, VPs)]), Result), !.

poly_plus(m(C1, TD1, VPs1), m(C2, TD2, VPs2), Result) :-
    poly_plus(poly([m(C1, TD1, VPs1)]), 
			poly([m(C2, TD2, VPs2)]), Result), !.

% Caso Ricorsivo
poly_plus(poly(Poly1), poly(Poly2), poly(Result)):-
    append(Poly1, Poly2, Z),
    sort_poly(Z, Zsorted),
    poly_plus_a(Zsorted, PolyPlus),
    flat(PolyPlus, FlatResult),
    normalize_poly(poly(FlatResult), poly(NormP)),
    canc_zero(poly(NormP), poly(Result)), !.

poly_plus_a([], []) :- !.

poly_plus_a([X], []) :- is_zero(X), !.

poly_plus_a([X], X) :- !.

poly_plus_a([m(C, TD, Vps), m(C1, TD, Vps) | T], Z) :- 
    C2 is C + C1,
    is_zero(m(C2, TD, Vps)),
    poly_plus_a(T, [Z]).

poly_plus_a([m(C, TD, Vps), m(C1, TD, Vps) | T], [m(C2, TD, Vps), Z]) :-
    C2 is C + C1,
    poly_plus_a(T, Z).

poly_plus_a([X, Y | T], [X, Z]) :- 
    poly_plus_a([Y | T], Z).




% poly_minus(Poly1, Poly2, Result)
% Il predicato poly_minus è vero quando Result è il polinomio 
% differenza di Poly1 e Poly2

% Se sono tutti e due vuoti
poly_minus(poly([]), poly([]), poly([])).

% Se primo vuoto
poly_minus(poly([]), Poly, Result):- 
    change_coeff(Poly, Result), !.

% Se secondo vuoto
poly_minus(Result, poly([]), Result).

%primo monomio
poly_minus(m(C, TD, VPs), poly(Poly2), Result) :-
    change_coeff(poly(Poly2), poly(ReversePoly2)), !,
    poly_plus(poly([m(C, TD, VPs)]), poly(ReversePoly2), Result).

%secondo monomio
poly_minus(poly(Poly1), m(C, TD, VPs), Result) :-
    change_coeff(poly([m(C, TD, VPs)]), poly(ReversePoly2)), !,
    poly_plus(poly(Poly1), poly(ReversePoly2), Result).

%due monomi
poly_minus(m(C1, TD1, VPs1), m(C2, TD2, VPs2), Result) :-
    change_coeff(poly([m(C2, TD2, VPs2)]), poly(ReversePoly2)), !,
    poly_plus(poly([m(C1, TD1, VPs1)]), poly(ReversePoly2), Result).

% Cambio tutti i coeff del secondo poly e faccio la somma
poly_minus(poly(Poly1), poly(Poly2), Result) :-
    change_coeff(poly(Poly2), poly(ReversePoly2)), !,
    poly_plus(poly(Poly1), poly(ReversePoly2), Result).

change_coeff(poly([]), poly([])) :- !.
change_coeff(poly([m(C, TD, VPs) | Monos]), 
				poly([m(NC, TD, VPs) | OppMonos])) :-
    NC is -C, !,
    change_coeff(poly(Monos), poly(OppMonos)).



%poly_times(Poly1, Poly2, Result) è vero quando Result è il polinomio 
%risultante dalla moltiplicazione di Poly1 e Poly2.

poly_times(_, poly([]), poly([])) :- !.

poly_times(poly([]), _, poly([])) :- !.

poly_times(m(C, TD, Vps), poly(Y), Result) :-
    poly_times(poly(Y), poly([m(C, TD, Vps)]), Result), !.

poly_times(poly(X), m(C, TD, Vps), Result) :-
    poly_times(poly(X), poly([m(C, TD, Vps)]), Result), !.

poly_times(m(C1, TD1, Vps1), m(C2, TD2, Vps2), Result) :-
    poly_times(poly([m(C1, TD1, Vps1)]), 
						poly([m(C2, TD2, Vps2)]), Result), !.

poly_times(poly([H1 | T1]), poly(Poly2), poly(Result)) :-
    poly_times_a(H1, Poly2, Result1), !,
    poly_times(poly(T1), poly(Poly2), Result2),
    poly_plus(Result1, Result2, poly(PolyPlus)), !,
    sort_monos(PolyPlus, SortResult),
    flat(SortResult, FlatResult),
    normalize_poly(poly(FlatResult), poly(NormP)),
    canc_zero(poly(NormP), poly(Result)), !.

poly_times(poly([H1 | T1]), poly(Poly2), poly(Result)) :-
    poly_times_a(H1, Poly2, Result1),
    is_monomial(Result1), !,
    poly_times(poly(T1), poly(Poly2), Result2),
    poly_plus(poly([Result1]), Result2, poly(PolyPlus)), !,
    sort_monos(PolyPlus, SortResult),
    flat(SortResult, FlatResult),
    normalize_poly(poly(FlatResult), poly(Result)),
    !.

poly_times_a(m(C1, _, Vp1), [m(C2, _, Vp2)], NormMono) :-
    var_times(Vp1, Vp2, Vps),
    C is C1 * C2,
    total_degree(Vps, TD),
    normalize_mon(m(C, TD, Vps), NormMono).

poly_times_a(m(C1, _, Vp1), [m(C2, _, Vp2) | T], [NormMono, Z]) :-
    var_times(Vp1, Vp2, Vps),
    C is C1 * C2,
    total_degree(Vps, TD), !,
    normalize_mon(m(C, TD, Vps), NormMono),
    poly_times_a(m(C1, _, Vp1), T, Z).

%le due liste di variabili vuote
var_times([], [], []) :- !.

%una delle due liste di variabili vuota
var_times([], [v(E2, V1)], [v(E2, V1)]) :- !.

var_times([v(E1, V1)], [], [v(E1, V1)]) :- !.

var_times([], X, X) :- !.

var_times(X, [], X) :- !.

%solo una varabile
var_times([v(E1, V)], [v(E2, V)], [v(E, V)]) :-
    E is E1 + E2, !.

var_times([v(E1, V1)], [v(E2, V2)], [v(E1, V1), v(E2, V2)]) :- !.

var_times([v(E1, V1)], [v(E2, V2) | Vps2], [v(E2, V2) | Vps]) :-
    var_times([v(E1, V1)], Vps2, Vps).

var_times([v(E1, V)], [v(E2, V) | Vps2], [v(E, V) | Vps2]) :-
    E is E1 + E2, !.

%più variabili
var_times([v(E1, V) | Vps1], [v(E2, V) | Vps2], [v(E, V) | Vps]) :-
    E is E1 + E2, !,
    var_times(Vps1, Vps2, Vps).

var_times([v(E1, V1) | Vps1], [v(E2, V2) | Vps2], Vps) :-
    var_times([v(E1, V1)], Vps2, Vps3), !,
    var_times(Vps1, [v(E2, V2) | Vps3], Vps).



%total_degree(Vps, TD) è vero quando TD è il grado totale della lista
%di variabili Vps
    
total_degree([], 0).

total_degree([v(E,_)], E).

total_degree([v(E1,_) | Vps], E) :-
    total_degree(Vps, E2), !,
    E is E1 + E2.



% as_monomial(Expression, Monomial) da espressione a monomio

as_monomial(Expression, Monomial) :- 
    as_monomial_a(Expression, UglyMono),
    normalize_mon(UglyMono, Monomial),
    !.

% Necessario per utilizzare la sort ed unire variabili uguali
as_monomial_a(Expression, m(C, TD, VPs)) :-
    as_monomial_b(Expression, m(C, TD, VPs2)),
    sort(2, @=<, VPs2, VPs).

%% Se ho solo un numero negativo. es - 42
as_monomial_b(- SingleVar, m(NegVar, 0, [])) :- 
    integer(SingleVar), 
    !,
    NegVar is SingleVar * -1.

as_monomial_b(-Mono, m(NC, TD, VPs)) :-
    !,
    as_monomial_b(Mono, m(C, TD, VPs)), !,
    NC is -C.

%% Se ho solo un numero es. 42
as_monomial_b(SingleVar, m(SingleVar, 0, [])) :- 
    integer(SingleVar), !.

%% Se ho solo una lettera es. y
as_monomial_b(SingleVar, m(1, 1, [v(1, SingleVar)])) :- 
    atom(SingleVar), !.

%% Se ho solo uno zero
as_monomial_b(SingleVar, m(0, 0, [])) :- 
    is_zero(SingleVar), !.

%% Lettera^esponente es. z^2
as_monomial_b(H ^ T, m(1, T, [v(T, H)])) :-
    T \= 0,
    atom(H),
    integer(T),
    !.

%% Esponente 0
as_monomial_b(H ^ T, m(1, 0, [])) :-
    atom(H), !,
    integer(T).

%% H qualsiasi ed esponente delle variabili mancante (1)
%% TotalDegree + 1
as_monomial_b(Head * Tail, m(C, TD, [v(1, Tail) | VPs])) :-
    atom(Tail),	!,
    as_monomial_b(Head, m(C, TD1, VPs)),
    TD is TD1 + 1.

%% H qualsiasi ed esponente presente
%% TotalDegree sarà il grado del primo + il grado del resto
as_monomial_b(Head * A ^ B, m(C, TD, [v(B, A) | VPs])) :-
    number(B), !,
    atom(A), !,
    as_monomial_b(Head, m(C, TD1, VPs)),
    TD is TD1 + B.



% Normalizzazione monomio -> es. x * x * y -> x^2 * y

% Se monomio zero
normalize_mon(UglyMono, m(0, 0, [])) :- is_zero(UglyMono), !.

% Se ho solo coeff
normalize_mon(m(C, 0, []), m(C, 0, [])) :- !.

% Se ho un monomio con una sola var
normalize_mon(m(C, TD, [v(Exp, Var)]), m(C, TD, [v(Exp, Var)])) :- !.

normalize_mon(m(C, TD, [v(Degree1, Var), v(Degree2, Var) | VPs]), 
												m(C, TD, VPsSorted)) :-
    Z is Degree1 + Degree2,
    normalize_mon(m(C, TD, [v(Z, Var) | VPs]), m(C, TD, VPsReduced)),
    sort(2, @=<, VPsReduced, VPsSorted).

normalize_mon(m(C, TD, [v(Degree1, Var), v(Degree2, DiffVar) | VPs]), 
							m(C, TD, [v(Degree1, Var) | VPsSorted])) :-
    normalize_mon(m(C, TD, [v(Degree2, DiffVar) | VPs]), 
										m(C, TD, VPsReduced)),
    sort(2, @=<, VPsReduced, VPsSorted).



% as_polynomial(Expression, Polynomial) da espressione e polinomio

% Caso base vuoto
as_polynomial(m(0, _, _), poly([])) :- !.

as_polynomial(Espressione, poly(Poly)) :-
    as_polynomial_a(Espressione, UglyPoly),
    flat(UglyPoly, FlatPoly),
    sort_poly(FlatPoly, SortPoly),
    normalize_poly(poly(SortPoly), poly(Poly)), !.

%come ultimo monomio un numero negativo
as_polynomial_a(Poly - N, [MonoParsed, Monomials]) :-
    integer(N), !,
    N1 is N * -1,
    as_monomial(N1, MonoParsed), !,
	as_polynomial_a(Poly, Monomials).

%come ultimo monomio un monomio con coeff negativo
as_polynomial_a(Poly - Mono, [MonoParsed, Monomials]) :-
    as_monomial(-Mono, MonoParsed), !,
	as_polynomial_a(Poly, Monomials).

%come ultimo monomio un semplice monomio
as_polynomial_a(Poly + Mono, [MonoParsed, Monomials]) :-
    as_monomial(Mono, MonoParsed), !,
	as_polynomial_a(Poly, Monomials).

%primo elemento del polinomioun numero negativo
as_polynomial_a(-N, MonoParsed) :-
	integer(N), !,
    N1 is N * -1,
    as_monomial(N1, MonoParsed).

%primo elemento del polinomio monomio normale
as_polynomial_a(Mono, MonoParsed) :-
	as_monomial(Mono, MonoParsed).



% normalize_poly serve per normalizzare il polinomio quindi 
% somma-sottrae i simili
normalize_poly(poly([]), poly([])).

normalize_poly(poly([Mono]), poly([Mono])).

% Nel caso la somma risultasse con coeff 0
normalize_poly(poly([m(C1, TD, VPs), m(C2, TD, VPs) | Tail1]), 
												poly(Tail2)) :-
    Z is C1+C2,
    is_zero(Z),
    normalize_poly(poly(Tail1), poly(Tail2)).

% Se hanno le stesse variabili e total degree allora somma i coeff 
% ed aggiungo il risultato senza portarmi i due monomi
normalize_poly(poly([m(C1, TD, VPs), m(C2, TD, VPs) | Tail1]), 
													poly(Tail2)) :-
    Z is C1+C2,
    normalize_poly(poly([m(Z, TD, VPs) | Tail1]), poly(Tail2)).

normalize_poly(poly([Mono1, Mono2 | Tail1]), poly([Mono1 | Tail2])) :-
    normalize_poly(poly([Mono2 | Tail1]), poly(Tail2)).



% canc_zero serve per eliminare i monomi con coeff 0

canc_zero(poly([]), poly([])) :- !.

% Se 0 allora lo tolgo
canc_zero(poly([m(0, _, _) | Tail]), poly(Tail2)) :-
    canc_zero(poly(Tail), poly(Tail2)).

% Se diverso da 0 allora lo tengo
canc_zero(poly([m(C, TD, VPs) | Tail]),poly([m(C, TD, VPs) | Tail2])) :-
    canc_zero(poly(Tail), poly(Tail2)).



% poly_val(Polynomial, VariableValues, Value) è vero quando Value contiene
% il valore del polinomio Polynomial (che può anche essere un monomio), 
% nel punto n-dimensionale rappresentato dalla lista VariableValues, 
% che contiene un valore per ogni variabile ottenuta 
% con il predicato variables/2.

poly_val(m(C, _, VPs), VariableValues, Value) :-
	variables(m(C, _, VPs), VarList),
    check_lists(VarList, VariableValues), !,
    key_value(VarList, VariableValues, Kv),
    poly_val_a([m(C, _, VPs)], Kv, Value), !.
    
poly_val(poly(Poly), VariableValues, Value) :-
    variables(poly(Poly), VarList),
    check_lists(VarList, VariableValues), !,
    key_value(VarList, VariableValues, Kv),
    poly_val_a(Poly, Kv, Value), !.

poly_val_a([m(C, _, VPs)], Kv, Value) :-
    poly_val_b(VPs, Kv, Val),
    Value is Val * C.

poly_val_a([m(C, _, VPs) | T], Kv, Value) :-
    poly_val_b(VPs, Kv, Val),
    First is Val * C,
    poly_val_a(T, Kv, Rest),
    Value is First + Rest.

%caso base
poly_val_b([], _, 0).

poly_val_b([v(E, V)], [V, Y | _], Val) :-
    Val is Y^E.

poly_val_b([v(E, V)], [_, _ | T], Val) :-
    poly_val_b([v(E, V)], T, Val).

%caso passo
poly_val_b([v(E, V) | T], Kv, Val) :-
    poly_val_b([v(E, V)], Kv, Val1),
    poly_val_b(T, Kv, Val2),
    Val is Val1 * Val2.



%check_lists(VarList, VariableValues) è vero quando la lunghezza delle due
%liste è uguale

check_lists(VarList, VariableValues) :-
    length(VarList, N1),
    length(VariableValues, N2),
    N1 =:= N2.

check_lists(VarList, VariableValues) :-
    length(VarList, N1),
    length(VariableValues, N2),
    N1 =\= N2,
    write("la lista di numeri ha lunghezza 
           diversa rispetto alla lista di variabili").



%key_value(Var, Values, Kv) è vero quando Kv è la lista chiave valore 
%creata associando le variabili di Var con i numeri di Values.
key_value([X], [Y], [X , Y]).

key_value([X | T], [Y | Z], [X , Y | W]) :-
    key_value(T, Z, W).



% sort_poly(Poly, SortedPoly) sorta un polinomio prima in ordine 
% crescente del grado dei monomi con spareggi determinati dalle variabili

sort_poly(Poly, SortedPoly) :-
    predsort(compare_monomials, Poly, SortedPoly).

compare_monomials(<, m(_C1, TD, VPs1), m(_C2, TD, VPs2)) :-
    compare_variables(<, VPs1, VPs2),
    !.
                                                 
compare_monomials(>, m(_C1, TD1, _VPs1), m(_C2, TD2, _VPs2)) :-
    TD1 @> TD2, !.
                                                 
compare_monomials(>, m(_C1, TD, VPs1), m(_C2, TD, VPs2)) :-
    compare_variables(>, VPs1, VPs2),
    !.
                                                 
compare_monomials(<, m(_C1, TD1, _VPs1), m(_C2, TD2, _VPs2)) :-
    TD1 @< TD2, !.

compare_variables(>, [], _) :- !.
                                                 
compare_variables(<, _, []) :- !.
                                                 
compare_variables(<, v(_, Var1), v(_, Var2)) :-
    Var1 @< Var2,
    !.
                                                 
compare_variables(>, v(_, Var1), v(_, Var2)) :-
    Var1 @> Var2,
    !.
                                                 
compare_variables(<, v(Exp1, Var), v(Exp2, Var)) :-
    Exp1 @< Exp2,
    !.
                                                 
compare_variables(>, v(Exp1, Var), v(Exp2, Var)) :-
    Exp1 @> Exp2,
    !.
                                                 
compare_variables(<, [v(Exp, Var) | Vs1] , [v(Exp, Var) | Vs2]) :-
    !,
    compare_variables(<, Vs1, Vs2).
                                                 
compare_variables(<, [v(Exp1, Var) | _] , [v(Exp2, Var) | _]) :-
    Exp1 @< Exp2,
    !.
                                                 
compare_variables(>, [v(Exp, Var) | Vs1] , [v(Exp, Var) | Vs2]) :-
    !,
    compare_variables(>, Vs1, Vs2).
                                                 
compare_variables(>, [v(Exp1, Var) | _] , [v(Exp2, Var) | _]) :-
    Exp1 @> Exp2,
    !.
                                                 
compare_variables(<, [v(_, Var1) | _Vs1] , [v(_, Var2) | _Vs2]) :-
    Var1 @< Var2,
    !.
                                                 
compare_variables(>, [v(_, Var1) | _Vs1] , [v(_, Var2) | _Vs2]) :-
    Var1 @> Var2,
    !.



%sort_monos(Result, SortResult) è vero quando SortResult 
%è il poly con vps sortate

sort_monos([m(C, TD, VPs)], m(C, TD, VPsSorted)) :-
    sort(2, @<, VPs, VPsSorted).

sort_monos(m(C, TD, VPs), m(C, TD, VPsSorted)) :-
    sort(2, @<, VPs, VPsSorted).

sort_monos([m(C, TD, VPs) | T], [m(C, TD, VPsSorted), Z]) :-
    sort(2, @<, VPs, VPsSorted),
    sort_monos(T, Z).



% pprint_polynomial(Polynomial)
% Stampo polinomio in rappresentazione tradizionale ,  
% Si puó omettere il simbolo di moltiplicazione.

% monomio
pprint_polynomial(m(C, TD, VPs)) :-
    pprint_polynomial(poly([m(C, TD, VPs)])), !.

% Polinomio vuoto o zero
pprint_polynomial(Poly):- 
    is_zero(Poly), !,
    write("Polinomio uguale a 0").

% Polinomio composto da solo un coeff
pprint_polynomial([m(C, 0, [])]):- 
    format('~d ', [C]), !.

% Polinomio composto da un monomio senza coeff
pprint_polynomial(poly([m(1, _, VPs)])) :-
    pprint_m(VPs), !.

% Polinomio composto monomio e coeff -> stampo coeff poi tutto il resto
pprint_polynomial(poly([m(C, _, VPs)])) :-
    format('~d ', [C]),
    pprint_m(VPs), !.

pprint_polynomial(poly([H, m(C, _, VP) | T])) :-
    C < 0,
    !,
    pprint_polynomial(poly([H])),
    pprint_polynomial(poly([m(C, _, VP) | T])).

pprint_polynomial(poly([H, m(C, _, VP) | T])) :-
    pprint_polynomial(poly([H])),
    write("+ "),
    pprint_polynomial(poly([m(C, _, VP) | T])).

% Caso passo Polinomio da più monomi
% Se variabile vuota
pprint_m([]):- !.

% Se variabile senza esponente
pprint_m([v(1, Var)]):- 
    format('~a ', [Var]).

% Se variabile ha esponente
pprint_m([v(E, V)]) :-
    E \= 1, !, % Serve perchè se E è 1 non serve stamparlo
    format('~a^~d ', [V, E]).

% Caso passo -> ho più variabili
pprint_m([v(Exp, Var) | VPs]) :-
    pprint_m([v(Exp, Var)]),
    pprint_m(VPs).



%flat(List, FlatList) appiattisce una lista

flat([X|R],Result) :-
    !,
    flat(X,XF),
    flat(R,XR),
    append(XF,XR,Result).

flat([],[]) :- !.
flat(X,[X]).



%%%% end of file -- mvpoly.pl --