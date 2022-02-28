%%%% mvpoly.pl

Una delle prime e più importanti applicazioni dei calcolatori fu la manipolazione simbolica di operazioni
matematiche. In particolare, i sistemi noti come Computer Algebra Systems (cfr., Mathematica, Maple,
Maxima, Axiom, etc.) forniscono funzionalità per la manipolazione di polinomi multivariati.
Lo scopo di questo progetto è la costruzione di due librerie (in Prolog ed in Common Lisp) per la
manipolazione – per l’appunto – di polinomi multivariati.

Elenco dei predicati utilizzati:

-is_zero : predicato che risulta vero quando X è una rappresentazione dello 0, o lo stesso zero.

-coefficients: predicato che dato un polinomio, restituisce la lista dei coefficienti presenti al suo interno. I coefficienti si ricavano prendendo il primo elemento del monomio.

-variables: predicato che dato un polinomio, restituisce la lista delle variabili presenti al suo interno. Le variabili si ricavano prendendo il terzo elemento del vars and power. All'interno di questo predicato,
utilizziamo un predicato di supporto, chiamato vp_list, che, restituisce una lista contenenti tutti i vars and power del polinomio.

-monomials: predicato che dato un polinomio, restituisce la lista dei monomi, ordinati, presenti all'interno di quest'ultimo. Per l'ordinamento facciamo affidamento al predicato sort_poly. Tale funzione, sorta un polinomio
prima in ordine crescente dei gradi dei monomi, con spareggi determinati dalle variabili. Tali spareggi vengono calcolati tramite il predicato compare_monomials.

- max_degree: predicato che dato un polinomio, restituisce il grado massimo di quest'ultimo, tramite l'utilizzo di una lista ottenuta tramite predicato degree_list, e un predicato max_member, che compara i valori contenuti all'interno
della Degree_list restituendone il maggiore.

-min_degree: predicato analogo alla max_degree. In questo caro verrà restituito il grado minimo.

-poly_plus: predicato che somma due polinomi. Nel caso ricorsivo, utilizziamo predicati di supporto: poly_plus_a, normalize_poly, che effettua attivamente la somma tra i coefficienti dei polinomi, nel caso in cui i vars and power siano uguali, e canc_zero
che serve ad eliminare i monomi con coefficiente 0.

-poli_minus: predicato che esegue la sottrazione tra due polinomi. In tale predicato utilizziamo il poly_plus, con in aggiunta il predicato change_coeff che serve a ritornare il monomio con segno opposto in caso di sottrazione senza termini simili.

-poly_times: predicato che esegue la moltiplicazione tra due polinomi. Dopo che viene computata l'operazione, la lista risultante viene ordinata, tramite sort_monos, appiattita, tramite flat, normalizzata, e vengono cancellati gli zeri.

-as_monomial: predicato che rappresenta il monomio risultante dal parsing dell’espressione Expression. Tale monomio verrà ordinato secondo le regole definite. 

-as_polynomial: predicato che rappresenta il polinomio risultante dal parsing dell’espressione Expression. Tale monomio verrà ordinato secondo le regole definite.

-poly-val: predicato a cui viene passato un polinomio e una lista di valori, sostituisce tali valori, alle variabili presenti all'interno del polinomio. Una volta sostituiti i valori, li moltiplica e ritorna il risultato ottenuto.
Viene chiamato il predicato variables, che restituisce la lista delle variabili del polinomio, viene chiamato il predicato check_list, che controlla che la lunghezza delle due liste sia uguale, key_value, che crea una lista contenente la coppia
chiave-valore associanto le variabili di var, con i numeri presenti della lista di valori passati in input, e poly_val_a che effettua l'operazione desiderata.

-pprint_polynomial: predicato che stampa il polinomio secondo la rappresentazione tradizionale. Verrà omesso il simbolo di moltiplicazione.


