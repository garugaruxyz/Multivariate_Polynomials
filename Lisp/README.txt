;;;; mvpoly.lisp

Una delle prime e più importanti applicazioni dei calcolatori fu la manipolazione simbolica di operazioni
matematiche. In particolare, i sistemi noti come Computer Algebra Systems (cfr., Mathematica, Maple,
Maxima, Axiom, etc.) forniscono funzionalità per la manipolazione di polinomi multivariati.
Lo scopo di questo progetto è la costruzione di due librerie (in Prolog ed in Common Lisp) per la
manipolazione – per l’appunto – di polinomi multivariati.

Funzioni Implementate:

-is-zero: Data un'espressione, se questa risulta essere una rappresentazione dello zero, ritorna true.

-vars-powers: Dato un monomio, o un polinomio, ritorna la lista di varsNPower, ovvero una lista strutturata in questo modo: (V grado della variabile variabile).

-vars-of: Dato un monomio, o un polinomio, ritorna la lista delle variabili contenuti all'interno di quest ultimo.
Tale lista si ottiene tramite l'ausilio della funzione vars-powers e della funzione var-list.

-monomial-degree: Dato un monomio, ritorna il grado totale del monomio, ovvero il terzo elemento del primo elemento della lista. es: ((M 1 7).... in questo caso ritornerebbe 7.

-monomial-coefficients: Dato un monomio, ritorna il suo coefficiente, ovvero, il secondo elemento del primo elemento della lista. es: ((M 1 7).... in questo caso il valore 1.

-coefficients: Dato un polinomio, ritorna la lista dei coefficienti presenti al suoi interno, usando la funzione monomial coefficient e successivamente chiamando ricorsivamente 
coefficients sul resto della lista. 

-variables: Dato un polinomio, ritorna la lista delle variabili presenti al suo interno. Tale lista si ottiene tramite l'utilizzo della funzione vars-sort sul primo elemento, la chiamata
ricorsiva variables sul resto del polinomio e l'append di queste due liste così ottenute. Successivamente tale lista verrà ordinata, tramite la funzione sort, e verranno rimosse variabili
doppie tramite la funzione remove-duplicates.

-monomials: Dato un polinomio, ritorna la lista ordinata dei monomi che lo compongono. Tale ordinamento viene computato dalla funzione di supporto sort-poly. sort-poly ordina la lista di
monomi, in ordine lessiografico. Utilizziamo la funzione stable-sort, su una copia della lista contenente tutti i monomi, utilizzando come chiave il terzo elemento del monomio, ovvero, la 
variabile.

-max-degree: Dato un polinomio, ritorna il grado massimo di quest'ultimo.

-min-degree: Dato un polinomio, ritorna il grado minimo di quest'ultimo.

-poly-plus: Operazione che computa la somma tra due polinomi. La funzione controlla, tramite funzione ausiliaria poly-sum che i varsnpowers dei due operandi siano uguali (e che quindi abbiano grado e variabile uguale).
Se così fosse somma i coefficienti dei due, e chiama ricorsivamente la funzione sul resto della lista. Se così non fosse, appende dale monomio al resto della lista e chiama ricorsivamente la funzione sul resto.

-poly-minus: Operazione che ha funzionamento analogo alla poly-plus. L'unica differenza è l'operazione computata tra i due operandi. In questo caso, differenza.

-poly-times: Operazione che computa la moltiplicazione tra due polinomi, tramite l'utilizzo delle funzioni ausiliarie poly-mol e vp-mol. Tramite la prima calcolo il valore del coefficiente risultante, tramite moltiplicazione.
La seconda invece, fa un controllo sul vars n power dei monomi. Nel caso in cui le variabili di questi ultimi fossero uguali, opererebbe sul vars and power risultate, aumentando il grado delle variabili uguali, sommandole.

-as-monomial: Ritorna il monomio sotto forma di lista risultante dal parsing dell’espressione Expression, e ordinato secondo le specifiche. Nel caso in cui il monomio sia uguale a zero, viene ritornato il seguente monomio: (M 0 0 NIL).
Nel caso in cui fosse diverso da zero, la struttura sarebbe la seguente. (M Coefficiente GradoTotale VarsNPower).

-as-polynomial: Ritorna la struttura dati (lista) che rappresenta il monomio risultante dal parsing dell’espressione Expression, e ordinato secondo le specifiche. Dato che un polinomio è una concatenazione di più monomi, tale funzione
utilizzerà la funziuone as-monomial per calcolare i monomi componenti il monomio. In seguito, tramite operazione cons, creeremo la lista risultante, contenente il polinomio finale.

-poly-val: Dato un polinomio, e una lista di valori, sostituisce tali valori, alle variabili presenti all'interno del polinomio. Una volta sostituiti i valori, li moltiplica e ritorna il risultato ottenuto.

-pprint-polynomial: Stampa un polinomio secondo la rappresentazione tradizionale, e ritorna NIL.


Funzioni di supporto:

-sort mono: ordina la lista di monomi, per grado, controllando il valore all'interno di vars and power.

-check-duplicatest: controlla ed elimina la prima occorrenza di m1 in p2, se presente. 

-double-zero: controlla se all'interno del polinomio siano presente degli zeri. In tal caso, li elimina.

-get-total-degree: data un espressione, ritorna il grado totale di quest'ultima sommando ad uno ad uno i gradi presenti all'interno.

-get-vars-and-power: calcola, dato un'espressione, il var and power risultante.

-pw-list: crea una lista dei gradi (potenze) dei monomi, estrando il secondo elemento del vars and power.

-is-operator: controlla che l'operatore sia uno tra somma, differenza, moltiplicazione o divisione. Ritorna nil altrimenti.

-inverti-coeff: inverte il coefficiente in caso di operazione con cambio di segno, nel caso in cui gli operandi non vengano sommati.


