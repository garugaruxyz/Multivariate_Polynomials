;;;; -*- Mode: Lisp -*-

;;;; mvpoly.lisp


(defun is-monomial (m)
	(and (listp m)
		 (eq 'm (first m))
		 (let ((mtd (monomial-degree m))
			   (vps (var-powers m))
			   (coeff (monomial-coefficient m)))
			(or (and (integerp mtd)
				 (>= mtd 0)
				 (numberp coeff)
				 (not (eql coeff 0))
				 (listp vps)
				 (every #'is-varpower vps))
				(and (integerp mtd)
				 (>= mtd 0)
				 (numberp coeff)
				 (eql coeff 0)
				 (null vps))))))


(defun is-varpower (vp)
	(and (listp vp)
		 (eq 'v (first vp))
		 (let ((p (second vp))
			   (v (third vp)))
	      (and (integerp p)
			   (symbolp v)))))


(defun is-polynomial (p)
	(and (listp p)
		 (eq 'poly (first p))
		 (let ((ms (monomials p)))
		  (and (listp ms)
			(every #'is-monomial ms)))))


;quando X è una rappresentazione dello 0 ritorna T
(defun is-zero (x)
	(if (or (eql x 0)
		(equal x '(m 0 0 ()))
		(eql (second x) 0))
			t))


; ritorna la lista di varpowers 
(defun var-powers (monomial)
     (fourth monomial))


; ritorna la lista di variabili dato un monomio
(defun vars-of (monomial)
    (let ((variabili (var-powers monomial)))
         (var-list variabili)))

(defun var-list (variables)
    (cond ((null variables)
                nil)
          (t (cons (third (first variables))
                   (var-list (rest variables))))))


; ritorna il grado totale del monomio 
(defun monomial-degree (monomial)
    (third monomial))


; ritorna il coefficiente del monomio
(defun monomial-coefficient (monomial)
    (second monomial))


; ritorna una lista dei coefficienti di Poly.   
(defun coefficients (poly)     
    (cond ((null poly)
            nil)
          ((eql (first poly) 'poly)
            (cons (monomial-coefficient (first (second poly)))
                    (coefficients (rest (second poly)))))
		  ((eql (first poly) 'm)
			(coefficients (list 'poly (list poly))))
           (t (cons (monomial-coefficient (first poly))
                    (coefficients (rest poly))))))


; ritorna una lista dei simboli di variabile di Poly. 
(defun variables (poly)
    (cond ((null poly)
            nil)
          ((eql (first poly) 'poly)
             (stable-sort (remove-duplicates 
				(append (vars-of (first (second poly)))
                   (variables (rest (second poly))))) #'string<=))
		  ((eql (first poly) 'm)
		  	 (vars-of poly))	   
          (t (append (vars-of (first poly))
                   (variables (rest poly))))))


; ritorna la lista ordinata dei monomi di Poly. 
(defun monomials (poly)
	(cond ((null poly)
			nil)
		  ((eql (first poly) 'm)
			(monomials (list poly)))
		  ((eql (first poly) 'poly)
			(sort-poly (second poly)))
		  (t (sort-poly poly))))


; ritorna il massimo grado dei monomi che appaiono in Poly.  
(defun max-degree (poly)
	(if (eql (first poly) 'm)
		(max-degree (list 'poly (list poly)))
    (reduce #'max (first (rest poly)) :key #'third)))
	

; ritorna il minimo grado dei monomi che appaiono in Poly.
(defun min-degree (poly)
	(if (eql (first poly) 'm)
		(min-degree (list 'poly (list poly)))
    (reduce #'min (first (rest poly)) :key #'third)))


; produce il polinomio somma di Poly1 e Poly2.
(defun poly-plus (poly1 poly2)
	(cond ((null poly1)
			poly2)
		  ((eql (first poly1) 'm)
            (let ((sum (poly-sum poly1 (second poly2))))
             (if (eql sum poly1)
                       (list 'poly (monomials (cons sum (second poly2))))
                (cons sum (check-duplicates poly1 poly2)))))
		  ((eql (first poly2) 'm)
            (let ((sum (poly-sum poly2 (second poly1))))
             (if (eql sum poly2)
                       (list 'poly (monomials (cons sum (second poly1))))
                (cons sum (check-duplicates poly2 poly1)))))	
		  ((eql (first poly1) 'poly)
			  (let ((p1 (first (second poly1)))
	    		   (p2 (second poly2))
				   (sum (poly-sum (first (second poly1)) (second poly2))))
		  	 	 (if (eql sum p1)
				     (list 'poly (monomials (double-zero 
						(cons (poly-sum p1 p2) 
							(poly-plus (rest (second poly1)) p2)))))	 
				(list 'poly (monomials (double-zero 
					(cons sum (poly-plus (rest (second poly1)) 
						(check-duplicates p1 p2)))))))))
		 (t (let ((p1 (first poly1))
		   			(sum (poly-sum (first poly1) poly2)))
				 (if (eql sum p1)
		  		 	 (monomials (cons sum (poly-plus (rest poly1) poly2)))
				 (monomials (cons sum (poly-plus (rest poly1) 
							(check-duplicates p1 poly2)))))))))


(defun poly-sum (mono poly)
	(cond
		((null poly)
			mono)
		((equal (var-powers mono) (var-powers (first poly)))
		  (let ((risultato (list 'm (+ (second mono) (second (first poly)))
							(third mono) (var-powers mono))))
			(if (is-zero risultato)
				(list 'm 0 0 '())
			risultato)))
		(t (poly-sum mono (rest poly)))))



; produce il polinomio differenza di Poly1 e Poly2.
(defun poly-minus (poly1 poly2)
	(cond ((eql (first poly2) 'm)
			(poly-plus poly1 (list 'poly (change-sign (list poly2)))))
		  ((eql (first poly2) 'poly)
			(poly-plus poly1 (list 'poly (change-sign (second poly2)))))))

(defun change-sign (monos)
	(if (null monos)
		nil
	(cons (list 'm (* (second (first monos)) -1) 
					(third (first monos))
					(fourth (first monos)))
			(change-sign (rest monos)))))



;ritorna il polinomio risultante dalla moltiplicazione di Poly1 e Poly2.
(defun poly-times (p1 p2)
	(cond ((or (is-zero p1) (is-zero p2))
			(list 'm 0 0 '()))
		  ((null p1)
			nil)
		  ((equal (first p1) 'm)
		   	(if (equal (first p2) 'm)
			   (list 'poly (monomials (poly-mol p1 (list p2))))
		    (list 'poly (monomials (poly-mol p1 (second p2))))))
		  ((equal (first p2) 'm)
		  	 (list 'poly (monomials (poly-mol p2 (second p1)))))			
		  ((equal (first p1) 'poly)
			(let ((m1 (first (second p1))))
				(list 'poly (monomials (poly-plus (poly-mol m1 (second p2)) 
							(poly-times (rest (second p1)) (second p2)))))))
		  ((not (null (rest p1)))
		  	(monomials (poly-plus (poly-mol (first p1) p2) 
							(poly-times (rest p1) p2))))		   
		  (t (monomials (poly-mol (first p1) p2)))))


(defun poly-mol (m p)
	(cond ((or (null p) (null m))
			nil)
		  (t (let ((coeff (* (second m) (second (first p))))
				   (vp (vp-mol (fourth m) (fourth (first p))
						(var-list (fourth (first p))))))
			  (cons (list 'm coeff (tot-degree vp) vp) 
						(poly-mol m (rest p)))))))


(defun vp-mol (m p lista)
	(cond ((null lista)
			m)
		   ((null p)
		  	nil)	
		  ((null m)
			p)
		  ((member (third (first m)) lista)
			(if (equal (third (first m)) (third (first p)))
				(cons (list 'v (+ (second (first m)) (second (first p)))
							(third (first m))) 
					(vp-mol (rest m) (rest p) lista))
			(cons (first p) (vp-mol m (rest p) lista))))
		  (t (cons (first m) (vp-mol (rest m) p lista)))))


(defun tot-degree (vp)
	(if (null vp)
		0
	(+ (second (first vp)) (tot-degree (rest vp)))))



;ritorna la struttura dati (lista) che rappresenta il monomio risultante dal
;“parsing” dell’espressione Expression
(defun as-monomial (expression)
	(cond  
		((atom expression)
			 (if (numberp expression)
		 	 	 (list 'm expression '0 nil)
		 	 (list 'm '1 '1 (list (list 'v '1 expression)))))
		((and (listp expression) (is-operator (first expression)))
			 (if (numberp (second expression))
			 	 (list 'm (second expression) 
						  (get-total-degree (cddr expression)) 
						  (get-vars-and-power (cddr expression)))
			 (list 'm '1 (get-total-degree (cdr expression)) 
						 (get-vars-and-power (cdr expression)))))
		(t (error "Espressione non valida"))))


;ritorna la struttura dati (lista) che rappresenta il polinomio 
; risultante dal “parsing” dell’espressione Expression
(defun as-polynomial (expression)
    (cond 
         ((null expression)
            nil)
         ((eql (first expression) '+)
			(let ((monoList (cons (as-monomial (second expression))
									     (as-polynomial (cddr expression)))))
            (list 'poly (monomials (sum-duplicates monoList monoList)))))
         (t (cons (as-monomial (first expression)) 
				  (as-polynomial (rest expression))))))


;La funzione poly-val restituisce il valore Value del polinomio Polynomial 
;(che può anche essere un monomio), nel punto n-dimensionale rappresentato 
;dalla lista VariableValues, che contiene un valore per ogni variabile 
;ottenuta con la funzione variables.
(defun poly-val (poly vvalues) 
	(if (equal (first poly) 'poly)
		(let ((vals (variables poly))
			  (monoList (second poly)))
			(poly-val-a monoList (create-pairs vals vvalues)))
	(if (equal (first poly) 'm)
		(let ((vals (variables poly))
			  (monoList(list poly)))
			(poly-val-a monoList (create-pairs vals vvalues))))))

(defun poly-val-a (monoList pairs)
	(cond ((null monoList)
			0)
		  (t (+ (* (poly-val-vp (fourth (first monolist)) pairs) 
					(second (first monolist)))
				(poly-val-a (rest monoList) pairs)))))


(defun poly-val-vp (varsNpow pairs)
	(cond ((null varsNpow)
			1)
		  (t (let ((val (find-pair (first varsNpow) pairs)))
				(* (expt val (second (first varsNpow)))
					(poly-val-vp (rest varsNpow) pairs))))))

					
(defun find-pair (varsNpow pairs)
	(if (equal (third varsNpow) (first (first pairs)))
		(second (first pairs))
	(find-pair varsNpow (rest pairs))))

(defun create-pairs (vals vvalues)
	(cond ((null vals)
			nil)
		  ((null vvalues)
		   (error "lista di valori troppo corta"))
		  (t (cons (list (first vals) (first vvalues))
				(create-pairs (rest vals) (rest vvalues))))))

;ritorna NIL dopo aver stampato (sullo “standard output”)
;una rappresentazione tradizionale del termine polinomio
;associato a Polynomial.
(defun pprint-polynomial (expression)
	(if (equal (first expression) 'm)
		(if (null (print-poly (list expression)))
			nil
		(error "Non è possibile stampare"))
	(if (null (print-poly (second expression)))
		nil
	(error "Non è possibile stampare"))))


(defun print-poly (expression)
	(cond  
		 ((null expression)
		 	nil)
		 ((not (null (second expression))) 
		 	(print-mono (monomial-coefficient (first expression)) 
						(var-powers (first expression)))
			(if (> (monomial-coefficient (first (rest expression))) 0)
				(format t "+ "))
		  (print-poly (rest expression)))
		 (t (print-mono (monomial-coefficient (first expression)) 
						(var-powers (first expression))))))



(defun print-mono (coeff vp)
	(cond ((and (null vp) (not (null coeff)))
			(format t "~d " coeff))
		  ((null vp)
			nil)	
		  ((and (null coeff) (eql (second (first vp)) 1))
				(format t "~a " (third (first vp)))
				(print-mono coeff (rest vp)))
		  ((null coeff)
		   	(format t "~a^~d " (third (first vp)) (second (first vp)))
		    (print-mono coeff (rest vp)))
		  ((eql coeff 1)
		  	 (print-mono nil vp))
		  (t (format t "~d " coeff)
		  	 (print-mono nil vp))))
		   		 


;FUNZIONI SUPPORTO

;SORTATO SOLO PER GRADO
(defun sort-poly (mono-list)
	(let ((sortedmono (sort-mono mono-list)))
		(stable-sort (copy-seq sortedmono) #'<= :key #'third)))

(defun sort-mono (mono-list)
	(if (null mono-list)
		nil
	(let ((mono (fourth (first mono-list))))
		(cons (list 'm (second (first mono-list)) (third (first mono-list))
				(stable-sort (copy-seq mono) #'string<= :key #'third))
			(sort-mono (rest mono-list))))))


;somma monomi uguali del polinomio dato
(defun sum-duplicates (poly1 poly2)
	(cond ((null poly2)
			nil)
		  (t (double-zero (cons (sum-duplicates-a (first poly1) (rest poly2))
					(sum-duplicates-a (check-duplicates (first poly1) 
														(rest poly1)) 
						(rest (check-duplicates (first poly1) 
										(rest poly2)))))))))


(defun sum-duplicates-a (mono1 poly2)
	(cond ((null poly2)
			mono1)
		  ((equal (var-powers mono1) (var-powers (first poly2)))
			(list 'm (+ (monomial-coefficient mono1) 
						(monomial-coefficient (first poly2)))
					(monomial-degree mono1) 
					(var-powers mono1)))
		  (t (sum-duplicates-a mono1 (rest poly2)))))

	
;elimina la prima occorrenza di m1 in p2 se la trova
(defun check-duplicates (m1 p2)
	(cond ((null p2)
			nil)		
	      ((equal (var-powers m1) (var-powers (first p2)))
			(rest p2))
	  	  (t (cons (first p2) (check-duplicates m1 (rest p2))))))


;toglie i monomi 0 dal polinomio se ci sono
(defun double-zero (poly)
	(cond ((null poly)
				nil) 
		  ((is-zero (first poly))
				(double-zero (rest poly)))	
		  (t (cons (first poly) (double-zero (rest poly))))))


;calcola il grado totale data un espressione
(defun get-total-degree (expression)
  (cond 
    ((null expression)
     0)
    ((atom (first expression))
		 (+ 1 (get-total-degree (rest expression))))
    ((listp (first expression))
	     (+ (third (first expression)) 
		    (get-total-degree (rest expression))))))


;calcola vars-and-power data un espressione
(defun get-vars-and-power (expression)
  (cond 
        ((null expression)
            nil)
        ((atom (first expression))
            (stable-sort (cons (list 'v '1 (first expression)) 
			      (get-vars-and-power (rest expression)))
				  #'string<= :key #'third))
        ((listp (first expression))
            (stable-sort (cons (list 'v (third (first expression)) 
			                     (second (first expression)))
					(get-vars-and-power (rest expression)))
				#'string<= :key #'third))))


(defun pw-list (var-pws)
	(cond ((null var-pws)
                nil)
          (t (cons (second (first var-pws))
                   (pw-list (rest var-pws))))))


(defun is-operator (expr)
  (if (or (eql expr '*) (eql expr '/) (eql expr '-) (eql expr '+))
      t nil))



;;;; end of file -- mvpoly.lisp --