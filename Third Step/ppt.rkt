#lang racket

(provide (all-defined-out))

;; Același arbore de TPP obținut în etapa 1 prin aplicarea
;; transformărilor T1, T2, T3 poate fi generat folosind 
;; tupluri GH (Gopal-Hemachandra).
;;
;; Pentru o pereche oarecare (g, e), secvența GH este:
;;    g, e, g + e, g + 2e, 2g + 3e, 3g + 5e ...
;; Pentru (g, e) = (1, 1) obținem șirul lui Fibonacci.
;;
;; Primele 4 numere din secvență formează cvartetul GH:
;;    (g, e, f, h) = (g, e, g + e, g + 2e)
;;
;; Pentru un asemenea cvartet (g, e, f, h), definim:
;;    a = gh,   b = 2ef,   c = e^2 + f^2
;; și putem demonstra că (a,b,c) este un triplet pitagoreic.
;;
;; (a,b,c) este chiar TPP, dacă adăugăm condițiile:
;;    g, e, f, h prime între ele
;;    g impar
;; însă nu veți avea nevoie să faceți asemenea verificări,
;; întrucât avem la dispoziție un algoritm care generează
;; exclusiv TPP.
;;
;; Acest algoritm este foarte asemănător cu cel din etapa
;; anterioară, cu următoarele diferențe:
;;  - nodurile din arbore sunt cvartete, nu triplete
;;    (din cvartet obținem un TPP conform formulelor)
;;    (ex: (1,1,2,3) => (1*3,2*1*2,1^2+2^2) = (3,4,5))
;;  - obținem următoarea generație de cvartete folosind 
;;    trei transformări Q1, Q2, Q3 pentru cvartete, în loc
;;    de T1, T2, T3 care lucrau cu triplete
;; 
;; Q1(g,e,f,h) = (h,e,h+e,h+2e)
;; Q2(g,e,f,h) = (h,f,h+f,h+2f) 
;; Q3(g,e,f,h) = (g,f,g+f,g+2f)
;;
;; Arborele rezultat arată astfel:
;;
;;                        (1,1,2,3)
;;              ______________|______________
;;             |              |              |
;;         (3,1,4,5)      (3,2,5,7)      (1,2,3,5)
;;       ______|______  ______|______  ______|______
;;      |      |      ||      |      ||      |      |
;;  (5,1,6,7) .........................................

;; Definim funcțiile Q1, Q2, Q3:
(define (Q1 g e f h) (list h e (+ h e) (+ h e e)))
(define (Q2 g e f h) (list h f (+ h f) (+ h f f)))
(define (Q3 g e f h) (list g f (+ g f) (+ g f f)))

;; Vom refolosi matricile T1, T2, T3:
(define T1 '((-1 2 2) (-2 1 2) (-2 2 3)))
(define T2 '( (1 2 2)  (2 1 2)  (2 2 3)))
(define T3 '((1 -2 2) (2 -1 2) (2 -2 3)))


; TODO
; Reimplementați funcția care calculează produsul scalar
; a doi vectori X și Y, astfel încât să nu folosiți
; recursivitate explicită (ci funcționale).
; Memento:
; Se garantează că X și Y au aceeași lungime.
; Ex: (-1,2,2)·(3,4,5) = -3 + 8 + 10 = 15
(define (dot-product X Y)
  (apply + (map * X Y)))


; TODO
; Reimplementați funcția care calculează produsul dintre
; o matrice M și un vector V, astfel încât să nu folosiți
; recursivitate explicită (ci funcționale).
; Memento:
; Se garantează că M și V au dimensiuni compatibile.
; Ex: |-1 2 2| |3|   |15|
;     |-2 1 2|·|4| = | 8|
;     |-2 2 3| |5|   |17|
(define (multiply M V)
   (map ( λ(x) (dot-product x V)) M)
  )
;; (((curry map) ( λ(x) (dot-product x V))) M))
;; Rezolvare cu foldl
;;  (foldl (λ (e rez) (append rez (list (dot-product e V))))
;;         '()
;;         M))


; TODO
; Aduceți aici (nu sunt necesare modificări) implementarea
; funcției get-transformations de la etapa 1.
; Această funcție nu este re-punctată de checker, însă este
; necesară implementărilor ulterioare.
(define (get-transformations n)
  (if (zero? (level n 0 0))
      (get-path n (level n 0 0) 0 1 '())
      (get-path n (level n 0 0) (get-bound (sub1 (level n 0 0)) 0) (get-bound (level n 0 0) 0) '())))

(define (get-path n i min max rez)
  (cond ((zero? (level n 0 0)) rez)
        ((< (- max min) 3) (reverse rez))
        ((cond ((<= n (+ (expt 3 (sub1 i)) min))
                (get-path n (sub1 i) min (+ (expt 3 (sub1 i)) min) (cons 1 rez)));; T1
               ((<= n (+ (+ (expt 3 (sub1 i)) min) (expt 3 (sub1 i))))
                (get-path n (sub1 i) (+ (expt 3 (sub1 i)) min) (+ (+ (expt 3 (sub1 i)) min) (expt 3 (sub1 i))) (cons 2 rez))) ;;T2
               (else (get-path n (sub1 i) (+ (+ (expt 3 (sub1 i)) min) (expt 3 (sub1 i))) max (cons 3 rez))))))) ;;T3
      
(define (level n i rez)
  (if (>= rez n) (sub1 i)
      ( level  n (add1 i) (+ (expt 3 i) rez))))

(define (get-bound i rez)
  (if (zero? i) (add1 rez)
      (get-bound (sub1 i) (+ (expt 3 i) rez)))) 



; TODO
; În etapa anterioară ați implementat o funcție care primea
; o listă Ts de tipul celei întoarsă de get-transformations
; și un triplet de start ppt și întorcea tripletul rezultat
; în urma aplicării transformărilor din Ts asupra ppt.
; Acum dorim să generalizăm acest proces, astfel încât să
; putem reutiliza funcția atât pentru transformările de tip
; T1, T2, T3, cât și pentru cele de tip Q1, Q2, Q3.
; În acest scop operăm următoarele modificări:
;  - primul parametru este o listă de funcții Fs
;    (în loc de o listă numerică Ts)
;  - al doilea parametru reprezintă un tuplu oarecare
;    (aici modificarea este doar "cu numele", fără a schimba
;    funcționalitatea, este responsabilitatea funcțiilor din
;    Fs să primească parametri de tipul lui tuple)
; Nu folosiți recursivitate explicită (ci funcționale).
(define (apply-functional-transformations Fs tuple)
  (foldl (λ(e rez)
           (e rez)
           )
         tuple
         Fs))


; TODO
; Tot în spiritul abstractizării, veți defini o nouă funcție
; get-nth-tuple, care calculează al n-lea tuplu din arbore. 
; Această funcție va putea fi folosită:
;  - și pentru arborele de triplete (caz în care plecăm de la
;    (3,4,5) și avansăm via T1, T2, T3)
;  - și pentru arborele de cvartete (caz în care plecăm de la
;    (1,1,2,3) și avansăm via Q1, Q2, Q3)
; Rezultă că, în afară de parametrul n, funcția va trebui să
; primească un tuplu de start și 3 funcții de transformare a
; tuplurilor.
; Definiți get-nth-tuple astfel încât să o puteți reutiliza
; cu minim de efort pentru a defini funcțiile următoare:
;    get-nth-ppt-from-matrix-transformations
;    get-nth-quadruple
; (Hint: funcții curry)
; În define-ul de mai jos nu am precizat parametrii funcției
; get-nth-tuple pentru ca voi înșivă să decideți care este
; modul optim în care funcția să își primească parametrii.
; Din acest motiv checker-ul nu testează separat această funcție,
; dar asistentul va observa dacă implementarea respectă cerința.

(define (transform-path L Flist F1 F2 F3)
  (if (null? L) Flist
  (cond ((equal? (car L) 1) (transform-path (cdr L) (append Flist (list F1)) F1 F2 F3))
        ((equal? (car L) 2) (transform-path (cdr L) (append Flist (list F2)) F1 F2 F3))
        ( else (transform-path (cdr L) (append Flist (list F3)) F1 F2 F3)))))

;;(transform-path '(1 2 3 1 3) '() reverse car cdr)
(define (get-nth-tuple n tuple F1 F2 F3)
 (apply-functional-transformations (transform-path (get-transformations n) '() F1 F2 F3) tuple))


; TODO
; Din get-nth-tuple, obțineți în cel mai succint mod posibil
; (hint: aplicare parțială) o funcție care calculează al n-lea
; TPP din arbore, folosind transformările pe triplete.

(define get-nth-ppt-from-matrix-transformations
  (λ(n) (get-nth-tuple n '(3 4 5) FT1 FT2 FT3)))

(define FT1
  ((curry multiply) T1))

(define FT2
  ((curry multiply) T2))

(define FT3
  ((curry multiply T3)))

; TODO
; Din get-nth-tuple, obțineți în cel mai succint mod posibil 
; (hint: aplicare parțială) o funcție care calculează al n-lea 
; cvartet din arbore, folosind transformările pe cvartete.
(define FQ1
  ((curry apply) Q1))

(define FQ2
  ((curry apply) Q2))

(define FQ3
  ((curry apply) Q3))

(define get-nth-quadruple 
  (λ(n) (get-nth-tuple n '(1 1 2 3) FQ1 FQ2 FQ3)))


; TODO
; Folosiți rezultatul întors de get-nth-quadruple pentru a 
; obține al n-lea TPP din arbore.
(define get-nth-ppt-from-GH-quadruples
  (λ(n) (list (*(first (get-nth-quadruple n)) (last (get-nth-quadruple n))) (* (cadr (get-nth-quadruple n)) (caddr (get-nth-quadruple n)) 2)
        (+ (expt (cadr (get-nth-quadruple n)) 2) (expt (caddr (get-nth-quadruple n)) 2)))))
