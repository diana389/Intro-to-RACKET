#lang racket

(provide (all-defined-out))

; TODO 1
; Implementați o funcție care primește lista preferințelor
; bărbaților și calculează lista bărbaților din problemă.
; Folosiți recursivitate pe stivă.
(define (get-men mpref)
  (if (null? mpref) 
      '() ; cazul de baza
      (append (list (car (car mpref))) (get-men (cdr mpref))) ; adauga primul element al fiecarei liste in lista de barbati
  ))


; TODO 2
; Implementați o funcție care primește lista preferințelor
; femeilor și calculează lista femeilor din problemă.
; Folosiți recursivitate pe coadă.
(define (get-women-a wpref women-list)
  (if (null? wpref) 
      women-list ; cazul de baza
      (get-women-a (cdr wpref) (append women-list (list (car (car wpref))))) ; adauga primul element al fiecarei liste in lista de femei
   ))

(define (get-women wpref)
  (get-women-a wpref '())
  )


; TODO 3
; Implementați o funcție recursivă care primește o listă de liste
; de preferințe (ori lista preferințelor bărbaților, ori a
; femeilor) și o persoană ale cărei preferințe apar în listă,
; și întoarce lista preferințelor acelei persoane.
; Se garantează că persoana apare în listă.
; Exemplu: dacă în lista wpref apare linia (ana bobo adi cos),
; (get-pref-list wpref 'ana) => '(bobo adi cos)
; Observație: de fiecare dată când ne referim la lista
; preferințelor unei persoane p, ne referim la o listă care conține
; doar persoanele de sex opus, nu și pe p pe prima poziție.
(define (get-pref-list pref person)
  (if (equal? (car (car pref)) person) ; se verifica daca primul element este lista preferintelor persoanei
      (cdr (car pref)) ; se returneaza lista preferintelor
      (get-pref-list (cdr pref) person); se continua verificarea pe restul listei
  ))


; TODO 4
; Implementați o funcție recursivă care primește o listă de tipul
; întors la exercițiul precedent (lista preferințelor unei persoane),
; respectiv două persoane x și y care apar în lista respectivă,
; și întoarce true dacă x este mai sus decât y în topul preferințelor
; și false în caz contrar.
; Nu folosiți operatori condiționali, folosiți în schimb operatori
; logici pentru a obține același efect.
(define (preferable? pref-list x y)
  (or
   (equal? (car pref-list) x) ;; daca primul este x => #t
   (and
    (not (equal? (car pref-list) y)) ;; daca primul este y => #f
    (preferable? (cdr pref-list) x y)) ;; daca primul nu este nici x, nici y => continuam pe restul listei
  ))


; TODO 5
; Implementați o funcție recursivă care primește o listă de logodne
; (în care fiecare logodnă este o pereche cu punct între parteneri)
; și o persoană, și, în cazul în care această persoană apare pe prima
; poziție într-o logodnă, este întors partenerul său, altfel se
; întoarce false.
; Folosiți cond.
(define (get-partner engagements person)
  (cond
    ((null? engagements) #f)
    ((equal? (car (car engagements)) person) (cdr (car engagements)))
    (else (get-partner (cdr engagements) person))
  ))


; TODO 6
; Implementați o funcție care primește 2 persoane logodite, p1 și p2,
; lista preferințelor lui p1, lista preferințelor tuturor persoanelor
; de același gen cu p2, respectiv lista tuturor logodnelor, și întoarce
; true dacă există un partener mai potrivit pentru p1, și false altfel.
; Precizări (aspecte care se garantează, nu trebuie verificate):
; - lista logodnelor este completă, este un posibil rezultat al problemei
; - logodnele din listă au pe prima poziție persoana de același gen cu p2
; - un partener p' este mai potrivit decât p2 dacă îndeplinește 2 condiții:
;   - p1 îl preferă pe p' în raport cu p2
;   - p' îl preferă pe p1 în raport cu persoana cu care este logodit
(define (better-match-exists? p1 p2 p1-list pref2 engagements)
  (if (null? pref2)
      #f ; cazul de baza
      (if (equal? (car (get-men pref2)) p2) ; verificam daca prima persoana din lista 
                                        ; persoanelor de același gen cu p2 este p2
          (better-match-exists? p1 p2 p1-list (cdr pref2) engagements) ; trecem la urmatorul element
          (if(and
              (preferable? ; verificam daca p1 îl preferă pe p' în raport cu p2
               p1-list ; lista preferintelor lui p1
               (car (get-men pref2)) ; p'
               p2) ; p2
              (preferable? ; verificam daca p' îl preferă pe p1 în raport cu persoana cu care este logodit
               (get-pref-list pref2 (car (get-men pref2))) ; lista preferintelor lui p'
               p1 ; p1
               (get-partner engagements  (car (get-men pref2))))) ; persoana cu care este logodit p'
             #t
             (better-match-exists? p1 p2 p1-list (cdr pref2) engagements) ; trecem la urmatorul element
  ))))