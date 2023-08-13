#lang racket

(provide (all-defined-out))

; ATENȚIE - Veți avea de reimplementat mai multe funcții
; din etapa 1, însă cu un alt mod de rezolvare (folosind
; funcționale sau alte funcții solicitate în enunț).
; Enunțul acestor exerciții va debuta prin "Ca în etapa 1,
; dar este interzisă recursivitatea explicită".


; TODO 1
; Ca în etapa 1, dar este interzisă recursivitatea explicită:
; Implementați o funcție care primește lista preferințelor
; bărbaților și calculează lista bărbaților din problemă.
; Folosiți orice funcțională exceptând foldl/foldr.
(define (get-men mpref)
  (map car mpref) ; pentru fiecare lista se pastreaza primul element
  )


; TODO 2
; Ca în etapa 1, dar este interzisă recursivitatea explicită:
; Implementați o funcție care primește lista preferințelor
; femeilor și calculează lista femeilor din problemă.
; Folosiți foldl sau foldr, astfel încât să nu fie necesare
; operații de tip append sau reverse.
(define (get-women wpref)
  (foldr (lambda (x acc) ; functie care lipeste primul element la acumulator
           (cons (car x) acc))
         '() wpref)
  )


; TODO 3
; Ca în etapa 1, dar este interzisă recursivitatea explicită:
; Implementați o funcție care primește o listă de liste
; de preferințe (ori lista preferințelor bărbaților, ori a
; femeilor) și o persoană ale cărei preferințe apar în listă,
; și întoarce lista preferințelor acelei persoane.
; Se garantează că persoana apare în listă.
; Exemplu: dacă în lista wpref apare linia (ana bobo adi cos),
; (get-pref-list wpref 'ana) => '(bobo adi cos)
; Folosiți minim o funcțională și minim o funcție anonimă.
(define (get-pref-list pref person)
    (cdar ; returnez restul listei
         (filter (lambda (x)
             (equal? (car x) person)) ; caut ca prima persoana sa fie person
         pref)
  ))


; TODO 4
; Ca în etapa 1, dar este interzisă recursivitatea explicită
; și sunt permiși operatorii condiționali:
; Implementați o funcție care primește o listă de tipul
; întors la exercițiul precedent (lista preferințelor unei persoane),
; respectiv două persoane x și y care apar în lista respectivă,
; și întoarce true dacă x este mai sus decât y în topul preferințelor
; și false în caz contrar.
; Folosiți funcția member.
(define (preferable? pref-list x y)
  (if (list? (member x pref-list)) ; conditie ca x sa se gaseasca in pref-list
      (if (list? (member y (member x pref-list))) ; conditie ca y sa se gaseasca in lista de la x incolo
          #t
          #f
          )
      #f
   ))

; TODO 5
; Implementați recursiv funcționala find-first, care primește
; un predicat și o listă și întoarce primul element al listei
; care satisface predicatul, sau false dacă un asemenea element
; nu există.
; Implementarea trebuie să fie eficientă în sensul că nu trebuie
; să continue explorarea listei odată ce s-a găsit elementul.
(define (find-first p L)
  (if (null? L)
      #f ; caz de baza
      (if (p (car L)) ; primul element satisface preidcatul
          (car L) ; se returneaza primul element
          (find-first p (cdr L)) ; se continua cautarea
  )))


; TODO 6
; Ca în etapa 1, dar este interzisă recursivitatea explicită:
; Implementați o funcție care primește o listă de logodne
; (în care fiecare logodnă este o pereche cu punct între parteneri)
; și o persoană, și, în cazul în care această persoană apare pe prima
; poziție într-o logodnă, este întors partenerul său, altfel se
; întoarce false.
; Folosiți find-first, fără să îl apelați de 2 ori (hint: define în define).
(define (get-partner engagements person)
  (define f (lambda (pair) (equal? (car pair) person))) ; verifica daca perechea are pe prima pozitie person
  (define var (find-first f engagements)) ; var contine prima (si singura) pereche care contine person
  (if (false? var)
      #f
      (cdr var)
   ))
  

; TODO 7
; Implementați recursiv funcționala change-first care primește
; un predicat p, o listă L și o valoare val, și întoarce o nouă 
; listă în care primul element din L care satisface predicatul p
; a fost înlocuit cu valoarea val, celelalte rămânând la fel.
; Dacă niciun element din L nu satisface predicatul, lista L
; rămâne neschimbată.
(define (change-first p L val)
  (if (null? L) ; se verifica daca s-a ajuns la sfarsitul listei
      null
      (if (p (car L)) ; se verifica daca primul element satisface predicatul
          (append (list val) (cdr L)) ; se inlocuieste valoarea
          (append (list (car L)) (change-first p (cdr L) val)) ; se pastreaza elementele anterioare si se continua cautarea
  )))


; TODO 8
; Implementați funcția update-engagements care primește o listă de
; logodne engagements și două persoane p1 și p2, și întoarce lista
; actualizată de logodne, în care vechiul partener al lui p1 este
; înlocuit cu p2.
; Precizări (aspecte care se garantează, nu trebuie verificate):
; - p1 era logodită în prealabil
; - fiecare cuplu din lista engagements are pe prima poziție
;   persoanele de același gen cu p1
; Folosiți change-first.
(define (update-engagements engagements p1 p2)
  (change-first
   (lambda (pair) (equal? (car pair) p1)) ; predicatul verifica daca perechea contine pe prima pozitie p1
   engagements (cons p1 p2)) ; se inlocuieste perechea cu noua pereche
  )


; TODO
; Copiați implementarea funcției better-match-exists? din etapa 1.
; Funcția nu este repunctată de checker, dar este necesară pentru
; implementarea funcției stable-match? de mai jos.
; Dacă nu ați implementat better-match-exists? în etapa 1, solicitați 
; o rezolvare de la asistent, astfel încât să puteți continua.
(define (get-partner2 engagements person)
  (define f (lambda (pair) (equal? (cdr pair) person)))
  (if (find-first f engagements)
      (car (find-first f engagements))
      #f
   ))

 ; in checker lista de perechi incepe cu p1, deci am folosit o functie de returnare a partenerului modificata

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
               (get-partner2 engagements  (car (get-men pref2))))) ; persoana cu care este logodit p'
             #t
             (better-match-exists? p1 p2 p1-list (cdr pref2) engagements) ; trecem la urmatorul element
  ))))

; TODO 9
; Implementați funcția stable-match? care primește o listă 
; completă de logodne engagements, o listă de preferințe masculine 
; mpref și o listă de preferințe feminine wpref, și întoarce true 
; dacă toate cuplurile din engagements sunt stabile.
; Un cuplu este stabil dacă pentru niciunul din membrii cuplului
; nu există un alt partener mai potrivit (conform definiției de
; la funcția better-match-exists?).
; Precizări (aspecte care se garantează, nu trebuie verificate):
; - fiecare cuplu din lista engagements are pe prima poziție
;   o femeie
(define (stable-match? engagements mpref wpref)
  (if (false?
       (find-first (lambda (x) ; x -> (p1 . p2)
                     (better-match-exists?
                      (car x) ; p1
                      (get-partner engagements (car x)) ; p2
                      (get-pref-list wpref (car x)) ; p1-list
                      mpref ; pref2
                      engagements)) ; engagements
                   engagements))
      #t ; nu s-a gasit better match
      #f ; s-a gasit better match
  ))
