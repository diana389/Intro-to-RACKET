#lang racket

(require "etapa2.rkt")

(provide (all-defined-out))

; TODO 1
; După modelul funcției stable-match?, implementați funcția
; get-unstable-couples care primește o listă de logodne
; engagements, o listă de preferințe masculine mpref și o 
; listă de preferințe feminine wpref, și întoarce lista
; tuturor cuplurilor instabile din engagements.
; Precizări (aspecte care se garantează, nu trebuie verificate):
; - fiecare cuplu din lista engagements are pe prima poziție
;   o femeie
; Nu este permisă recursivitatea pe stivă.
; Nu sunt permise alte funcții ajutătoare decât
; better-match-exists? și funcțiile de manipulare a listelor de
; preferințe definite în etapele anterioare.
; Nu este permisă apelarea multiplă a aceleiași funcții pe
; aceleași argumente.
; Folosiți una sau mai multe dintre expresiile let, let*, letrec,
; named let pentru a vă putea conforma acestor restricții.

(define (get-unstable-couples engagements mpref wpref)
  (filter ; se formeaza o lista cu perechile instabile
   (lambda (x) ; x -> (p1 . p2)
      (let ((p1 (car x)) (p2 (cdr x))) ; prima persoana = p1; a doua persoana = p2
        (if (or
            (better-match-exists? ; se verifica daca exita "better match" pentru p1 
            p1 
            p2 
            (get-pref-list wpref p1) ; p1-list
            mpref ; pref2
            engagements)
            (better-match-exists? ; se verifica daca exita "better match" pentru p2
            p2 
            p1 
            (get-pref-list mpref p2) ; p2-list
            wpref ; pref1
            (map (lambda (pair) ; se inverseaza persoanele in perechi, deoarece functia verifica doar pentru prima persoana din pereche
                 (let ((a (car pair)) (b (cdr pair)))
                    (cons b a)))
                 engagements)))
         #t ; daca pentru cel putin una dintre persoane exista un "better match", perechea este instabila 
         #f)))
    engagements
  ))


; TODO 2
; Implementați funcția engage care primește o listă free-men
; de bărbați încă nelogodiți, o listă de logodne parțiale 
; engagements (unde fiecare cuplu are pe prima poziție o femeie),
; o listă de preferințe masculine mpref și o listă de preferințe 
; feminine wpref, și întoarce o listă completă de logodne stabile,
; obținută conform algoritmului Gale-Shapley:
; - cât timp există un bărbat m încă nelogodit
;   - w = prima femeie din preferințele lui m pe care m nu a cerut-o încă
;   - dacă w este nelogodită, adaugă perechea (w, m) la engagements
;   - dacă w este logodită cu m'
;     - dacă w îl preferă pe m lui m'
;       - m' devine liber
;       - actualizează partenerul lui w la m în engagements
;     - altfel, repetă procesul cu următoarea femeie din lista lui m
; Folosiți named let pentru orice proces recursiv ajutător (deci nu
; veți defini funcții ajutătoare recursive).
; Folosiți let și/sau let* pentru a evita calcule duplicate.

(define (engage free-men engagements mpref wpref)
  (let next-man ((free-men free-men) (engagements engagements))
     (if (null? free-men)
         engagements
         (let* ((man (car free-men))) ; man = barbatul curent
           (let next-woman ((pref (get-pref-list mpref man))) ; pref = lista preferintelor barbatului
             (let* ((woman (car pref)) (partner (get-partner engagements woman))) ; woman = femeia verificata in prezent; partner = partenerul actual al femeii (sau #f)
               (if (false? partner) ; se verifica daca femeia nu este logodita
                 (next-man (cdr free-men) (cons (cons woman man) engagements)) ; perechea se adauga in lista si se trece la urmatorul barbat
                 (if (preferable? (get-pref-list wpref woman) man partner) ; in cazul in care este logodita, se verifica daca il prefera pe barbat in raport cu partenerul actual
                    (next-man (append (cdr free-men) (list partner)) (update-engagements engagements woman man)) ; partenerul se muta in "free-men" si se actualizeaza lista "engagements"
                    (next-woman (cdr pref)))))))))) ; se trece la urmatoarea femeie din lista preferintelor


; TODO 3
; Implementați funcția gale-shapley care este un wrapper pentru
; algoritmul implementat de funcția engage. Funcția gale-shapley
; primește doar o listă de preferințe masculine mpref și o listă
; de preferințe feminine wpref și calculează o listă completă de
; logodne stabile conform acestor preferințe.

(define (gale-shapley mpref wpref)
  (engage (get-men mpref) '() mpref wpref)) ; se apeleaza functia "engage" pe o lista vida


; TODO 4
; Implementați funcția get-couple-members care primește o listă
; de perechi cu punct și întoarce o listă simplă cu toate elementele 
; care apar în perechi.
; Folosiți funcționale, fără recursivitate explicită.
(define (get-couple-members pair-list)
  (foldr (lambda (pair acc) ; functia adauga persoanele din pereche in acumulator
           (cons (car pair) (cons (cdr pair) acc)))
         '() pair-list))

