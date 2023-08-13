#lang racket

(require "etapa2.rkt")
(require "etapa3.rkt")
(require "etapa4.rkt")

; ignorați următoarele linii de cod...
(define show-defaults 999) ; câte exerciții la care s-au întors rezultate default să fie arătate detaliat
(define prepend #t) (define nopoints #f) (define name-ex '(testul testele trecut capitolul))
(define default-results `(#f 0 () your-code-here)) (define (default-result r) (set! default-results (cons r default-results))) (define : 'separator) (define punct 'string) (define puncte 'string) (define BONUS 'string) (define exerciții 'string)
(define total 0) (define all '()) (define n-ex 0) (define p-ex 0) (define n-exercs -1) (define default-returns '()) (define (ex n sep p . s) (set! n-ex n) (set! p-ex p) (set! all (cons (list n p) all))) (define exercițiul ex) (define (sunt n s) (set! n-exercs n)) (define s-a string-append)
(define (p . L) (map (λ (e) (display e) (when (> (string-length (format "~a" e)) 0) (display " "))) L) (newline)) (define (p-n-ex) (format "[~a]" (if nopoints (string-join (list (symbol->string (cadddr name-ex)) (number->string n-ex) "/" (number->string n-exercs))) n-ex)))
(define (epart ep% pfix full) (if (< (caddr ep%) 1) (s-a pfix (if full "" (s-a (symbol->string (car name-ex)) " ")) (if (and nopoints (not full)) "" (number->string n-ex)) (symbol->string (cadr ep%))) (if (and nopoints (not full)) "" (s-a pfix (if full "" (s-a (symbol->string (car name-ex)) " ")) (number->string n-ex)))))
(define (whengood ep%) (let [(pts (* p-ex (caddr ep%)))] (and (if prepend (printf "+~v: " pts) (printf "~a[OK] " (p-n-ex))) (if nopoints (p (epart ep% "" #f) "rezolvat") (p (epart ep% "" #f) "rezolvat: +" pts (if (= pts 1) 'punct 'puncte))) (set! total (+ total pts)))))
(define (whenbad ep% gvn expcd msg) (and (when (member gvn default-results) (set! default-returns (cons (epart ep% "" #t) default-returns))) (when (or (not (member gvn default-results)) (<= (length default-returns) show-defaults)) (bad-res ep% gvn expcd msg))))
(define (bad-res ep% gvn expcd msg) (p (if prepend "+0.0:" (format "~a[--]" (p-n-ex))) (epart ep% "la " #f) 'rezultatul gvn msg expcd))
(define (check-conds e gvn conds) (or (null? conds) (let ([r ((car conds) gvn)]) (if (eq? r #t) (check-conds e gvn (cdr conds)) (whenbad e gvn "" (or r "nu îndeplinește condițiile (completitudine, stabilitate)"))))))
(define (check-part part per given main-test expected . conds) (let* ([e (list n-ex part per)] [p? (pair? (cdr main-test))] [p (if p? (car main-test) identity)] [t ((if p? cadr car) main-test)] [m ((if p? cddr cdr) main-test)]) (when (eq? #t (check-conds e given conds)) (if (t (p given) expected) (whengood e) (whenbad e (p given) expected m)))))
(define (check given main-test expected . conds) (apply check-part '- 1 given main-test expected conds))
(define the cons) (define is (cons equal? "diferă de cel așteptat")) (define in (cons member "nu se află printre variantele așteptate"))
(define same-set-as (cons (λ (x y) (apply equal? (map list->seteqv (list x y)))) "nu este aceeași mulțime cu"))
(define same-unique (cons (λ (x y) (and (apply = (map length (list x y))) ((car same-set-as) x y))) "nu sunt aceleași rezultate cu"))
(define (sumar) (when (and (not (null? default-returns)) (< show-defaults (length default-returns))) (p "... rezultatul implicit dat la" (cadr name-ex) (reverse default-returns))) (when (not nopoints) (p 'total: total 'puncte)))
(define (mark-helper) (printf "---~nEx  puncte    Total până aici~n") (foldr (λ (e-p t) (p (car e-p) ': (cadr e-p) "puncte. total 1 -" (car e-p) ': (+ t (cadr e-p))) (+ t (cadr e-p))) 0 all) (newline))

(define men-preferences-0
  '([adi  ana  bia cora]
    [bobo cora ana bia ]
    [cos  cora bia ana ]))
(define women-preferences-0
  '([ana  bobo adi cos ]
    [bia  adi  cos bobo]
    [cora bobo cos adi ]))

(define men-preferences-1
  '([abe  abi  eve  cath ivy  jan  dee  fay  bea  hope gay ]
    [bob  cath hope abi  dee  eve  fay  bea  jan  ivy  gay ]
    [col  hope eve  abi  dee  bea  fay  ivy  gay  cath jan ]
    [dan  ivy  fay  dee  gay  hope eve  jan  bea  cath abi ]
    [ed   jan  dee  bea  cath fay  eve  abi  ivy  hope gay ]
    [fred bea  abi  dee  gay  eve  ivy  cath jan  hope fay ]
    [gav  gay  eve  ivy  bea  cath abi  dee  hope jan  fay ]
    [hal  abi  eve  hope fay  ivy  cath jan  bea  gay  dee ]
    [ian  hope cath dee  gay  bea  abi  fay  ivy  jan  eve ]
    [jon  abi  fay  jan  gay  eve  bea  dee  cath ivy  hope]))
(define women-preferences-1
  '([abi  bob  fred jon  gav  ian  abe  dan  ed   col  hal ]
    [bea  bob  abe  col  fred gav  dan  ian  ed   jon  hal ]
    [cath fred bob  ed   gav  hal  col  ian  abe  dan  jon ]
    [dee  fred jon  col  abe  ian  hal  gav  dan  bob  ed  ]
    [eve  jon  hal  fred dan  abe  gav  col  ed   ian  bob ]
    [fay  bob  abe  ed   ian  jon  dan  fred gav  col  hal ]
    [gay  jon  gav  hal  fred bob  abe  col  ed   dan  ian ]
    [hope gav  jon  bob  abe  ian  dan  hal  ed   col  fred]
    [ivy  ian  col  hal  gav  fred bob  abe  ed   jon  dan ]
    [jan  ed   hal  gav  abe  bob  jon  col  ian  fred dan ]))

(define men-preferences-2
  '([abe  abi  eve  cath ivy  jan  dee  fay  bea  hope gay ]
    [bob  abi hope  cath dee  eve  fay  bea  jan  ivy  gay ]
    [col  hope eve  abi  dee  bea  fay  ivy  gay  cath jan ]
    [dan  ivy  bea  dee  gay  hope eve  jan  fay  cath abi ]
    [ed   jan  dee  bea  cath fay  eve  abi  ivy  hope gay ]
    [fred bea  abi  dee  gay  eve  ivy  cath jan  hope fay ]
    [gav  gay  eve  ivy  bea  cath abi  dee  hope jan  fay ]
    [hal  abi  eve  hope fay  ivy  cath jan  bea  gay  dee ]
    [ian  hope cath dee  gay  bea  abi  fay  ivy  jan  eve ]
    [jon  abi  fay  jan  gay  eve  bea  dee  cath ivy  hope]))
(define women-preferences-2
  '([abi  bob  fred jon  gav  ian  abe  dan  ed   col  hal ]
    [bea  bob  abe  col  fred gav  dan  ian  ed   jon  hal ]
    [cath fred bob  ed   gav  hal  col  ian  abe  dan  jon ]
    [dee  fred jon  col  abe  ian  hal  gav  dan  bob  ed  ]
    [eve  jon  hal  fred dan  abe  gav  col  ed   ian  bob ]
    [fay  bob  abe  ed   ian  jon  dan  fred gav  col  hal ]
    [gay  jon  gav  hal  fred bob  abe  col  ed   dan  ian ]
    [hope gav  jon  bob  abe  ian  dan  hal  ed   col  fred]
    [ivy  ian  col  hal  gav  fred bob  abe  ed   jon  dan ]
    [jan  ed   hal  gav  abe  bob  jon  col  ian  fred dan ]))

(define men-preferences-3
  '([abe  abi  eve  cath ivy  jan  dee  fay  bea  hope gay ]
    [bob  abi hope  cath dee  eve  fay  bea  jan  ivy  gay ]
    [col  hope eve  abi  dee  bea  fay  ivy  gay  cath jan ]
    [dan  ivy  bea  dee  gay  hope eve  jan  fay  cath abi ]
    [ed   fay  dee  bea  cath jan  eve  abi  ivy  hope gay ]
    [fred bea  abi  dee  gay  eve  ivy  cath jan  hope fay ]
    [gav  gay  eve  ivy  bea  cath abi  dee  hope jan  fay ]
    [hal  abi  eve  hope fay  ivy  cath jan  bea  gay  dee ]
    [ian  hope cath dee  gay  bea  abi  fay  ivy  jan  eve ]
    [jon  abi  fay  jan  gay  eve  bea  dee  cath ivy  hope]))
(define women-preferences-3
  '([abi  bob  fred jon  gav  ian  abe  dan  ed   col  hal ]
    [bea  bob  abe  col  fred gav  dan  ian  ed   jon  hal ]
    [cath fred bob  ed   gav  hal  col  ian  abe  dan  jon ]
    [dee  fred jon  col  abe  ian  hal  gav  dan  bob  ed  ]
    [eve  jon  hal  fred dan  abe  gav  col  ed   ian  bob ]
    [fay  bob  abe  ed   ian  jon  dan  fred gav  col  hal ]
    [gay  jon  gav  hal  fred bob  abe  col  ed   dan  ian ]
    [hope gav  jon  bob  abe  ian  dan  hal  ed   col  fred]
    [ivy  ian  col  hal  gav  fred bob  abe  ed   jon  dan ]
    [jan  ed   hal  gav  abe  bob  jon  col  ian  fred dan ]))

(define men-preferences-4
  '([abe  abi  eve  cath ivy  jan  dee  fay  bea  hope gay ]
    [bob  abi hope  cath dee  eve  fay  bea  jan  ivy  gay ]
    [col  hope eve  abi  dee  bea  fay  ivy  gay  cath jan ]
    [dan  jan  bea  dee  gay  hope eve  ivy  fay  cath abi ]
    [ed   fay  dee  bea  cath jan  eve  abi  ivy  hope gay ]
    [fred bea  abi  dee  gay  eve  ivy  cath jan  hope fay ]
    [gav  gay  eve  ivy  bea  cath abi  dee  hope jan  fay ]
    [hal  abi  eve  hope fay  ivy  cath jan  bea  gay  dee ]
    [ian  hope cath dee  gay  bea  abi  fay  ivy  jan  eve ]
    [jon  abi  fay  jan  gay  eve  bea  dee  cath ivy  hope]))
(define women-preferences-4
  '([abi  bob  fred jon  gav  ian  abe  dan  ed   col  hal ]
    [bea  bob  abe  col  fred gav  dan  ian  ed   jon  hal ]
    [cath fred bob  ed   gav  hal  col  ian  abe  dan  jon ]
    [dee  fred jon  col  abe  ian  hal  gav  dan  bob  ed  ]
    [eve  jon  fred dan  abe  gav  col  hal  ed   ian  bob ]
    [fay  bob  abe  ed   ian  jon  dan  fred gav  col  hal ]
    [gay  jon  gav  hal  fred bob  abe  col  ed   dan  ian ]
    [hope gav  jon  bob  abe  ian  dan  hal  ed   col  fred]
    [ivy  ian  col  hal  gav  fred bob  abe  ed   jon  dan ]
    [jan  ed   hal  gav  abe  bob  jon  col  ian  fred dan ]))

(define men-preferences-5
  '([adi  cora bia ana]
    [bobo cora ana bia]
    [cos  cora bia ana]))
(define women-preferences-5
  '([ana  bobo adi cos ]
    [bia  adi  cos bobo]
    [cora bobo cos adi ]))

; Test în care perechile se schimbă complet între cele două etape.
(define men-preferences-6
  '([remu emi eli cam mon]
    [euge mon emi eli cam]
    [caro emi eli cam mon]
    [mada cam emi eli mon]))
(define women-preferences-6
  '([cam remu mada caro euge]
    [emi remu mada caro euge]
    [eli caro remu mada euge]
    [mon remu euge caro mada]))
(define men-preferences-7
  '([remu eli cam mon emi]
    [euge emi eli cam mon]
    [caro emi cam mon eli]
    [mada emi eli mon cam]))
(define women-preferences-7
  '([cam remu caro euge mada]
    [emi mada caro remu euge]
    [eli remu mada euge caro]
    [mon remu caro mada euge]))

; Test în care perechile nu se schimbă.
(define women-preferences-8
  '([oli euge cris ionu]
    [lun ionu euge cris]
    [emi euge ionu cris]))
(define men-preferences-8
  '([cris oli emi lun]
    [ionu emi oli lun]
    [euge oli lun emi]))
(define women-preferences-9
  '([oli cris ionu euge]
    [lun ionu cris euge]
    [emi ionu euge cris]))
(define men-preferences-9
  '([cris emi lun oli]
    [ionu emi lun oli]
    [euge emi oli lun]))

; Transformă o listă într-un stream.
(define (list->stream l)
  (foldr (lambda (x acc) (stream-cons x acc)) empty-stream l))

(define pref-stream-a
  (list->stream (list (cons men-preferences-0 women-preferences-0)
                      (cons men-preferences-5 women-preferences-5))))

(define pref-stream-b
  (list->stream (list (cons men-preferences-1 women-preferences-1)
                      (cons men-preferences-2 women-preferences-2)
                      (cons men-preferences-3 women-preferences-3)
                      (cons men-preferences-4 women-preferences-4))))

(define pref-stream-change-a-lot
  (list->stream (list (cons men-preferences-6 women-preferences-6)
                      (cons men-preferences-7 women-preferences-7))))

(define pref-stream-dont-change
  (list->stream (list (cons men-preferences-8 women-preferences-8)
                      (cons men-preferences-9 women-preferences-9))))

(define (sort-engagements engagements)
  (sort engagements
        (λ (p1 p2)
          (cond ((and (not (car p1)) (not (car p2))) (symbol<? (cdr p1) (cdr p2)))
                ((not (car p2)) #f)
                (else (or (not (car p1)) (symbol<? (car p1) (car p2))))))))

; Hack pentru a nu testa exact valoarea unui rezultat, ci a folosi condițiile.
(define ignore-exact-test (cons (lambda (_ __) #t) "this shouldn't happen?..."))

; Verifică dacă toate persoanele au fost împerecheate într-un mod stabil.
(define (correct-engagements mpref wpref engagements)
  (if (equal? engagements 'your-code-here) #f
      (let ([women-matched (map car engagements)]
            [men-matched (map cdr engagements)])
        (and (andmap (lambda (pref) (member (car pref) women-matched)) wpref)
             (andmap (lambda (pref) (member (car pref) men-matched)) mpref)
             (stable-match? engagements mpref wpref)))))

; Verifică dacă toate configurațiile de preferințe au primit logodne stabile.
(define (correct-engagements-stream pref-stream engagements-stream)
  (define success #t)
  (define (fail-with-msg . msg-tokens) (displayln (apply format msg-tokens)) #f)

  (if (equal? engagements-stream 'your-code-here) #f
      (let iter [(i 1)
                 (engs (stream->list engagements-stream))
                 (prefs (stream->list pref-stream))]
        (cond
          [(and (empty? engs) (empty? prefs))
           success]
          [(and (empty? engs) (not (empty? prefs)))
           (fail-with-msg "prea puține soluții returnate")]
          [(and (not (empty? engs)) (empty? prefs))
           (fail-with-msg "prea multe soluții returnate")]
          [(not (correct-engagements (caar prefs) (cdar prefs) (car engs)))
           (fail-with-msg "al ~v-lea rezultat e instabil: ~v" i (car engs))]
          [else
           (iter (add1 i) (cdr engs) (cdr prefs))]))))


(sunt 4 exerciții)

(exercițiul 1 : 40 puncte)
(check-part 'a (/ 1 4)
 (let ([result (match 'adi '((cora . bobo)) men-preferences-0 women-preferences-0 '(ana bia cos))])
   (if (equal? result 'your-code-here)
       'your-code-here
       (sort-engagements result)))  is '((#f . adi) (cora . bobo)))

(check-part 'b (/ 1 4)
 (let ([result (match 'ana '((bobo . cora) (adi . bia)) women-preferences-0 men-preferences-0 '(cos))])
   (if (equal? result 'your-code-here)
       'your-code-here
       (sort-engagements result))) is '((#f . bia) (adi . ana) (bobo . cora)))

(check-part 'c (/ 1 4)
 (let ([result (match 'bob '((fay . jon) (dee . col) (gay . gav) (bea . fred) (jan . ed) (ivy . dan) (eve . hal)) men-preferences-1 women-preferences-1 '(cath hope ian abi abe))])
   (if (equal? result 'your-code-here)
       'your-code-here
       (sort-engagements result))) is '((#f . dan) (bea . fred) (dee . col) (eve . hal) (fay . bob) (gay . jon) (ivy . gav) (jan . ed)))

(check-part 'd (/ 1 4)
 (let ([result (match 'abe '((#f . dan) (fay . bob) (dee . col) (gay . jon) (bea . fred) (jan . ed) (ivy . gav) (eve . hal)) men-preferences-1 women-preferences-1 '(cath hope ian abi))])
   (if (equal? result 'your-code-here)
       'your-code-here
       (sort-engagements result))) is '((#f . dan) (#f . gav) (bea . abe) (dee . fred) (eve . hal) (fay . bob) (gay . jon) (ivy . col) (jan . ed)))


(exercițiul 2 : 30 puncte)
(let* ([mpref men-preferences-0]
       [wpref women-preferences-0]
       [result (path-to-stability '((cora . bobo)) mpref wpref '(adi ana bia cos))]
       [checker ((curry correct-engagements) mpref wpref)])
  (check-part 'a (/ 1 5) result ignore-exact-test 'nil . (checker)))

(let* ([mpref men-preferences-0]
       [wpref women-preferences-0]
       [result (path-to-stability '((cora . bobo) (bia . adi)) mpref wpref '(ana cos))]
       [checker ((curry correct-engagements) mpref wpref)])
  (check-part 'b (/ 1 5) result ignore-exact-test 'nil . (checker)))

(let* ([mpref men-preferences-1]
       [wpref women-preferences-1]
       [result (path-to-stability '((#f . dan) (fay . bob) (dee . col) (gay . jon) (bea . fred) (jan . ed) (ivy . gav) (eve . hal)) mpref wpref '(abe cath hope ian abi))]
       [checker ((curry correct-engagements) mpref wpref)])
  (check-part 'c (/ 1 5) result ignore-exact-test 'nil . (checker)))

(let* ([mpref men-preferences-2]
       [wpref women-preferences-2]
       [result (path-to-stability '((fay . dan) (dee . col) (eve . hal) (gay . gav) (bea . fred) (jan . ed) (ivy . abe)) mpref wpref '(jon bob cath hope ian abi))]
       [checker ((curry correct-engagements) mpref wpref)])
  (check-part 'd (/ 1 5) result ignore-exact-test 'nil . (checker)))

(let* ([mpref men-preferences-3]
       [wpref women-preferences-3]
       [result (path-to-stability '((dee . col) (gay . gav) (bea . fred) (ivy . dan) (hope . ian) (eve . hal) (abi . bob)) mpref wpref '(jon cath fay jan ed abe))]
       [checker ((curry correct-engagements) mpref wpref)])
  (check-part 'e (/ 1 5) result ignore-exact-test 'nil . (checker)))


(exercițiul 3 : 20 puncte)
(let* ([mpref men-preferences-2]
       [wpref women-preferences-2]
       [result (update-stable-match '((fay . dan) (dee . col) (eve . hal) (gay . gav) (bea . fred) (jan . ed) (ivy . abe) (hope . ian) (cath . bob) (abi . jon)) mpref wpref)]
       [checker ((curry correct-engagements) mpref wpref)])
  (check-part 'a (/ 1 2) result ignore-exact-test 'nil . (checker)))

(let* ([mpref men-preferences-3]
       [wpref women-preferences-3]
       [result (update-stable-match '((fay . jon) (dee . col) (cath . abe) (gay . gav) (bea . fred) (jan . ed) (ivy . dan) (hope . ian) (eve . hal) (abi . bob)) mpref wpref)]
       [checker ((curry correct-engagements) mpref wpref)])
  (check-part 'b (/ 1 2) result ignore-exact-test 'nil . (checker)))


(exercițiul 4 : 30 puncte)
(let* ([pref-stream pref-stream-a]
       [result (build-stable-matches-stream pref-stream)]
       [checker ((curry correct-engagements-stream) pref-stream)])
  (check-part 'a (/ 7 30) result ignore-exact-test 'nil . (checker)))

(let* ([pref-stream pref-stream-b]
       [result (build-stable-matches-stream pref-stream)]
       [checker ((curry correct-engagements-stream) pref-stream)])
  (check-part 'b (/ 7 30) result ignore-exact-test 'nil . (checker)))

; Toate perechile devin instabile după schimbare.
(let* ([pref-stream pref-stream-change-a-lot]
       [result (build-stable-matches-stream pref-stream)]
       [checker ((curry correct-engagements-stream) pref-stream)])
  (check-part 'c (/ 7 30) result ignore-exact-test 'nil . (checker)))

; Perechile nu se schimbă, coada va fi goală.
(let* ([pref-stream pref-stream-dont-change]
       [result (build-stable-matches-stream pref-stream)]
       [checker ((curry correct-engagements-stream) pref-stream)])
  (check-part 'd (/ 7 30) result ignore-exact-test 'nil . (checker)))

; Funcția nu ar trebui să dea eroare pentru un flux de intrare gol.
(let* ([pref-stream empty-stream]
       [result (build-stable-matches-stream pref-stream)])
  (check-part 'e (/ 1 15) result is empty-stream))


(sumar)
