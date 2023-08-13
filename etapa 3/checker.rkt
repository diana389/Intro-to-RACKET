#lang racket

(require "etapa2.rkt")
(require "etapa3.rkt")

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

; Admite '((sof . ovid) (ema . iani) (nat . ionu))
;    sau '((sof . iani) (nat . ionu) (ema . ovid))
(define men-preferences-5
  '([ionu nat sof ema]
    [iani nat ema sof]
    [ovid nat sof ema]))
(define women-preferences-5
  '([sof iani ionu ovid]
    [nat ionu iani ovid]
    [ema ovid iani ionu]))

(define (sort-engagements engagements) (sort engagements (λ (p1 p2) (symbol<? (car p1) (car p2)))))

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


(sunt 5 exerciții)

(exercițiul 1 : 40 puncte)
(check-part 'a (/ 1 5)
            (let ([result (map (λ (eng) (get-unstable-couples eng men-preferences-0 women-preferences-0))
                               '(((ana . adi) (bia . cos) (cora . bobo))
                                 ((ana . cos) (bia . adi) (cora . bobo))))])
              (if (equal? (car result) 'your-code-here)
                  'your-code-here
                  (map sort-engagements result)))
            is '(() ((ana . cos) (bia . adi))))

(check-part 'b (/ 1 5)
            (let ([result (map (λ (eng) (get-unstable-couples eng men-preferences-1 women-preferences-1))
                               '(((fay . jon) (dee . col) (cath . abe) (gay . gav) (bea . fred) (jan . ed) (ivy . dan) (hope . ian) (eve . hal) (abi . bob))
                                 ((fay . dan) (dee . col) (eve . hal) (gay . gav) (bea . fred) (jan . ed) (ivy . abe) (hope . ian) (cath . bob) (abi . jon))))])
              (if (equal? (car result) 'your-code-here)
                  'your-code-here
                  (map sort-engagements result)))
            is '(((abi . bob) (cath . abe) (hope . ian)) ()))

(check-part 'c (/ 1 5)
            (let ([result (map (λ (eng) (get-unstable-couples eng men-preferences-2 women-preferences-2))
                               '(((fay . jon) (dee . col) (cath . abe) (gay . gav) (bea . bob) (jan . ed) (ivy . dan) (hope . ian) (eve . hal) (abi . fred))
                                 ((fay . dan) (dee . col) (eve . hal) (gay . gav) (bea . fred) (jan . ed) (ivy . abe) (hope . ian) (cath . bob) (abi . jon))))])
              (if (equal? (car result) 'your-code-here)
                  'your-code-here
                  (map sort-engagements result)))
            is '(((abi . fred) (bea . bob) (cath . abe) (fay . jon) (hope . ian)) ((abi . jon) (cath . bob) (hope . ian))))

(check-part 'd (/ 1 5)
            (let ([result (map (λ (eng) (get-unstable-couples eng men-preferences-3 women-preferences-3))
                               '(((fay . ed) (dee . col) (cath . abe) (gay . gav) (bea . bob) (jan . jon) (ivy . dan) (hope . ian) (eve . hal) (abi . fred))
                                 ((fay . dan) (dee . fred) (eve . hal) (gay . gav) (bea . col) (jan . ed) (ivy . abe) (hope . ian) (cath . bob) (abi . jon))))])
              (if (equal? (car result) 'your-code-here)
                  'your-code-here
                  (map sort-engagements result)))
            is '(((abi . fred) (bea . bob) (cath . abe) (fay . ed) (hope . ian))
                 ((abi . jon) (cath . bob) (dee . fred) (fay . dan) (hope . ian) (jan . ed))))

(check-part 'e (/ 1 5)
            (let ([result (map (λ (eng) (get-unstable-couples eng men-preferences-5 women-preferences-5))
                               '(((sof . ovid) (ema . iani) (nat . ionu))
                                 ((sof . iani) (nat . ionu) (ema . ovid))
                                 ((sof . iani) (nat . ovid) (ema . ionu))))])
              (if (equal? (car result) 'your-code-here)
                  'your-code-here
                  (map sort-engagements result)))
            is '(() () ((ema . ionu) (nat . ovid) (sof . iani))))


(exercițiul 2 : 50 puncte)
(let* ([mpref men-preferences-0]
       [wpref women-preferences-0]
       [result (engage '(adi bobo) '((cora . cos)) mpref wpref)]
       [checker ((curry correct-engagements) mpref wpref)])
  (check-part 'a (/ 1 5) result ignore-exact-test 'nil . (checker)))

(let* ([mpref men-preferences-1]
       [wpref women-preferences-1]
       [result (engage '(abe bob col dan ed fred gav hal ian jon) '() mpref wpref)]
       [checker ((curry correct-engagements) mpref wpref)])
  (check-part 'b (/ 1 5) result ignore-exact-test 'nil . (checker)))

(let* ([mpref men-preferences-2]
       [wpref women-preferences-2]
       [result (engage '(abe bob dan ed fred gav ian jon) '((abi . hal) (hope . col)) mpref wpref)]
       [checker ((curry correct-engagements) mpref wpref)])
  (check-part 'c (/ 1 5) result ignore-exact-test 'nil . (checker)))

(let* ([mpref men-preferences-3]
       [wpref women-preferences-3]
       [result (engage '(abe bob col dan ed fred gav hal ian jon) '() mpref wpref)]
       [checker ((curry correct-engagements) mpref wpref)])
  (check-part 'd (/ 1 5) result ignore-exact-test 'nil . (checker)))

(let* ([mpref men-preferences-4]
       [wpref women-preferences-4]
       [result (engage '(gav abe col dan ed bob fred hal ian jon) '() mpref wpref)]
       [checker ((curry correct-engagements) mpref wpref)])
  (check-part 'e (/ 1 5) result ignore-exact-test 'nil . (checker)))


(exercițiul 3 : 10 puncte)
(let* ([mpref men-preferences-0]
       [wpref women-preferences-0]
       [result (gale-shapley mpref wpref)]
       [checker ((curry correct-engagements) mpref wpref)])
  (check-part 'a (/ 1 2) result ignore-exact-test 'nil . (checker)))

(let* ([mpref men-preferences-1]
       [wpref women-preferences-1]
       [result (gale-shapley mpref wpref)]
       [checker ((curry correct-engagements) mpref wpref)])
  (check-part 'b (/ 1 2) result ignore-exact-test 'nil . (checker)))


(exercițiul 4 : 20 puncte)
(check-part 'a (/ 1 2)
            (let ([result (get-couple-members '((fay . dan) (dee . col) (eve . hal) (gay . gav)))])
              (if (equal? result 'your-code-here)
                  'your-code-here
                  (sort result symbol<?))) is '(col dan dee eve fay gav gay hal))

(check-part 'b (/ 1 2)
            (let ([result (get-couple-members '((cora . bobo)))])
              (if (equal? result 'your-code-here)
                  'your-code-here
                  (sort result symbol<?))) is '(bobo cora))

(sumar)
