; factor as many 2s out of n as possible:
; input n, output (k, e) where n=k2^e with k odd
(define (divide-out-powers-of-2 n)
  (define (DOP2 n e)
    (if (= (remainder n 2) 1)
	(cons n e)
	(DOP2 (/ n 2) (+ 1 e))))
  (DOP2 n 0))

(define (square x) (* x x))

; compute b^e modulo m, taken from SICP file
(define (expmod b e m)
  (cond ((zero? e) 1)
        ((even? e) (remainder (square (expmod b (/ e 2) m)) m))
        (else (remainder (* b (expmod b (- e 1) m)) m))))

; is a an MR witness mod n? 
; that is, is it true that all of the congruences:
; a^k \equiv 1 \mod n
; a^k \equiv -1 \mod n
; a^{2k} \equiv -1 \mod n
; ...
; a^{2^{e-1}} \equiv -1 \mod n
;... are false?
(define (witness? a e k n)
  (let ((x (expmod a k n))) ; x= a^k mod n
    (and (not (= x 1)) (witness-test-iter x e n))))

(define (witness-test-iter x e n)
  (cond ((= x (- n 1)) #f) ; a congruence is true. x isn't a witness
	((<= e 1) #t)
	(else (witness-test-iter (remainder (square x) n) (- e 1) n))))
    
; MR primality test, with numberOfTrials runs. n must be odd.
(define (is-prime-MR? n trials)
  (let ( (k (car (divide-out-powers-of-2 (- n 1))))
	 (e (cdr (divide-out-powers-of-2 (- n 1)))) ); this is silly but it doesn't seem that we can let u be divideOutPowersOf2 (- n 1) and k be car of u in the same let line.
    (define (is-prime-MR-iter trials)
      (cond ((= trials 0) #t)
	    ((witness? (+ 1 (random (- n 2))) e k n) #f)
	    (else (is-prime-MR-iter (- trials 1)))))
    (is-prime-MR-iter trials) ))
