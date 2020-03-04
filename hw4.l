(defun my-length (L)
    (cond ((null L) 0)
        (t( + 1 (my-length(cdr L) ) )
        )
    )
)

(defun my-memq (a L)
    (cond ((null L) nil)
        ((eq a (car L)) L)
        (T (my-memq a (cdr L) )
        )
    )
)




(defun my-append (L1 L2)
    (cond ((null L1) L2)
        (t (cons (car L1) (my-append (cdr L1) L2) )
        )
    )
)

(defun my-attach (O L)
    (cond (L (cons (car L)  (my-attach O (cdr L))))
        (t (cons O nil)
        )
    )
)

(defun my-assoc (a L)
    (cond ( (null L) nil)
        ((eq (caar L) a)
            (car L)
        )
        (T(my-assoc a (cdr L)))
    )
)
(defun freq (A L)
    (cond
        ((null L) 0)
        ((atom (car L))
           (freq-helper A L)
        )
        (T (+ (freq A (car L)) (freq A (cdr L))) )
    )
)
(defun freq-helper (A L)
    (cond
        ((eq A (car L))
            (+ 1 (freq A (cdr L)))
        )
        (T (freq A (cdr L)))
    )
)

(defun mapping (L val)
    (cond ( (null L ) nil)
        (
            (< (car (car L) ) val)
            (cons (cadar L) (mapping (cdr L) val))
        )
        (T (mapping  (cdr L) val))
    )
)

(defun my-reverse (L)
    (cond ( (null L ) nil)
        ( (eq (my-length L) 1) L
        )
        (T (my-append (my-reverse(cdr L)) (list (car L) )  )
        )
    )
)





(defun my-memq-advance (a L)
    (cond ((null L) nil)
        ((eq a (car L)) nil)
        (T  (cons (car L)(my-memq-advance a (cdr L)) ))
    )
)

(defun my-last (A L)
    (cond
        ((null L ) nil)
        ((eq (car L) A)
            (cond
                (T(cons (car L)  (my-reverse(my-memq-advance A (my-reverse L)))
                  )
                )
            )
        )
      (T(my-last A (cdr L)))
    )
)

(defun is-pattern? (pat str)
    (cond
        ((null pat) nil)
        ((null str) nil)
        ((eq (car pat) (car str))
            ; (is-pattern?helper pat str)
            (cond
                ((null (is-pattern?helper pat str)) nil)
                (T(my-append pat (is-pattern?helper pat str)
                ))
            )
        )
        (T(is-pattern? pat (cdr str))
        )
    )
)

(defun is-pattern?helper (pat str)
    (cond
        ((null str) nil)
        ((eq (my-length pat) 0)
            str
        )
        ((eq (car pat) (car str))
            (is-pattern?helper (cdr pat) (cdr str))
        )
        (T nil)
    )
)


(defun first-atom (L)
    (cond
        ((null L ) nil)
        ((atom L) L)
        (t(first-atom(car L)))
    )
)
(defun normalizeList (L)
    (cond
        ((null L)nil)
        ((atom (car L))
            (my-append (list(car L)) (normalizeList (cdr L)))
        )
        (T(my-append (normalizeList (car L))(normalizeList (cdr L)))
        )
    )
)
(defun find-all-helper (A L)
    (cond
        ((null L) nil)
        ((eq A (car L))
            (cons (cadr L)(find-all-helper A (cdr L)))
        )
        (T(find-all-helper A (cdr L))
        )
    )
)
(defun find-all(A L)
    (cond
        ((null L) nil)
        ((null A) nil)
        (T(find-all-helper A (normalizeList L))
        )
    )
)
