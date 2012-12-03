(set (quote defun)
     (macro (a)
       (cons (quote set) (cons (cons (quote quote) (cons (car (cdr a)) nil)) (cons (cons (quote lambda) (cdr (cdr a))) nil)))))

;; IMPLEMENTED USING ONLY PRIMITIVES
(set (quote defmacro)
     (macro (a)
       (cons (quote set) (cons (cons (quote quote) (cons (car (cdr a)) nil)) (cons (cons (quote macro) (cdr (cdr a))) nil)))))

(defun %load-loop (stream)
  (if (eof? stream)
      t
      ((lambda () (eval (read stream))
                  (%load-loop stream)))))

(defun load (file)
  ((lambda (stream)
     (%load-loop stream)
     (close stream))
   (open file 'in)))

(defun 1- (n) (- n 1))
(defun 1+ (n) (+ n 1))

(defun terpri rst
  (write-char #\
              (car rst))
  nil)

(defun print (obj . rst)
  (prin1 obj (car rst))
  (terpri (car rst))
  obj)

(defun displace (old new)
  (rplaca old (car new))
  (rplacd old (cdr new)))

(defun error (str) (throw 'simple-error str)) ; Make me better, more like CL I guess.

(defun end? (lst)
  (if (atom? lst)
      (if lst
          (error "Not list!")
          t)
      nil))

(defun not (obj) (eq? obj nil))
(set (quote null?) not)
(defun cons? (obj) (not (atom? obj)))
(defun list? (lst)
  (if (atom? lst)
      (if lst nil t)
      t))
(defun zero? (n) (= n 0))
(defun pos? (n) (if (zero? n) nil (not (neg? n))))
(defun /= (a b) (not (= a b)))
(defun < (a b) (neg? (- a b)))
(defun > (a b) (neg? (- b a)))
(defun <= (a b) (if (= a b) t (< a b)))
(defun >= (a b) (if (= a b) t (> a b)))

(defun list lst lst)

(defun list* (arg . others)
  (if (null? others)
      arg
      (if (null? (cdr others))
          (cons arg (car others))
          ((lambda (roop)
             (roop others)
             (cons arg others))
           (lambda (x)
             (if (null? (cdr (cdr x)))
                 (rplacd x (car (cdr x)))
                 (roop (cdr x))))))))

(defmacro progn (a) (displace a
  (list (list* (quote lambda) () (cdr a)))))

(defun reverse (lst)
  ((lambda (rev) (rev lst nil))
   (lambda (lst acc)
     (if (end? lst)
         acc
         (rev (cdr lst) (cons (car lst) acc))))))

(set 'reverse! reverse)                 ; TODO: Implement actual reverse!

#;(defun three () 3)
#;(append '() '())

(defmacro setq (a) (displace a
  ((lambda (frob) (cons (quote progn) (frob (cdr a) nil)))
   (lambda (lst acc)
     (if (end? lst)
         (reverse! acc)
         (frob (cdr (cdr lst))
               (cons (list (quote set) (list (quote quote) (car lst)) (car (cdr lst))) acc)))))))
