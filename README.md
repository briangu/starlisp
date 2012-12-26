starlisp
========

A Common LISP with support for *Lisp and other useful features.

LISP on the jvm impl inspired by "A Lisp compiler for the JVM" ANTON KINDESTAM
  http://www.csc.kth.se/utbildning/kth/kurser/DD143X/dkand12/Group2Mads/report/AntonKindestam.pdf

Starlisp: Connection Machine *Lisp implementation

features
========

Starlisp is currently interpreted, though plans for enabling compiling to jasmin are planned.

Starlisp supports:

* lexical scoping.
* java interop using the dot syntax (.toString obj)

Planned
=======

* Embedded web server to enable online debugging and REPL
* Interfacing with Finagle for good async execution
* backquotes

TODO
====

* [fix] java interop using the dot syntax (.toString obj)

building
========

compile:

    $ mvn clean install

run the REPL:

    $ java -jar target/org.starlisp.core-0.0.1-jar-with-dependencies.jar

examples
========

bootstrap:

    $ java -jar target/org.starlisp.core-0.0.1-jar-with-dependencies.jar

try:

    (defun foo (x) (+ x x))
    (aeq 2 (foo 1))
    (aeq 1 (1- 2))
    (aeq 3 (1+ 2))

scope excercises:

    (set 'x 1)
    (aeq 16 (progn (set 'x 8) (+ x x)))
    (aeq 8 x)
    
    (aeq 1 (let () 1))
    (set 'q 10)
    (aeq 1 (let ((q 1)) q))

    (let ((a 1)) (+ a a))
    (let ((a 1) (b 2)) (+ a b))
    (let ((a 1) (b 2)) (cons a (cons b #(hej din fisk (1 2 3)))))
    
    (let ((a 1) (b 2)) (cons a '(#\W (1231312312312312312312312312312313123 . 5343412914294967296) (<a> <b> <c>) b #(hej din fisk (1 2 3)) "potatismossa" . 12.4)))
   
other examples:
   
    (fib-trec 100)

    (aeq 1 (subst-symbols '<a> '(<a> 1)))
    (aeq '(+ 1 2) (subst-symbols '(+ <a> <b>) '(<a> 1 <b> 2)))

    (defun foo (tree replacings)
      (progn
        (prin1 (env-depth))
        (if (atom? tree)
            (progn (prin1 '>>)
                   (prin1 (env-depth))
                   (prin1 tree)
                   (prin1 '<<))
            (progn (prin1 '$)
                   (prin1 tree)
                   (prin1 (env-depth))
                   (prin1 '$)
                   (prin1 (car tree))
                   (let ((q (car tree))) (foo4 q (cdr replacings)))
                   (prin1 '[)
                   (prin1 (env-depth))
                   (prin1 tree)
                   (prin1 '])
                   (foo4 (cdr tree) (cdr replacings))))))
    
    (foo '(+ <a> <b> <c>) '(<a> 1 <b> 2))

    
