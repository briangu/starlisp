starlisp
========

A Common LISP with support for *Lisp and other useful features.

LISP on the jvm impl inspired by "A Lisp compiler for the JVM" ANTON KINDESTAM
  http://www.csc.kth.se/utbildning/kth/kurser/DD143X/dkand12/Group2Mads/report/AntonKindestam.pdf

Starlisp: Connection Machine *Lisp implementation

building
========

$ mvn clean install

$ cat src/main/lisp/stuff.ljsp - | java -jar target/org.starlisp.core-0.0.1-jar-with-dependencies.jar 

