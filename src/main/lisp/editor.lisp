;
; The lisp editor. Written for P-Lisp by Jeff Shrager
;
; -----
; The main function is ED. It is a FEXPR so that the user can type
; (ED name) without having to quote the name. All that ED itself does
; is to check that the named function really exists [has an EXPR] and
; then call ED-SUB on the function's body. If there is no EXPR then ED
; yells and quits.
;
(DEFINE (ED (FLAMBDA (N) ; N will be a list of the function name
 (PROG (BODY)
  (GC NIL) ; Turn off garbage collection messages
  (SETQ N (CAR N)) ; Fix the arg from being a list to just the name
; If there's nothing in the EXPR property of the named symbol then
; yell.
  (COND ((NULL (SETQ BODY (GET N 'EXPR)))
         (RETURN ' "NO FUNCTION DEFINITION."))
; If a NIL comes back from ED-SUB then the edit was aborted so tell the
; user to confirm. Otherwise, the result is bound to BODY and that gets
; replaced in the EXPR property of the named symbol by the T expression.
        ((NULL (SETQ BODY (ED-SUB BODY)))
         (RETURN ' "EDIT ABORTED."))
        (T (PUT N 'EXPR BODY))
  )
 )
)))
;
; ED-SUB does all the real work. It takes any expression and returns it
; edited as per command. If an ABORT command is given, ED-SUB returns
; NIL thus indicating to ED that an ABORT was performed.
;
(DEFINE (ED-SUB (LAMBDA (BODY)
 ; POV starts out NIL by virtue of the way PROG works.
 ; WINDOW is simply used to speed up processing so that that display
 ; does not have to be recalculated all the time. COMMAND holds the
 ; command for processing.
 (PROG (WORK WINDOW POV COMMAND)
 ; Loop to EDPRINT in order to redisplay the window. It is recomputed
 ; here also.
  EDPRINT
          ; Print the little "where am I" display.
          (ED-POV-PRINT POV)
          ; Print the window in compressed form and put it in WINDOW.
          ; Although it gets printed compressed, the function returns
          ; the full form for the SETQ.
          (SETQ WINDOW (ED-PRINT BODY POV))
 ; Loop here to reinput a command if errors occur that do not require
 ; recomputing or redisplaying the window.
   EDREAD
       ; Read a command word and jam it in COMMAND. Note that the
        ; individual functions have to (read) their own arguments.
        (SETQ COMMAND (READ))
        ; This is the main command decision structure.
               ; If the command was numeric then movement is attempted.
        (COND ((NUMBER COMMAND)
                    ; If the number is 0 then this is an UP command.
                    ; Make sure that there is someplace to go and then
                    ; simply lop the end off the POV.
              (COND ((ZERO COMMAND)
                    (COND ((NULL POV) (PRINT ' "NO UP FROM HERE.")
                                      (GO EDREAD))
                          (T (SETQ POV (ED-DETAIL POV))
                             (GO EDPRINT))
              ))
             ; Not a 0 so fix negatives (ED-FIXNUM) to the
             ; corresponding positive and the add that to the
             ; POV. ED-FIXNUM will return () if the number is
             ; not a legal element of the window.
             ((NULL (SETQ COMMAND (ED-FIXNUM WINDOW COMMAND)))
              (GO EDREAD))
             ; If no errors occurred then we'll get here with a
             ; sure positive and legal position number. One
             ; last check -- not an atom! (Can't go to those.)
             (T (COND ((ATOM (ED-NTH WINDOW COMMAND))
                         (PRINT ' "CANNOT GO THERE.")
                         (GO EDPRINT))
                )
                ; Okay -- add the number to the POV and return.
                (SETQ POV (CONC POV (LIST COMMAND)))
                (GO EDPRINT))
        ))
       ; Print command. An easy one.
       ((EQUAL COMMAND 'P)
        (PP WINDOW) (PRINT) (GO EDPRINT))
       ; Delete command. Gets one arg, fixes it, then replaces
       ; the POV with the window less the deleted element.
       ((EQUAL COMMAND 'D)
        (COND ((SETQ WORK (ED-FIXNUM WINDOW (READ)))
               (SETQ BODY (ED-REPLACE BODY POV
                                 (ED-DELETE WINDOW WORK)))))
        (GO EDPRINT))
       ; Next command
       ((EQUAL COMMAND 'NX)
        (SETQ POV (ED-NEXT POV BODY)) (GO EDPRINT))
       ; Back command.
       ((EQUAL COMMAND 'BK)
        (SETQ POV (ED-BACK POV BODY)) (GO EDPRINT))
       ; Insert is much like delete but it reads WHAT, HOW and
       ; WHERE and does some checking first. All that's done in
       ; ED-INSERT-CHECK which returns () if somethings wrong.
       ((EQUAL COMMAND 'I)
        (SETQ WORK (ED-INSERT-CHECK WINDOW (READ) (READ) (READ)))
        (COND ((NULL WORK) (GO EDREAD))
               (T (SETQ BODY (ED-REPLACE BODY POV WORK))
                  (GO EDPRINT))))
        ; Go command -- boring.
        ((EQUAL COMMAND 'GO)
         (SETQ POV (ED-GO (READ) BODY POV))
         (GO EDPRINT))
        ; Exit -- even more boring.
        ((EQUAL COMMAND 'EXIT)
         (RETURN BODY))
        ; Abort -- same boringness as Exit, I guess.
        ((EQUAL COMMAND 'ABORT)
         (RETURN ()))
        ; If nothing worked out then yell at the user and get a
        ; new command.
        (T (PRINT ' "ILLEGAL COMMAND.")
           (GO EDREAD))
 )
)))
;
; ED-FIXNUM changes negative position values into their positive
; equivalents. If this can't be done then it screams and give nil.
;
(DEFINE (ED-FIXNUM (LAMBDA (WINDOW N)
 (COND ((OR (ZERO N) (GREATER N (LENGTH WINDOW))
            (GREATER (MULT -1 N) (LENGTH WINDOW)) )
        (PRINT ' "INVALID ELEMENT.")
        NIL)
       ((GREATER 0 N) (ADD 1 (ADD (LENGTH WINDOW) N)))
       (T N)
 )
)))
;
; ED-PRINT does two jobs. It displays the current window in compressed
; form where all lists are replaced by "&" so that long lists can be
; easily read. ED-PRINT also returns the current window [in full] so
; that caller can utilize that information and not have to recalculate.
;
(DEFINE (ED-PRINT (LAMBDA (BODY POV)
        ; If this is the right place then print her and return.
 (COND ((NULL POV) (PRINT (MAPCAR 'ED-TRANS BODY)) BODY)
         ; otherwise standard recursion on the selected subelement.
         (T (ED-PRINT (ED-NTH BODY (CAR POV)) (CDR POV)))
 )
)))
(DEFINE (ED-TRANS (LAMBDA (N)
 (COND ((ATOM N) N)
       (T '&)
 )
)))
;
; ED-POV-PRINT and ED-POV-EXPAND are used to display the lexical name
; of the current POV starting with TOP: .... The only hairiness in this
; is that in order not to clutter the oblist, after the name is made,
; it is REMOBed.
;
(DEFINE (ED-POV-PRINT (LAMBDA (POV)
 (PROG (WORK)
        (SETQ WORK (PRINT (IMPLODE (CONS ' "TOP:"
                                       (ED-POV-EXPAND POV)))))
        ; Remob the name unless it was () pov in which case you
        ; don't want to remob "TOP:".
        (COND (POV (EVAL (LIST 'REMOB WORK))))
 )
)))
; This guy just put ":"s in between the parts of the POV.
(DEFINE (ED-POV-EXPAND (LAMBDA (POV)
 (COND ((NULL POV) ())
       (T (CONC (LIST (CAR POV) ':)
                (ED-POV-EXPAND (CDR POV)) ))
 )
)))
;
; ED-DETAIL is used to remove the tail from things -- primarily the POV.
;
(DEFINE (ED-DETAIL (LAMBDA (L)
 (COND ((EQUAL 1 (LENGTH L)) ())
       (T (CONS (CAR L) (ED-DETAIL (CDR L))))
 )
)))
;
; ED-INSERT does the task of inserting before/after/ or for an existing
; element in the current window. It is passed the window and three READ
; inputs that indicate the various argument to I in order. The type of
; insert should have been normed by called to one of "B", "A", or "F".
;
(DEFINE (ED-INSERT (LAMBDA (WINDOW WHAT HOW WHERE)
        ; The left hand of this cond selects the proper test according
        ; to the B/A/F option selected.
 (COND ((COND ((EQUAL HOW 'B) (EQUAL WHERE 1))
              ((EQUAL HOW 'A) (EQUAL WHERE 0))
              (T (EQUAL WHERE 1)) )
                ; For "For" kill the selected top element
        (COND ((EQUAL HOW 'F)
               (CONS WHAT (CDR WINDOW)))
               (T (CONS WHAT WINDOW))
        )
       )
        ; Recursion if we haven't yet found the right place then hold
        ; the car element and scan the rest of the list.
        (T CONS (CAR WINDOW)
                 (ED-INSERT (CDR WINDOW) WHAT HOW (SUB WHERE 1))
          ))
 )
)))
;
; ED-REPLACE is used to put a window into a structure at a particular
; POV point. It breaks the structure up "around" the pov trail, jams
; the new object in place and then zips the structure up again.
;
(DEFINE (ED-REPLACE (LAMBDA (BODY POV NEW)
 (COND ((NULL POV) NEW)
       (T (CONC (ED-LEFTBREAK BODY (CAR POV))
           (CONS (ED-REPLACE (ED-NTH BODY (CAR POV)) (CDR POV) NEW)
                 (ED-RIGHTBREAK BODY (CAR POV))
           )
          )
       )
 )
)))
;
; ED-RIGHTBREAK takes the right of a list beginning with the element
; AFTER the Nth. That is, (ed-rightbreak '(1 2 3) 2) = (3)
;
(DEFINE (ED-RIGHTBREAK (LAMBDA (L N)
 (COND ((ZERO N) L)
       (T (ED-RIGHTBREAK (CDR L) (SUB N 1)))
 )
)))
;
; ED-LEFTBREAK takes the left N elements of a list. That is,
;                (ed-leftbreak '(1 2 3) 2) = (1)
;
(DEFINE (ED-LEFTBREAK (LAMBDA (L N)
 (COND ((EQUAL N 1) ())
       (T (CONS (CAR L) (ED-LEFTBREAK (CDR L) (SUB N 1))))
 )
)))
;
; ED-DELETE takes a location and simply returns the window without the
; specified element.
;
(DEFINE (ED-DELETE (LAMBDA (WINDOW WHERE)
 (COND ((EQUAL 1 WHERE) (CDR WINDOW))
       (T (CONS (CAR WINDOW) (ED-DELETE (CDR WINDOW) (SUB WHERE 1))))
 )
)))
;
; ED-INSERT-CHECK normalizes the input to insert. It fixes numbers and
; replaces FOR/BEFORE/AFTER for their one letter equivalents.
;
(DEFINE (ED-INSERT-CHECK (LAMBDA (WINDOW WHAT HOW WHERE)
 (COND ((AND (ED-MEMBER HOW '(F FOR A AFTER B BEFORE))
            (SETQ WHERE (ED-FIXNUM WINDOW WHERE)) )
        (ED-INSERT WINDOW WHAT
           (COND ((EQUAL HOW 'FOR) 'F)
                 ((EQUAL HOW 'AFTER) 'A)
                 ((EQUAL HOW 'BEFORE) 'B)
                 (T HOW)
           )
           WHERE
        )
       )
       (T (PRINT ' "ILLEGAL INSERT COMMAND.")
          ())
 )
)))
;
; ED-BACK, ED-GO, and associated routines were added to the editor
; by Stewart Schiffman.
;
; ED-BACK arranges the POV so that the window has shifted treewise left.
; If you hit the front, an error is returned. The tough part of these
; maneuvers is that they have to skip over atomic elements since the
; window cannot contain an atom.
;
(DEFINE (ED-BACK (LAMBDA (POV BODY)
 (PROG (CMD OPOV WINDOW)
  (SETQ OPOV POV)
 BLOOP
  ; Try to move the end pointer in the POV back by one.
  (SETQ CMD (SUB (ED-NTH POV (LENGTH POV)) 1))
  (SETQ POV (ED-DETAIL POV))
  ; Figure out what the window looking down on this one sees.
  (SETQ WINDOW (ED-SETW BODY POV))
  ; Test for hitting the front. If not then fix the POV.
  (COND ((EQUAL CMD 0) (PRINT ' "CANNOT GO BACK.")
                       (SETQ POV OPOV) (RETURN POV)
        )
        (T (SETQ POV (APPEND POV CMD))
          (SETQ WINDOW (ED-SETW BODY POV)))
  )
  (COND ((ATOM WINDOW) (GO BLOOP)))
  (RETURN POV)
 )
)))
;
; ED-CHNUM and ED-SETW are used by BK and NX to get window images and
; check for illegal position values.
;
(DEFINE (ED-CHNUM (LAMBDA (WINDOW N)
 (NOT (GREATER N (LENGTH WINDOW)))
)))
(DEFINE (ED-SETW (LAMBDA (BODY POV)
 (COND ((ATOM BODY) BODY)
        ((NULL POV) (MAPCAR 'ED-TRANS BODY) BODY)
        (T (ED-SETW (ED-NTH BODY (CAR POV)) (CDR POV)))
 )
)))
;
; ED-NEXT is exactly like ED-BACK except that it moves forward instead
; of backward [obviously].
;
(DEFINE (ED-NEXT (LAMBDA (POV BODY)
 (COND ((NULL POV) (SETQ POV '(3)))
       (T (PROG (CMD OPOV WINDOW)
                  (SETQ OPOV POV)
                ILOOP
                  (SETQ CMD (ADD (ED-NTH POV (LENGTH POV)) 1))
                  (SETQ POV (ED-DETAIL POV))
                  (SETQ WINDOW (ED-SETW BODY POV))
                  (COND ((NULL (ED-CHNUM WINDOW CMD))
                               (PRINT ' "CANNOT GO FORWARD.")
                               (SETQ POV OPOV) (RETURN POV)
                              )
                              (T (SETQ POV (APPEND POV CMD))
                                 (SETQ WINDOW (ED-SETW BODY POV)))
                        )
                        (COND ((ATOM WINDOW) (GO ILOOP)))
                        (RETURN POV)
          )
       )
 )
)))
;
; ED-GO simply replaces the POV. It has to make sure that the new
; value is not an atom.
;
(DEFINE (ED-GO (LAMBDA (NPOV BODY POV)
 (COND ((ATOM NPOV (PRINT ' "ILLEGAL GO COMMAND: MUST BE A
 LIST.") POV)
          (COND ((ATOM WINDOW) (PRINT ' "CANNOT GO THERE.") POV)
                (T NPOV) ) )
 )
)))
;
; ED-NTH and ED-MEMBER are just utilities that work as one would expect.
; That is, they return the Nth element of a list and find out whether a
; particular target is in a list.
;
(DEFINE (ED-NTH (LAMBDA (L N)
 COND ((EQUAL 1 N) (CAR L))
      (T (ED-NTH (CDR L) (SUB N 1)))
 )
)))
(DEFINE (ED-MEMBER (LAMBDA (A L)
 (COND ((NULL L) ())
       ((EQUAL A (CAR L)) T)
       (T (ED-MEMBER A (CDR L)))
 )
)))
