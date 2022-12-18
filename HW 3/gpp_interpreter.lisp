(load "gpp_lexer.lisp")




(defun returnValue(list index)
	(elt list index)
)

(defun parantesis-check (lexer-list count)
	(if (null lexer-list)
			(if (= count 0)
				t
				nil
			)
			(if (string= (returnValue (car lexer-list) 1) "OP_OP")
				(parantesis-check (cdr lexer-list) (+ count 1))
				(if (string= (returnValue (car lexer-list) 1) "OP_CP")
						(parantesis-check (cdr lexer-list) (- count 1))
						(parantesis-check (cdr lexer-list) count)
				)
			)
	)
)
;; nested expression 
(defun nested-expression (list index nested-list flag)
	(when (equal (returnValue (elt list index) 1) "OP_CP")
		(setq nested-list (append nested-list (list (elt list index))))
		(setf flag nil)
		nested-list
		(return-from nested-expression nested-list)
	)
	(when flag
		(setq nested-list (append nested-list (list (elt list index))))
		(setq index (+ index 1))
		(nested-expression list index nested-list flag)
	)	
)


;; + - * / check function return result
(defun check-arith-ops (list result value1 value2)
	(setf value1Index 2)
	(setf value2Index 3)
	(when (equal (returnValue (elt list 1) 1) "OP_PLUS")
		(cond 
			((equal (returnValue (elt list value1Index) 1) "OP_OP")
				(setf value1 (check-arith-ops (nested-expression list value1Index '() t) 0 0 0))
				(setf value2Index 7)
			)
			(t (setf value1 (parse-integer (returnValue (elt list value1Index) 0))))
		)
		(cond 
			((equal (returnValue (elt list value2Index) 1) "OP_OP")
				(setf value2 (check-arith-ops (nested-expression list value2Index '() t) 0 0 0))
			)
			(t (setf value2 (parse-integer (returnValue (elt list value2Index) 0))))
		)
		(setf result (+ value1 value2))
	)
	(when (equal (returnValue (elt list 1) 1) "OP_MINUS")
		(cond 
			((equal (returnValue (elt list value1Index) 1) "OP_OP")
				(setf value1 (check-arith-ops (nested-expression list value1Index '() t) 0 0 0))
				(setf value2Index 7)
			)
			(t (setf value1 (parse-integer (returnValue (elt list value1Index) 0))))
		)
		(cond 
			((equal (returnValue (elt list value2Index) 1) "OP_OP")
				(setf value2 (check-arith-ops (nested-expression list value2Index '() t) 0 0 0))
			)
			(t (setf value2 (parse-integer (returnValue (elt list value2Index) 0))))
		)
		(setf result (- value1 value2))
	)
	(when (equal (returnValue (elt list 1) 1) "OP_MUL")
		(cond 
			((equal (returnValue (elt list value1Index) 1) "OP_OP")
				(setf value1 (check-arith-ops (nested-expression list value1Index '() t) 0 0 0))
				(setf value2Index 7)
			)
			(t (setf value1 (parse-integer (returnValue (elt list value1Index) 0))))
		)
		(cond 
			((equal (returnValue (elt list value2Index) 1) "OP_OP")
				(setf value2 (check-arith-ops (nested-expression list value2Index '() t) 0 0 0))
			)
			(t (setf value2 (parse-integer (returnValue (elt list value2Index) 0))))
		)
		(setf result (* value1 value2))
	)
	(when (equal (returnValue (elt list 1) 1) "OP_DIV")
		(cond 
			((equal (returnValue (elt list value1Index) 1) "OP_OP")
				(setf value1 (check-arith-ops (nested-expression list value1Index '() t) 0 0 0))
				(setf value2Index 7)
			)
			(t (setf value1 (parse-integer (returnValue (elt list value1Index) 0))))
		)
		(cond 
			((equal (returnValue (elt list value2Index) 1) "OP_OP")
				(setf value2 (check-arith-ops (nested-expression list value2Index '() t) 0 0 0))
			)
			(t (setf value2 (parse-integer (returnValue (elt list value2Index) 0))))
		)
		(setf result (/ value1 value2))
	)
	result
)
;; and or not equal check function return result
(defun check-logical-ops (list result value1 value2)
	(setf value1Index 2)
	(setf value2Index 3)
	(when (equal (returnValue (elt list 1) 1) "KW_AND")
		(cond 
			((equal (returnValue (elt list value1Index) 1) "OP_OP")
				(setf value1 (check-logical-ops (nested-expression list value1Index '() t) 0 0 0))
				(setf value2Index 7)
			)
			(t (setf value1 (parse-integer (returnValue (elt list value1Index) 0))))
		)
		(cond 
			((equal (returnValue (elt list value2Index) 1) "OP_OP")
				(setf value2 (check-logical-ops (nested-expression list value2Index '() t) 0 0 0))
			)
			(t (setf value2 (parse-integer (returnValue (elt list value2Index) 0))))
		)
		(setf result (and value1 value2))
	)
	(when (equal (returnValue (elt list 1) 1) "KW_OR")
		(cond 
			((equal (returnValue (elt list value1Index) 1) "OP_OP")
				(setf value1 (check-logical-ops (nested-expression list value1Index '() t) 0 0 0))
				(setf value2Index 7)
			)
			(t (setf value1 (parse-integer (returnValue (elt list value1Index) 0))))
		)
		(cond 
			((equal (returnValue (elt list value2Index) 1) "OP_OP")
				(setf value2 (check-logical-ops (nested-expression list value2Index '() t) 0 0 0))
			)
			(t (setf value2 (parse-integer (returnValue (elt list value2Index) 0))))
		)
		(setf result (or value1 value2))
	)
	(when (equal (returnValue (elt list 1) 1) "KW_NOT")
		(cond 
			((equal (returnValue (elt list value1Index) 1) "OP_OP")
				(setf value1 (check-logical-ops (nested-expression list value1Index '() t) 0 0 0))
			)
			(t (setf value1 (parse-integer (returnValue (elt list value1Index) 0))))
		)
		(setf result (not value1))
	)
	(when (equal (returnValue (elt list 1) 1) "KW_EQUAL")
		(cond 
			((equal (returnValue (elt list value1Index) 1) "OP_OP")
				(setf value1 (check-logical-ops (nested-expression list value1Index '() t) 0 0 0))
				(setf value2Index 7)
			)
			(t (setf value1 (parse-integer (returnValue (elt list value1Index) 0))))
		)
		(cond 
			((equal (returnValue (elt list value2Index) 1) "OP_OP")
				(setf value2 (check-logical-ops (nested-expression list value2Index '() t) 0 0 0))
			)
			(t (setf value2 (parse-integer (returnValue (elt list value2Index) 0))))
		)
		(setf result (equal value1 value2))
	)
	result
)
;; control the input is arith-ops or not
(defun is-arith-ops (list arith-ops)
	(if (null arith-ops)
		nil
		(if (equal (returnValue (elt list 1) 1) (car arith-ops))
			t
			(is-arith-ops list (cdr arith-ops))
		)
	)
)
;; control the input is logical-ops or not
(defun is-logical-ops (list logical-ops)
	(if (null logical-ops)
		nil
		(if (equal (returnValue (elt list 1) 1) (car logical-ops))
			t
			(is-logical-ops list (cdr logical-ops))
		)
	)
)
(defun main ()
	(setf arith-ops '("OP_PLUS" "OP_MINUS" "OP_DIV" "OP_MULT" ))
	(setf logic-ops '("KW_AND" "KW_OR" "KW_NOT" "KW_EQUAL"))
    (when *args*
        (setq filename (elt *args* 0))
        ; call file read function
        (setq fileContent (read-from-file filename))
        ; call gppinterpreter function
        (setq lexer-list (gpplexer fileContent))
		(cond 
			((parantesis-check lexer-list 0) 
				(print "SYNTAX OK")
			)
			(t (print "SYNTAX ERROR"))
		)		
    )
    (when (not *args*)
        (princ "Enter input string: ")
        
        (setq inputString (read-line))
        (setq lexer-list (gpplexer (list inputString)))
		;;(print lexer-list)
		(cond 
			((parantesis-check lexer-list 0)
				(cond 
					((is-arith-ops lexer-list arith-ops)
						(print (check-arith-ops lexer-list 0 0 0))
					)
				)
				(cond 
					((is-logical-ops lexer-list logic-ops)
						(print (check-logical-ops lexer-list 0 0 0))
					)
				)

			)
			(t (print "SYNTAX ERROR"))
		)
    )

)
;; call main function
(main)