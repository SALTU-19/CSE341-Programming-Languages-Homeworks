(load "gpp_lexer.lisp")


(setf arith-ops '("OP_PLUS" "OP_MINUS" "OP_DIV" "OP_MULT" ))
(setf logic-ops '("KW_AND" "KW_OR" "KW_NOT" "KW_EQUAL"))
(setf identifier-list '())
(setq op_index 0)





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
(defun nested-expression (list index nested-list flag count)
	(when (and (equal count 0) (not flag))
		(setq op_index index)
		(return-from nested-expression nested-list)
	)
	(setq nested-list (append nested-list (list (elt list index))))
	(when (equal (returnValue (elt list index) 1) "OP_OP")
		(setq count (1+ count))
	)
	(when (equal (returnValue (elt list index) 1) "OP_CP")
		(setq count (1- count))
	)
	(setf flag nil)
	(setq index (+ index 1))
	(nested-expression list index nested-list flag count)	
)


;; + - * / check function return result
(defun check-arith-ops (list result value1 value2)
	(setf value1Index 2)
	(setf value2Index 3)
	(when (equal (returnValue (elt list 1) 1) "OP_PLUS")
		(cond 
			((equal (returnValue (elt list value1Index) 1) "OP_OP")
				(setf value1 (check-arith-ops (nested-expression list value1Index '() t 0) 0 0 0))
				(setf value2Index 7)
			)
			((equal (returnValue (elt list value1Index) 1) "IDENTIFIER")
				(setf value1 (get-value (returnValue (elt list value1Index) 0) identifier-list))
				(if (equal value1 -99999)
					(return-from check-arith-ops "SYNTAX ERROR")
				)
			)
			(t (setf value1 (float (parse-integer (returnValue (elt list value1Index) 0)))))
		)
		(cond 
			((equal (returnValue (elt list value2Index) 1) "OP_OP")
				(setf value2 (check-arith-ops (nested-expression list value2Index '() t 0) 0 0 0))
			)
			((equal (returnValue (elt list value2Index) 1) "IDENTIFIER")
				(setf value2 (get-value (returnValue (elt list value2Index) 0) identifier-list))
				(if (equal value2 -99999)
					(return-from check-arith-ops "SYNTAX ERROR")
				)
			)
			(t (setf value2 (float (parse-integer (returnValue (elt list value2Index) 0)))))
		)
		(setf result (+ value1 value2))
	)
	(when (equal (returnValue (elt list 1) 1) "OP_MINUS")
		(cond 
			((equal (returnValue (elt list value1Index) 1) "OP_OP")
				(setf value1 (check-arith-ops (nested-expression list value1Index '() t 0) 0 0 0))
				(setf value2Index 7)
			)
			((equal (returnValue (elt list value1Index) 1) "IDENTIFIER")
				(setf value1 (get-value (returnValue (elt list value1Index) 0) identifier-list))
				(if (equal value1 -99999)
					(return-from check-arith-ops "SYNTAX ERROR")
				)
			)
			(t (setf value1 (float (parse-integer (returnValue (elt list value1Index) 0)))))
		)
		(cond 
			((equal (returnValue (elt list value2Index) 1) "OP_OP")
				(setf value2 (check-arith-ops (nested-expression list value2Index '() t 0) 0 0 0))
			)
			((equal (returnValue (elt list value2Index) 1) "IDENTIFIER")
				(setf value2 (get-value (returnValue (elt list value2Index) 0) identifier-list))
				(if (equal value2 -99999)
					(return-from check-arith-ops "SYNTAX ERROR")
				)
			)
			(t (setf value2 (float (parse-integer (returnValue (elt list value2Index) 0)))))
		)
		(setf result (- value1 value2))
	)
	(when (equal (returnValue (elt list 1) 1) "OP_MULT")
		(cond 
			((equal (returnValue (elt list value1Index) 1) "OP_OP")
				(setf value1 (check-arith-ops (nested-expression list value1Index '() t 0) 0 0 0))
				(setf value2Index 7)
			)
			((equal (returnValue (elt list value1Index) 1) "IDENTIFIER")
				(setf value1 (get-value (returnValue (elt list value1Index) 0) identifier-list))
				(if (equal value1 -99999)
					(return-from check-arith-ops "SYNTAX ERROR")
				)
			)
			(t (setf value1 (float (parse-integer (returnValue (elt list value1Index) 0)))))
		)
		(cond 
			((equal (returnValue (elt list value2Index) 1) "OP_OP")
				(setf value2 (check-arith-ops (nested-expression list value2Index '() t 0) 0 0 0))
			)
			((equal (returnValue (elt list value2Index) 1) "IDENTIFIER")
				(setf value2 (get-value (returnValue (elt list value2Index) 0) identifier-list))
				(if (equal value2 -99999)
					(return-from check-arith-ops "SYNTAX ERROR")
				)
			)
			(t (setf value2 (float (parse-integer (returnValue (elt list value2Index) 0)))))
		)
		(setf result (* value1 value2))
	)
	(when (equal (returnValue (elt list 1) 1) "OP_DIV")
		(cond 
			((equal (returnValue (elt list value1Index) 1) "OP_OP")
				(setf value1 (check-arith-ops (nested-expression list value1Index '() t 0) 0 0 0))
				(setf value2Index 7)
			)
			((equal (returnValue (elt list value1Index) 1) "IDENTIFIER")
				(setf value1 (get-value (returnValue (elt list value1Index) 0) identifier-list))
				(if (equal value1 -99999)
					(return-from check-arith-ops "SYNTAX ERROR")
				)
			)
			(t (setf value1 (float (parse-integer (returnValue (elt list value1Index) 0)))))
		)
		(cond 
			((equal (returnValue (elt list value2Index) 1) "OP_OP")
				(setf value2 (check-arith-ops (nested-expression list value2Index '() t 0) 0 0 0))
			)
			((equal (returnValue (elt list value2Index) 1) "IDENTIFIER")
				(setf value2 (get-value (returnValue (elt list value2Index) 0) identifier-list))
				(if (equal value2 -99999)
					(return-from check-arith-ops "SYNTAX ERROR")
				)
			)
			(t (setf value2 (float (parse-integer (returnValue (elt list value2Index) 0)))))
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
				(setf value1 (check-logical-ops (nested-expression list value1Index '() t 0) 0 0 0))
				(setf value2Index 7)
			)
			((equal (returnValue (elt list value1Index) 1) "KW_TRUE") 
				(setf value1 t)
			)
			((equal (returnValue (elt list value1Index) 1) "KW_FALSE") 
				(setf value1 nil)
			)
		)
		(cond 
			((equal (returnValue (elt list value2Index) 1) "OP_OP")
				(setf value2 (check-logical-ops (nested-expression list value2Index '() t 0) 0 0 0))
			)
			((equal (returnValue (elt list value2Index) 1) "KW_TRUE")
				(setf value2 t)
			)
			((equal (returnValue (elt list value2Index) 1) "KW_FALSE") 
				(setf value2 nil)
			)
		)
		(setf result (and value1 value2))
	)
	(when (equal (returnValue (elt list 1) 1) "KW_OR")
		(cond 
			((equal (returnValue (elt list value1Index) 1) "OP_OP")
				(setf value1 (check-logical-ops (nested-expression list value1Index '() t 0) 0 0 0))
				(setf value2Index 7)
			)
			((equal (returnValue (elt list value1Index) 1) "KW_TRUE") 
				(setf value1 t)
			)
			((equal (returnValue (elt list value1Index) 1) "KW_FALSE") 
				(setf value1 nil)
			)
		)
		(cond 
			((equal (returnValue (elt list value2Index) 1) "OP_OP")
				(setf value2 (check-logical-ops (nested-expression list value2Index '() t 0) 0 0 0))
			)
			((equal (returnValue (elt list value2Index) 1) "KW_TRUE") 
				(setf value2 t)
			)
			((equal (returnValue (elt list value2Index) 1) "KW_FALSE") 
				(setf value2 nil)
			)
		)
		(setf result (or value1 value2))
	)
	(when (equal (returnValue (elt list 1) 1) "KW_NOT")
		(cond 
			((equal (returnValue (elt list value1Index) 1) "OP_OP")
				(setf value1 (check-logical-ops (nested-expression list value1Index '() t 0) 0 0 0))
			)
			((equal (returnValue (elt list value1Index) 1) "KW_TRUE") 
				(setf value1 t)
			)
			((equal (returnValue (elt list value1Index) 1) "KW_FALSE") 
				(setf value1 nil)
			)
		)
		(setf result (not value1))
	)
	(when (equal (returnValue (elt list 1) 1) "KW_EQUAL")
		(cond 
			((equal (returnValue (elt list value1Index) 1) "OP_OP")
				(setf value1 (check-logical-ops (nested-expression list value1Index '() t 0) 0 0 0))
				(setf value2Index 7)
			)
			((equal (returnValue (elt list value1Index) 1) "IDENTIFIER")
				(setf value1 (get-value (returnValue (elt list value1Index) 0) identifier-list))
				(if (equal value1 -99999)
					(return-from check-arith-ops "SYNTAX ERROR")
				)
			)
			(t (setf value1 (float (parse-integer (returnValue (elt list value1Index) 0)))))
		)
		(cond 
			((equal (returnValue (elt list value2Index) 1) "OP_OP")
				(setf value2 (check-logical-ops (nested-expression list value2Index '() t 0) 0 0 0))
			)
			((equal (returnValue (elt list value2Index) 1) "IDENTIFIER")
				(setf value2 (get-value (returnValue (elt list value2Index) 0) identifier-list))
				(if (equal value1 -99999)
					(return-from check-arith-ops "SYNTAX ERROR")
				)
				
			)
			(t (setf value2 (float (parse-integer (returnValue (elt list value2Index) 0)))))
		)
		(setf result (equal value1 value2))
	)
	result
)

;; get the value of the variable
(defun get-value (identifier identifier-list)
	(if (null identifier-list)
		(return-from get-value -99999)
	)
	(if (equal (returnValue (car identifier-list) 0) identifier)
		(returnValue (car identifier-list) 1)
		(get-value identifier (cdr identifier-list))
	)
)

(defun search-value(element identifier-list index)
	(if (null identifier-list)
		(return-from search-value -99999)
	)
	(if (equal (returnValue (car identifier-list) 0) element)
		index
		(search-value element (cdr identifier-list) (setq index (1+ index)))
	)
)


;; set the value of the variable
(defun set-value (list)
	(setf tempList '())
	(setf value 0)
	(setq index 0)
	(cond 
		((equal (returnValue (elt list 2) 1) "IDENTIFIER")
			;;(setf tempList (list (returnValue (elt list 2) 0) (parse-integer (returnValue (elt list 3) 0))))
			(cond 
				((equal (returnValue (elt list 3) 1) "OP_OP")
					(setf value (check-arith-ops (nested-expression list 3 '() t 0) 0 0 0))
				)
				(t (setf value (float (parse-integer (returnValue (elt list 3) 0)))))
			)
			(setf tempList (list (returnValue (elt list 2) 0) value))
		)
		(t (print "SYNTAX ERROR!") (terpri))
	)
	(setq index (search-value (returnValue (elt list 2) 0) identifier-list 0))
	(if (equal index -99999) 
		(setf identifier-list (append identifier-list (list tempList)))		
		(setf (nth index identifier-list) tempList)
	)
	(return-from set-value "SYNTAX OK!")
)
;; check if operation
(defun check-if(list statement value1 value2 count index)
	(if (equal count 3)
		(if statement
			(return-from check-if value1)
			(return-from check-if value2)
		)
	)
	(cond
		((equal count 0)
			(cond
				((equal (returnValue (elt list index) 1) "OP_OP")
					(setf statement (check-logical-ops (nested-expression list index '() t 0) 0 0 0))
					(setq index op_index)
				)
				((equal (returnValue (elt list index) 1) "KW_TRUE") 
					(setf statement t)
					(setq index (1+ index))
				)
				((equal (returnValue (elt list index) 1) "KW_FALSE") 
					(setf statement nil)
					(setq index (1+ index))
				)
			)
			(setq count (1+ count))
			(check-if list statement value1 value2 count index)							
		)
		((equal count 1)
			(cond
				((equal (returnValue (elt list index) 1) "OP_OP")
					(setq value1 (check-arith-ops (nested-expression list index '() t 0) 0 0 0))
					(setq index op_index)
				)
				((equal (returnValue (elt list index) 1) "VALUEI") 
					(setq value1 (float (parse-integer (returnValue (elt list index) 0))))
					(setq index (1+ index))
				)
				((equal (returnValue (elt list index) 1) "IDENTIFIER") 
					(setq value1 (get-value (returnValue (elt list index) 0) identifier-list))
					(setq index (1+ index))
				)
			)
			(setq count (1+ count))
			(check-if list statement value1 value2 count index)
		)
		((equal count 2)
			(cond
				((equal (returnValue (elt list index) 1) "OP_OP")
					(setq value2 (check-arith-ops (nested-expression list index '() t 0) 0 0 0))
					(setq index op_index)
				)
				((equal (returnValue (elt list index) 1) "VALUEI") 
					(setq value2 (float (parse-integer (returnValue (elt list index) 0))))
					(setq index (1+ index))
				)
				((equal (returnValue (elt list index) 1) "IDENTIFIER") 
					(setq value2 (get-value (returnValue (elt list index) 0) identifier-list))
					(setq index (1+ index))
				)
			)
			(setq count (1+ count))
			(check-if list statement value1 value2 count index)
		)
	)
)

;; check while operation
(defun check-while(list statement index statementFlag)
	(if statement
		(return-from check-while "SYNTAX OK!")
	)
	(when (equal (1+ index) (length list))
		(setq index 2)
		(setf statementFlag (not statementFlag))
	)
	(cond
		(statementFlag
			(cond
				((equal (returnValue (elt list index) 1) "OP_OP")
					(setf statement (check-logical-ops (nested-expression list index '() t 0) 0 0 0))
					(setq index op_index)
				)
				((equal (returnValue (elt list index) 1) "KW_TRUE") 
					(setf statement t)
					(setq index (1+ index))
				)
				((equal (returnValue (elt list index) 1) "KW_FALSE") 
					(setf statement nil)
					(setq index (1+ index))
				)
			)
			(check-while list statement index (setf statementFlag (not statementFlag)))
		)
		((not statementFlag)
			(setf sub-list (nested-expression list index '() t 0))
			(setq index op_index)
			(gppinterpreter sub-list)
			(check-while list statement index statementFlag)
		)
	)	
)
;; check disp
(defun check-disp(list index)
	(cond
		((equal (returnValue (elt list index) 1) "OP_OP")
			(setf sub-list (nested-expression list index '() t 0))
			(setq result (gppinterpreter sub-list))
		)
		((equal (returnValue (elt list index) 1) "VALUEI")
			(setq result (float (parse-integer (returnValue (elt list index) 0))))
		)
		((equal (returnValue (elt list index) 1) "IDENTIFIER") 
			(setq result (get-value (returnValue (elt list index) 0) identifier-list))
		)
	)

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
(defun gppinterpreter(lexer-list)
	(setq set-op "OP_SET")
	;;(print lexer-list)
	(cond 
		((parantesis-check lexer-list 0)
			(cond 
				((is-arith-ops lexer-list arith-ops)
					(return-from gppinterpreter (check-arith-ops lexer-list 0 0 0))
				)
			)
			(cond 
				((is-logical-ops lexer-list logic-ops)
					;; (return-from gppinterpreter (check-logical-ops lexer-list 0 0 0))
					(if (check-logical-ops lexer-list 0 0 0)
						(return-from gppinterpreter "TRUE")
						(return-from gppinterpreter "FALSE")
					)
				)
			)
			(cond 
				((equal (returnValue (elt lexer-list 1) 1) "KW_SET")
					(return-from gppinterpreter (set-value lexer-list))
				)
			)
			(cond 
				((equal (returnValue (elt lexer-list 1) 1) "KW_IF")
					(return-from gppinterpreter (check-if lexer-list nil 0 0 0 2))
				)
			)
			(cond 
				((equal (returnValue (elt lexer-list 1) 1) "KW_WHILE")
					(return-from gppinterpreter (check-while lexer-list nil 2 t))
				)
			)
			(cond 
				((equal (returnValue (elt lexer-list 1) 1) "KW_DISP")
					(return-from gppinterpreter (check-disp lexer-list 2))
				)
			)
		)
		(t (return-from gppinterpreter "SYNTAX ERROR"))
	)
)



(defun main ()
	(setq inputString " ")
	(setf result 0)
	(loop while (not (equal inputString "(exit)")) do
		(princ "Enter input string: ")	
		(setq inputString (read-line))
		(when (not (equal inputString ""))
			(setq lexer-list (gpplexer (list inputString)))
			(setf result (gppinterpreter lexer-list))
			(print result)
			(terpri)
		)
	)
)
;; call main function
(main)