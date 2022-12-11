; split function it splits the string character
(defun split (string)
    (loop for i from 0 to (1- (length string))
        collect (subseq string i (1+ i))))
; split keywords
(setq listKeywords (split "list"))
(setq appendKeywords (split "append"))
(setq concatKeywords (split "concat"))
(setq setKeywords (split "set"))
(setq deffunKeywords (split "deffun"))
(setq forKeywords (split "for"))
(setq ifKeywords (split "if"))
(setq exitKeywords (split "exit"))
(setq loadKeywords (split "load"))
(setq dispKeywords (split "disp"))
(setq trueKeywords (split "true"))
(setq falseKeywords (split "false"))
(setq andKeywords (split "and"))
(setq orKeywords (split "or"))
(setq notKeywords (split "not"))
(setq equalKeywords (split "equal"))
(setq lessKeywords (split "less"))
(setq nilKeywords (split "nil"))
; collect operators
(setf operators '("(" "+" "-" "/" "*" "\"" "," ")"))
;(setq keywords '("AND" "OR" "NOT" "EQUAL" "LESS" "NIL" "LIST" "APPEND" "CONCAT" "SET" "DEFFUN" "FOR" "IF" "EXIT" "LOAD" "DISP" "TRUE" "FALSE"))
(setf prin1Operators '( "OP_OP"  "OP_PLUS" "OP_MINUS" "OP_DIV" "OP_MULT" "OP_OC" "OP_COMMA" "OP_CP"))
(setq operatorFlag 0)
(setq deffunFlag 1)(setq ifFlag 0)(setq equalFlag 0)(setq lessFlag 0)(setq andFlag 0)
(setq orFlag 0)(setq notFlag 0)(setq forFlag 0)(setq exitFlag 0)(setq loadFlag 0)(setq dispFlag 0)(setq trueFlag 0)
(setq falseFlag 0)(setq nilFlag 0)(setq listFlag 0)(setq appendFlag 0)(setq concatFlag 0)(setq setFlag 0)(setq counter 0)
(setq valueFlag 0)(setq identifierFlag 0)(setf floatValue nil)
(setq cCounter 0)
(setf flag t)(setf exitLoopFlag nil)(setf strFlag nil)

; read from file in lisp 
(defun read-from-file (filename)
    (with-open-file (stream filename :direction :input)
    (loop for line = (read-line stream nil)
            while line
            collect line)))


(defun concatString (list)
  (if (listp list)
      (let ((result ""))
        (dolist (item list)
          (if (stringp item)
              (setq result (concatenate 'string result item))))
        result)))
; find substring in string
(defun find-substring (string substring)
    (let ((index (search substring string)))
        (if index
            (values index (1+ index))
            nil)))
; search for values in words
(defun search-values (word returnedList) 
    (loop for i from 0 to (1- (length word))
        do (when (not exitLoopFlag)
            (setq operatorFlag 0) (setf flag t)(setf strFlag nil) 
            (loop for j from 0 to (1- (length operators))
                do (when (equal (elt word i) (elt operators j))
                    (cond 
                        ((and (equal (elt word i) "*") (equal (elt word (1+ i)) "*")) (setq returnedList (append returnedList (list (list "**" "OP_DBLMULT")))) (setf flag nil)(setq operatorFlag 1))
                        ((and (equal (elt word i) "*") (equal (elt word (1- i)) "*")) (setf flag nil))
                        ((equal (elt word i) "\"") (setq cCounter (1+ cCounter)) (when (equal (mod cCounter 2) 0) (setq returnedList (append returnedList (list (list "\"" "OP_CC"))))) (when (equal (mod cCounter 2) 1) (setq returnedList (append returnedList (list (list "\"" "OP_OC"))))) (setf flag nil)(setq operatorFlag 1))
                        (flag (setq returnedList (append returnedList (list (list (elt word i) (elt prin1Operators j)))))(setq operatorFlag 1))
                        
                    )    
                )
            )
            (when (and (equal operatorFlag 0))
                ;; check deffun keywords
                (when (and (equal deffunFlag 1) (not (equal counter (length deffunKeywords))))
                    (setq deffunEqual (equal (elt word i) (elt deffunKeywords counter)))
                    (when deffunEqual
                        (setq deffunFlag 1)
                        (setq counter (1+ counter))
                    )
                    (when (not deffunEqual)
                        (setq deffunFlag 0)
                    )
                )
                (when (and (equal deffunFlag 1) (equal counter (length deffunKeywords)))
                    (setf valueStr (equal (elt word (1+ i)) "\""))
                    (setf spaceStr (equal (elt word (1+ i)) " "))
                    (cond
                        (valueStr (setq returnedList (append returnedList (list (list "DEFFUN" "VALUESTR")))) )
                        (spaceStr (setq returnedList (append returnedList (list (list "DEFFUN" "KW_DEFFUN")))) )
                        ((and (not spaceStr) (not valueStr))  (setq deffunFlag 0))
         
                    )
                    (setq counter 0)
                    
                )
                (when (and (equal deffunFlag 0)(equal equalFlag 0)(equal ifFlag 0)(equal lessFlag 0)(equal andFlag 0)(equal orFlag 0)(equal notFlag 0)(equal forFlag 0)(equal exitFlag 0)(equal loadFlag 0)(equal dispFlag 0)(equal trueFlag 0)(equal falseFlag 0)(equal nilFlag 0)(equal listFlag 0)(equal appendFlag 0)(equal concatFlag 0)(equal setFlag 0)(equal valueFlag 0)(equal identifierFlag 0))


                    (setq i (- i counter))
                    (setq counter 0)
                    (setq ifFlag 1)
                )
                ;; check if keywords
                (when (and (equal ifFlag 1) (not (equal counter (length ifKeywords))))
                    (setq ifEqual (equal (elt word i) (elt ifKeywords counter)))
                    (when ifEqual
                        (setq ifFlag 1)
                        (setq counter (1+ counter))
                    )
                    (when (not ifEqual)
                        (setq ifFlag 0)
                    )
                )
                (when (and (equal ifFlag 1) (equal counter (length ifKeywords)))
                    (setf valueStr (equal (elt word (1+ i)) "\""))
                    (setf spaceStr (equal (elt word (1+ i)) " "))
                    (cond
                        (valueStr (setq returnedList (append returnedList (list (list "IF" "VALUESTR")))) )
                        (spaceStr (setq returnedList (append returnedList (list (list "IF" "KW_IF")))) )
                        ((and (not spaceStr) (not valueStr))  (setq ifFlag 0))
         
                    )
                    (setq counter 0)
                    
                )
                (when (and (equal deffunFlag 0)(equal equalFlag 0)(equal ifFlag 0)(equal lessFlag 0)(equal andFlag 0)(equal orFlag 0)(equal notFlag 0)(equal forFlag 0)(equal exitFlag 0)(equal loadFlag 0)(equal dispFlag 0)(equal trueFlag 0)(equal falseFlag 0)(equal nilFlag 0)(equal listFlag 0)(equal appendFlag 0)(equal concatFlag 0)(equal setFlag 0)(equal valueFlag 0)(equal identifierFlag 0))
                    (setq i (- i counter))
                    (setq counter 0)
                    (setq equalFlag 1)
                )
                ;; check equal keywords
                (when (and (equal equalFlag 1) (not (equal counter (length equalKeywords))))
                    (setq equalEqual (equal (elt word i) (elt equalKeywords counter)))
                    (when equalEqual
                        (setq equalFlag 1)
                        (setq counter (1+ counter))
                    )
                    (when (not equalEqual)
                        (setq equalFlag 0)
                    )
                )
                (when (and (equal equalFlag 1) (equal counter (length equalKeywords)))
                    (setf valueStr (equal (elt word (1+ i)) "\""))
                    (setf spaceStr (equal (elt word (1+ i)) " "))
                    (cond
                        (valueStr (setq returnedList (append returnedList (list (list "EQUAL" "VALUESTR")))) )
                        (spaceStr (setq returnedList (append returnedList (list (list "EQUAL" "KW_EQUAL")))) )
                        ((and (not spaceStr) (not valueStr))  (setq equalFlag 0))
         
                    )
                    (setq counter 0)
                    
                )
                (when (and (equal deffunFlag 0)(equal equalFlag 0)(equal ifFlag 0)(equal lessFlag 0)(equal andFlag 0)(equal orFlag 0)(equal notFlag 0)(equal forFlag 0)(equal exitFlag 0)(equal loadFlag 0)(equal dispFlag 0)(equal trueFlag 0)(equal falseFlag 0)(equal nilFlag 0)(equal listFlag 0)(equal appendFlag 0)(equal concatFlag 0)(equal setFlag 0)(equal valueFlag 0)(equal identifierFlag 0))
                    (setq i (- i counter))
                    (setq counter 0)
                    (setq lessFlag 1)
                )
                ;; check less keywords
                (when (and (equal lessFlag 1) (not (equal counter (length lessKeywords))))
                    (setq lessEqual (equal (elt word i) (elt lessKeywords counter)))
                    (when lessEqual
                        (setq lessFlag 1)
                        (setq counter (1+ counter))
                    )
                    (when (not lessEqual)
                        (setq lessFlag 0)
                    )
                )
                (when (and (equal lessFlag 1) (equal counter (length lessKeywords)))
                    (setf valueStr (equal (elt word (1+ i)) "\""))
                    (setf spaceStr (equal (elt word (1+ i)) " "))
                    (cond
                        (valueStr (setq returnedList (append returnedList (list (list "LESS" "VALUESTR")))) )
                        (spaceStr (setq returnedList (append returnedList (list (list "LESS" "KW_LESS")))) )
                        ((and (not spaceStr) (not valueStr))  (setq lessFlag 0))
         
                    )
                    (setq counter 0)
                    
                )
                (when (and (equal deffunFlag 0)(equal equalFlag 0)(equal ifFlag 0)(equal lessFlag 0)(equal andFlag 0)(equal orFlag 0)(equal notFlag 0)(equal forFlag 0)(equal exitFlag 0)(equal loadFlag 0)(equal dispFlag 0)(equal trueFlag 0)(equal falseFlag 0)(equal nilFlag 0)(equal listFlag 0)(equal appendFlag 0)(equal concatFlag 0)(equal setFlag 0)(equal valueFlag 0)(equal identifierFlag 0))
                    (setq i (- i counter))
                    (setq counter 0)
                    (setq andFlag 1)
                )
                ;; check and keywords
                (when (and (equal andFlag 1) (not (equal counter (length andKeywords))))
                    (setq andEqual (equal (elt word i) (elt andKeywords counter)))
                    (when andEqual
                        (setq andFlag 1)
                        (setq counter (1+ counter))
                    )
                    (when (not andEqual)
                        (setq andFlag 0)
                    )
                )
                (when (and (equal andFlag 1) (equal counter (length andKeywords)))
                    (setf valueStr (equal (elt word (1+ i)) "\""))
                    (setf spaceStr (equal (elt word (1+ i)) " "))
                    (cond
                        (valueStr (setq returnedList (append returnedList (list (list "AND" "VALUESTR")))) )
                        (spaceStr (setq returnedList (append returnedList (list (list "AND" "KW_AND")))) )
                        ((and (not spaceStr) (not valueStr))  (setq andFlag 0))
         
                    )
                    (setq counter 0)
                    
                )
                (when (and (equal deffunFlag 0)(equal equalFlag 0)(equal ifFlag 0)(equal lessFlag 0)(equal andFlag 0)(equal orFlag 0)(equal notFlag 0)(equal forFlag 0)(equal exitFlag 0)(equal loadFlag 0)(equal dispFlag 0)(equal trueFlag 0)(equal falseFlag 0)(equal nilFlag 0)(equal listFlag 0)(equal appendFlag 0)(equal concatFlag 0)(equal setFlag 0)(equal valueFlag 0)(equal identifierFlag 0))
                    (setq i (- i counter))
                    (setq counter 0)
                    (setq orFlag 1)
                )
                ;; check or keywords
                (when (and (equal orFlag 1) (not (equal counter (length orKeywords))))
                    (setq orEqual (equal (elt word i) (elt orKeywords counter)))
                    (when orEqual
                        (setq orFlag 1)
                        (setq counter (1+ counter))
                    )
                    (when (not orEqual)
                        (setq orFlag 0)
                    )
                )
                (when (and (equal orFlag 1) (equal counter (length orKeywords)))
                    (setf valueStr (equal (elt word (1+ i)) "\""))
                    (setf spaceStr (equal (elt word (1+ i)) " "))
                    (cond
                        (valueStr (setq returnedList (append returnedList (list (list "OR" "VALUESTR")))) )
                        (spaceStr (setq returnedList (append returnedList (list (list "OR" "KW_OR")))) )
                        ((and (not spaceStr) (not valueStr))  (setq orFlag 0))
         
                    )
                    (setq counter 0)
                    
                )
                (when (and (equal deffunFlag 0)(equal equalFlag 0)(equal ifFlag 0)(equal lessFlag 0)(equal andFlag 0)(equal orFlag 0)(equal notFlag 0)(equal forFlag 0)(equal exitFlag 0)(equal loadFlag 0)(equal dispFlag 0)(equal trueFlag 0)(equal falseFlag 0)(equal nilFlag 0)(equal listFlag 0)(equal appendFlag 0)(equal concatFlag 0)(equal setFlag 0)(equal valueFlag 0)(equal identifierFlag 0))
                    (setq i (- i counter))
                    (setq counter 0)
                    (setq notFlag 1)
                )
                ;; check not keywords
                (when (and (equal notFlag 1) (not (equal counter (length notKeywords))))
                    (setq notEqual (equal (elt word i) (elt notKeywords counter)))
                    (when notEqual
                        (setq notFlag 1)
                        (setq counter (1+ counter))
                    )
                    (when (not notEqual)
                        (setq notFlag 0)
                    )
                )
                (when (and (equal notFlag 1) (equal counter (length notKeywords)))
                    (setf valueStr (equal (elt word (1+ i)) "\""))
                    (setf spaceStr (equal (elt word (1+ i)) " "))
                    (cond
                        (valueStr (setq returnedList (append returnedList (list (list "NOT" "VALUESTR")))) )
                        (spaceStr (setq returnedList (append returnedList (list (list "NOT" "KW_NOT")))) )
                        ((and (not spaceStr) (not valueStr))  (setq notFlag 0))
         
                    )
                    (setq counter 0)
                    
                )
                (when (and (equal deffunFlag 0)(equal equalFlag 0)(equal ifFlag 0)(equal lessFlag 0)(equal andFlag 0)(equal orFlag 0)(equal notFlag 0)(equal forFlag 0)(equal exitFlag 0)(equal loadFlag 0)(equal dispFlag 0)(equal trueFlag 0)(equal falseFlag 0)(equal nilFlag 0)(equal listFlag 0)(equal appendFlag 0)(equal concatFlag 0)(equal setFlag 0)(equal valueFlag 0)(equal identifierFlag 0))
                    (setq i (- i counter))
                    (setq counter 0)
                    (setq trueFlag 1)
                )
                ;; check true keywords
                (when (and (equal trueFlag 1) (not (equal counter (length trueKeywords))))
                    (setq trueEqual (equal (elt word i) (elt trueKeywords counter)))
                    (when trueEqual
                        (setq trueFlag 1)
                        (setq counter (1+ counter))
                    )
                    (when (not trueEqual)
                        (setq trueFlag 0)
                    )
                )
                (when (and (equal trueFlag 1) (equal counter (length trueKeywords)))
                    (setf valueStr (equal (elt word (1+ i)) "\""))
                    (setf spaceStr (equal (elt word (1+ i)) " "))
                    (cond
                        (valueStr (setq returnedList (append returnedList (list (list "TRUE" "VALUESTR")))) )
                        (spaceStr (setq returnedList (append returnedList (list (list "TRUE" "KW_TRUE")))) )
                        ((and (not spaceStr) (not valueStr))  (setq trueFlag 0))
         
                    )
                    (setq counter 0)
                    
                )
                (when (and (equal deffunFlag 0)(equal equalFlag 0)(equal ifFlag 0)(equal lessFlag 0)(equal andFlag 0)(equal orFlag 0)(equal notFlag 0)(equal forFlag 0)(equal exitFlag 0)(equal loadFlag 0)(equal dispFlag 0)(equal trueFlag 0)(equal falseFlag 0)(equal nilFlag 0)(equal listFlag 0)(equal appendFlag 0)(equal concatFlag 0)(equal setFlag 0)(equal valueFlag 0)(equal identifierFlag 0))
                    (setq i (- i counter))
                    (setq counter 0)
                    (setq falseFlag 1)
                )
                ;; check false keywords
                (when (and (equal falseFlag 1) (not (equal counter (length falseKeywords))))
                    (setq falseEqual (equal (elt word i) (elt falseKeywords counter)))
                    (when falseEqual
                        (setq falseFlag 1)
                        (setq counter (1+ counter))
                    )
                    (when (not falseEqual)
                        (setq falseFlag 0)
                    )
                )
                (when (and (equal falseFlag 1) (equal counter (length falseKeywords)))
                    (setf valueStr (equal (elt word (1+ i)) "\""))
                    (setf spaceStr (equal (elt word (1+ i)) " "))
                    (cond
                        (valueStr (setq returnedList (append returnedList (list (list "FALSE" "VALUESTR")))) )
                        (spaceStr (setq returnedList (append returnedList (list (list "FALSE" "KW_FALSE")))) )
                        ((and (not spaceStr) (not valueStr))  (setq falseFlag 0))
         
                    )
                    (setq counter 0)
                    
                )
                (when (and (equal deffunFlag 0)(equal equalFlag 0)(equal ifFlag 0)(equal lessFlag 0)(equal andFlag 0)(equal orFlag 0)(equal notFlag 0)(equal forFlag 0)(equal exitFlag 0)(equal loadFlag 0)(equal dispFlag 0)(equal trueFlag 0)(equal falseFlag 0)(equal nilFlag 0)(equal listFlag 0)(equal appendFlag 0)(equal concatFlag 0)(equal setFlag 0)(equal valueFlag 0)(equal identifierFlag 0))
                    (setq i (- i counter))
                    (setq counter 0)
                    (setq nilFlag 1)
                )
                ;; check nil keywords
                (when (and (equal nilFlag 1) (not (equal counter (length nilKeywords))))
                    (setq nilEqual (equal (elt word i) (elt nilKeywords counter)))
                    (when nilEqual
                        (setq nilFlag 1)
                        (setq counter (1+ counter))
                    )
                    (when (not nilEqual)
                        (setq nilFlag 0)
                    )
                )
                (when (and (equal nilFlag 1) (equal counter (length nilKeywords)))
                    (setf valueStr (equal (elt word (1+ i)) "\""))
                    (setf spaceStr (equal (elt word (1+ i)) " "))
                    (cond
                        (valueStr (setq returnedList (append returnedList (list (list "NIL" "VALUESTR")))) )
                        (spaceStr (setq returnedList (append returnedList (list (list "NIL" "KW_NIL")))) )
                        ((and (not spaceStr) (not valueStr))  (setq nilFlag 0))
         
                    )
                    (setq counter 0)
                    
                )
                (when (and (equal deffunFlag 0)(equal equalFlag 0)(equal ifFlag 0)(equal lessFlag 0)(equal andFlag 0)(equal orFlag 0)(equal notFlag 0)(equal forFlag 0)(equal exitFlag 0)(equal loadFlag 0)(equal dispFlag 0)(equal trueFlag 0)(equal falseFlag 0)(equal nilFlag 0)(equal listFlag 0)(equal appendFlag 0)(equal concatFlag 0)(equal setFlag 0)(equal valueFlag 0)(equal identifierFlag 0))
                    (setq i (- i counter))
                    (setq counter 0)
                    (setq dispFlag 1)
                )
                ;; check disp keywords
                (when (and (equal dispFlag 1) (not (equal counter (length dispKeywords))))
                    (setq dispEqual (equal (elt word i) (elt dispKeywords counter)))
                    (when dispEqual
                        (setq dispFlag 1)
                        (setq counter (1+ counter))
                    )
                    (when (not dispEqual)
                        (setq dispFlag 0)
                    )
                )
                (when (and (equal dispFlag 1) (equal counter (length dispKeywords)))
                    (setf valueStr (equal (elt word (1+ i)) "\""))
                    (setf spaceStr (equal (elt word (1+ i)) " "))
                    (cond
                        (valueStr (setq returnedList (append returnedList (list (list "DISP" "VALUESTR")))) )
                        (spaceStr (setq returnedList (append returnedList (list (list "DISP" "KW_DISP")))) )
                        ((and (not spaceStr) (not valueStr))  (setq dispFlag 0))
         
                    )
                    (setq counter 0)
                    
                )
                (when (and (equal deffunFlag 0)(equal equalFlag 0)(equal ifFlag 0)(equal lessFlag 0)(equal andFlag 0)(equal orFlag 0)(equal notFlag 0)(equal forFlag 0)(equal exitFlag 0)(equal loadFlag 0)(equal dispFlag 0)(equal trueFlag 0)(equal falseFlag 0)(equal nilFlag 0)(equal listFlag 0)(equal appendFlag 0)(equal concatFlag 0)(equal setFlag 0)(equal valueFlag 0)(equal identifierFlag 0))
                    (setq i (- i counter))
                    (setq counter 0)
                    (setq exitFlag 1)
                )
                ;; check exit keywords
                (when (and (equal exitFlag 1) (not (equal counter (length exitKeywords))))
                    (setq exitEqual (equal (elt word i) (elt exitKeywords counter)))
                    (when exitEqual
                        (setq exitFlag 1)
                        (setq counter (1+ counter))
                    )
                    (when (not exitEqual)
                        (setq exitFlag 0)
                    )
                )
                (when (and (equal exitFlag 1) (equal counter (length exitKeywords)))
                    (setf valueStr (equal (elt word (1+ i)) "\""))
                    (setf spaceStr (equal (elt word (1+ i)) " "))
                    (cond
                        (valueStr (setq returnedList (append returnedList (list (list "EXIT" "VALUESTR")))) )
                        (spaceStr (setq returnedList (append returnedList (list (list "EXIT" "KW_EXIT")))) )
                        ((and (not spaceStr) (not valueStr))  (setq exitFlag 0))
         
                    )
                    (setq counter 0)
                    
                )
                (when (and (equal deffunFlag 0)(equal equalFlag 0)(equal ifFlag 0)(equal lessFlag 0)(equal andFlag 0)(equal orFlag 0)(equal notFlag 0)(equal forFlag 0)(equal exitFlag 0)(equal loadFlag 0)(equal dispFlag 0)(equal trueFlag 0)(equal falseFlag 0)(equal nilFlag 0)(equal listFlag 0)(equal appendFlag 0)(equal concatFlag 0)(equal setFlag 0)(equal valueFlag 0)(equal identifierFlag 0))
                    (setq i (- i counter))
                    (setq counter 0)
                    (setq loadFlag 1)
                )
                ;; check load keywords
                (when (and (equal loadFlag 1) (not (equal counter (length loadKeywords))))
                    (setq loadEqual (equal (elt word i) (elt loadKeywords counter)))
                    (when loadEqual
                        (setq loadFlag 1)
                        (setq counter (1+ counter))
                    )
                    (when (not loadEqual)
                        (setq loadFlag 0)
                    )
                )
                (when (and (equal loadFlag 1) (equal counter (length loadKeywords)))
                    (setf valueStr (equal (elt word (1+ i)) "\""))
                    (setf spaceStr (equal (elt word (1+ i)) " "))
                    (cond
                        (valueStr (setq returnedList (append returnedList (list (list "LOAD" "VALUESTR")))) )
                        (spaceStr (setq returnedList (append returnedList (list (list "LOAD" "KW_LOAD")))) )
                        ((and (not spaceStr) (not valueStr))  (setq loadFlag 0))
         
                    )
                    (setq counter 0)
                    
                )
                (when (and (equal deffunFlag 0)(equal equalFlag 0)(equal ifFlag 0)(equal lessFlag 0)(equal andFlag 0)(equal orFlag 0)(equal notFlag 0)(equal forFlag 0)(equal exitFlag 0)(equal loadFlag 0)(equal dispFlag 0)(equal trueFlag 0)(equal falseFlag 0)(equal nilFlag 0)(equal listFlag 0)(equal appendFlag 0)(equal concatFlag 0)(equal setFlag 0)(equal valueFlag 0)(equal identifierFlag 0))
                    (setq i (- i counter))
                    (setq counter 0)
                    (setq concatFlag 1)
                )
                ;; check concat keywords
                (when (and (equal concatFlag 1) (not (equal counter (length concatKeywords))))
                    (setq concatEqual (equal (elt word i) (elt concatKeywords counter)))
                    (when concatEqual
                        (setq concatFlag 1)
                        (setq counter (1+ counter))
                    )
                    (when (not concatEqual)
                        (setq concatFlag 0)
                    )
                )
                (when (and (equal concatFlag 1) (equal counter (length concatKeywords)))
                    (setf valueStr (equal (elt word (1+ i)) "\""))
                    (setf spaceStr (equal (elt word (1+ i)) " "))
                    (cond
                        (valueStr (setq returnedList (append returnedList (list (list "CONCAT" "VALUESTR")))) )
                        (spaceStr (setq returnedList (append returnedList (list (list "CONCAT" "KW_CONCAT")))) )
                        ((and (not spaceStr) (not valueStr))  (setq concatFlag 0))
         
                    )
                    (setq counter 0)
                    
                )
                (when (and (equal deffunFlag 0)(equal equalFlag 0)(equal ifFlag 0)(equal lessFlag 0)(equal andFlag 0)(equal orFlag 0)(equal notFlag 0)(equal forFlag 0)(equal exitFlag 0)(equal loadFlag 0)(equal dispFlag 0)(equal trueFlag 0)(equal falseFlag 0)(equal nilFlag 0)(equal listFlag 0)(equal appendFlag 0)(equal concatFlag 0)(equal setFlag 0)(equal valueFlag 0)(equal identifierFlag 0))
                    (setq i (- i counter))
                    (setq counter 0)
                    (setq appendFlag 1)
                )
                ;; check append keywords
                (when (and (equal appendFlag 1) (not (equal counter (length appendKeywords))))
                    (setq appendEqual (equal (elt word i) (elt appendKeywords counter)))
                    (when appendEqual
                        (setq appendFlag 1)
                        (setq counter (1+ counter))
                    )
                    (when (not appendEqual)
                        (setq appendFlag 0)
                    )
                )
                (when (and (equal appendFlag 1) (equal counter (length appendKeywords)))
                    (setf valueStr (equal (elt word (1+ i)) "\""))
                    (setf spaceStr (equal (elt word (1+ i)) " "))
                    (cond
                        (valueStr (setq returnedList (append returnedList (list (list "APPEND" "VALUESTR")))) )
                        (spaceStr (setq returnedList (append returnedList (list (list "APPEND" "KW_APPEND")))) )
                        ((and (not spaceStr) (not valueStr))  (setq appendFlag 0))
         
                    )
                    (setq counter 0)
                    
                )
                (when (and (equal deffunFlag 0)(equal equalFlag 0)(equal ifFlag 0)(equal lessFlag 0)(equal andFlag 0)(equal orFlag 0)(equal notFlag 0)(equal forFlag 0)(equal exitFlag 0)(equal loadFlag 0)(equal dispFlag 0)(equal trueFlag 0)(equal falseFlag 0)(equal nilFlag 0)(equal listFlag 0)(equal appendFlag 0)(equal concatFlag 0)(equal setFlag 0)(equal valueFlag 0)(equal identifierFlag 0))
                    (setq i (- i counter))
                    (setq counter 0)
                    (setq setFlag 1)
                )
                ;; check set keywords
                (when (and (equal setFlag 1) (not (equal counter (length setKeywords))))
                    (setq setEqual (equal (elt word i) (elt setKeywords counter)))
                    (when setEqual
                        (setq setFlag 1)
                        (setq counter (1+ counter))
                    )
                    (when (not setEqual)
                        (setq setFlag 0)
                    )
                )
                (when (and (equal setFlag 1) (equal counter (length setKeywords)))
                    (setf valueStr (equal (elt word (1+ i)) "\""))
                    (setf spaceStr (equal (elt word (1+ i)) " "))
                    (cond
                        (valueStr (setq returnedList (append returnedList (list (list "SET" "VALUESTR")))) )
                        (spaceStr (setq returnedList (append returnedList (list (list "SET" "KW_SET")))) )
                        ((and (not spaceStr) (not valueStr))  (setq setFlag 0))
         
                    )
                    (setq counter 0)
                    
                )
                (when (and (equal deffunFlag 0)(equal equalFlag 0)(equal ifFlag 0)(equal lessFlag 0)(equal andFlag 0)(equal orFlag 0)(equal notFlag 0)(equal forFlag 0)(equal exitFlag 0)(equal loadFlag 0)(equal dispFlag 0)(equal trueFlag 0)(equal falseFlag 0)(equal nilFlag 0)(equal listFlag 0)(equal appendFlag 0)(equal concatFlag 0)(equal setFlag 0)(equal valueFlag 0)(equal identifierFlag 0))
                    (setq i (- i counter))
                    (setq counter 0)
                    (setq listFlag 1)
                )
                ;; check list keywords
                (when (and (equal listFlag 1) (not (equal counter (length listKeywords))))
                    (setq listEqual (equal (elt word i) (elt listKeywords counter)))
                    (when listEqual
                        (setq listFlag 1)
                        (setq counter (1+ counter))
                    )
                    (when (not listEqual)
                        (setq listFlag 0)
                    )
                )
                (when (and (equal listFlag 1) (equal counter (length listKeywords)))
                    (setf valueStr (equal (elt word (1+ i)) "\""))
                    (setf spaceStr (equal (elt word (1+ i)) " "))
                    (cond
                        (valueStr (setq returnedList (append returnedList (list (list "LIST" "VALUESTR")))) )
                        (spaceStr (setq returnedList (append returnedList (list (list "LIST" "KW_LIST")))) )
                        ((and (not spaceStr) (not valueStr))  (setq listFlag 0))
         
                    )
                    (setq counter 0)
                    
                )
                (when (and (equal deffunFlag 0)(equal equalFlag 0)(equal ifFlag 0)(equal lessFlag 0)(equal andFlag 0)(equal orFlag 0)(equal notFlag 0)(equal forFlag 0)(equal exitFlag 0)(equal loadFlag 0)(equal dispFlag 0)(equal trueFlag 0)(equal falseFlag 0)(equal nilFlag 0)(equal listFlag 0)(equal appendFlag 0)(equal concatFlag 0)(equal setFlag 0)(equal valueFlag 0)(equal identifierFlag 0))
                    (setq i (- i counter))
                    (setq counter 0)
                    (setq forFlag 1)
                )
                ;; check for keywords
                (when (and (equal forFlag 1) (not (equal counter (length forKeywords))))
                    (setq forEqual (equal (elt word i) (elt forKeywords counter)))
                    (when forEqual
                        (setq forFlag 1)
                        (setq counter (1+ counter))
                    )
                    (when (not forEqual)
                        (setq forFlag 0)
                    )
                )
                (when (and (equal forFlag 1) (equal counter (length forKeywords)))
                    (setf valueStr (equal (elt word (1+ i)) "\""))
                    (setf spaceStr (equal (elt word (1+ i)) " "))
                    (cond
                        (valueStr (setq returnedList (append returnedList (list (list "FOR" "VALUESTR")))) )
                        (spaceStr (setq returnedList (append returnedList (list (list "FOR" "KW_FOR")))) )
                        ((and (not spaceStr) (not valueStr))  (setq forFlag 0))
         
                    )
                    (setq counter 0)
                    
                )
                (when (and (equal deffunFlag 0)(equal equalFlag 0)(equal ifFlag 0)(equal lessFlag 0)(equal andFlag 0)(equal orFlag 0)(equal notFlag 0)(equal forFlag 0)(equal exitFlag 0)(equal loadFlag 0)(equal dispFlag 0)(equal trueFlag 0)(equal falseFlag 0)(equal nilFlag 0)(equal listFlag 0)(equal appendFlag 0)(equal concatFlag 0)(equal setFlag 0)(equal valueFlag 0)(equal identifierFlag 0))
                    (setq i (- i counter))
                    (setq counter 0)
                    (setq valueFlag 1)
                )
                ;; check value
                (when (and (equal valueFlag 1))
                    (setf digitValue (every #'digit-char-p (elt word i)))
                    (when (not digitValue)
                        (setf floatValue (and (equal (elt word i) "f") (not (equal counter 0))))
                    )
                    (setf b (or digitValue floatValue))
                    (when b
                        (setq valueFlag 1)
                        (setq counter (1+ counter))
                    )
                    (when (not b)
                        (setq valueFlag 0)
                    )
                )
                (when (and (equal valueFlag 1) (or (equal (elt word (1+ i)) " ") (equal (elt word (1+ i)) ")") (equal (elt word (1+ i)) "\"")))
                    (when (equal (elt word (1+ i)) "\"")
                        (setf strFlag t)
                    )                    
                    (setq valueList (list ""))
                    (loop for k from (- i (1- counter)) to i do
                        ;(prin1 (elt word k))
                        (setq valueList (append valueList (list (elt word k))))
                    )
                    (setq valueListTemp (cdr valueList))                
                    (cond
                        (strFlag (setq returnedList (append returnedList (list (list (concatString valueListTemp) "VALUESTR")))) )
                        ((and floatValue (not exitLoopFlag)) (setq returnedList (append returnedList (list (list (concatString valueListTemp) "VALUEF")))) )
                        ((and (not floatValue) (not exitLoopFlag)) (setq returnedList (append returnedList (list (list (concatString valueListTemp) "VALUEI")))) )
                    )
                    (setq counter 0)
                )
                (when (and (equal deffunFlag 0)(equal equalFlag 0)(equal ifFlag 0)(equal lessFlag 0)(equal andFlag 0)(equal orFlag 0)(equal notFlag 0)(equal forFlag 0)(equal exitFlag 0)(equal loadFlag 0)(equal dispFlag 0)(equal trueFlag 0)(equal falseFlag 0)(equal nilFlag 0)(equal listFlag 0)(equal appendFlag 0)(equal concatFlag 0)(equal setFlag 0)(equal valueFlag 0)(equal identifierFlag 0))
                    
                    (setq i (- i counter))
                    (setq counter 0)
                    (setq identifierFlag 1)
                )
                ;; check identifier
                (when (and (equal identifierFlag 1))
                
                    (setf b (or (every #'alpha-char-p (elt word i)) (equal (elt word i) "_")))
                    (when b
                        (setq identifierFlag 1)
                        (setq counter (1+ counter))
                        ;(prin1 (elt word (1+ i)))    
                    )
                    (when (not b)
                        (setq identifierFlag 0)
                    )
                    
                )
                
                (when (and (equal identifierFlag 1) (or (equal (elt word (1+ i)) " ") (equal (elt word (1+ i)) ")") (equal (elt word (1+ i)) "\"")))
                    
                    (when (equal (elt word (1+ i)) "\"")
                        (setf strFlag t) 
                    )
                    
                    (setq identifierList (list ""))
                    (loop for k from (- i (1- counter)) to i do
                        
                        (setq identifierList (append identifierList (list (elt word k))))
                       
                    )
                    
                    (setq identiferListTemp (cdr identifierList))
                    (when (< i counter)
                        (setq i counter)
                    )
                    (when (and (every #'digit-char-p (elt word (- i counter))) (not strFlag)  )
                        
                        ;;(prin1 (list (concatString identiferListTemp) "LEXICAL ERROR: IDENTIFIER CONTAINS A NUMBER"))
                        (setq returnedList (append returnedList (list (list (concatString identiferListTemp) "LEXICAL ERROR: IDENTIFIER CANNOT START WITH A NUMBER"))))
                        (setf exitLoopFlag t)
                        (setq counter 0)
                        (setq deffunFlag 1)(setq ifFlag 0)(setq equalFlag 0)(setq lessFlag 0)(setq andFlag 0)
                        (setq orFlag 0)(setq notFlag 0)(setq forFlag 0)(setq exitFlag 0)(setq loadFlag 0)(setq dispFlag 0)(setq trueFlag 0)
                        (setq falseFlag 0)(setq nilFlag 0)(setq listFlag 0)(setq appendFlag 0)(setq concatFlag 0)(setq setFlag 0)(setq counter 0)
                        (setq valueFlag 0)(setq identifierFlag 0)
                    )
                    ;;(prin1 (list (concatString identiferListTemp) "IDENTIFIER"))
                    (cond 
                        ((and strFlag (not exitLoopFlag)) (setq returnedList (append returnedList (list (list (concatString identiferListTemp) "VALUESTR")))) )
                        ((not exitLoopFlag) (setq returnedList (append returnedList (list (list (concatString identiferListTemp) "IDENTIFIER")))) )
                    )
                    ;;(setq returnedList (append returnedList (list (list (concatString identiferListTemp) "IDENTIFIER"))))
                    (setq counter 0)
                )
                (when (and (equal deffunFlag 0)(equal equalFlag 0)(equal ifFlag 0)(equal lessFlag 0)(equal andFlag 0)(equal orFlag 0)(equal notFlag 0)(equal forFlag 0)(equal exitFlag 0)(equal loadFlag 0)(equal dispFlag 0)(equal trueFlag 0)(equal falseFlag 0)(equal nilFlag 0)(equal listFlag 0)(equal appendFlag 0)(equal concatFlag 0)(equal setFlag 0)(equal valueFlag 0)(equal identifierFlag 0))
                    
                    (setq i (- i counter))
                    (setq counter 0)
                    (setq deffunFlag 1)(setq ifFlag 0)(setq equalFlag 0)(setq lessFlag 0)(setq andFlag 0)
                    (setq orFlag 0)(setq notFlag 0)(setq forFlag 0)(setq exitFlag 0)(setq loadFlag 0)(setq dispFlag 0)(setq trueFlag 0)
                    (setq falseFlag 0)(setq nilFlag 0)(setq listFlag 0)(setq appendFlag 0)(setq concatFlag 0)(setq setFlag 0)(setq counter 0)
                    (setq valueFlag 0)(setq identifierFlag 0)
                )
            )
        )           
    )
    returnedList
)

(defun gppinterpreter(fileContent)
    (setq returnedList (list ""))
    ; parse file content split by space
    (loop for line in fileContent do
        ; comment line check
        (when (find-substring line ";;")
            ;;(prin1 (list line "COMMENT"))
            (setq returnedList (append returnedList (list (list line "COMMENT"))))
        )
        (when (not (find-substring line ";;"))
            
            (setq words (split line))
            (setq words (append words (list " ")))
            ;; value check
            (setq returnedList (search-values words returnedList))
        )
    )
    (setq returnedList (cdr returnedList))
    returnedList
)

(defun main ()
    (when *args*
        (setq filename (elt *args* 0))
        ; call file read function
        (setq fileContent (read-from-file filename))
        ; call gppinterpreter function
        (print (gppinterpreter fileContent))
    )
    (when (not *args*)
        (princ "Enter input string: ")
        
        (setq inputString (read-line))
        (print (gppinterpreter (list inputString)))
    )

)
;; call main function
(main)









