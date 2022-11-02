; define file name
(setq filename "file.txt")

; split function
(defun split (string)
    (loop for i from 0 to (1- (length string))
        collect (subseq string i (1+ i))))
; split keywords
(setq listKeywords (split "list "))
(setq appendKeywords (split "append "))
(setq concatKeywords (split "concat "))
(setq setKeywords (split "set "))
(setq deffunKeywords (split "deffun "))
(setq forKeywords (split "for "))
(setq ifKeywords (split "if "))
(setq exitKeywords (split "exit "))
(setq loadKeywords (split "load "))
(setq dispKeywords (split "disp "))
(setq trueKeywords (split "true "))
(setq falseKeywords (split "false "))
(setq andKeywords (split "and "))
(setq orKeywords (split "or "))
(setq notKeywords (split "not "))
(setq equalKeywords (split "equal "))
(setq lessKeywords (split "less "))
(setq nilKeywords (split "nil "))
; collect operators
(setf operators '("(" "+" "-" "/" "*"   "**" "\"" "\"" "," ")"))
(setf printOperators '( "OP_OP"  "OP_PLUS" "OP_MINUS" "OP_DIV" "OP_MULT" "OP_DBLMULT" "OP_OC" "OP_CC" "OP_COMMA" "OP_CP"))
(setq operatorFlag 0)
(setq deffunFlag 1)(setq ifFlag 0)(setq equalFlag 0)(setq lessFlag 0)(setq andFlag 0)
(setq orFlag 0)(setq notFlag 0)(setq forFlag 0)(setq exitFlag 0)(setq loadFlag 0)(setq dispFlag 0)(setq trueFlag 0)
(setq falseFlag 0)(setq nilFlag 0)(setq listFlag 0)(setq appendFlag 0)(setq concatFlag 0)(setq setFlag 0)(setq counter 0)
(setq valueFlag 0)(setq identifierFlag 0)

; read from file in lisp 
(defun read-from-file (filename)
    (with-open-file (stream filename :direction :input)
    (loop for line = (read-line stream nil)
            while line
            collect line)))

; find substring in string
(defun find-substring (string substring)
    (let ((index (search substring string)))
        (if index
            (values index (1+ index))
            nil)))
; search for values in words
(defun search-values (word) 
    (loop for i from 0 to (1- (length word))
        do(setq operatorFlag 0)
        do (loop for j from 0 to (1- (length operators))
            do (when (equal (elt word i) (elt operators j))
                (format t "~a~%" (elt printOperators j))
                    (setq operatorFlag 1)
                )
        )
        do (when (and (equal operatorFlag 0))
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
                ;; (setq a (elt word (1+ i)))
                ;; (when (and (equal counter (length deffunKeywords)) (not (equal a " ")))
                ;;     (print a)
                ;;     (setq deffunFlag 0)
                ;;     ;;(setq i (1+ i))
                ;; )
            )
            (when (and (equal deffunFlag 1) (equal counter (length deffunKeywords)))
                ;; (print a)
                (format t "~a~%" "KW_DEFFUN")
                (setq counter 0)
            )
            (when (and (equal deffunFlag 0)(equal equalFlag 0)(equal ifFlag 0)(equal lessFlag 0)(equal andFlag 0)(equal orFlag 0)(equal notFlag 0)(equal forFlag 0)(equal exitFlag 0)(equal loadFlag 0)(equal dispFlag 0)(equal trueFlag 0)(equal falseFlag 0)(equal nilFlag 0)(equal listFlag 0)(equal appendFlag 0)(equal concatFlag 0)(equal setFlag 0)(equal valueFlag 0)(equal identifierFlag 0))

                ;;(print "SA")
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
                (format t "~a~%" "KW_IF")
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
                (format t "~a~%" "KW_EQUAL")
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
                (format t "~a~%" "KW_LESS")
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
                (format t "~a~%" "KW_AND")
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
                (format t "~a~%" "KW_OR")
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
                (format t "~a~%" "KW_NOT")
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
                (format t "~a~%" "KW_TRUE")
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
                (format t "~a~%" "KW_FALSE")
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
                (format t "~a~%" "KW_NIL")
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
                (format t "~a~%" "KW_DISP")
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
                (format t "~a~%" "KW_EXIT")
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
                (format t "~a~%" "KW_LOAD")
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
                (format t "~a~%" "KW_CONCAT")
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
                (format t "~a~%" "KW_APPEND")
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
                (format t "~a~%" "KW_SET")
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
                (format t "~a~%" "KW_LIST")
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
                (format t "~a~%" "KW_FOR")
                (setq counter 0)
            )
            (when (and (equal deffunFlag 0)(equal equalFlag 0)(equal ifFlag 0)(equal lessFlag 0)(equal andFlag 0)(equal orFlag 0)(equal notFlag 0)(equal forFlag 0)(equal exitFlag 0)(equal loadFlag 0)(equal dispFlag 0)(equal trueFlag 0)(equal falseFlag 0)(equal nilFlag 0)(equal listFlag 0)(equal appendFlag 0)(equal concatFlag 0)(equal setFlag 0)(equal valueFlag 0)(equal identifierFlag 0))
                (setq i (- i counter))
                (setq counter 0)
                (setq valueFlag 1)
            )
            ;; check value
            (when (and (equal valueFlag 1))
                (setf b (every #'digit-char-p (elt word i)))
                (when b
                    (setq valueFlag 1)
                    (setq counter (1+ counter))
                )
                (when (not b)
                    (setq valueFlag 0)
                )
            )
            (when (and (equal valueFlag 1) (or (equal (elt word (1+ i)) " ") (equal (elt word (1+ i)) ")")))
                (format t "~a~%" "VALUE")
                (setq counter 0)
            )
            (when (and (equal deffunFlag 0)(equal equalFlag 0)(equal ifFlag 0)(equal lessFlag 0)(equal andFlag 0)(equal orFlag 0)(equal notFlag 0)(equal forFlag 0)(equal exitFlag 0)(equal loadFlag 0)(equal dispFlag 0)(equal trueFlag 0)(equal falseFlag 0)(equal nilFlag 0)(equal listFlag 0)(equal appendFlag 0)(equal concatFlag 0)(equal setFlag 0)(equal valueFlag 0)(equal identifierFlag 0))
                
                (setq i (- i counter))
                (setq counter 0)
                (setq identifierFlag 1)
            )
            ;; check identifier
            (when (and (equal identifierFlag 1))
                (setf b (every #'alpha-char-p (elt word i)))
                (when b
                    (setq identifierFlag 1)
                    (setq counter (1+ counter))
                    ;(print (elt word (1+ i)))    
                )
                (when (not b)
                    (setq identifierFlag 0)
                )
            )
            (when (and (equal identifierFlag 1) (or (equal (elt word (1+ i)) " ") (equal (elt word (1+ i)) ")")))
                (format t "~a~%" "IDENTIFIER")
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
    ))
                

; call file read function
(setq fileContent (read-from-file filename))
; parse file content split by space
(loop for line in fileContent do
    (when (find-substring line ";;")
        (format t "~a~%" "COMMENT")
    )
    (when (not (find-substring line ";;"))
        (setq words (split line))
        (search-values words)
    )

)







