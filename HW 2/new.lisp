; define file name
(setq filename "file.txt")

; split function
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
(setf operators '("(" "+" "-" "/" "*"   "**" "\"" "\"" "," ")"))
(setq printKeywords '("KW_LIST" "KW_APPEND" "KW_CONCAT" "KW_SET" "KW_DEFFUN" "KW_FOR" "KW_IF" "KW_EXIT" "KW_LOAD" "KW_DISP" "KW_TRUE" "KW_FALSE" "KW_AND" "KW_OR" "KW_ NOT" "KW_EQUAL" "KW_LESS" "KW_NIL" ))
(setf printOperators '( "OP_OP"  "OP_PLUS" "OP_MINUS" "OP_DIV" "OP_MULT" "OP_DBLMULT" "OP_OC" "OP_CC" "OP_COMMA" "OP_CP"))
(setq operatorFlag 0)
(setq deffunFlag 1)
(setq counter 0)
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
        do (when (equal operatorFlag 0)
            (when (equal (elt word i) (elt deffunKeywords counter))
                (setq deffunFlag 1)
                (setq counter (1+ counter))
                )
            )           
    ))
                

; call file read function
(setq fileContent(read-from-file filename))
; parse file content split by space
(loop for line in fileContent do 
    (setq words (split line))
    (search-values words)
    )







