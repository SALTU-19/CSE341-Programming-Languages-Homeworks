; define file name
(setq filename "file.txt")
(setq keywords '("list" "append" "concat" "set" "deffun" "for" "if" "exit" "load" "disp" "true" "false" "and" "or" "not" "equal" "less" "nil" ))
(setf operators '("(" "+" "-" "/" "*"   "**" "\"" "\"" "," ")"))
(setq printKeywords '("KW_LIST" "KW_APPEND" "KW_CONCAT" "KW_SET" "KW_DEFFUN" "KW_FOR" "KW_IF" "KW_EXIT" "KW_LOAD" "KW_DISP" "KW_TRUE" "KW_FALSE" "KW_AND" "KW_OR" "KW_ NOT" "KW_EQUAL" "KW_LESS" "KW_NIL" ))
(setf printOperators '( "OP_OP"  "OP_PLUS" "OP_MINUS" "OP_DIV" "OP_MULT" "OP_DBLMULT" "OP_OC" "OP_CC" "OP_COMMA" "OP_CP"))

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
; split function
(defun split (string)
    (loop for i from 0 to (1- (length string))
        collect (subseq string i (1+ i))))
; search for operators in words
(defun search-operators (word)
    (loop for i from 0 to (1- (length word))
        do (loop for j from 0 to (1- (length operators))
            do (if (equal (elt word i) (elt operators j))
                (format t "~a~%" (elt printOperators j))))      
    ))
                

; call file read function
(setq fileContent(read-from-file filename))
; parse file content split by space
(loop for line in fileContent do 
    (setq words (split line))
    (search-operators words)
    )







