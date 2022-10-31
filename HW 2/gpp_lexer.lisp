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
; split function by character
(defun split_by_char (string char)
    (let ((pos (position char string)))
        (if pos
            (list (subseq string 0 pos)
                  (subseq string (1+ pos)))
            (list string))))
; split string into list of words
(defun split-string (string)
    (loop for word in (split_by_char string #\space)
            collect word))
; print list of words
(defun print-words (words)
    (loop for word in words
            do (format t "~a~%" word)))

(defun search-values (words)
    (loop for word in words



        do (loop for i from 0 to (1- (length operators))
            do (if (find-substring word (elt operators i))
                        (format t "~a~%" (elt printOperators i))
                        (setq operatorFlag 1)
                        )
        )
        do (loop for i from 0 to (1- (length keywords))
            do (if (find-substring word (elt keywords i))
                        (format t "~a~%" (elt printKeywords i))
                        (setq keywordFlag 1)
                        )
        )
        do (setq identifierFlag (not (and operatorFlag keywordFlag)))
        (if identifierFlag
            (format t "~a~%" "IDENTIFIER")
            (setq identifierFlag 0)
            )
                     
    )
)

(defun my-split (string &key (delimiterp #'delimiterp))
  (loop :for beg = (position-if-not delimiterp string)
    :then (position-if-not delimiterp string :start (1+ end))
    :for end = (and beg (position-if delimiterp string :start beg))
    :when beg :collect (subseq string beg end)
    :while end))
(defun delimiterp (c) (or (char= c #\Space) (char= c #\,)))

; call file read function
(setq fileContent(read-from-file filename))
; parse file content split by space
(loop for line in fileContent do 
    (setq words (my-split line))
    (print-words words)
    (format t "~a~%" "--------------")
    (search-values words))







