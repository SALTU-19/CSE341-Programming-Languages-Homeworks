; define file name
(setq filename "file.txt")
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
; call file read function
(setq fileContent (read-from-file filename))
; parse file content split by space
(loop for line in fileContent do 
    (setq words (split-string line))
    (print-words words))

(print (find-substring "hello world" "world"))



