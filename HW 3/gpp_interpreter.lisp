(load "gpp_interpreter.lisp")



(deffun main()
        (princ "Enter input string: ")
        
        (setq inputString (read-line))
        (print (gppinterpreter (list inputString)))
)
(main)