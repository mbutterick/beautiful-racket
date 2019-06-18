#lang s-exp javascriptlike-demo/expander

(assignment x 42)
(assignment s "string")

(sumlike x x)
(sumlike s x)

(assignment thing (object
                   ("foo" 42)
        
                   ("bar" (func-def (x)
                                (return (sumlike x 15))))))



(dotted-id (thing foo))
(dotted-id (thing bar))
(func-app (dotted-id (thing bar)) 3)

(assignment console (object ("log" (func-def (str) (displayln str))))) ; simulates global console (don't put in parse tree)
(if (comparison (dotted-id (thing foo)) == 42)
    (func-app (dotted-id (console log)) (sumlike "The correct answer is " (dotted-id (thing foo)))))


(assignment idx 0)
(while (comparison idx != 50)
       (if (comparison (func-app (dotted-id (thing bar)) idx) == 35)
           (alert (sumlike "Calamity at " idx "!")))
       (increment idx))