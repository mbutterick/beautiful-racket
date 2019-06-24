#lang brag

top : (fun | app)*
fun : /"fun" var /"(" argvars /")" /"=" expr
/argvars : [var (/"," var)*]
@expr : s-or-d
s-or-d : [s-or-d ("+" | "-")] p-or-q
p-or-q : [p-or-q ("*" | "/")] value
@value : var | INT | app | /"(" expr /")"
app : var /"(" [expr (/"," expr)*] /")"
@var : ID