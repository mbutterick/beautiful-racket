#lang brag

top : (fun | app)*
fun : /"fun" var /"(" argvars /")" /"=" expr
/argvars : [var (/"," var)*]
@expr : add-or-sub
add-or-sub : [add-or-sub ("+" | "-")] mult-or-div
mult-or-div : [mult-or-div ("*" | "/")] value
@value : var | INT | app | /"(" expr /")"
app : var /"(" [expr (/"," expr)*] /")"
@var : ID