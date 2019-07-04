#lang brag

top : @statement*
statement : (var | expr | return | defun) /";" | if-else | while
var : /"var" (varname /"=")+ expr
@expr : reassignment
reassignment : ID [("+=" | "-=") expr] | ternary
ternary : expr /"?" expr /":" expr | logical-or
logical-or : [logical-or "||"] logical-and
logical-and : [logical-and "&&"] equal-or-not
equal-or-not : [equal-or-not ("!=" | "==")] gt-or-lt
gt-or-lt : [gt-or-lt ("<" | "<=" | ">" | ">=")] add-or-sub
add-or-sub : [add-or-sub ("+" | "-")] mult-or-div
mult-or-div : [mult-or-div ("*" | "/")] value
@value :  NUMBER  | STRING | object
       | fun | app | increment | varname | /"(" expr /")"
increment : ("++" | "--") varname | varname ("++" | "--")
object : /"{" @kvs /"}"
kvs : [kv (/"," kv)*]
/kv : expr /":" expr
defun : /"function" ID /"(" varnames /")" @block
fun : /"function" /"(" varnames /")" @block
/varnames : [varname (/"," varname)*]
@varname : ID | deref
deref : DEREF
block : /"{" @statement* /"}"
return : /"return" expr
app : varname /"(" @exprs /")"
exprs : [expr (/"," expr)*]
if-else : /"if" /"(" expr /")" @block ["else" @block]
while : /"while" /"(" expr /")" @block