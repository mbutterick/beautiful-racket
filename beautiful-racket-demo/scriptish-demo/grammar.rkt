#lang brag

top : @statement*
statement : (var | expr | return) /";" | if | while
var : /"var" id /"=" expr
@expr : comparison
comparison : [comparison ("!=" | "==")] add-or-sub
add-or-sub : [add-or-sub ("+" | "-")] mult-or-div
mult-or-div : [mult-or-div ("*" | "/")] value
@value : id | NUMBER  | STRING | object
       | fun | app | increment
increment : id /"++"
object : /"{" @kvs /"}"
kvs : [kv (/"," kv)*]
/kv : expr /":" expr
fun : /"function" /"(" ids /")" @block
/ids : [id (/"," id)*]
@id : ID | deref
deref : DEREF
block : /"{" @statement* /"}"
return : /"return" expr
app : id /"(" @exprs /")"
exprs : [expr (/"," expr)*]
if : /"if" /"(" expr /")" @block ["else" @block]
while : /"while" /"(" expr /")" @block