#lang brag

top : @statement*
statement : (assignment | expr | return) /";" | if | while
assignment : /"var" id /"=" expr
@expr : comparison
comparison : [comparison ("!=" | "==")] sumlike
sumlike : [@sumlike /"+"] value 
@value : id | INTEGER  | STRING | object
       | func-def | func-app | increment
increment : id /"++"
object : /"{" @kvs /"}"
kvs : [kv (/"," kv)*]
/kv : expr /":" expr
func-def : /"function" /"(" ids /")" @block
/ids : [id (/"," id)*]
@id : ID | dotted-id
dotted-id : DOTTED-ID
block : /"{" @statement* /"}"
return : /"return" expr
func-app : id /"(" @exprs /")"
exprs : [expr (/"," expr)*]
if : /"if" /"(" expr /")" @block
while : /"while" /"(" expr /")" @block