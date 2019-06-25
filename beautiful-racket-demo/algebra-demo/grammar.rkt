#lang brag

top : (fun | app)*
fun : /"fun" ID /"(" ID [/"," ID] /")" /"=" expr
expr : ID /"+" ID | app
app : ID /"(" (ID | INT) [/"," ID] /")"