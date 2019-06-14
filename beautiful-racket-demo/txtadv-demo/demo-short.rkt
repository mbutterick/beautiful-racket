#lang txtadv-demo

===VERBS===

north, n
 "go north"

south, s
 "go south"

get _, grab _, take _
 "take"

===THINGS===

---cactus---
get
  "You win!"

===PLACES===

---meadow---
"Welcome to the Cactus Game! You're standing in a meadow. There is a desert to the south."
[]

south
 desert

---desert---
"You're in a desert. There is nothing for miles around."
[cactus]

north
  meadow

===START===

meadow