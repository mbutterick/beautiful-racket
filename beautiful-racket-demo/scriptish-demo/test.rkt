#lang scriptish-demo

var x = 42;
var s = "string";

x + x; // prints 84
s + x; // prints "string42"

var thing = {
    "foo" : 42,
    'bar' : function(x) {
        return x + 15;
    }
};

thing.foo; // prints 42
thing.bar; // prints #<procedure:...>
thing.bar(3); // prints 18

if ( thing.foo == 42 ) {
    // prints "The correct answer is 42"
    alert("The correct answer is " + thing.foo);
}

var idx = 0;
while ( idx != 50 ) {
    if ( thing.bar(idx) == 35 ) {
       // prints "Calamity at 20!"
       alert("Calamity at " + idx + "!"); 
    }
    idx++;
}
