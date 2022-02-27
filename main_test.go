package main

import (
	"bytes"
	"strconv"
	"testing"
)

func Test_Run(t *testing.T) {
	tests := []struct {
		name   string
		source string
		stdOut string
		stdErr string
	}{
		// atoms
		{"string", "print \"hello world\";", "hello world\n", ""},
		{"number", "print 342.32461932591235;", "342.32461932591235\n", ""},
		{"string as boolean", "print \"\" and 34;", "34\n", ""},
		{"nil as boolean", "print nil and 34;", "nil\n", ""},

		// comments
		{"single-line comment after source", "print 1 + 1; // hello", "2\n", ""},
		{"single-line comment", `// hello
print 1 + 1;`, "2\n", ""},

		// unary, binary, and ternary operations
		{"arithmetic operations", "print -1 + 2 * 3 - 4 / 5;", "4.2\n", ""},
		{"decimal arithmetic", "print 1.234 / 5.678;", "0.2173300457907714\n", ""},
		{"logical operations", "print (!true or false) and false;", "false\n", ""},
		{"ternary", "print 3 < 4 ? 2 > 5 ? \"no\" : \"yes\" : \"also no\";", "yes\n", ""},
		{"string concatenation", "print \"hello\" + \" \" + \"world\";", "hello world\n", ""},
		{"greater than or equal to", "print 4 >= 3 ? 3 >= 3 ? 2 >= 3 : true : true;", "false\n", ""},
		{"less than or equal to", "print 4 <= 5 ? 5 <= 5 ? 6 <= 5 : true : true;", "false\n", ""},
		{"equal to", "print 5 == 5 ? 4 == 5 : true;", "false\n", ""},
		{"not equal to", "print 4 != 5 ? 5 != 5 : true;", "false\n", ""},
		{"comma", "print (1, 2);", "2\n", ""},

		// variables
		{"variable declaration", "var a = 10; print a*2;", "20\n", ""},
		{"variable assignment after declaration", "var a; a = 20; print a*2;", "40\n", ""},
		{"variable re-assignment", "var a = 10; print a; a = 20; print a*2;", "10\n40\n", ""},

		// block scoping
		{"block scoping", `var a = "global a";
var b = "global b";
var c = "global c";
{
    var a = "outer a";
    var b = "outer b";
    {
        var a = "inner a";
        print a;
        print b;
        print c;
    }
    print a;
    print b;
    print c;
}
print a;
print b;
print c;`, "inner a\nouter b\nglobal c\nouter a\nouter b\nglobal c\nglobal a\nglobal b\nglobal c\n", ""},

		// conditionals
		{"if block", "if (true) { if (false) { print \"hello\"; } else { print \"world\"; } }", "world\n", ""},

		// loops
		{"for loop", `var a = 0;
var temp;

for (var b = 1; a < 10; b = temp + b) {
    print a;
    temp = a;
    a = b;
}`, "0\n1\n1\n2\n3\n5\n8\n", ""},
		{"while loop", `var a = 0;
var temp;
var b = 1;

while (a < 10) {
    print a;
    temp = a;
    a = b;
		b = temp + b;
}`, "0\n1\n1\n2\n3\n5\n8\n", ""},
		{"break statement", `var a = 1;
while (true) {
		a = a + 1;
		print a;
		if (a == 4) break;
}`, "2\n3\n4\n", ""},
		{"continue statement", `var a = 1;
while (a < 10) {
		a = a * 2;
		print a;
		if (a > 4) {
			continue;
		} else {
			a = a + 1;
		}
}`, "2\n6\n12\n", ""},

		// functions
		{"function", `fun sayHi(first, last) {
    print "Hello, " + first + " " + last;
}

sayHi("Dear", "Reader");`, "Hello, Dear Reader\n", ""},
		{"return statement", `fun sayHi(first, last) {
    return "Hello, " + first + " " + last;
}

print sayHi("Dear", "Reader");`, "Hello, Dear Reader\n", ""},
		{"closure", `fun makeCounter() {
		var i = 0;
		fun count() {
				i = i + 1;
				print i;
		}
		return count;
}

var counter = makeCounter();
counter();
counter();`, "1\n2\n", ""},
		{"anonymous function", `fun makeCounter() {
		var i = 0;
		return fun () {
				i = i + 1;
				print i;
		};
}

var counter = makeCounter();
counter();
counter();`, "1\n2\n", ""},
		{"iife", `(fun count(next) {
		print next;
		if (next < 5) return count(next + 1);
		return;
})(1);`, "1\n2\n3\n4\n5\n", ""},
		{"calling function with wrong arity", `fun sayHello(a, b) {
		print a + b;
}
sayHello("only first");`, "", "Expected 2 arguments but got 1.\n[line 3]"},

		// Variable scoping
		{"scoping", `var a = "global";
{
		fun showA() {
				print a;
		}

		showA();
		var a = "block";
		showA();
		a; // mutes error about the local variable not being used
}`, "global\nglobal\n", ""},
		{"re-declaring variables in same scope", `{
				var a = "global";
				var a = "global2";
		}`, "", "[line 2] Error at 'a': Already a variable with this name in this scope\n[line 2] Error at 'a': Variable 'a' declared but not used.\n"},
		{"unused local variable", `{
		var a = "global";
}`, "", "[line 1] Error at 'a': Variable 'a' declared but not used.\n"},

		// Classes
		{"class method", `class Bacon {
	eat() {
		print "Crunch crunch";
	}
}
Bacon().eat();
`, "Crunch crunch\n", ""},
		{"this", `class Cake {
	taste() {
		var adjective = "delicious";
		print "The " + this.flavor + " cake is " + adjective + "!";
	}
}

var cake = Cake();
cake.flavor = "German chocolate";
cake.taste();
`, "The German chocolate cake is delicious!\n", ""},
		{"init class", `class Circle {
	init(radius) {
		this.radius = radius;
	}

	area() {
		return 3.141592653 * this.radius * this.radius; 
	}
}

var circle = Circle(7);
print circle.area();`, "153.938039997\n", ""},
		{"class with static methods", `class Math {
	add(x, y) {
		return x + y;
	}
}

print Math.add(1, 2);`, "3\n", ""},
		{"getter", `class Circle {
	init(radius) {
		this.radius = radius;
	}

	area {
		return 3.141592653 * this.radius * this.radius; 
	}
}

var circle = Circle(7);
print circle.area;`, "153.938039997\n", ""},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			stdOut := &bytes.Buffer{}
			stdErr := &bytes.Buffer{}
			r := newRunner(stdOut, stdErr)
			r.run(tt.source)

			if stdOut.String() != tt.stdOut {
				t.Errorf("stdOut: got %s, expected %s", strconv.Quote(stdOut.String()), strconv.Quote(tt.stdOut))
			}

			if stdErr.String() != tt.stdErr {
				t.Errorf("stdErr: got %s, expected %s", strconv.Quote(stdErr.String()), strconv.Quote(tt.stdErr))
			}
		})
	}
}
