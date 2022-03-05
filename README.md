# glox

An implementation of the tree-walking interpreter from [_Crafting Interpreters_](https://craftinginterpreters.com/)
written in Go.

## Installation

To install, run:

```shell
go get github.com/chidiwilliams/glox
```

## Usage

To start a new REPL session:

```shell
glox
```

To run a Lox file:

```shell
glox -filePath [filePath]
```

## Parser grammar

glox implements the [original Lox definition](https://craftinginterpreters.com/appendix-i.html) as well as some features
from the _Challenges_ sections in the book: series expressions, function expressions, anonymous functions, and getter
methods.

Here's the full parser grammar:

```text
program      => declaration* EOF
declaration  => classDecl | funcDecl | varDecl | statement
classDecl    => "class" IDENTIFIER ( "<" IDENTIFIER )? "{" function* "}"
funDecl      => "fun" function
function     => IDENTIFIER "(" parameters? ")" block
parameters   => IDENTIFIER ( "," IDENTIFIER )*
varDecl      => "var" IDENTIFIER ( "=" expression )? ";"
statement    => exprStmt | ifStmt | forStmt | printStmt | returnStmt | whileStmt
                         | breakStmt | continueStmt | block
exprStmt     => expression ";"
ifStmt       => "if" "(" expression ")" statement ( "else" statement )?
forStmt      => "for" "(" ( varDecl | exprStmt | ";" ) expression? ";" expression? ")" statement
printStmt    => "print" expression ";"
returnStmt   => "return" expression? ";"
whileStmt    => "while" "(" expression ")" statement
block        => "{" declaration* "}" ;
expression   => series
series       => assignment ( "," assignment )*
assignment   => ( call "." )? IDENTIFIER "=" assignment | ternary
ternary      => logic_or ( "?" ternary ":" ternary )*
logic_or     => logic_and ( "or" logic_and )*
logic_and    => equality ( "and" equality )*
equality     => comparison ( ( "!=" | "==" ) comparison )
comparison   => term ( ( ">" | ">=" | "<" | "<=" ) term )*
term         => factor ( ( "+" | "-" ) factor )*
factor       => unary ( ( "/" | "*" ) unary )*
unary        => ( "!" | "-" ) unary | call
call         => primary ( "(" arguments? ")" | "." IDENTIFIER )*
arguments    => expression ( "," expression )*
primary      => NUMBER | STRING | "true" | "false" | "nil" | "(" expression ")"
                                | IDENTIFIER | functionExpr | "super" . IDENTIFIER
functionExpr => "fun" IDENTIFIER? "(" parameters? ")" block
```

## Examples

A few example programs from [Crafting Interpreters](https://craftinginterpreters.com/the-lox-language.html):

```lox
print "Hello, world!";
```

If statements:

```lox
if (condition) {
  print "yes";
} else {
  print "no";
}
```

Loops:

```lox
var a = 1;
while (a < 10) {
  print a;
  a = a + 1;
}

for (var a = 1; a < 10; a = a + 1) {
  print a;
}
```

Functions and closures:

```lox
fun printSum(a, b) {
  print a + b;
}
printSum(1, 2);

fun returnFunction() {
  var outside = "outside";

  fun inner() {
    print outside;
  }

  return inner;
}

var fn = returnFunction();
fn();
```

Classes:

```lox
class Breakfast {
  init(meat, bread) {
    this.meat = meat;
    this.bread = bread;
  }
}

class Brunch < Breakfast {
  init(meat, bread, drink) {
    super.init(meat, bread);
    this.drink = drink;
  }
}
```
