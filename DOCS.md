Documentation
====

# The basics

Fundamentally, (unnamed) is a stack-based language.  What this means is that all actions performed in the language are based off of a stack.  A stack is a structure with properties that may seem restrictive at first.  There are two fundamental actions you can perform with a stack, *push* and *pop*.  When something is pushed to the stack, it is essentially placed on the top of the stack.  Pop takes the item from the top of the stack.  

There is a third action that builds on pop.  This is how functions are processed in stack-based languages.  They have a number of input parameters and a number of output values.  When a function (or 'word') is pushed to the stack, it pops from the stack the amount of input parameters there are.  It then returns its output by pushing the output to the stack.

Here is an example:

```
1 2 add
```

Here, `1` and then `2` are pushed to the stack.  Then, `+` is pushed.  `+` takes two inputs and yields one output; in many stack-based langauges, this is stylized as `( a b -- a+b )`.  Note that the symbol, `a+b`, is not actually performing any action, it is just a symbol.  `+` pops the `1` and `2` and adds them, returning to the stack:

```
3
```

These are the fundamentals of stack-based languages, which is the base of (unnamed).

There is another structure that is somewhat irregular compared to the stack format, and that is the syntax for making an *expression*.  An expression is simply a list of uncalled actions that can be called from within the stack.  Expressions mostly play a role in defining rules, which are discussed later.

# Basics of parsing with modes and rules

This is the actual main part of the language, rules and modes.  A *mode* is essentially a list of rules.  A *rule* consists of two parts, what pattern it is supposed to recognize and what it will replace the pattern with.  It works in a somewhat similar way to macros.  These rules and modes allow abstraction from the standard stack-based format.

The syntax for creating a rule is as follows:

```
{ add } { #Int + #Int } (rule)
```

Essentially, the function, `(rule)`, takes two arguments: its output, and the pattern it is to recognize.  If the expression, `1 + 2`, is given, the rule will take that and return the expression, `1 2 add`.  The type, `#Int`, restricts what type of values the rule will take.  the expression, `1.2 + 1.3` technically matches the order of the pattern of the rule, but fails because the types of the values don't match up; instead, the types of the two values in the example are `#Float`.

Rules defined like this do not function alone, however.  This just pushes the rule to the stack.  A mode is needed to be defined, and the rule must be added to it.

```
infix-add (mode)
```

This basically pushes an empty mode named `infix-add` to the stack.  Using the `(add-rule)` function, the rule defined above can be added:

```
infix-add (mode) { add } { #Int + #Int } (rule) (add-rule)
```

Now the mode, infix-add, has a rule.  Using `(push-mode)`, the mode will be pushed to a global list of modes which can be called:

```
infix-add (mode) { add } { #Int + #Int } (rule) (add-rule) (push-mode)
```

A mode can be called by its name and acts as a function that takes an expression as an argument:

```
{ 1 + 2 } infix-add
```

The expression is parsed from left to right in an attempt to find matches to the mode's rules.  What is yielded in the end is the expression, `1 2 add`.

This seems to be the same output as the rule alone.  However, there is a difference:

```
{ 1 + 2 + 3 } infix-add
```

The rule alone would deny this expression.  However, when a mode is called on it, it parses the expression in a different way.  From left to right, subsets of the expression of the size of the rule's input are taken.  For this, `1 + 2` is taken.  That expression is then checked, and passes, pushing `3` to the front of the original expression.  What is left is the expression, `3 + 3`, which by default passes, ultimately returning `6`.  The mode essentially simplifies the expression from left to right.

Modes can be used to abstract from the base language.

# Functions

Right now, there is only one function, `add` which has the signature, `( a b -- a+b)`.

The functions essentially are directly relatable to their counterpart in C.  The expression, `1 2 add`, becomes `add(1,2);` in C.