# List of all performed contextual checks and associated tests.

## Class Declarations

* Perform Constructor checks
* Perform Methods checks
* --
* No class derived from Integer or String ✔
* No duplicate class declaration ✔
* No reserved class name (Integer, String) ✔
* No duplicate static method declaration ✔
* No duplicate instance method declarations ✔
* Herited class exists ✔
* No cycle in inheritance graph ✔

## Attribute

* No duplicate instance attribute declaration ✔
* No duplicate static attribute declaration ✔
* No reserved keywords in attribute declaration ✔
## Methods

* Perform *Instruction* checks on method body
* --
* No reserved keyword in params ✔
* No method with *override* keyword in a base class ✔
* Override methods have the *override* keyword ✔
* Override methods match the overriden method signature ✔
* If method returns something, all code paths lead to an assign to *result* before return or block ends
* No static override ✔

## Constructor

* No reserved keyword in params ✔
* Constructor name and class name are equal ✔
* Constructor parameters and class parameters are equal ✔
* Constructor calls the right super constructor if class is derived
* Constructor does not call any super constructor if class is base

## Instructions

* Perform expression checks for Expr, Ite, Return and Assign
* --
* No reserved keyword declared in Block instructions ✔
* Can only Assign to idents, attributes or static attributes
* Assign rhs is compatible with lhs
* Expression in an Ite instruction is of type Integer

## Expressions

* Call to new exists ✔
* Called method exists ✔
* Called method params are compatible with declaration ✔
* Called static method exists in static class
* Called static method params are compatible with declaration
* Params in new call are compatible with ctor ✔
* Numeric operators are used on Integer types
* StrCat is used on Strings
* Identifiers are in scope ✔
* Attributes exist ✔
* Static Attributes exist
