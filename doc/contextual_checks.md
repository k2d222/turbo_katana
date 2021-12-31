# List of all performed contextual checks and associated tests.

## Class Declarations

* Perform Constructor checks
* Perform Methods checks
* --
* (**Missing**) No class derived from Integer or String
* (**Missing**) No duplicate class declaration
* (**Missing**) No reserved class name (Integer, String)
* (**Missing**) No duplicate attribute declaration (static or not, ctor or not)
* No duplicate method declaration
* Herited class exists
* No cycle in inheritance graph

## Methods

* Perform *Instruction* checks on method body
* --
* No reserved keyword in params
* No method with *override* keyword in a base class
* Override methods have the *override* keyword
* Override methods match the overriden method signature
* If method returns something, all code paths lead to a return or assign to *result*
* If methods returns something, all Return instructions are compatible with return type
* If method returns nothing, no Return instruction

## Constructor

* (**Missing**) No Return instruction
* No reserved keyword in params
* Constructor name and class name are equal
* Constructor parameters and class parameters are equal (**TODO: match types, not names**)
* Constructor calls the right super constructor if class is derived
* Constructor does not call any super constructor if class is base

## Instructions

* Perform expression checks for Expr, Ite, Return and Assign
* --
* No reserved keyword declared in Block instructions
* Can only Assign to idents, attributes or static attributes
* Assign rhs is compatible with lhs
* Expression in an Ite instruction is of type Integer

## Expressions

* (**Missing**) Called method exists
* (**Missing**) Called method params are compatible with declaration
* (**Missing**) Called static method exists in static class
* (**Missing**) Called static method params are compatible with declaration
* (**Missing**) Call to New exists
* (**Missing**) Params in New call are compatible with ctor
* (**Missing**) Numeric operators are used on Integer types
* (**Missing**) StrCat is used on Strings
* Identifiers are in scope
* Attributes exist
* Static Attributes exist

## Main Instruction

* Perform instruction checks
* --
* No Return in main instruction
