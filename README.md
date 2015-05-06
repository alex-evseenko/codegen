Abstract Declarative Application Language -- Code Generation DSL
===============================================================

Code Generation DSL (Codegen DSL or just Codegen) is a meta-programming framework that allows generating of Java-classes based on dynamic composition of fragments of code into methods and classes.
Codegen DSL consists of Scala classes described below.

Code
----

Presents any fragment of code.
Contains anything that could be treated as target code. Trait `Code` is immutable, initializing is possible either via extending and overriding of `holder` method or using implicit `StringContext` implementation.
Overriding makes sense when target code is a closure that depends on some values:
```
val expr = new Code {
  override def holder = someUserInput + ";"
}
```

The same is possible using lambda definition from the `StringContext`:
```
val expr = $"""$someUserInput;"""
```

After `someUserInput` would be defined as a string the lambda code generation (by `~ operator`) returns something like this:
```
~expr === "Barselona, Spain;"
```

It's also possible to generate a code fragment without lambda, i.e. immediately when `expr` defined (in prediction that dependents `city`, `country` have already defined in moment of `expr` initialized):
```
val expr = code""""$city, $country";"""
```

If `city = "Kiev"`, `country = "Ukraine"` generation produces target code:
```
~expr === "Kiev, Ukraine;"
```

Type
----

Reflects a target type in sense that can generates (well, it's descendants really) some target specific types like interfaces, classes, etc.
Consists of immutable part that defines target type's attributes like package, name, fields and methods. Fields and methods can be defined via extending or dynamically added using += operators. Members defined using extending can be accessed directly using dot-notation. For dynamically defined members apply() methods have to use for locating a particular field or method.
Another one part of Type is an ancestor code. It brings code capabilities to support evaluations in target code and reflects how this object was evaluated, ie it produces target code evaluation that provides a given object. Each access to the member will return a new one Type instance that contains the previous ancestor code. For instance, define some type:
```
Object Coordinate extends Type('adal, 'Coordinate) {
def address = JavaLangString(code".address")
}
```

If some other type contains callable Method getCoordinate that returns Coordinate type the code gen DSL be able to make transitive calls getCoordinate().address.length(). After length() returns type JavaInt generating of target JavaCode returns:
```
~getCoordinate().address.length() === "getCoordinate().address.length()"
```

Expressing of operations is not possible in code generation DSL. To declare such operations need to use Code:
```
~Code"""${getCoordinate().address.length()} * 2""" === "getCoordinate().address.length() * 2"
```

Property
--------

Callable
--------

CleanMethod
-----------

Method
------

Interface
---------

Class
-----

AnonymousClass
--------------

InnerClass
----------

These traits/classes interact to each other and allow to generate target code.
