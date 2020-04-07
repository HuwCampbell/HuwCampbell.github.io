---
title: Dumb Spark Tricks - Function Registration
description: How to force Spark to register your non-UDF catalyst expressions
tags: spark
---

I've been working with Spark for a few years now, and have pushed it into realms where it was
not really meant to go. Over this time, I have written a lot of custom aggregators, catalyst
expressions, custom RDDs and streaming interfaces.

This is the second part of this series, and is a short one.

#### How to trick Spark into registering a custom Catalyst or Imperative Expression

Spark provides quite a rich set of interfaces for writing custom functions and aggregates
to its runtime.

Unfortunately, most of these interfaces are considered relatively internal, and relatively
complicated; so the Spark developers have exposed an interface for user defined functions.

User Defined Functions, or UDF, and their relatives User Defined Aggregate Function have
reasonable support in Scala, Python, and R; and for a good few many jobs, work ok.

The biggest issue with these however, is that they automatically marshall to the host language
using reflection techniques, and their semantics with regards to polymorphic expressions is
either non-existant, or drastically broken. They also are limited in even the monomorphic types
they can work with, and types like maps and structures just don't seem to work.

Luckily though, when working in Scala, one can use the interfaces Spark uses itself to write
add required functionality.

There are a good range of these, including `TypedImperativeAggregate`, and `DeclarativeAggregate`,
and the plain old `Expression`.

Here, we're going to quickly defined a polymorphic identity function, which is a `UnaryExpression`,
these are a simple extension of `Expression` with just one child in the Spark DAG.

```scala
case class Ident(child: Seq[Expression])
  extends UnaryExpression with CodegenFallback {

  override def prettyName: String =
    "ident"

  override def dataType: DataType =
    child.dataType

  override def eval(input: InternalRow): Any =
    child eval input
}
```

The `CodeGenFallback` here says that we trust Spark to build efficient enough Scala code when
doing a full stage compilation, and we're not going to write a custom Scala pretty printer for
this expression.

To use this function in Scala code, it's quite simple. We just need to provide a function which
operates as a `Column` by wrapping and unwrapping the internal expression. I tend to just use
the apply method of a companion object

```scala
object Ident {
  def apply(child: Column): Column =
    new Column(child.expr)
}
```

We can now just use this function in a Spark `select` for instance, or anywhere else a `Column`
is required.

However, if we want to provide this so we can use it in a parsed SQL text expression, we're put
in a very difficult place. The Spark developers have provided a way to register functions defined as
UDFs and native Scala functions to be surfaced, but not functions written the way normal Spark
definitions are defined!

Here's what we've been given for unary function like what we'd like to define:

```scala
class UDFRegistration extends Logging {
  def register[RT: TypeTag, A1: TypeTag](name: String, func: (A1) ⇒ RT):
    UserDefinedFunction

  def register(name: String, udf: UserDefinedFunction):
    UserDefinedFunction
}
```

The first of these allows us to use a standard Scala function as a spark expression, but requires
that its type is monomorphic and able to be reflected upon (behind the scenes, Spark will perform
an implicit cast so that the types line up).

The second is relatively uninteresting, as a `UserDefinedFunction` is created by the function `udf`
which uses the same reflection techniques as the first `register`.

What we can do though, is utterly abuse this second function, and Scala's class inheritance. The
interesting this about the `register` function, is that the only function which `register` calls
is the UDF's `apply` method, and it is extremely similar to the one we wrote above. So what we
can do, is extend the `UserDefinedFunction` class and override its apply method.

We need some dummy parameters for the case class, but don't worry, they're never going to be used.


```scala
object Ident extends UserDefinedFunction(identity[Nothing] _, NullType, None) {
  // What we had before to make our Scala code nicer
  def apply(child: Column): Column =
    new Column(child.expr)

  // How we overrides the UDF's apply method, allowing
  // it to be used in imported SQL literals and expressions.
  override def apply(exprs: Column*): Column = {
    exprs.toList match {
      case child :: Nil =>
        Ident(child)
      case _ =>
        throw new Exception("Ident takes one argument")
    }
  }
}
```

And that's about it, we can now register this custom catalyst expression, even though the Spark
developers didn't really seem to think we would want to.

I've been using this trick a fair bit recently, as writing UDFs is extremely limiting when any
form of polymorphism is required.
