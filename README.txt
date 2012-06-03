The ParenScript Object System is a CLOS-like object system for ParenScript.  Using the system is  very similar to CLOS.  it includes a dynamic runtime environment (found in paren-src/*.paren).

The ParenScript Object System
Red Daly, Stanford Department of Computer Science
reddaly@gmail.com

Introduction
The JavaScript language supports prototype-based inheritance.  Although adequate for most scripting tasks, JavaScript has become used more extensively in client-side applications to the point of replicating desk-top application functionality.  In large applications like these, developers often opt for a more extensive object system.  Google has developed a toolkit for compiling Java into JavaScript, and many options in native JavaScript already exist that implement class-based inheritance.

The ParenScript Object System (PSOS) is a facility for object-oriented programming in ParenScript, a lispy language that compiles to JavaScript.  PSOS as a whole was largely inspired by the Common Lisp Object System (CLOS).  Its features include, or will include, class-based inheritance, generic functions, and a meta-object protocol.  The utility of this system has already been demonstrated by [1], an implementation of a Web Ontology Language (OWL) reasoner that incorporates OWL classes into the ParenScript language.

Requirements
PSOS requires some extensions beyond the base features provided by parenscript a modified version of parenscript can be downloaded at https://github.com/3b/parenscript/

Example Usage
Using the ParenScript system for generating JavaScript, the syntax for defining a new class is simple:

    (defclass person ())
    (defclass politician (person))

After defining classes, we can define methods that operate on these classes.  The first step is defining a generic function, which is a type of function that acts differently depending on the classes of its arguments.  Next, methods that specialize the generic function can be defined.

    (defgeneric discuss-environment (individual))
    (defmethod discuss-environment ((individual person))
        (alert "I don't know anything about the environment"))
    (defmethod discuss-environment ((individual politician))
        (alert "The environment is an important issue, but so is Osama Bin Laden"))
