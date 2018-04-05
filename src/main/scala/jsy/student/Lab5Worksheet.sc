/*
 * CSCI 3155: Lab 5 Worksheet
 *
 * This worksheet demonstrates how you could experiment
 * interactively with your implementations in Lab5.scala.
 */

// Imports the parse function from jsy.lab1.Parser
import jsy.lab5.Parser.parse

// Imports the ast nodes
import jsy.lab5.ast._

// Imports all of the functions form jsy.student.Lab2 (your implementations in Lab2.scala)
import jsy.student.Lab5._

// Parse code with assignments
parse("var x = 1; x = 2; console.log(x)")
parse("const x = {f: 1}; x.f = 2; console.log(x.f)")

// Parse code with null
parse("null")
parse("<Null>null")
parse("<{f: number}>null")

// Parse functions
parse("(x: number) => x")
parse("function (x: var number) { x = 0; return x }")
parse("var y = 1; (function (x: ref number) { x = 0; return x })(y)")
parse("((x: name number) => 0)(console.log(100))")

// Aliasing example
val aliasingex = parse("""
  const x = { f: 1 }
  const y = x
  x.f = 2
  console.log(y.f)
""")
//iterateStep(aliasingex) // uncomment when you are ready to test your step function.
import jsy.util._
import jsy.util.DoWith._

val d0:DoWith[String, Unit] = doput("Fooo")
val d1:DoWith[Int, Int] = doget
d0("Cow")
d1(98)

val d2 = d0 flatMap((x) => doget: DoWith[String, String])
d2("lajsdf")
val d3 = d0.flatMap((x) => doget: DoWith[String, String]).map((x) => x + "!")
d3("test")
// doget, doput messes with state (W)
// doreturn messes with R
val d4 :DoWith[String, Int] = doreturn(15)
d4("")

// do modify takes in an argument that is a function modifying the state return DoWith[W, Unit]
val d5 : DoWith[String ,Unit] = domodify((w) => w + "!!")
d5("hats")

val map1 = Map("a" -> 2, "b" -> 3, "c" -> 4)
val map2 = Map("a" ->3)
map2.toSet subsetOf map1.toSet