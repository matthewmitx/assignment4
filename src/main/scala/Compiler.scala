// This is the compiler that translates regex abstract syntax trees into regular
// expression matching virtual machine programs.

package edu.ucsb.cs.cs162.regex.vm.compiler

import edu.ucsb.cs.cs162.regex._
import edu.ucsb.cs.cs162.regex.vm._

object Compiler {

  import Regex._
  // Return a virtual machine program that implements the given regex.
  def compile(re: Regex): Program = helpCompile(re) ++ IndexedSeq(Accept)

  def helpCompile(re: Regex): Program = {
  	re match {
  		case `∅` => IndexedSeq(Reject)
  		case `ε` => IndexedSeq(PushEmpty)
  		case Chars(s) => IndexedSeq(MatchSet(s), PushChar)
  		case Concatenate(r, s) => helpCompile(r) ++ helpCompile(s) ++ IndexedSeq(PushConcat)
  		case Union(r, s) => {
  			val firstFork = helpCompile(r) ++ IndexedSeq(PushLeft)
  			val secondFork = helpCompile(s) ++ IndexedSeq(PushRight)
  			IndexedSeq(Fork(1, firstFork.size + 2)) ++ firstFork ++ IndexedSeq(Jump(secondFork.size + 1)) ++ secondFork
  		}
  		case KleeneStar(r) => {
  			val withRepeat = helpCompile(r) ++ IndexedSeq(PushStar)
  			val withFork = IndexedSeq(Fork(1, withRepeat.size + 2)) ++ withRepeat
  			if(r.nullable == ε)
  				IndexedSeq(InitStar, CheckProgress) ++ withFork ++ IndexedSeq(Jump(-(withFork.size + 1)))
  			else
  				IndexedSeq(InitStar) ++ withFork ++ IndexedSeq(Jump(-(withFork.size)))
  		}
  		case Capture(str, r) => helpCompile(r) ++ IndexedSeq(PushCapture(str))
      case _ => throw new AssertionError("Invalid regex: cannot compile program")
  	}
  }
}
