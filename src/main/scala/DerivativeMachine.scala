// A virtual machine implementation of derivative-based matching.

package edu.ucsb.cs.cs162.regex.derivative

import edu.ucsb.cs.cs162.regex._

object `package` {
  // Programs for the DerivativeMachine.
  type Program = Seq[Instruction]

  // Pretty-print derivative virtual machine programs.
  def programToString(prog: Program): String = {
    val strs = for (inst <- prog) yield inst match {
      case `PushDerive` => "derive"
      case `PushConcatenate` => "concatenate"
      case `PushUnion` => "union"
      case `PushComplement` => "complement"
      case `PushIntersect` => "intersect"
      case `PushNullable` => "nullable"
      case PushRe(re) => "push " + re.toString
    }

    strs.mkString("\n")
  }
}

// Instructions for the virtual machine.
//
// - Derive: pop the top of the operand stack, compute its derivative w.r.t. the
//   machine's given char, then push the result back on the operand stack.
// - PushConcatentate: pop the top two elements of the operand stack and push
//   their concatenation back on.
// - PushUnion: pop the top two elements of the operand stack and push their
//   union back on.
// - PushComplement: pop the top of the operand stack, take its complement, and
//   push the result back on.
// - PushIntersect: pop the top two elements of the operand stack and push
//   their intersection back on.
// - PushNullable: pop the top of the operand stack, compute its nullability,
//   and push the result back on the operand stack.
// - PushRe(re): push re onto the top of the operand stack.
sealed abstract class Instruction
case object PushDerive extends Instruction
case object PushConcatenate extends Instruction
case object PushUnion extends Instruction
case object PushComplement extends Instruction
case object PushIntersect extends Instruction
case object PushNullable extends Instruction
case class PushRe(re: Regex) extends Instruction

class DerivativeMachine(re: Regex) {
  import Regex._

  //----------------------------------------------------------------------------
  // Public API.
  //----------------------------------------------------------------------------

    // Returns true iff 'str' is recognized by 're'.
  def eval(str: String): Boolean = {
    str.foldLeft(re)((currentRe, char) => new DerivativeMachine(currentRe).derive(char)).nullable == ε
  }

  // Returns the derivative of 're' w.r.t. 'char'.
  def derive(char: Char): Regex = {
    run(Seq(re), Seq(PushDerive), char)
  }

  //----------------------------------------------------------------------------
  // Private details.
  //----------------------------------------------------------------------------

  // Derives a regular expression from the top of 'operands' w.r.t. 'char'.
  @annotation.tailrec
  private def run(operands: Seq[Regex], program: Program, char: Char): Regex = {
    if (program.isEmpty) {
      assert(operands.size == 1)
      operands.head
    }
    else {
      program.last match{
        case PushDerive => {
          operands.last match{
            case `∅` => run(operands.dropRight(1) ++ Seq(∅), program.dropRight(1), char) 
            case `ε` => run(operands.dropRight(1) ++ Seq(∅), program.dropRight(1), char) 
            case Chars(x) => { 
              if (x.contains(char)) 
                run(operands.dropRight(1) ++ Seq(ε), program.dropRight(1), char)
              else 
                run(operands.dropRight(1) ++ Seq(∅), program.dropRight(1), char) 
            }
            case Concatenate(re1, re2) => run(operands.dropRight(1) ++ Seq(re1), program.dropRight(1) ++ Seq(PushUnion, PushConcatenate, PushDerive, PushRe(re2), PushNullable, PushRe(re1), PushConcatenate, PushRe(re2), PushDerive), char)
            case Union(re1, re2) => run(operands.dropRight(1) ++ Seq(re1), program.dropRight(1) ++ Seq(PushUnion, PushDerive, PushRe(re2), PushDerive), char)
            case KleeneStar(re1) => run(operands.dropRight(1) ++ Seq(re1), program.dropRight(1) ++ Seq(PushConcatenate, PushRe(KleeneStar(re1)), PushDerive), char)
            case Complement(re1) => run(operands.dropRight(1) ++ Seq(re1), program.dropRight(1) ++ Seq(PushComplement, PushDerive), char)
            case Intersect(re1, re2) => run(operands.dropRight(1) ++ Seq(re1), program.dropRight(1) ++ Seq(PushIntersect, PushDerive, PushRe(re2), PushDerive), char)
          }
        }
        case PushConcatenate => {
          val reg2 = operands.last
          val reg1 = operands(operands.length - 2)
          run(operands.dropRight(2) ++ Seq(reg1 ~ reg2), program.dropRight(1), char)
        }
        case PushUnion => {
          val reg2 = operands.last
          val reg1 = operands(operands.length - 2)
          run(operands.dropRight(2) ++ Seq(reg1 | reg2), program.dropRight(1), char)
        }
        case PushComplement => {
          val reg = operands.last
          run(operands.dropRight(1) ++ Seq(!reg), program.dropRight(1), char)
        }
        case PushIntersect => {
          val reg2 = operands.last
          val reg1 = operands(operands.length - 2)
          run(operands.dropRight(2) ++ Seq(reg1 & reg2), program.dropRight(1), char)
        }
        case PushNullable => {
          val reg = operands.last
          run(operands.dropRight(1) ++ Seq(reg.nullable), program.dropRight(1), char)
        }
        case PushRe(x) => run(operands ++ Seq(x), program.dropRight(1), char)
      }
    }
  }
}
