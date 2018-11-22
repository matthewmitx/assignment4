// Provides a derivative-based static analysis of regular expressions that
// yields a DFA describing the language recognized by an expression.

package edu.ucsb.cs.cs162.regex.derivative

import edu.ucsb.cs.cs162.dfa._
import edu.ucsb.cs.cs162.range_set._
import edu.ucsb.cs.cs162.regex._

object DerivativeAnalysis {
  import Derive._
  import Regex._

  //----------------------------------------------------------------------------
  // Public API.
  //----------------------------------------------------------------------------

  // Statically analyzes 're' using derivatives in order to compute the DFA of
  // the language recognized by 're'. The resulting DFA has an explicit error
  // state and is approximately minimal.
  def analyze(re: Regex): Dfa[Regex] = {
    val (states, transitions) = computeDfa(Set[Regex](re), Set[Regex](), Map[Regex, Seq[(CharSet, Regex)]]())
    val finalStates = states.filter(_.nullable == `ε`)
    Dfa[Regex](transitions, re, finalStates)
  }

  //----------------------------------------------------------------------------
  // Private details.
  //----------------------------------------------------------------------------

  // Compute the transitions and set of reachable states (i.e., Regexes) for all
  // Regexes in 'todo'.
  @annotation.tailrec
  private def computeDfa(todo: Set[Regex], visitedStates: Set[Regex],
    transitions: Transitions[Regex]) : (Set[Regex], Transitions[Regex]) = {
    if(todo.isEmpty)
      return (visitedStates, transitions)
    if(!visitedStates.contains(todo.head)){
      val (nextStates, nextTransitions) = computeNext(todo.head)
      computeDfa((todo - todo.head) ++ (nextStates -- visitedStates), visitedStates + todo.head, transitions ++ nextTransitions)
    }
    else
      computeDfa(todo - todo.head, visitedStates, transitions)
  }

  // Compute the transitions and destination states from the given regex.
  private def computeNext(state: Regex): (Set[Regex], Transitions[Regex]) = {
    val transitionCharsets = C(state) // set of CharSet
    val machine = new DerivativeMachine(state)  
    val nextStatesWithTransitions = (transitionCharsets.flatMap(currSet => currSet.minElement match 
      {case Some(c) => Some(currSet, machine.derive(c))
       case None => None})).toSeq // set of (CharSet, Regex)
    val nextStates = nextStatesWithTransitions.map((x) => x._2)
    (nextStates.toSet, Map(state -> nextStatesWithTransitions))
  }

  private def C(re: Regex): Set[CharSet] = {
    re match{
      case `∅` => Set(CharSet(IndexedSeq(Char.MinValue -> Char.MaxValue)))
      case `ε` => Set(CharSet(IndexedSeq(Char.MinValue -> Char.MaxValue)))
      case Chars(x) => Set(x) + (!x)
      case KleeneStar(re) => C(re)
      case Complement(re) => C(re)
      case Union(re1, re2) => pairwiseUnion(C(re1), C(re2))
      case Intersect(re1, re2) => pairwiseUnion(C(re1), C(re2))
      case Concatenate(re1, re2) if (re1.nullable == ∅) => C(re1)
      case Concatenate(re1, re2) => pairwiseUnion(C(re1), C(re2))
    }
  }

  private def pairwiseUnion(s1: Set[CharSet], s2: Set[CharSet]): Set[CharSet] = {
    for(x <- s1; y <- s2) yield x & y
  }

}
