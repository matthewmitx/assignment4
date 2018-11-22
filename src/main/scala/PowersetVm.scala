package edu.ucsb.cs.cs162.regex.vm

import edu.ucsb.cs.cs162.regex.parse_tree._
import edu.ucsb.cs.cs162.range_set._
import edu.ucsb.cs.cs162.regex._

// A virtual machine that uses Thompson's powerset strategy to implement a
// non-backtracking algorithm for regular expression matching.
class PowersetVm(program: Program) extends VirtualMachine(program) {
  override def eval(str: String): Option[ParseTree] = {
    // Algorithm:
    // 1. compute initial set of threads (the ε-closure of the nfa start state)
    // 2. if the input string is empty go to step 7
    // 3. run the threads until they reach a match or accept instruction
    // 4. compact them to enforce at most one thread per program counter
    // 5. execute the surviving threads one step (i.e., the match or accept instruction)
    // 6. go to step 2
    // 7. compact the final set of threads
    // 8. if there is a surviving thread at an accept instruction, then that
    //    thread's 'parse' contains the final answer; otherwise there is no answer

    // Execute all given threads until they reach either a MatchSet or an Accept
    // instruction; returns the resulting set of Threads.
    @annotation.tailrec
    def runUntilMatchOrAccept(thread: Thread, todo: Set[Thread],
      result: Set[Thread]): Set[Thread] = {
      if(todo.isEmpty) program.apply(thread.pc) match{
        case `Accept` => result + thread
        case `Reject` => result
        case MatchSet(chars) => result + thread
        case `CheckProgress` => {
          if(thread.progress.contains(thread.pc)) runUntilMatchOrAccept(todo.head, todo.tail, result)
          else runUntilMatchOrAccept(new Thread(thread.pc + 1, thread.progress + thread.pc, thread.priority, thread.parse), todo, result)
        }
        case Jump(x) =>runUntilMatchOrAccept(new Thread(thread.pc + x, thread.progress, thread.priority, thread.parse), todo, result)
        case Fork(x,y) => runUntilMatchOrAccept(new Thread(thread.pc + x, thread.progress, thread.priority, thread.parse), todo + new Thread(thread.pc + y, thread.progress, thread.priority, thread.parse), result)
        case `PushEmpty` => runUntilMatchOrAccept(new Thread(thread.pc + 1, thread.progress, thread.priority, EmptyLeaf +: thread.parse), todo, result)
        case `PushConcat` => runUntilMatchOrAccept(new Thread(thread.pc + 1, thread.progress, thread.priority, ConcatNode(thread.parse.tail.head, thread.parse.head) +: thread.parse.drop(2)), todo, result)
        case `PushLeft` => runUntilMatchOrAccept(new Thread(thread.pc + 1, thread.progress, thread.priority + "l", LeftNode(thread.parse.head) +: thread.parse.tail), todo, result)
        case `PushRight` => runUntilMatchOrAccept(new Thread(thread.pc + 1, thread.progress, thread.priority + "r", RightNode(thread.parse.head) +: thread.parse.tail), todo, result)
        case `InitStar` => runUntilMatchOrAccept(new Thread(thread.pc + 1, thread.progress, thread.priority, StarNode(Seq()) +: thread.parse), todo, result)
        case `PushStar` => {
          val body = thread.parse.head
          val star = thread.parse.tail.head
          val rest = thread.parse.tail.tail
          star match {
            case StarNode(seq) => runUntilMatchOrAccept(new Thread(thread.pc + 1, thread.progress, thread.priority, StarNode(body +: seq) +: rest), todo, result)
            case _ => throw new AssertionError("Incorrect orderinng of program")
          }
        }
        case PushCapture(str) => runUntilMatchOrAccept(new Thread(thread.pc + 1, thread.progress, thread.priority, CaptureNode(str, thread.parse.head) +: thread.parse.tail), todo, result)
        case _ => throw new AssertionError("Incorrect ordering of program")
      }
      else{
        program.apply(thread.pc) match {
          case `Accept` => runUntilMatchOrAccept(todo.head, todo.tail, result + thread)
          case `Reject` => runUntilMatchOrAccept(todo.head, todo.tail, result)
          case `CheckProgress` => {
           if(thread.progress.contains(thread.pc)) runUntilMatchOrAccept(todo.head, todo.tail, result)
            else runUntilMatchOrAccept(new Thread(thread.pc + 1, thread.progress + thread.pc, thread.priority, thread.parse), todo, result)
          }
          case MatchSet(chars) => runUntilMatchOrAccept(todo.head, todo.tail, result + thread)
          case Jump(x) =>runUntilMatchOrAccept(new Thread(thread.pc + x, thread.progress, thread.priority, thread.parse), todo, result)
          case Fork(x,y) => runUntilMatchOrAccept(new Thread(thread.pc + x, thread.progress, thread.priority, thread.parse), todo + new Thread(thread.pc + x, thread.progress, thread.priority, thread.parse), result)
          case `PushEmpty` => runUntilMatchOrAccept(new Thread(thread.pc + 1, thread.progress, thread.priority, EmptyLeaf +: thread.parse), todo, result)
          case `PushConcat` => runUntilMatchOrAccept(new Thread(thread.pc + 1, thread.progress, thread.priority, ConcatNode(thread.parse.tail.head, thread.parse.head) +: thread.parse.drop(2)), todo, result)
          case `PushLeft` => runUntilMatchOrAccept(new Thread(thread.pc + 1, thread.progress, thread.priority + "l", LeftNode(thread.parse.head) +: thread.parse.tail), todo, result)
          case `PushRight` => runUntilMatchOrAccept(new Thread(thread.pc + 1, thread.progress, thread.priority + "r", RightNode(thread.parse.head) +: thread.parse.tail), todo, result)
          case `InitStar` => runUntilMatchOrAccept(new Thread(thread.pc + 1, thread.progress, thread.priority, StarNode(Seq(EmptyLeaf)) +: thread.parse), todo, result)
          case `PushStar` => {
            val body = thread.parse.head
            val star = thread.parse.tail.head
            val rest = thread.parse.tail.tail
            star match {
              case StarNode(seq) => runUntilMatchOrAccept(new Thread(thread.pc + 1, thread.progress, thread.priority, StarNode(body +: seq) +: rest), todo, result)
              case _ => throw new AssertionError("Incorrect orderinng of program")
            }
          }
          case PushCapture(str) => runUntilMatchOrAccept(new Thread(thread.pc + 1, thread.progress, thread.priority, CaptureNode(str, thread.parse.head) +: thread.parse.tail), todo, result)
          case _ => throw new AssertionError("Incorrect ordering of program")
        }
      }
    }

    // Remove any threads s.t. there exists another thread at the same program
    // point with a smaller Priority.
    def compact(threads: Set[Thread]): Set[Thread] = {
      if(threads.isEmpty)
        threads
      val groups: Map[Int, Set[Thread]] = threads.groupBy(_.pc)
      groups.flatMap( { case (a,b) => Set( b.foldLeft (new Thread(0, Set(), "z", Seq())) (lesser(_,_)) ) } ).toSet
    }

    def lesser(t1: Thread, t2: Thread): Thread = {
      if (t1.priority <= t2.priority) t1
      else t2
    }

    // Return the result of matching the current string position on all the
    // given threads.
    val matchStringPosition: (Set[Thread], Char) => Set[Thread] = (threads, currChar) => {
      val validThreads = threads.filter((thread) => program.apply(thread.pc) match {
        case Accept => {
          if (str == "") true
          else if(currChar == '\u0000') true
          else false
        }
        case MatchSet(chars) => {
          if(chars.contains(currChar)) true
          else false
        }
        case _ => throw new AssertionError("Incorrect position in threads")
      })
      validThreads.map((thread) => program.apply(thread.pc) match{
        case MatchSet(chars) => new Thread(thread.pc + 2, thread.progress, thread.priority, CharLeaf(currChar) +: thread.parse)
        case _ => thread
      })
    }

    def recurse(sp: Int, threads: Set[Thread]): Set[Thread] = {
      if(str == "" || sp == str.length){
        val setOfThreads = runUntilMatchOrAccept(threads.head, threads.tail, Set())
        val compactedSetOfThreads = compact(setOfThreads)
        matchStringPosition(compactedSetOfThreads, '\u0000')
      }
      else{
        val setOfThreads = runUntilMatchOrAccept(threads.head, threads.tail, Set())
        val compactedSetOfThreads = compact(setOfThreads)
        val survivingSetOfThreads = matchStringPosition(compactedSetOfThreads, str.apply(sp))
        if(survivingSetOfThreads.isEmpty)
          survivingSetOfThreads
        else
          recurse(sp + 1, survivingSetOfThreads)
      }
    } 

    val firstThreadSet = Set(Thread(0, Set(), "", Seq()))
    val finalThreads = recurse(0, firstThreadSet)
    val compactedFinalSetOfThreads = compact(finalThreads)
    val survivor = compactedFinalSetOfThreads.find((thread) => program.apply(thread.pc) match {
        case Accept => true
        case _ => false
      })
    survivor match {
      case Some(finalThread) => Some(finalThread.parse.last)
      case None => None
    }
  }

  // A thread of execution for the VM, where 'pc' is the program counter,
  // 'progress' is the set of encountered CheckProgress instructions, 'priority'
  // is the thread priority (lower is better), 'parse' is the current parsing
  // stack. We don't need a separate string position per thread because all
  // executing threads will, by construction, always be at the same string
  // position. 
  private case class Thread(pc: Int, progress: Set[Int], priority: String,
    parse: Seq[ParseTree])
}
