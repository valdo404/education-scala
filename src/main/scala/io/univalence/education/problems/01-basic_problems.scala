package io.univalence.education.problems

import io.univalence.education.internal.exercise_tools.*

/**
 * Here are a series of problems to solve in Scala.
 *
 * The goal is to solve those problems with a code:
 *   - respecting the principles of functional programming
 *   - which is the more possibly readable.
 */
@main
def _01_basic_problems(): Unit = {
  exercise("salary increase") {
    def increase(salaries: List[Double], rate: Double): List[Double] = salaries.map(salary => salary * (1 + rate))

    check(increase(List(1000.0, 2000.0, 3500.0), 0.02) == List(1020.0, 2040.0, 3570.0))
  }

  exercise("average") {
    def average(it: Iterator[Double]): Double = {
      var sum = 0.0
      var count = 0
      while (it.hasNext) {
        sum += it.next()
        count += 1
      }
      if (count == 0) 0.0 else sum / count
    }

    check(average(Iterator.empty) == 0.0)
    check(average(Iterator(1.0)) == 1.0)
    check(average(Iterator(1.0, 3.0)) == 2.0)
  }

  exercise("recursive factorial") {
    def factorial(n: Int): Int = {
      if (n <= 1) 1
      else n * factorial(n - 1)
    }

    check(factorial(0) == 1)
    check(factorial(1) == 1)
    check(factorial(2) == 2)
    check(factorial(3) == 6)
    check(factorial(4) == 24)
  }

  exercise("recursive Fibonacci sequence") {

    /**
     * The Fibonacci sequence is a sequence where the value of an
     * iteration is the sum of the value of the two previous iterations.
     * The two first values are 1 and 1.
     *
     * @param n
     *   n should be > 0
     */
    def fibonacci(n: Int): Int = {
      if (n <= 1) 1
      else fibonacci(n - 1) + fibonacci(n - 2)
    }

    check(fibonacci(0) == 1)
    check(fibonacci(1) == 1)
    check(fibonacci(2) == 2)
    check(fibonacci(3) == 3)
    check(fibonacci(4) == 5)
    check(fibonacci(5) == 8)
  }

  exercise("word count") {
    def wordCount(text: String): Map[String, Int] = {
      if (text.isEmpty) Map.empty
      else text.split(" ").groupBy(identity).view.mapValues(_.length).toMap
    }

    check(wordCount("") == Map.empty)
    check(wordCount("ab") == Map("ab" -> 1))
    check(wordCount("ab ab") == Map("ab" -> 2))
    check(wordCount("ab cd") == Map("ab" -> 1, "cd" -> 1))
    check(wordCount("ab cd ab ef ef ef") == Map("ab" -> 2, "cd" -> 1, "ef" -> 3))
  }

  exercise("anagram") {
    def isPalindrome(text: String): Boolean = {
      text == text.reverse
    }

    check(isPalindrome(""))
    check(isPalindrome("a"))
    check(isPalindrome("aa"))
    check(!isPalindrome("ab"))
    check(isPalindrome("abba"))
    check(isPalindrome("radar"))
  }

  enum Tree[+A]:
    case Leaf
    case Node(value: A, left: Tree[A], right: Tree[A])

  object Tree:
    def simpleNode[A](value: A): Tree[A] = Tree.Node(value, Tree.Leaf, Tree.Leaf)

  import Tree.*

  exercise("size of a tree") {
    def size(tree: Tree[_]): Int = tree match {
      case Leaf => 0
      case Node(_, left, right) => 1 + size(left) + size(right)
    }

    check(size(Leaf) == 0)
    check(size(simpleNode(1)) == 1)
    check(size(Node(1, Leaf, simpleNode(2))) == 2)
    check(size(Node(1, Leaf, Node(2, simpleNode(3), simpleNode(4)))) == 4)
  }

  exercise("size of the longuest path of a tree") {
    def longuestPath(tree: Tree[_]): Int = tree match {
      case Leaf => 0
      case Node(_, left, right) => 1 + math.max(longuestPath(left), longuestPath(right))
    }

    check(longuestPath(Leaf) == 0)
    check(longuestPath(simpleNode(1)) == 1)
    check(longuestPath(Node(1, Leaf, simpleNode(2))) == 2)
    check(longuestPath(Node(1, Leaf, Node(2, simpleNode(3), simpleNode(4)))) == 3)
    check(longuestPath(Node(1, simpleNode(5), Node(2, simpleNode(3), simpleNode(4)))) == 3)
  }

  exercise("make string, depth first approach - v1") {
    def mkString[A](tree: Tree[A], separator: String): String = {
      def traverse(t: Tree[A]): List[A] = t match {
        case Leaf => List.empty
        case Node(value, left, right) => value :: (traverse(left) ++ traverse(right))
      }
      traverse(tree).mkString(separator)
    }

    check(mkString(Leaf, ", ") == "")
    check(mkString(simpleNode("a"), ", ") == "a")
    check(mkString(Node("a", simpleNode("b"), Leaf), ", ") == "a, b")
    check(mkString(Node("a", Leaf, simpleNode("b")), ", ") == "a, b")
    check(mkString(Node("a", simpleNode("b"), simpleNode("c")), ", ") == "a, b, c")
    check(
      mkString(Node("a", simpleNode("b"), Node("c", simpleNode("d"), simpleNode("e"))), ", ")
        == "a, b, c, d, e"
    )
  }

  exercise("make string, depth first approach - v2") {
    def mkString[A](tree: Tree[A], separator: String): String = {
      def traverse(t: Tree[A]): List[A] = t match {
        case Leaf => List.empty
        case Node(value, left, right) => (traverse(left) ++ traverse(right)) :+ value
      }
      traverse(tree).mkString(separator)
    }

    check(mkString(Leaf, ", ") == "")
    check(mkString(simpleNode("a"), ", ") == "a")
    check(mkString(Node("a", simpleNode("b"), Leaf), ", ") == "b, a")
    check(mkString(Node("a", Leaf, simpleNode("b")), ", ") == "b, a")
    check(mkString(Node("a", simpleNode("b"), simpleNode("c")), ", ") == "b, c, a")
    check(
      mkString(Node("a", simpleNode("b"), Node("c", simpleNode("d"), simpleNode("e"))), ", ")
        == "b, d, e, c, a"
    )
  }

  exercise("make string, depth first approach - v3") {
    def mkString[A](tree: Tree[A], separator: String): String = {
      def traverse(t: Tree[A]): List[A] = t match {
        case Leaf => List.empty
        case Node(value, left, right) => (traverse(left) ++ traverse(right)) :+ value
      }
      traverse(tree).mkString(separator)
    }

    check(mkString(Leaf, ", ") == "")
    check(mkString(simpleNode("a"), ", ") == "a")
    check(mkString(Node("a", simpleNode("b"), Leaf), ", ") == "b, a")
    check(mkString(Node("a", Leaf, simpleNode("b")), ", ") == "b, a")
    check(mkString(Node("a", simpleNode("b"), simpleNode("c")), ", ") == "b, c, a")
    check(
      mkString(Node("a", simpleNode("b"), Node("c", simpleNode("d"), simpleNode("e"))), ", ")
        == "b, d, e, c, a"
    )
  }
}
