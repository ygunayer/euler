package euler

import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

/**
 * The base problem class. Implementing classes should present the problem as comments, but not the solution itself.
 * 
 * @param number the number of the problem
 */
@RunWith(classOf[JUnitRunner])
abstract class Problem(number: Int) extends FunSuite {
  /**
   * Runs a problem as a test and outputs the result and the time it took to solve.
   */
  test(f"Problem #$number") {
    val t0 = System.currentTimeMillis()
    val result = solve
    val dt = System.currentTimeMillis() - t0
    info(f"Result: $result ($dt ms)")
  }
  
  /**
   * The actual function that solves a given problem. This must be implemented by all solutions.
   */
  def solve: Any
}
