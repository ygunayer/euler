package euler

import scala.collection.mutable.BitSet

/**
 * Let d(n) be defined as the sum of proper divisors of n (numbers less than n which divide evenly into n).
 * If d(a) = b and d(b) = a, where a ≠ b, then a and b are an amicable pair and each of a and b are called amicable numbers.
 *
 * For example, the proper divisors of 220 are 1, 2, 4, 5, 10, 11, 20, 22, 44, 55 and 110; therefore d(220) = 284. The proper divisors of 284 are 1, 2, 4, 71 and 142; so d(284) = 220.
 *
 * Evaluate the sum of all the amicable numbers under 10000.
 */
class Problem21 extends Problem(21) {
  def solve: Any = {
    def d(n: Long): Long = Utils.divisorsOf(n).sum

    def amicable(n: Long): Boolean = {
      val dn = d(n)
      n != dn && d(dn) == n
    }

    (1L until 10000).filter(amicable).sum
  }
}

/**
 * Using names.txt (src/main/resources/p022_names.txt), a 46K text file containing over five-thousand first names, begin by sorting it into alphabetical order. Then working out the alphabetical value for each name, multiply this value by its alphabetical position in the list to obtain a name score.
 *
 * For example, when the list is sorted into alphabetical order, COLIN, which is worth 3 + 15 + 12 + 9 + 14 = 53, is the 938th name in the list. So, COLIN would obtain a score of 938 × 53 = 49714.
 *
 * What is the total of all the name scores in the file?
 */
class Problem22 extends Problem(22) {
  def solve: Any = {
    // note: quotes are removed from the names file
    def wordScore(name: String): Int = name.map(_ - 'A' + 1).sum

    val names = problem22Names.getLines.next.split(",").sorted.zipWithIndex

    names.map({ case (name, idx) => wordScore(name) * (idx + 1) }).sum
  }
}

/**
 * A perfect number is a number for which the sum of its proper divisors is exactly equal to the number. For example, the sum of the proper divisors of 28 would be 1 + 2 + 4 + 7 + 14 = 28, which means that 28 is a perfect number.
 *
 * A number n is called deficient if the sum of its proper divisors is less than n and it is called abundant if this sum exceeds n.
 *
 * As 12 is the smallest abundant number, 1 + 2 + 3 + 4 + 6 = 16, the smallest number that can be written as the sum of two abundant numbers is 24. By mathematical analysis, it can be shown that all integers greater than 28123 can be written as the sum of two abundant numbers. However, this upper limit cannot be reduced any further by analysis even though it is known that the greatest number that cannot be expressed as the sum of two abundant numbers is less than this limit.
 *
 * Find the sum of all the positive integers which cannot be written as the sum of two abundant numbers.
 */
class Problem23 extends Problem(23) {
  def solve: Any = {
    val max = 28123
    val abundantSums = BitSet(max)
    lazy val abundants = (1 until max) filter Utils.isAbundant

    for {
      x <- abundants
      y <- abundants
      if (x + y) < max
    } abundantSums(x + y) = true

    (1 to max).filter(!abundantSums(_)).sum
  }
}

/**
 * A permutation is an ordered arrangement of objects. For example, 3124 is one possible permutation of the digits 1, 2, 3 and 4. If all of the permutations are listed numerically or alphabetically, we call it lexicographic order. The lexicographic permutations of 0, 1 and 2 are:
 *
 * 012   021   102   120   201   210
 *
 * What is the millionth lexicographic permutation of the digits 0, 1, 2, 3, 4, 5, 6, 7, 8 and 9?
 */
class Problem24 extends Problem(24) {
  def solve: Any = {
    (0 to 9).toList.permutations.drop(999999).take(1).next.mkString("")
  }
}

/**
 * The Fibonacci sequence is defined by the recurrence relation:
 *
 * F(n) = F(n−1) + F(n−2), where F(1) = 1 and F(2) = 1.
 *
 * Hence the first 12 terms will be:
 *
 * F(1) = 1
 * F(2) = 1
 * F(3) = 2
 * F(4) = 3
 * F(5) = 5
 * F(6) = 8
 * F(7) = 13
 * F(8) = 21
 * F(9) = 34
 * F(10) = 55
 * F(11) = 89
 * F(12) = 144
 *
 * The 12th term, F(12), is the first term to contain three digits.
 *
 * What is the first term in the Fibonacci sequence to contain 1000 digits?
 */
class Problem25 extends Problem(25) {
  def solve: Any = {
    Utils.fibonacci.takeWhile(_.toString.length < 1000).size
  }
}

/**
 * A unit fraction contains 1 in the numerator. The decimal representation of the unit fractions with denominators 2 to 10 are given:
 *
 * 1/2 =   0.5
 * 1/3 =   0.(3)
 * 1/4 =   0.25
 * 1/5 =   0.2
 * 1/6 =   0.1(6)
 * 1/7 =   0.(142857)
 * 1/8 =   0.125
 * 1/9 =   0.(1)
 * 1/10    =   0.1
 *
 * Where 0.1(6) means 0.166666..., and has a 1-digit recurring cycle. It can be seen that 1/7 has a 6-digit recurring cycle.
 *
 * Find the value of d < 1000 for which 1/d contains the longest recurring cycle in its decimal fraction part.
 */
class Problem26 extends Problem(26) {
  def solve: Any = {
    /**
     * for a given decimal number 1/n, the number of repeating decimals, x, satisfies 10^x = 1 (mod n)
     * for example, 1/7 = 0.(142857), and 10^6 = 1 (mod 7)
     * we can simply find the x for a given number n by brute force
     * just don't forget to use the BigInt because we're working with big numbers such as 10^999
     */
    def rcc(n: Int) = (1 to 1000).filter(x => (BigInt(10) pow x) % n == 1).headOption.getOrElse(0)
    val maxRcc = (1 to 1000).map(x => (x, rcc(x))).maxBy(_._2)
    maxRcc._1 + " (" + maxRcc._2 + " digits)"
  }
}

/**
 * Euler discovered the remarkable quadratic formula:
 *
 * n^2 + n + 41
 *
 * It turns out that the formula will produce 40 primes for the consecutive values n = 0 to 39. However, when n = 40, 40^2 + 40 + 41 = 40(40 + 1) + 41 is divisible by 41, and certainly when n = 41, 41^2 + 41 + 41 is clearly divisible by 41.
 *
 * The incredible formula  n^2 − 79n + 1601 was discovered, which produces 80 primes for the consecutive values n = 0 to 79. The product of the coefficients, −79 and 1601, is −126479.
 *
 * Considering quadratics of the form:
 *
 * n^2 + an + b, where |a| < 1000 and |b| < 1000
 *
 * where |n| is the modulus/absolute value of n
 * e.g. |11| = 11 and |−4| = 4
 *
 * Find the product of the coefficients, a and b, for the quadratic expression that produces the maximum number of primes for consecutive values of n, starting with n = 0.
 */
class Problem27 extends Problem(27) {
  def solve: Any = {
    lazy val primeSieve = new SievedPrimeCheck(1000001)

    def primeCount(a: Int, b: Int): Int = Stream.from(0).map(n => n * (a + n) + b).takeWhile(x => x > 0 && primeSieve.isPrime(x)).length

    val range = -999 to 999

    val (a, b) = (for {
      a <- range
      b <- range
    } yield (a, b)).maxBy({ case (a, b) => primeCount(a, b) })

    a * b
  }
}

/**
 * Starting with the number 1 and moving to the right in a clockwise direction a 5 by 5 spiral is formed as follows:
 *
 * 21 22 23 24 25
 * 20  7  8  9 10
 * 19  6  1  2 11
 * 18  5  4  3 12
 * 17 16 15 14 13
 *
 * It can be verified that the sum of the numbers on the diagonals is 101.
 *
 * What is the sum of the numbers on the diagonals in a 1001 by 1001 spiral formed in the same way?
 */
class Problem28 extends Problem(28) {
  def solve: Any = {
    val width = 1001

    /**
     * Centered octagonal number function
     */
    def con(k: Int): Int = {
      val x = 2 * k - 1
      x * x
    }

    1 + (3 to width by 2).map(con).sum
  }
}

/**
 * Consider all integer combinations of ab for 2 ≤ a ≤ 5 and 2 ≤ b ≤ 5:
 *
 * 2^2=4, 2^3=8, 2^4=16, 2^5=32
 * 3^2=9, 3^3=27, 3^4=81, 3^5=243
 * 4^2=16, 4^3=64, 4^4=256, 4^5=1024
 * 5^2=25, 5^3=125, 5^4=625, 5^5=3125
 *
 * If they are then placed in numerical order, with any repeats removed, we get the following sequence of 15 distinct terms:
 *
 * 4, 8, 9, 16, 25, 27, 32, 64, 81, 125, 243, 256, 625, 1024, 3125
 *
 * How many distinct terms are in the sequence generated by ab for 2 ≤ a ≤ 100 and 2 ≤ b ≤ 100?
 */
class Problem29 extends Problem(29) {
  def solve: Any = {
    val range = 2 to 100
    (for {
      a <- range
      b <- range
    } yield BigInt(a) pow b.toInt).toSet.size
  }
}

/**
 * Surprisingly there are only three numbers that can be written as the sum of fourth powers of their digits:
 *
 * 1634 = 1^4 + 6^4 + 3^4 + 4^4
 * 8208 = 8^4 + 2^4 + 0^4 + 8^4
 * 9474 = 9^4 + 4^4 + 7^4 + 4^4
 *
 * As 1 = 14 is not a sum it is not included.
 *
 * The sum of these numbers is 1634 + 8208 + 9474 = 19316.
 *
 * Find the sum of all the numbers that can be written as the sum of fifth powers of their digits.
 */
class Problem30 extends Problem(30) {
  def solve: Any = {
    def check(x: Int, p: Int): Boolean = x.toString.map(c => Math.pow(c.asDigit, p)).sum == x
    (2 to 1000000).filter(check(_, 5)).sum
  }
}