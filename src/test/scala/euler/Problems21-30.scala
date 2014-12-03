package euler

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
 * Using names.txt (test/resources/p022_names.txt), a 46K text file containing over five-thousand first names, begin by sorting it into alphabetical order. Then working out the alphabetical value for each name, multiply this value by its alphabetical position in the list to obtain a name score.
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
    def abundant(n: Long): Boolean = Utils.divisorsOf(n).sum > n
    lazy val abundants = (1L to max / 2).view filter abundant
    
    abundants.sum
  }
}