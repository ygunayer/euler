package euler

import Utils.RichInt;

/**
 * In England the currency is made up of pound, £, and pence, p, and there are eight coins in general circulation:
 *
 * 1p, 2p, 5p, 10p, 20p, 50p, £1 (100p) and £2 (200p).
 *
 * It is possible to make £2 in the following way:
 *
 * 1×£1 + 1×50p + 2×20p + 1×5p + 1×2p + 3×1p
 *
 * How many different ways can £2 be made using any number of coins?
 */
class Problem31 extends Problem(31) {
  def solve: Any = {
    def make(x: Int, coins: List[Int]): Int = {
      if (x == 0) 1
      else if (x < 0 || coins.isEmpty) 0
      else make(x, coins.tail) + make(x - coins.head, coins)
    }
    make(200, List(1, 2, 5, 10, 20, 50, 100, 200))
  }
}

/**
 * We shall say that an n-digit number is pandigital if it makes use of all the digits 1 to n exactly once; for example, the 5-digit number, 15234, is 1 through 5 pandigital.
 *
 * The product 7254 is unusual, as the identity, 39 × 186 = 7254, containing multiplicand, multiplier, and product is 1 through 9 pandigital.
 *
 * Find the sum of all products whose multiplicand/multiplier/product identity can be written as a 1 through 9 pandigital.
 * HINT: Some products can be obtained in more than one way so be sure to only include it once in your sum.
 */
class Problem32 extends Problem(32) {
  def solve: Any = {
    def toNum(x: Seq[Int]): Int = x.reduceLeft((a, b) => 10 * a + b)

    val candidates = (1 to 9).permutations
    val validProducts = for {
      x <- candidates
      i <- 1 to 2
      a = toNum(x.take(i)) // first two digits
      b = toNum(x.slice(i, 5)) // next three digits
      n = toNum(x.drop(5)) // last 4 digits
      if (a * b == n)
    } yield n
    validProducts.toSet.sum
  }
}

/**
 * The fraction 49/98 is a curious fraction, as an inexperienced mathematician in attempting to simplify it may incorrectly believe that 49/98 = 4/8, which is correct, is obtained by cancelling the 9s.
 *
 * We shall consider fractions like, 30/50 = 3/5, to be trivial examples.
 *
 * There are exactly four non-trivial examples of this type of fraction, less than one in value, and containing two digits in the numerator and denominator.
 *
 * If the product of these four fractions is given in its lowest common terms, find the value of the denominator.
 */
class Problem33 extends Problem(33) {
  def solve: Any = {
    // if we name digits as A, B and K (K being the one that's cancelled) there are four possible outcomes: AK/BK = A/B, KA/KB = A/B, AK/KB = A/B, KA/BK = A/B
    // when reduced, the first two equations require A = B which isn't possible because the fraction A/B must be less than 1
    // therefore we can only test the other two equations which require (10*B*K - A*K = 9*A*B) and (10*A*K - B*K = 9*A*B) respectively

    val nums = for {
      a <- 1 to 9
      b <- a + 1 to 9
      k <- 1 to 9
    } yield (a, b, k)

    val (num, denom) = nums.map({
      case (a, b, k) => {
        if ((10 * b * k - a * k) == 9 * a * b) Some((10 * k + a), (10 * b + k)) // KA/BK == A/B
        if ((10 * a * k - b * k == 9 * a * b)) Some((10 * a + k), (10 * k + b)) // AK/KB == A/B
        else None
      }
    }).filter(p => !p.isEmpty).map(_.get).reduceLeft((a, b) => (a._1 * b._1, a._2 * b._2))
    val gcd = Utils.gcd(num, denom)
    denom / gcd
  }
}

/**
 * 145 is a curious number, as 1! + 4! + 5! = 1 + 24 + 120 = 145.
 *
 * Find the sum of all numbers which are equal to the sum of the factorial of their digits.
 *
 * Note: as 1! = 1 and 2! = 2 are not sums they are not included.
 */
class Problem34 extends Problem(34) {
  def solve: Any = {
    val factorials = (0 to 9).map(Utils.factorial)
    (10 to 999999).filter(k => k.digits.map(factorials).sum == k).sum
  }
}

/**
 * The number, 197, is called a circular prime because all rotations of the digits: 197, 971, and 719, are themselves prime.
 *
 * There are thirteen such primes below 100: 2, 3, 5, 7, 11, 13, 17, 31, 37, 71, 73, 79, and 97.
 *
 * How many circular primes are there below one million?
 */
class Problem35 extends Problem(35) {
  def solve: Any = {
    val lim = 1000000
    lazy val sieve = new SievedPrimeCheck(lim)

    def rotations(x: List[Int]): Set[List[Int]] = {
      def rotateOnce(a: List[Int]): List[Int] = a.tail :+ a.head
      def rotateAcc(c: List[Int], acc: List[List[Int]]): List[List[Int]] = {
        if (c == x) acc
        else rotateAcc(rotateOnce(c), c :: acc)
      }
      val rot0 = rotateOnce(x)
      rotateAcc(rot0, List(x, rot0)).toSet
    }

    def toNum(x: Seq[Int]): Int = x.reduceLeft((a, b) => 10 * a + b)
    def isValid(x: Int): Boolean = rotations(x.digits.toList).forall(p => sieve.isPrime(toNum(p)))
    (1 to lim).count(isValid)
  }
}

/**
 * The decimal number, 585 = 1001001001 (binary), is palindromic in both bases.
 *
 * Find the sum of all numbers, less than one million, which are palindromic in base 10 and base 2.
 *
 * (Please note that the palindromic number, in either base, may not include leading zeros.)
 */
class Problem36 extends Problem(36) {
  def solve: Any = {
    def isPalindrome(s: String): Boolean = s.reverse == s
    (1 to 1000000).filter(x => isPalindrome(x.toBinaryString) && isPalindrome(x.toString)).sum
  }
}

/**
 * The number 3797 has an interesting property. Being prime itself, it is possible to continuously remove digits from left to right, and remain prime at each stage: 3797, 797, 97, and 7.
 * Similarly we can work from right to left: 3797, 379, 37, and 3.
 *
 * Find the sum of the only eleven primes that are both truncatable from left to right and right to left.
 *
 * NOTE: 2, 3, 5, and 7 are not considered to be truncatable primes.
 */
class Problem37 extends Problem(37) {
  def solve: Any = {
    val lim = 1000000
    lazy val sieve = new SievedPrimeCheck(lim)

    def toNum(x: Seq[Int]): Int = x.reduceLeft((a, b) => 10 * a + b)

    // very dirty implementation
    def isTruncatableLTR(x: Int): Boolean = {
      val isPrime = sieve.isPrime(x)
      if (!isPrime) false
      else if (x < 10) isPrime
      else {
        val r = toNum(x.digits.tail)
        isTruncatableLTR(r)
      }
    }

    def isTruncatableRTL(x: Int): Boolean = {
      val isPrime = sieve.isPrime(x)
      if (!isPrime) false
      else if (x < 10) isPrime
      else {
        val r = toNum(x.digits.reverse.tail.reverse)
        isTruncatableRTL(r)
      }
    }

    def isTruncatable(x: Int): Boolean = isTruncatableLTR(x) && isTruncatableRTL(x)

    (11 to lim).filter(isTruncatable).sum
  }
}

/**
 * Take the number 192 and multiply it by each of 1, 2, and 3:
 *
 * 192 × 1 = 192
 * 192 × 2 = 384
 * 192 × 3 = 576
 *
 * By concatenating each product we get the 1 to 9 pandigital, 192384576. We will call 192384576 the concatenated product of 192 and (1,2,3)
 *
 * The same can be achieved by starting with 9 and multiplying by 1, 2, 3, 4, and 5, giving the pandigital, 918273645, which is the concatenated product of 9 and (1,2,3,4,5).
 *
 * What is the largest 1 to 9 pandigital 9-digit number that can be formed as the concatenated product of an integer with (1,2, ... , n) where n > 1?
 */
class Problem38 extends Problem(38) {
  def solve: Any = {
    def multiples(x: Int, n: Int): List[Int] = (1 to n).map(_ * x).toList
    def isPandigital(p: List[Int]): Boolean = p.mkString("").sorted == "123456789"

    val candidates = for {
      x <- 1 to 10000
      n <- 1 to 9
    } yield multiples(x, n)

    candidates.filter(isPandigital).map(_.mkString("").toInt).max
  }
}

/**
 * If p is the perimeter of a right angle triangle with integral length sides, {a,b,c}, there are exactly three solutions for p = 120.
 *
 * {20,48,52}, {24,45,51}, {30,40,50}
 *
 * For which value of p ≤ 1000, is the number of solutions maximised?
 */
class Problem39 extends Problem(39) {
  def solve: Any = {
    val candidates = for {
      c <- 1 until 500
      a <- 1 until c
      b <- 1 until c
      p = a + b + c
      if (p <= 1000 && (c * c == a * a + b * b))
    } yield (a, b, c, p)
    candidates.groupBy(_._4).maxBy(_._2.size)._1
  }
}

/**
 * An irrational decimal fraction is created by concatenating the positive integers:
 *
 * 0.123456789101112131415161718192021...
 *
 * It can be seen that the 12th digit of the fractional part is 1.
 *
 * If d_n represents the nth digit of the fractional part, find the value of the following expression.
 *
 * d_1 × d_10 × d_100 × d_1000 × d_10000 × d_100000 × d_1000000
 */
class Problem40 extends Problem(40) {
  def solve: Any = {
    val fraction = (1 to 200000).mkString("") // an upper limit of 200000 is enough to construct a fraction of over 1000000 digits
    val digits = for {
      i <- 0 to 6
      n = Math.pow(10, i).toInt
    } yield fraction(n - 1).asDigit
    digits.product
  }
}