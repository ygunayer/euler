package euler

import java.util.concurrent.ConcurrentMap

trait PrimeCheck {
  def isPrime(x: Long): Boolean
}

object SimplePrimeCheck extends PrimeCheck {
  def isPrime(x: Long): Boolean = !(2L to math.sqrt(x).toLong).exists(a => x % a == 0)
}

object Utils {
  def factorial(x: Int): BigInt = (BigInt(1) to x).product
  
  def fibonacci: Stream[Long] = {
    def tail(h: Long, n: Long): Stream[Long] = h #:: tail(n, h + n)
    tail(0, 1)
  }

  def isPrime(x: Int)(implicit c: PrimeCheck = SimplePrimeCheck) = c.isPrime(x)

  val primes = Stream.from(2).filter(isPrime)

  def firstPrimeFactorFor(x: Int): Int = primes.filter(x % _ == 0)(0)

  def primeFactorsOf(x: Long): List[Long] = {
    val r = (2L to math.sqrt(x).toLong).find(x % _ == 0)
    r match {
      case Some(d) => d :: primeFactorsOf(x / d)
      case None => List(x)
    }
  }
  
  def divisorsOf(x: Long): List[Long] = (1L until x).filter(x % _ == 0).toList
}