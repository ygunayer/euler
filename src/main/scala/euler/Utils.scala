package euler

import java.util.concurrent.ConcurrentMap
import scala.collection.mutable.BitSet
import scala.annotation.tailrec

trait PrimeCheck {
  def isPrime(x: Long): Boolean
}

object SimplePrimeCheck extends PrimeCheck {
  def isPrime(x: Long): Boolean = !(BigInt(2) to math.sqrt(x).toLong).exists(a => x % a == 0)
}

class SievedPrimeCheck(size: Int) extends PrimeCheck {
  private lazy val sieve = buildSieve(size)
  
  // http://rosettacode.org/wiki/Sieve_of_Eratosthenes#Genuine_Eratosthenes_sieve
  def buildSieve(limit: Int) = {
    if(limit > Int.MaxValue)
      throw new Error("Cannot build a sieve larger than " + Int.MaxValue)
    val (primes: collection.mutable.Set[Int], sqrtLimit) = (collection.mutable.Set.empty ++ (2 to limit), math.sqrt(limit).toInt)
    @tailrec
    def prim(candidate: Int): Unit = {
      if (candidate <= sqrtLimit) {
        if (primes contains candidate) primes --= candidate * candidate to limit by candidate
        prim(candidate + 1)
      }
    }
    prim(2)
    primes
  }

  def isPrime(x: Long): Boolean = sieve(x.toInt) // x will hopefully fit into int
}

object Utils {
  def factorial(x: Int): BigInt = (BigInt(1) to x).product

  def fibonacci: Stream[BigInt] = {
    def tail(h: BigInt, n: BigInt): Stream[BigInt] = h #:: tail(n, h + n)
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

  def divisorsOf(x: Long): Set[Long] = (2L to x / 2).filter(x % _ == 0).toSet + 1
  def isAbundant(n: Int): Boolean = divisorsOf(n).sum > n
}