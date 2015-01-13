package com.davidkblackmon.projecteuler {
  object Problems {
    def main(args: Array[String]): Unit = {
      println("This project is intended to be run from a sbt console")
    }
  }

  package problem1 {
    object Problem1 {
      def solution(max: Int): Int = {
        (1 until max).filter(summable).sum
      }

      def summable(x: Int): Boolean = {
        x % 3 == 0 || x % 5 == 0
      }
    }
  }

  package problem2 {
    object Problem2 {
      def solution(max: Int): Int = {
        fibs.takeWhile(f => f < max).filter(summable).sum
      }
      val fibs: Stream[Int] = 0 #:: 1 #:: fibs.zip(fibs.tail).map(n => n._1 + n._2)

      def summable(x: Int): Boolean = {
        x % 2 == 0
      }
    }
  }

  package problem3 {
    object Problem3 {
      def solution(num: Double): Int = {
        largestPrimeFactorOf(num)
      }

      def largestPrimeFactorOf(num: Double): Int = {
        primes.takeWhile( i => i < Math.sqrt(num).toInt ).filter( x => num % x == 0 ).max
      }

      lazy val primes: Stream[Int] = 2 #:: Stream.from(3).filter(i =>
          primes.takeWhile{ j => j * j <= i }.forall{ k => i % k > 0 })
    }
  }

  package problem4 {
    object Problem4 {
      def solution(digits: Int): Int = {
        val max = Math.pow(10,digits).toInt
        products(max).filter(isPalindrome).max
      }

      def products(max: Int): List[Int] = {
        val l = List.range(1,max)
        l.flatMap( x => l.map(_ * x) )
      }

      def isPalindrome(num: Int): Boolean = {
        val str = num.toString
        str == str.reverse
      }
    }
  }

  package problem5 {
    object Problem5 {
      def solution(max: Int): Int = {
        val x = (1 to max).toList
        x.map(a => x.combinations(a).map(b => b.product).toList).flatten.distinct.filter(c => isDivisible(c,x)).min
      }

      def isDivisible(x: Int, divisors: List[Int]): Boolean = {
        divisors.forall( y => x % y == 0 )
      }
    }
  }

  package problem6 {
    object Problem6 {
      def solution(x: Int): Int = {
        squareOfSum(x) - sumOfSquares(x)
      }

      def sumOfSquares(x: Int): Int = {
        (1 to x).map( a => a * a).sum
      }

      def squareOfSum(x: Int): Int = {
        Math.pow((1 to x).sum, 2).toInt
      }
    }
  }

  package problem7 {
    object Problem7 {
      def solution(num: Int): Int = {
        primes.take(num).last
      }

      lazy val primes: Stream[Int] = 2 #:: Stream.from(3).filter(i =>
          primes.takeWhile{ j => j * j <= i }.forall{ k => i % k > 0 })
    }
  }

  package problem8 {
    object Problem8 {
      def solution(size: Int): Double = {
        input.sliding(size).toList.map(a => productOfChars(a)).max
      }

      def productOfChars(str: String): Long = {
        str.split("").tail.map(_.toLong).product
      }

      val input = """73167176531330624919225119674426574742355349194934
96983520312774506326239578318016984801869478851843
85861560789112949495459501737958331952853208805511
12540698747158523863050715693290963295227443043557
66896648950445244523161731856403098711121722383113
62229893423380308135336276614282806444486645238749
30358907296290491560440772390713810515859307960866
70172427121883998797908792274921901699720888093776
65727333001053367881220235421809751254540594752243
52584907711670556013604839586446706324415722155397
53697817977846174064955149290862569321978468622482
83972241375657056057490261407972968652414535100474
82166370484403199890008895243450658541227588666881
16427171479924442928230863465674813919123162824586
17866458359124566529476545682848912883142607690042
24219022671055626321111109370544217506941658960408
07198403850962455444362981230987879927244284909188
84580156166097919133875499200524063689912560717606
05886116467109405077541002256983155200055935729725
71636269561882670428252483600823257530420752963450""".filter(_ >= ' ')
    }
  }
}

