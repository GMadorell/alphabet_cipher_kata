package com.scalabcn.alphabetcipher

import scala.annotation.tailrec

object AlphabetCipher {
  def toOffset(c: Char): Int = c - 'a'

  @tailrec
  def mod26(n: Int): Int = if (n < 0) mod26(n + 26) else n % 26

  def fromOffset(i: Int): Char = ('a' + i).toChar

  def cycle(keyword: String): Stream[Char] = Stream.continually(keyword).flatten

  def encode(keyword: String, message: String): String =
    operateOnOffsets(message, cycle(keyword))(_ + _)

  def decode(keyword: String, message: String): String =
    operateOnOffsets(message, cycle(keyword))(_ - _)

  private def operateOnOffsets(s1: Seq[Char], s2: Seq[Char])(f: (Int, Int) => Int): String =
    s1.zip(s2).map {
      case (k,m) => fromOffset(mod26(f(toOffset(k), toOffset(m))))
    }.mkString

  private def unRepeat(s: String): String =
    prefixesOf(s).find(prefix => cycleUpTo(prefix, s.length) == s).getOrElse(s)

  private def prefixesOf(s: String) =  s.inits.toList.reverse.tail

  private def cycleUpTo(s: String, length: Int) = cycle(s).take(length).mkString

  def decipher(cipher: String, message: String): String = unRepeat(operateOnOffsets(cipher, message)(_ - _))
}
