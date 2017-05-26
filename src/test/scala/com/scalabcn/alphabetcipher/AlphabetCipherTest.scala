package com.scalabcn.alphabetcipher

import org.scalactic.TypeCheckedTripleEquals
import org.scalatest.{Matchers, WordSpec}

final class AlphabetCipherTest extends WordSpec with Matchers with TypeCheckedTripleEquals {
  "An AlphabetCipher" should {
    "translate a char into an offset" in {
      AlphabetCipher.toOffset('a') shouldBe 0
      AlphabetCipher.toOffset('z') shouldBe 25
    }

    "translate a offset into a char" in {
      AlphabetCipher.fromOffset(0) shouldBe 'a'
      AlphabetCipher.fromOffset(25) shouldBe 'z'
    }

    "generate an infinite stream given a word" in {
      AlphabetCipher.cycle("a").take(5).mkString should ===("aaaaa")
      AlphabetCipher.cycle("key").take(5).mkString should ===("keyke")
    }

    "encode given a secret keyword" in {
      AlphabetCipher.encode("vigilance", "meetmeontuesdayeveningatseven") shouldBe "hmkbxebpxpmyllyrxiiqtoltfgzzv"
      AlphabetCipher.encode("scones", "meetmebythetree") shouldBe "egsgqwtahuiljgs"
    }

    "decode an cyrpted message given a secret keyword" in {
      AlphabetCipher.decode("vigilance", "hmkbxebpxpmyllyrxiiqtoltfgzzv") shouldBe "meetmeontuesdayeveningatseven"
      AlphabetCipher.decode("scones", "egsgqwtahuiljgs") shouldBe "meetmebythetree"
    }

    "extract the secret keyword given an encrypted message and the original message" in {
      AlphabetCipher.decipher(
        "opkyfipmfmwcvqoklyhxywgeecpvhelzg",
        "thequickbrownfoxjumpsoveralazydog") shouldBe "vigilance"
      AlphabetCipher.decipher(
        "hcqxqqtqljmlzhwiivgbsapaiwcenmyu",
        "packmyboxwithfivedozenliquorjugs") shouldBe "scones"
    }
  }
}
