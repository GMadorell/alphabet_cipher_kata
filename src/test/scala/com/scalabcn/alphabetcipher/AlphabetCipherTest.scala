package com.scalabcn.alphabetcipher

import org.scalatest.{Matchers, WordSpec}

final class AlphabetCipherTest extends WordSpec with Matchers {
  "An AlphabetCipher" should {
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
