package ch09test

/**
  * Created by miloszmuszynski on 31/08/2016.
  */
import ch09.ParenthesesGenerator
import org.scalatest.{FlatSpec, Matchers}

/**
  * 9.6
  */
class ParenthesesGeneratorTest extends FlatSpec with Matchers {

  "Parentheses generator" should "produce all combination of 3 pairs of parentheses" in {
    ParenthesesGenerator.combinations(3) should contain theSameElementsAs List("((()))","(()())","(())()","()(())","()()()")
  }

  "Parentheses generator" should "produce all combination of 2 pairs of parentheses" in {
    ParenthesesGenerator.combinations(2) should contain theSameElementsAs List("(())","()()")
  }

  "Parentheses generator" should "produce all combination of 1 pairs of parentheses" in {
    ParenthesesGenerator.combinations(1) should contain theSameElementsAs List("()")
  }

  "insert pair to list of commbinations" should "produce new correct list combinations with the new pair" in {
    ParenthesesGenerator.insertPair(List("()")) should contain theSameElementsAs List("(())", "()()")
  }

  "insert pair to a commbination" should "produce new correct list combinations with the new pair for ()" in {
    ParenthesesGenerator.insertPair("()") should contain theSameElementsAs List("(())", "()()")
  }

  "insert pair to a commbination" should "produce new correct list combinations with the new pair for (())" in {
    ParenthesesGenerator.insertPair("(())") should contain theSameElementsAs List("((()))", "(()())", "(())()")
  }

  "insert pair to a commbination" should "produce new correct list combinations with the new pair for ()()" in {
    ParenthesesGenerator.insertPair("()()") should contain theSameElementsAs List("(())()", "()(())", "()()()")
  }

}

