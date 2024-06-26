// For more information on writing tests, see
// https://scalameta.org/munit/docs/getting-started.html
class MySuite extends munit.FunSuite:
  test("Day9 : main test on final data"):
    val (score1, score2) = Solver.solve
    assertEquals(score1, "112830")
    assertEquals(score2, "10931789799")

  test("Day9 : main test on initial data"):
    val (score1, score2) = Solver.solveTest
    assertEquals(score1, "18")
    assertEquals(score2, "20")
