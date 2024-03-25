// For more information on writing tests, see
// https://scalameta.org/munit/docs/getting-started.html
class MySuite extends munit.FunSuite:
  test("Day1 : main test on final data"):
    val (score1, score2) = Solver.solve
    assertEquals(score1, "242")
    assertEquals(score2, "150")

  test("Day1 : main test on initial data"):
    val (score1, score2) = Solver.solveTest
    assertEquals(score1, "8")
    assertEquals(score2, "4")
