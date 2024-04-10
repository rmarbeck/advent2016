// For more information on writing tests, see
// https://scalameta.org/munit/docs/getting-started.html
class MySuite extends munit.FunSuite:
  test("Day18 : main test on final data"):
    val (score1, score2) = Solver.solve
    assertEquals(score1, "1951")
    assertEquals(score2, "20002936")

  test("Day18 : main test on initial data"):
    val (score1, score2) = Solver.solveTest
    assertEquals(score1, "38")
    assertEquals(score2, "38")