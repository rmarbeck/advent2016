// For more information on writing tests, see
// https://scalameta.org/munit/docs/getting-started.html
class MySuite extends munit.FunSuite:
  test("Day3 : main test on final data"):
    val (score1, score2) = Solver.solve
    assertEquals(score1, "1050")
    assertEquals(score2, "1921")

  test("Day3 : main test on initial data"):
    val (score1, score2) = Solver.solveTest
    assertEquals(score1, "0")
    assertEquals(score2, "3")
