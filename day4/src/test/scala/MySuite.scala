// For more information on writing tests, see
// https://scalameta.org/munit/docs/getting-started.html
class MySuite extends munit.FunSuite:
  test("Day4 : main test on final data"):
    val (score1, score2) = Solver.solve
    assertEquals(score1, "137896")
    assertEquals(score2, "501")

  test("Day4 : main test on initial data"):
    val (score1, score2) = Solver.solveTest
    assertEquals(score1, "1514")
    assertEquals(score2, "N/A")
