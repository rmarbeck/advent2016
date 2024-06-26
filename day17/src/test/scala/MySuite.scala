// For more information on writing tests, see
// https://scalameta.org/munit/docs/getting-started.html
class MySuite extends munit.FunSuite:
  test("Day17 : main test on final data"):
    val (score1, score2) = Solver.solve
    assertEquals(score1, "RDURRDDLRD")
    assertEquals(score2, "526")

  test("Day17 : main test on initial data"):
    val (score1, score2) = Solver.solveTest
    assertEquals(score1, "DRURDRUDDLLDLUURRDULRLDUUDDDRR")
    assertEquals(score2, "830")
