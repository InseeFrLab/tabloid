testthat::test_that("guarantee_sum100_closest", {
  testthat::expect_equal(guarantee_sum100(v = c(19.39, 43.61, 17.5, 19.5),
                                          n = 0,
                                          option = "closest",
                                          verbose = FALSE),
                         c(19, 44, 17, 20))
  testthat::expect_equal(guarantee_sum100(v = c(0.1, 9.8, 70.5, 19.6),
                                          n = 0,
                                          option = "closest",
                                          verbose = FALSE),
                         c(0, 10, 70, 20))
  testthat::expect_equal(guarantee_sum100(v = c(0.51, 0, 9.62, 70.12, 19.75),
                                          n = 0,
                                          option = "closest",
                                          verbose = FALSE),
                         c(0, 0, 10, 70, 20))
  testthat::expect_equal(guarantee_sum100(v = c(0.51, 0, 4.62, 69.12, 15.75),
                                          n = -1,
                                          option = "closest",
                                          verbose = FALSE),
                         c(0, 0, 10, 70, 20))
  testthat::expect_equal(guarantee_sum100(v = c(0, 22.892, 43.376, 23.732),
                                          n = -1,
                                          option = "closest",
                                          verbose = FALSE),
                         c(0, 20, 40, 20))
})

testthat::test_that("guarantee_sum100_lowest_lie", {
  testthat::expect_equal(guarantee_sum100(v = c(19.39, 43.61, 17.5, 19.5),
                                          n = 0,
                                          option = "lowest_lie",
                                          verbose = FALSE),
                         c(19, 43, 18, 20))
  testthat::expect_equal(guarantee_sum100(v = c(0.1, 9.8, 70.5, 19.6),
                                          n = 0,
                                          option = "lowest_lie",
                                          verbose = FALSE),
                         c(0, 10, 70, 20))
  testthat::expect_equal(guarantee_sum100(v = c(0.51, 0, 9.62, 70.12, 19.75),
                                          n = 0,
                                          option = "lowest_lie",
                                          verbose = FALSE),
                         c(1, 0, 10, 70, 19))
  testthat::expect_equal(guarantee_sum100(v = c(0.51, 0, 4.62, 69.12, 15.75),
                                          n = -1,
                                          option = "lowest_lie",
                                          verbose = FALSE),
                         c(0, 0, 10, 70, 20))
  testthat::expect_equal(guarantee_sum100(v = c(0, 22.892, 43.376, 23.732),
                                          n = -1,
                                          option = "lowest_lie",
                                          verbose = FALSE),
                         c(0, 20, 40, 20))
})
