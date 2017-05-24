library(testthat)

expect_that(fars_map_state(1, 2017), throws_error())