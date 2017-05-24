library(testthat)

expect_that(make_filename(2017), equals("accident_2017.csv.bz2"))