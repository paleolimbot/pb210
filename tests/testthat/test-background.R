
test_that("excess function works", {
  expect_equal(as.numeric(pb210_excess(2:11)), 2:11)
  expect_equal(as.numeric(pb210_excess(2:11, 1)), 1:10)
})

test_that("excess function propogates error", {
  expect_is(pb210_excess(2:11), "errors")
  expect_equal(
    errors(pb210_excess(set_errors(3:12, 1), set_errors(1, 1))),
    rep(sqrt(2), 10)
  )
})

test_that("excess function accepts error objects", {
  expect_equal(
    pb210_excess(set_errors(3:11, 1), set_errors(1, 1)),
    set_errors(3:11, 1) - set_errors(1, 1)
  )
})

test_that("excess function nixes error after first background", {
  expect_equal(
    pb210_excess(c(1, 0, 1)),
    set_errors(c(1, NA, NA), c(0, NA, NA))
  )
})
