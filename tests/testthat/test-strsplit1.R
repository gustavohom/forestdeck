test_that("strsplit1() splits a string", {
  expect_equal(strsplit1("a,b,c", split = ","), c("a", "b", "c"))
})

test_that("strsplit1() splits a string 2", {
  expect_equal(strsplit1("a,b,c", split = ","), c("a", "b", "c"))
})
