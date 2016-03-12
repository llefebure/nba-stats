# Author: Luke Lefebure

test_that("endpoint querying", {
  e1 <- searchEndpoints()
  e2 <- searchEndpoints(".")
  e3 <- searchEndpoints("this should not match")
  expect_identical(e1, e2)
  expect_equal(length(e3), 0)
})

test_that("endpoint parameter querying", {
  expect_error(getEndpointParams("this does not exist"))
  expect_equal(class(getEndpointParams("shotchartdetail")), "character")
})

test_that("mapping", {
  m <- getIDMappings()
  expect_identical(names(m), c("player", "team"))
})