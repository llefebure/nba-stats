# Author: Luke Lefebure

test_that("endpoint querying", {
  e1 <- searchEndpoints()
  e2 <- searchEndpoints(".")
  e3 <- searchEndpoints("player")
  e4 <- searchEndpoints("this should not match")
  
  expect_equal(e1, e2) # both ways to get all endpoints
  expect_match(e3, "player") # results match what they should
  expect_equal(length(e4), 0) # no match
})

test_that("endpoint parameter querying", {
  expect_error(getEndpointParams("this does not exist")) # throw error if incorrect endpoint given
  expect_equal(typeof(getEndpointParams("shotchartdetail")), "character") # returns character vector
})

test_that("mapping", {
  m <- getIDMappings()
  expect_equal(names(m), c("player", "team")) # if not connected to internet, this should still pass
                                              # because m should still be a named list but with empty
                                              # data frame elements
})