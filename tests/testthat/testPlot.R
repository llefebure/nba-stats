# Author: Luke Lefebure

test_that("court outline", {
  court <- courtOutline()
  expect_equal(class(court), "data.frame")
  expect_equal(ncol(court), 4)
  expect_gt(nrow(court), 0)
})

test_that("court outline plot", {
  p <- courtOutlinePlot()
  expect_true("ggplot" %in% class(p))
})

test_that("shot chart", {
  p1 <- shotChart()
  p2 <- shotChart(d = stephCurryShots) # equivalent plot as above
  expect_true("ggplot" %in% class(p1))
  expect_true("ggplot" %in% class(p2))
})