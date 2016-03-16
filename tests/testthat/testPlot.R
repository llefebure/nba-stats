# Author: Luke Lefebure

test_that("court outline", {
  court <- courtOutline() # should return data frame of 4 columns and >0 rows
  expect_equal(class(court), "data.frame")
  expect_equal(ncol(court), 4)
  expect_gt(nrow(court), 0)
})

test_that("court outline plot", {
  p <- courtOutlinePlot() # produces a ggplot
  expect_true("ggplot" %in% class(p))
})

test_that("shot chart", {
  p1 <- shotChart()
  p2 <- shotChart(d = stephCurryShots) # equivalent plot as above
  expect_equal(p1$data, stephCurryShots)
  expect_equal(p1, p2)
  expect_true("ggplot" %in% class(p1$plot))
  expect_true("ggplot" %in% class(p2$plot))
})