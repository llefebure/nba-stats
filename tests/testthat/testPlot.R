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
  expect_identical(names(p1), c("plot", "data"))
  expect_identical(p1$data, stephCurryShots)
  expect_identical(p2$data, stephCurryShots)
  expect_true("ggplot" %in% class(p1$plot))
  expect_true("ggplot" %in% class(p2$plot))
})