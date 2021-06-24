test_that("calcPairwiseOverlaps", {
  testlist <- list(a = 1:3, b = 3, c = 3:4)
  r <- calcPairwiseOverlaps(testlist)
  expect_identical(r$name1, c("a", "a", "b"))
  expect_identical(r$name2, c("b", "c", "c"))
  expect_equal(r$length_set1, c(3, 3, 1))
  expect_equal(r$length_set2, c(1, 2, 2))
  expect_equal(r$length_union, c(3, 4, 2))
  expect_equal(r$length_intersection, c(1, 1, 1))
  expect_equal(r$overlap, c(1, 0.5, 1))
  expect_equal(r$jaccard, c(1/3, 1/4, 1/2))
  rownames(r) <- NULL
  ra <- calcPairwiseOverlaps(testlist, targetset = "a"); rownames(ra) <- NULL
  expect_equal(r[r$name1=="a", ], ra)
  rb <- calcPairwiseOverlaps(testlist, targetset = "b"); rownames(rb) <- NULL
  expect_equal(unique(rb$name1), "b")
  expect_equal(rb$jaccard, c(1/3, 1/2))
  expect_error(calcPairwiseOverlaps(testlist, targetset = "d"))
  expect_error(calcPairwiseOverlaps(testlist, targetset = c("a", "b")))
  testlist2 <- list(a = list(1:3), b = 3, c = 3:4)
  expect_error(calcPairwiseOverlaps(testlist2))
})

test_that("distmatrix", {
  testlist <- list(a = 1:3, b = 3, c = 3:4)
  r <- calcPairwiseOverlaps(testlist)
  rd <- makeDist(r)
  expect_equal(as.numeric(rd), c(2/3, 3/4, 1/2))
  expect_equal(as.numeric(rd), 1-r$jaccard)
  expect_error(makeDist(r, "hello"))
})