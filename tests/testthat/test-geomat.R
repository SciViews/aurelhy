test_that("Create a simple geomat object", {
  geom <- geomat(matrix(1L, nrow = 10), 0.1, 10, 20)
  attr(geom, "coords") <- NULL
  expect_equal(geom, matrix(1L, nrow = 10, ncol = 1))
  # coords attribute of a geomat object
  geom_coords <- attr(geomat(matrix(1L, nrow = 10), 0.1, 10, 20), "coords")
  expect_equal(geom_coords, c(size = 0.1, x = 10, y = 20))
})
