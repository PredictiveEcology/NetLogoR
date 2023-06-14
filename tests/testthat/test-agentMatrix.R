test_that("create agentMatrix does not work", {
  newAgent <- new("agentMatrix",
    coords = cbind(pxcor = c(1, 2, 5), pycor = c(3, 4, 6)),
    char = letters[c(1, 2, 6)],
    nums2 = c(4.5, 2.6, 2343),
    char2 = LETTERS[c(4, 24, 3)],
    nums = 5:7
  )
  expect_is(newAgent, "agentMatrix")

  # test levelsAM, which should be faster
  newAgent <- new("agentMatrix",
    mat = cbind(
      char = sample(1:3, replace = TRUE, size = 10),
      char2 = sample(1:2, replace = TRUE, size = 10),
      num2 = sample(1:10, replace = TRUE, size = 10)
    ),
    levelsAM = list(
      char = c("a", "b", "f"),
      char2 = c("test", "fail")
    )
  )
  expect_is(newAgent, "agentMatrix")
  expect_true(all(is.na(coordinates(newAgent))))
  expect_is(coordinates(newAgent), "matrix")
  expect_true(all(colnames(newAgent) == c("xcor", "ycor", "char", "char2", "num2")))

  # test agentMatrix call with levelsAM
  newAgent <- agentMatrix(
    mat = cbind(
      char = sample(1:3, replace = TRUE, size = 10),
      char2 = sample(1:2, replace = TRUE, size = 10),
      num2 = sample(1:10, replace = TRUE, size = 10)
    ),
    levelsAM = list(
      char = c("a", "b", "f"),
      char2 = c("test", "fail")
    )
  )
  expect_is(newAgent, "agentMatrix")
  expect_true(all(is.na(coordinates(newAgent))))
  expect_is(coordinates(newAgent), "matrix")
  expect_true(all(colnames(newAgent) == c("xcor", "ycor", "char", "char2", "num2")))

  # test agentMatrix call with levelsAM
  newAgent <- agentMatrix(
    coords = cbind(pxcor = 1:10, pycor = 1:10),
    mat = cbind(
      char = sample(1:3, replace = TRUE, size = 10),
      char2 = sample(1:2, replace = TRUE, size = 10),
      num2 = sample(1:10, replace = TRUE, size = 10)
    ),
    levelsAM = list(
      char = c("a", "b", "f"),
      char2 = c("test", "fail")
    )
  )
  expect_is(newAgent, "agentMatrix")
  expect_true(all(!is.na(coordinates(newAgent))))
  expect_is(coordinates(newAgent), "matrix")
  expect_true(all(colnames(newAgent) == c("xcor", "ycor", "char", "char2", "num2")))

  # test coordinates missing
  newAgent <- new("agentMatrix",
    char = letters[c(1, 2, 6)],
    nums2 = c(4.5, 2.6, 2343),
    char2 = LETTERS[c(4, 24, 3)],
    nums = 5:7
  )
  expect_is(newAgent, "agentMatrix")
  expect_true(all(is.na(coordinates(newAgent))))
  expect_is(coordinates(newAgent), "matrix")
  expect_equal(dim(coordinates(newAgent)), c(3, 2))

  # test all numeric matrix
  mat <- cbind(nums1 = 1:3, nums2 = 2:4)
  newAgent <- new("agentMatrix", mat = mat)
  expect_is(newAgent, "agentMatrix")
  expect_true(all(is.na(coordinates(newAgent))))
  expect_is(coordinates(newAgent), "matrix")
  expect_equal(dim(coordinates(newAgent)), c(3, 2))
  expect_equal(colnames(newAgent), c("xcor", "ycor", "nums1", "nums2"))

  mat <- cbind(nums1 = 1:3, nums2 = 2:4)
  mat2 <- cbind(nums3 = 1:3, nums4 = 2:4)
  expect_silent(newAgent <- new("agentMatrix", mat = mat, mat2 = mat2))
  expect_is(newAgent, "agentMatrix")
  expect_true(all(is.na(coordinates(newAgent))))
  expect_is(coordinates(newAgent), "matrix")
  expect_equal(dim(coordinates(newAgent)), c(3, 2))
  expect_equal(colnames(newAgent), c("xcor", "ycor", "nums1", "nums2", "nums3", "nums4"))


  mat <- cbind(nums1 = 1:3, nums2 = 2:4)
  newAgent <- new("agentMatrix", mat = mat, coords = matrix(1:6, ncol = 2))
  expect_is(newAgent, "agentMatrix")
  expect_true(all(!is.na(coordinates(newAgent))))
  expect_is(coordinates(newAgent), "matrix")
  expect_equal(dim(coordinates(newAgent)), c(3, 2))
  expect_equal(colnames(newAgent), c("xcor", "ycor", "nums1", "nums2"))

  # test all numeric vectors
  newAgent <- new("agentMatrix", nums1 = 1:3, nums2 = 2:4)
  expect_is(newAgent, "agentMatrix")
  expect_true(all(is.na(coordinates(newAgent))))
  expect_is(coordinates(newAgent), "matrix")
  expect_equal(dim(coordinates(newAgent)), c(3, 2))
  expect_equal(colnames(newAgent), c("xcor", "ycor", "nums1", "nums2"))

  # mixed numeric and character matrices and vectors
  newAgent <- new("agentMatrix",
    chars = cbind(char1 = letters[c(1, 2, 6)], char2 = LETTERS[c(4, 24, 3)]),
    nums1 = 4:6,
    charsA = cbind(char3 = letters[c(1, 2, 6)], char4 = LETTERS[c(4, 24, 3)]),
    nums2 = 1:3
  )
  expect_is(newAgent, "agentMatrix")
  expect_true(all(is.na(coordinates(newAgent))))
  expect_is(coordinates(newAgent), "matrix")
  expect_equal(dim(coordinates(newAgent)), c(3, 2))
  expect_equal(colnames(newAgent), c(
    "xcor", "ycor", "char1", "char2", "nums1", "char3",
    "char4", "nums2"
  ))

  # mixed numeric and character matrices and vectors
  newAgent <- new("agentMatrix",
    chars = cbind(char1 = letters[c(1, 2, 6)], char2 = LETTERS[c(4, 24, 3)]),
    nums1 = 4:6,
    charsA = cbind(char3 = letters[c(1, 2, 6)], char4 = LETTERS[c(4, 24, 3)]),
    nums2 = 1:3,
    coords = matrix(1:6, ncol = 2)
  )
  expect_is(newAgent, "agentMatrix")
  expect_true(all(!is.na(coordinates(newAgent))))
  expect_is(coordinates(newAgent), "matrix")
  expect_equal(dim(coordinates(newAgent)), c(3, 2))
  expect_equal(colnames(newAgent), c(
    "xcor", "ycor", "char1", "char2", "nums1", "char3",
    "char4", "nums2"
  ))
})

test_that("agentMatrix benchmarking", {
  skip_on_cran()
  skip_on_ci()
  set.seed(20180924) ## TODO: why are some seeds failing?

  # compare speeds -- if these fail, then should reconsider the need for agentMatrix
  if (require(microbenchmark)) {
    mb <- summary(microbenchmark(
      times = 50,
      spdf = {
        if (requireNamespace("sp", quietly = TRUE))
          sp::SpatialPointsDataFrame(
            coords = cbind(pxcor = c(1, 2, 5), pycor = c(3, 4, 6)),
            data = data.frame(
              char = letters[c(1, 2, 6)],
              nums2 = c(4.5, 2.6, 2343),
              char2 = LETTERS[c(4, 24, 3)],
              nums = 5:7
            )
          )
      },
      agentMat = {
        agentMatrix(
          coords = cbind(
            pxcor = c(1, 2, 5),
            pycor = c(3, 4, 6)
          ),
          char = letters[c(1, 2, 6)],
          nums2 = c(4.5, 2.6, 2343),
          char2 = LETTERS[c(4, 24, 3)],
          nums = 5:7
        )
      },
      agentMatDirect = {
        new("agentMatrix",
          coords = cbind(
            pxcor = c(1, 2, 5),
            pycor = c(3, 4, 6)
          ),
          char = letters[c(1, 2, 6)],
          nums2 = c(4.5, 2.6, 2343),
          char2 = LETTERS[c(4, 24, 3)],
          nums = 5:7
        )
      }
    ))
    if (requireNamespace("sp", quietly = TRUE))
      expect_gt(mb$median[1] / mb$median[3], 3) # expect it is ~3 times faster
  }

  # check just numerics
  if (requireNamespace("sf", quietly = TRUE)) {
    if (requireNamespace("microbenchmark", quietly = TRUE)) {
      mb <- summary(microbenchmark::microbenchmark(
        times = 50,
        spdf = {
          if (requireNamespace("sp", quietly = TRUE))
          sp::SpatialPointsDataFrame(
            coords = cbind(pxcor = c(1, 2, 5), pycor = c(3, 4, 6)),
            data = data.frame(
              nums2 = c(4.5, 2.6, 2343),
              nums = 5:7
            )
          )
        },
        sf = {
          a1 <- sf::st_point(cbind(1, 3))
          a2 <- sf::st_point(cbind(2, 4))
          a3 <- sf::st_point(cbind(5, 6))
          d <- data <- data.frame(nums2 = c(4.5, 2.6, 2343), nums = 5:7)
          d$geom <- sf::st_sfc(a1, a2, a3)
          df <- sf::st_as_sf(d)
        },
        agentMat = {
          agentMatrix(
            coords = cbind(pxcor = c(1, 2, 5), pycor = c(3, 4, 6)),
            nums2 = c(4.5, 2.6, 2343), nums = 5:7
          )
        },
        agentMatDirect = {
          new("agentMatrix",
            coords = cbind(pxcor = c(1, 2, 5), pycor = c(3, 4, 6)),
            nums2 = c(4.5, 2.6, 2343), nums = 5:7
          )
        }
      ))
      if (requireNamespace("sp", quietly = TRUE))
        expect_gt(mb$median[1] / mb$median[3], 4) # use 4 for safety
      if (interactive()) expect_gt(mb$median[2] / mb$median[3], 2) # use 2 for safety
    }
  }
})

test_that("agentMatrix coercion", {
  skip_if_not_installed("sp")
  spdf1 <- sp::SpatialPointsDataFrame(
    coords = matrix(1:6, ncol = 2),
    data = data.frame(tmp = 1:3, tmp2 = c("e", "f", "g"))
  )
  newAgent <- agentMatrix(spdf1)
  expect_is(newAgent, "agentMatrix")
  expect_true(all(!is.na(coordinates(newAgent))))
  expect_is(coordinates(newAgent), "matrix")
  expect_equal(dim(coordinates(newAgent)), c(3, 2))
  expect_equal(colnames(newAgent), c("xcor", "ycor", "tmp", "tmp2"))

  # test setAs from matrix
  mat <- cbind(coords = matrix(1:6, ncol = 2), tmp = 1:3, tmp2 = 4:6)
  newAgent <- as(mat, "agentMatrix")
  expect_is(newAgent, "agentMatrix")
  expect_true(all(!is.na(coordinates(newAgent))))
  expect_is(coordinates(newAgent), "matrix")
  expect_equal(dim(coordinates(newAgent)), c(3, 2))
  expect_equal(colnames(newAgent), c("xcor", "ycor", "tmp", "tmp2"))

  # test setAs from data.frame
  mat <- cbind(coords = matrix(1:6, ncol = 2), data.frame(tmp = 1:3, tmp2 = c("e", "f", "g")))
  newAgent <- as(mat, "agentMatrix")
  expect_is(newAgent, "agentMatrix")
  expect_true(all(!is.na(coordinates(newAgent))))
  expect_is(coordinates(newAgent), "matrix")
  expect_equal(dim(coordinates(newAgent)), c(3, 2))
  expect_equal(colnames(newAgent), c("xcor", "ycor", "tmp", "tmp2"))
})

test_that("agentMatrix subsetting", {
  # subsetting keeps an agentMatrix, just like subsetting SPDF
  # character column
  mat <- cbind(coords = matrix(1:6, ncol = 2), data.frame(tmp = 1:3, tmp2 = c("e", "f", "g")))
  newAgent <- as(mat, "agentMatrix")

  tmpA <- as(newAgent[2, 4], "data.frame")
  tmpB <- as(agentMatrix(
    coords = coordinates(newAgent)[2, , drop = FALSE],
    tmp2 = cbind(tmp2 = newAgent@levels[["tmp2"]][
      newAgent@.Data[2, 4, drop = FALSE]
    ])
  ), "data.frame")
  expect_true(all.equal(tmpA, tmpB))

  tmpA <- newAgent[2, 3]
  tmpB <- agentMatrix(
    coords = coordinates(newAgent)[2, , drop = FALSE],
    tmp2 = newAgent@.Data[2, 3, drop = FALSE]
  )
  expect_true(all.equal(tmpA, tmpB))

  # test various [ ]
  expect_equal(newAgent[], data.frame(
    xcor = newAgent$xcor, ycor = newAgent$ycor,
    tmp = newAgent$tmp, tmp2 = newAgent$tmp2,
    stringsAsFactors = FALSE
  ))
  expect_equal(newAgent[, "tmp"], agentMatrix(
    coords = coordinates(newAgent),
    tmp = newAgent@.Data[, "tmp", drop = FALSE]
  ))
  expect_equal(newAgent[1:2, "tmp"], agentMatrix(coords = coordinates(newAgent)[
    1:2, ,
    drop = FALSE
  ], tmp = newAgent@.Data[1:2, "tmp", drop = FALSE]))
  expect_equal(newAgent[, 3], agentMatrix(
    coords = coordinates(newAgent)[, , drop = FALSE],
    tmp = newAgent@.Data[, 3, drop = FALSE]
  ))

  # character column
  tmpA <- as(newAgent[, 4], "data.frame")
  tmpB <- as(agentMatrix(
    coords = coordinates(newAgent)[, , drop = FALSE],
    tmp = cbind(tmp2 = newAgent@levels[["tmp2"]][newAgent@.Data[
      , 4,
      drop = FALSE
    ]])
  ), "data.frame")
  expect_true(all.equal(tmpA, tmpB))

  # numeric
  tmpA <- newAgent[, 3]
  tmpB <- agentMatrix(coords = coordinates(newAgent), tmp2 = newAgent@.Data[, 3, drop = FALSE])
  expect_true(all.equal(tmpA, tmpB))

  mat <- cbind(coords = matrix(1:6, ncol = 2), data.frame(tmp = 1:3, tmp2 = c("e", "f", "g")))
  newAgent <- as(mat, "agentMatrix")

  expect_equal(1, sum(newAgent[, "tmp2"] == "f"))
  expect_equal(1, sum(newAgent[, "tmp"] == 2))

  mat <- cbind(coords = matrix(1:6, ncol = 2), data.frame(
    tmp = 1:3, tmp1 = 1:3,
    tmp2 = c("e", "f", "g")
  ))
  newAgent <- as(mat, "agentMatrix")
  expect_equal(2, sum(newAgent == 2))

  am <- agentMatrix(coords = matrix(1:6, ncol = 2))
  expect_warning(am == 1)
  expect_warning(am == "test")

  am <- agentMatrix(coords = matrix(1:6, ncol = 2), mat = 1:3)
  expect_false(all(am == "test"))
})

test_that("agentMatrix rbind cbind, tail, head, nrow, length, show", {
  mat <- cbind(coords = matrix(1:6, ncol = 2), data.frame(
    tmp = 1:3, tmp1 = 1:3,
    tmp2 = c("e", "f", "g")
  ))
  newAgent <- as(mat, "agentMatrix")

  mat <- cbind(coords = matrix(1:6, ncol = 2), data.frame(tmp = 1:3, tmp2 = c("e", "f", "g")))
  mat2 <- cbind(coords = matrix(1:6, ncol = 2), data.frame(tmp = 1:3, tmp2 = c("e", "f", "g")))
  newAgent1 <- as(mat, "agentMatrix")
  newAgent2 <- as(mat2, "agentMatrix")

  newA <- rbind(newAgent1, newAgent2)
  expect_true(NROW(newA) == 6)
  expect_true(all(colnames(newA) == c("xcor", "ycor", "tmp", "tmp2")))

  newA <- rbind(newAgent1[, 3], newAgent2[, 3])
  expect_true(NROW(newA) == 6)
  expect_true(ncol(newA) == 3)
  expect_true(all(colnames(newA) == c("xcor", "ycor", "tmp")))

  newA <- rbind(newAgent, newAgent1)
  expect_true(NROW(newA) == 6)
  expect_true(all(colnames(newA) == c("xcor", "ycor", "tmp", "tmp1", "tmp2")))

  mat <- cbind(coords = matrix(1:6, ncol = 2), data.frame(tmp3 = 1:3, tmp4 = c("e", "f", "g")))
  mat2 <- cbind(coords = matrix(1:6, ncol = 2), data.frame(tmp = 1:3, tmp2 = c("e", "f", "g")))
  newAgent1 <- as(mat, "agentMatrix")
  newAgent2 <- as(mat2, "agentMatrix")

  cbound <- cbind(newAgent1, newAgent2)
  expect_is(cbound, "agentMatrix")
  expect_true(ncol(cbound) == 6)
  expect_true(nrow(cbound) == 3)
  expect_true(all(colnames(cbound) == c("xcor", "ycor", "tmp3", "tmp4", "tmp", "tmp2")))
  expect_true(all(cbound@.Data[, "tmp2"] == 1:3))
  expect_true(all(cbound$tmp4 == c("e", "f", "g")))

  expect_error(cbound <- cbind(newAgent1, newAgent2, newAgent))
  expect_error(cbound <- cbind(newAgent1, newAgent1))

  mat <- cbind(coords = matrix(1:6, ncol = 2), data.frame(tmp3 = 1:3, tmp4 = c("e", "f", "g")))
  mat2 <- cbind(coords = matrix(1:2, ncol = 2), data.frame(tmp = 1, tmp2 = c("e")))
  newAgent1 <- as(mat, "agentMatrix")
  newAgent2 <- as(mat2, "agentMatrix")

  cbound <- cbind(newAgent1, newAgent2)
  expect_is(cbound, "agentMatrix")

  # test tail and head
  expect_true(nrow(tail(cbound, 1)) == 1)
  expect_is(tail(cbound, 1), "agentMatrix")
  expect_true(nrow(head(cbound, 1)) == 1)
  expect_true(all.equal(dim(cbound)[2], dim(head(cbound, 1))[2]))

  # test nrow
  expect_true(nrow(cbound) == nrow(cbound@.Data))
  expect_true(length(cbound) == length(cbound@.Data))

  # test show
  outShow <- capture.output(cbound)
  expect_true(length(outShow) == 4)
  expect_true(grep(outShow, pattern = c("tmp3 tmp4 tmp tmp2")) == 1)
  expect_true(all(grep(outShow, pattern = c("1    1")) == c(2)))
  expect_true(all(grep(outShow, pattern = c("e   1")) == c(2)))
  expect_true(all(grep(outShow, pattern = c("f   1")) == 3))

  newAgent <- agentMatrix()
  outShow <- capture.output(newAgent)
  expect_equivalent(outShow, "<0 x 0 matrix>")
  expect_is(newAgent, "agentMatrix")
})

test_that("agentMatrix replace methods don't work", {
  mat <- cbind(
    coords = matrix(1:6, ncol = 2),
    data.frame(tmp = 1:3, tmp1 = 1:3, tmp2 = c("e", "f", "g")),
    tmpCh = LETTERS[3:5]
  )
  newAgent <- as(mat, "agentMatrix")

  newAgent[1, 4] <- 3
  expect_true(newAgent[1, 4] == 3)

  newAgent[, 4] <- 4:2
  expect_true(all(newAgent[][, 4] == (4:2)))

  newAgent[, "tmp"] <- 5:3
  expect_true(all(newAgent[][, "tmp"] == (5:3)))

  newAgent[1, "tmp2"] <- "s"
  expect_true(newAgent$tmp2[1] == "s")

  # simple data.frame
  newAgent[1:2, c("tmp", "tmp2")] <- data.frame(
    tmp = 6:7, tmp2 = c("r", "w"),
    stringsAsFactors = FALSE
  )
  expect_true(all(newAgent$tmp[1:2] == 6:7)) # is numeric
  expect_true(all(newAgent$tmp2[1:2] == c("r", "w"))) # maintains character
  expect_true(all(newAgent$tmp2[3] == c("g"))) # untouched

  # more complicated data.frame
  newAgent[1:2, c("tmp", "tmp1", "tmp2", "tmpCh")] <- data.frame(
    tmp = 6:7, tmp1 = 11:12, tmp2 = c("r", "w"), tmpCh = LETTERS[13:14],
    stringsAsFactors = FALSE
  )
  expect_true(all(newAgent$tmp[1:2] == 6:7)) # is numeric
  expect_true(all(newAgent$tmp2[1:2] == c("r", "w"))) # maintains character
  expect_true(all(newAgent$tmp2[3] == c("g"))) # untouched

  # character
  newAgent[, 5] <- letters[6:8]
  expect_true(all(newAgent[, 5] == letters[6:8]))

  newAgent[, "tmp2"] <- letters[9:11]
  expect_true(all(newAgent$tmp2 == letters[9:11]))

  newAgent[1, 5] <- "t" # test a new value for the column
  expect_true(newAgent[1, 5] == "t")

  # data.frame with non existent columns
  expect_error(newAgent[1:2, c("tmp", "tmp4")] <- data.frame(
    tmp = 9:10, tmp4 = 15:16,
    stringsAsFactors = FALSE
  ))

  # Weird, numeric missing numeric ... this replaces a whole row
  mat <- cbind(coords = matrix(1:6, ncol = 2), data.frame(tmp = 1:3, tmp1 = 1:3))
  newAgent <- as(mat, "agentMatrix")

  newAgent[1, ] <- 1:4
  expect_true(all(newAgent[][1, ] == 1:4))
})
