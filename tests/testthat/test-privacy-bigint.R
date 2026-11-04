# The sampler's exact arithmetic is plain R since 2.7.2. These tests compare it
# with gmp, and the whole sampler with the gmp implementation of 2.7.1, so the
# noise (and every sticky release identity) is unchanged.

.big_random_decimal <- function(max_digits = 120L) {
  digits <- sample.int(max_digits, 1L)
  text <- paste(sample(0:9, digits, replace = TRUE), collapse = "")
  text <- sub("^0+", "", text)
  if (!nzchar(text)) text <- "0"
  if (text != "0" && runif(1L) < 0.5) text <- paste0("-", text)
  text
}

test_that("dsOMOP code does not need gmp", {
  imports <- utils::packageDescription("dsOMOP")$Imports
  expect_false(grepl("\\bgmp\\b", imports))
  ns <- asNamespace("dsOMOP")
  uses <- Filter(function(name) {
    f <- get(name, envir = ns)
    is.function(f) && any(grepl("gmp::", deparse(f), fixed = TRUE))
  }, ls(ns, all.names = TRUE))
  expect_identical(uses, character(0))
})

test_that("integers round-trip through numbers and decimal strings", {
  for (x in c(0, 1, -1, 65535, 65536, -65537, 2^53 - 1, -(2^53), 2^300)) {
    expect_identical(.dsomopBigNumeric(.dsomopBigInteger(x)), x)
  }
  expect_identical(.dsomopBigDecimal(2^300), paste0(
    "2037035976334486086268445688409378161051468393665936250636140449354381",
    "299763336706183397376"
  ))
  expect_identical(.dsomopBigDecimal("-000"), "0")
  expect_identical(.dsomopBigDecimal(-0), "0")
  expect_identical(.dsomopBigDecimal("10000"), "10000")
  expect_identical(as.character(.dsomopBigInteger("-18446744073709551617")),
                   "-18446744073709551617")
  expect_error(.dsomopBigInteger(1.5), "exact DP integer")
  expect_error(.dsomopBigInteger(Inf), "exact DP integer")
  expect_error(.dsomopBigInteger("1e5"), "exact DP integer")
})

test_that("integer arithmetic matches gmp on random operands", {
  skip_if_not_installed("gmp")
  set.seed(20261104)
  for (i in seq_len(400L)) {
    a <- .big_random_decimal()
    b <- .big_random_decimal()
    ga <- gmp::as.bigz(a)
    gb <- gmp::as.bigz(b)
    info <- paste(a, b)
    expect_identical(.dsomopBigDecimal(a), as.character(ga), info = info)
    expect_identical(.dsomopBigDecimal(.dsomopBigAdd(a, b)),
                     as.character(ga + gb), info = info)
    expect_identical(.dsomopBigDecimal(.dsomopBigSubtract(a, b)),
                     as.character(ga - gb), info = info)
    expect_identical(.dsomopBigDecimal(.dsomopBigMultiply(a, b)),
                     as.character(ga * gb), info = info)
    expect_identical(.dsomopBigCompare(a, b),
                     if (ga < gb) -1L else if (ga > gb) 1L else 0L,
                     info = info)
    expect_identical(.dsomopBigBitLength(a),
                     as.integer(gmp::sizeinbase(ga, 2L)), info = info)
    expect_identical(.dsomopBigIsOdd(a), isTRUE(abs(ga) %% 2L == 1L),
                     info = info)
    na <- abs(ga)
    nb <- abs(gb)
    if (nb > 0L) {
      expect_identical(
        .dsomopBigDecimal(.dsomopBigDivide(as.character(na), as.character(nb))),
        as.character(na %/% nb), info = info
      )
    }
    shift <- sample(0:200, 1L)
    expect_identical(.dsomopBigDecimal(.dsomopBigShiftLeft(a, shift)),
                     as.character(ga * gmp::as.bigz(2)^shift), info = info)
  }
})

test_that("exact rates equal gmp's binary rationals", {
  skip_if_not_installed("gmp")
  set.seed(42)
  epsilons <- c(0.1, 0.25, 1, 8, 1e-6, 0.05, 2^-1074, 7.999999999999999,
                runif(200L, 0, 8))
  sensitivities <- c(1, 3, 65535, 2^24 - 1, 2^53 - 1, 2^101, 2^300, 1e300,
                     0.1, 2^-1074, sample.int(1e6, 100L), runif(100L) * 1e4)
  for (i in seq_len(400L)) {
    epsilon <- sample(epsilons, 1L)
    sensitivity <- sample(sensitivities, 1L)
    rate <- .dsomopDpExactRate(epsilon, sensitivity)
    expected <- gmp::as.bigq(epsilon) / gmp::as.bigq(sensitivity)
    info <- sprintf("%a / %a", epsilon, sensitivity)
    expect_identical(.dsomopBigDecimal(rate$numerator),
                     as.character(gmp::numerator(expected)), info = info)
    expect_identical(.dsomopBigDecimal(rate$denominator),
                     as.character(gmp::denominator(expected)), info = info)
  }
})

# The gmp-based sampler of dsOMOP 2.7.1, verbatim apart from the names.
.ref_bit_stream <- function(key, context, coordinate, draw = 0L) {
  coordinate <- as.character(gmp::as.bigz(coordinate))
  draw <- gmp::as.bigz(draw)
  buffer <- raw(0L)
  position <- 1L
  function() {
    if (position > length(buffer)) {
      digest <- .dsomopDpHmacRaw(key, .dsomopDpCanonicalJson(list(
        protocol = "dsomop-dp-hmac-bit-stream-v1",
        context = context,
        coordinate = coordinate,
        draw = as.character(draw)
      )))
      buffer <<- rawToBits(digest)
      position <<- 1L
      draw <<- draw + 1L
    }
    bit <- as.integer(buffer[[position]])
    position <<- position + 1L
    bit
  }
}

.ref_uniform <- function(n, bits) {
  n <- gmp::as.bigz(n)
  if (n == 1L) return(gmp::as.bigz(0L))
  width <- gmp::sizeinbase(n - 1L, 2L)
  repeat {
    value <- gmp::as.bigz(0L)
    for (i in seq_len(width)) value <- 2L * value + bits()
    if (value < n) return(value)
  }
}

.ref_bernoulli <- function(numerator, denominator, bits) {
  numerator <- gmp::as.bigz(numerator)
  denominator <- gmp::as.bigz(denominator)
  repeat {
    if (numerator <= 0L) return(FALSE)
    if (numerator >= denominator) return(TRUE)
    numerator <- 2L * numerator
    if (bits() == 1L) numerator <- numerator - denominator
  }
}

.ref_bernoulli_exp <- function(numerator, denominator, bits) {
  numerator <- gmp::as.bigz(numerator)
  denominator <- gmp::as.bigz(denominator)
  while (numerator > denominator) {
    if (!.ref_bernoulli_exp(1L, 1L, bits)) return(FALSE)
    numerator <- numerator - denominator
  }
  k <- gmp::as.bigz(1L)
  while (.ref_bernoulli(numerator, denominator * k, bits)) {
    k <- k + 1L
  }
  k %% 2L == 1L
}

.ref_geometric <- function(numerator, denominator, bits) {
  numerator <- gmp::as.bigz(numerator)
  denominator <- gmp::as.bigz(denominator)
  repeat {
    u <- .ref_uniform(denominator, bits)
    if (.ref_bernoulli_exp(u, denominator, bits)) break
  }
  v <- gmp::as.bigz(0L)
  while (.ref_bernoulli_exp(1L, 1L, bits)) v <- v + 1L
  (u + denominator * v) %/% numerator
}

.ref_discrete_laplace <- function(key, context, coordinate, epsilon,
                                  sensitivity) {
  rate <- gmp::as.bigq(epsilon) / gmp::as.bigq(sensitivity)
  numerator <- gmp::numerator(rate)
  denominator <- gmp::denominator(rate)
  bits <- .ref_bit_stream(key, context, coordinate)
  repeat {
    magnitude <- .ref_geometric(numerator, denominator, bits)
    if (bits() == 1L) {
      if (magnitude == 0L) next
      return(-magnitude)
    }
    return(magnitude)
  }
}

test_that("the sampler draws exactly the noise of the 2.7.1 gmp sampler", {
  skip_if_not_installed("gmp")
  set.seed(7)
  epsilons <- c(0.1, 0.05, 0.25, 1, 4, 8, 1e-6, runif(6L, 0, 8))
  sensitivities <- c(1, 3, 10, 65535, 2^24 - 1, 2^53 - 1, 2^101, 0.5)
  for (i in seq_len(600L)) {
    key <- as.raw(sample(0:255, 32L, replace = TRUE))
    context <- list(query = sprintf("cross-check-%d", i),
                    component = sample(c("count", "sum"), 1L))
    coordinate <- if (i %% 50L == 0L) "9007199254740993" else
      sample.int(5000L, 1L)
    epsilon <- sample(epsilons, 1L)
    sensitivity <- sample(sensitivities, 1L)
    expect_identical(
      .dsomopBigDecimal(.dsomopDpDiscreteLaplace(
        key, context, coordinate, epsilon, sensitivity
      )),
      as.character(.ref_discrete_laplace(
        key, context, coordinate, epsilon, sensitivity
      )),
      info = sprintf("i=%d epsilon=%a sensitivity=%a", i, epsilon, sensitivity)
    )
  }
})

test_that("the DP bootstrap still refuses releases when the self-test fails", {
  testthat::local_mocked_bindings(
    .dsomopDpCanonicalSelfTest = function() {
      stop("The sticky-noise sampler changed; releases are disabled until an ",
           "explicit sampler protocol version is installed.", call. = FALSE)
    },
    .package = "dsOMOP"
  )
  expect_error(.dsomopDpBootstrap(), "sampler changed")
})
