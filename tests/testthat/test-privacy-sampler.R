.dp_sampler_bits <- function(values) {
  position <- 0L
  function() {
    position <<- position + 1L
    if (position > length(values)) stop("Scripted sampler bits exhausted.")
    as.integer(values[[position]])
  }
}

.dp_sampler_key <- as.raw(0:31)

test_that("the keyed bit stream replays and separates coordinates and blocks", {
  context <- list(query = "exact-sampler-stream")
  read_bits <- function(coordinate = 1L, draw = 0L, ctx = context,
                        key = .dp_sampler_key, n = 768L) {
    bits <- .dsomopDpBitStream(key, ctx, coordinate, draw)
    vapply(seq_len(n), function(i) bits(), integer(1))
  }
  original <- read_bits()
  expect_identical(original, read_bits())
  expect_true(all(original %in% 0:1))
  expect_false(identical(original, read_bits(coordinate = 2L)))
  expect_false(identical(original, read_bits(ctx = list(query = "other"))))
  expect_false(identical(original, read_bits(key = rev(.dp_sampler_key))))
  expect_false(identical(original, read_bits(draw = 1L)))
  expect_false(identical(original[1:256], original[257:512]))
  expect_false(identical(original[257:512], original[513:768]))
  large_coordinate <- gmp::as.bigz("9007199254740992")
  expect_false(identical(
    read_bits(coordinate = large_coordinate),
    read_bits(coordinate = large_coordinate + 1L)
  ))
  expect_identical(original[257:512], read_bits(draw = 1L, n = 256L))
  expect_identical(original[513:768], read_bits(draw = 2L, n = 256L))
})

test_that("stream blocks consume every raw bit and counters never round", {
  observed <- character()
  coordinates <- character()
  first <- gmp::as.bigz("9007199254740992")
  output <- testthat::with_mocked_bindings({
    bits <- .dsomopDpBitStream(
      .dp_sampler_key, list(query = "counter"), first + 1L, first
    )
    vapply(seq_len(768L), function(i) bits(), integer(1))
  }, .dsomopDpHmacRaw = function(key, value) {
    observed <<- c(observed, jsonlite::fromJSON(value)$draw)
    coordinates <<- c(coordinates, jsonlite::fromJSON(value)$coordinate)
    as.raw(0:31)
  }, .package = "dsOMOP")
  expect_identical(observed, c(
    "9007199254740992", "9007199254740993", "9007199254740994"
  ))
  expect_identical(coordinates, rep("9007199254740993", 3L))
  expect_identical(output, rep(as.integer(rawToBits(as.raw(0:31))), 3L))
})

test_that("the exact sampler KAT disables releases if its protocol drifts", {
  expect_invisible(.dsomopDpCanonicalSelfTest())
  expect_error(testthat::with_mocked_bindings(
    .dsomopDpCanonicalSelfTest(),
    .dsomopDpDiscreteLaplace = function(...) gmp::as.bigz(0L),
    .package = "dsOMOP"
  ), "sampler changed")
})

test_that("uniform integers use exact rejection and arbitrary precision", {
  expect_identical(as.character(.dsomopDpUniformInteger(
    1L, .dp_sampler_bits(integer())
  )), "0")
  expect_identical(as.character(.dsomopDpUniformInteger(
    3L, .dp_sampler_bits(c(1, 1, 0, 1))
  )), "1")
  expect_identical(as.character(.dsomopDpUniformInteger(
    gmp::as.bigz(2)^80 + 1L, .dp_sampler_bits(c(rep(0L, 80L), 1L))
  )), "1")
})

test_that("rational Bernoulli decisions compare binary prefixes exactly", {
  expect_false(.dsomopDpBernoulli(0L, 7L, .dp_sampler_bits(integer())))
  expect_true(.dsomopDpBernoulli(7L, 7L, .dp_sampler_bits(integer())))
  expect_true(.dsomopDpBernoulli(1L, 2L, .dp_sampler_bits(0L)))
  expect_false(.dsomopDpBernoulli(1L, 2L, .dp_sampler_bits(1L)))
  expect_true(.dsomopDpBernoulli(1L, 3L, .dp_sampler_bits(c(0L, 0L))))
  expect_false(.dsomopDpBernoulli(1L, 3L, .dp_sampler_bits(c(0L, 1L, 1L))))
  denominator <- gmp::as.bigz(2)^80
  expect_true(.dsomopDpBernoulli(
    1L, denominator, .dp_sampler_bits(rep(0L, 80L))
  ))
  expect_false(.dsomopDpBernoulli(
    1L, denominator, .dp_sampler_bits(c(rep(0L, 79L), 1L))
  ))
})

test_that("exponential Bernoulli follows the exact alternating-series branches", {
  expect_true(.dsomopDpBernoulliExp(0L, 1L, .dp_sampler_bits(integer())))
  expect_true(.dsomopDpBernoulliExp(1L, 1L, .dp_sampler_bits(c(0L, 1L))))
  expect_false(.dsomopDpBernoulliExp(1L, 1L, .dp_sampler_bits(1L)))
  expect_true(.dsomopDpBernoulliExp(1L, 2L, .dp_sampler_bits(1L)))
  expect_false(.dsomopDpBernoulliExp(1L, 2L, .dp_sampler_bits(c(0L, 1L))))
  expect_true(.dsomopDpBernoulliExp(
    8L, 1L, .dp_sampler_bits(rep(c(0L, 1L), 8L))
  ))
  expect_false(.dsomopDpBernoulliExp(8L, 1L, .dp_sampler_bits(1L)))
})

test_that("epsilon eight has finite positive-probability paths beyond four", {
  for (sign in 0:1) {
    # Each (0,1) makes Bernoulli(exp(-1)) succeed; (1) stops the
    # geometric after 40 successes. floor(40 / 8) is five.
    scripted <- .dp_sampler_bits(c(rep(c(0L, 1L), 40L), 1L, sign))
    noise <- testthat::with_mocked_bindings(
      .dsomopDpDiscreteLaplace(.dp_sampler_key, list(), 1L, 8, 1),
      .dsomopDpBitStream = function(...) scripted,
      .package = "dsOMOP"
    )
    expect_s3_class(noise, "bigz")
    expect_identical(as.character(noise), if (sign == 0L) "5" else "-5")
  }
})

test_that("two-sided geometric rejects the duplicate negative zero", {
  # First magnitude is zero, with negative sign: reject. The next
  # geometric is one, with positive sign: return one.
  scripted <- .dp_sampler_bits(c(1L, 1L, 0L, 1L, 1L, 0L))
  noise <- testthat::with_mocked_bindings(
    .dsomopDpDiscreteLaplace(.dp_sampler_key, list(), 1L, 1, 1),
    .dsomopDpBitStream = function(...) scripted,
    .package = "dsOMOP"
  )
  expect_identical(as.character(noise), "1")
})

test_that("the sampler preserves integers beyond exact double precision", {
  # U=2^100+1 in [0,2^101), exp(-U/t) succeeds, V=0 and sign=positive.
  scripted <- .dp_sampler_bits(c(1L, rep(0L, 99L), 1L, 1L, 1L, 1L, 0L))
  noise <- testthat::with_mocked_bindings(
    .dsomopDpDiscreteLaplace(.dp_sampler_key, list(), 1L, 1, 2^101),
    .dsomopDpBitStream = function(...) scripted,
    .package = "dsOMOP"
  )
  expect_s3_class(noise, "bigz")
  expect_identical(as.character(noise), "1267650600228229401496703205377")
})

test_that("epsilon canonicalisation keeps the exact supplied binary rational", {
  observed <- NULL
  noise <- testthat::with_mocked_bindings(
    .dsomopDpDiscreteLaplace(.dp_sampler_key, list(), 1L, 0.1, 3),
    .dsomopDpGeometric = function(numerator, denominator, bits) {
      observed <<- gmp::as.bigq(numerator, denominator)
      gmp::as.bigz(1L)
    },
    .dsomopDpBitStream = function(...) .dp_sampler_bits(0L),
    .package = "dsOMOP"
  )
  expect_identical(as.character(observed),
                   "3602879701896397/108086391056891904")
  expect_identical(as.character(noise), "1")
  expect_false(isTRUE(observed == gmp::as.bigq(1L, 30L)))
})

test_that("keyed discrete Laplace is deterministic across fresh streams", {
  context <- list(query = "exact-sampler-sticky", component = "count")
  draw <- function() .dsomopDpDiscreteLaplace(
    .dp_sampler_key, context, 7L, 0.1, 3
  )
  expect_identical(as.character(draw()), as.character(draw()))
  small_epsilon <- .dsomopDpDiscreteLaplace(
    .dp_sampler_key, context, 7L, 1e-6, 2^24 - 1
  )
  expect_s3_class(small_epsilon, "bigz")
  expect_length(small_epsilon, 1L)
})

test_that("noisy integer addition and clipping happen before numeric conversion", {
  policy <- list(
    mechanism = "test", sampler = .DSOMOP_DP_SAMPLER,
    privacy_epoch = 1L, keys = list(noise = .dp_sampler_key)
  )
  with_noise <- function(noise, value = 100, lower = 0, upper = 2^53 - 1) {
    testthat::with_mocked_bindings(
      .dsomopDpNoisyInteger(
        value, policy, list(), "count", 0.1, 1, lower, upper
      ),
      .dsomopDpDiscreteLaplace = function(...) noise,
      .package = "dsOMOP"
    )
  }
  expect_identical(with_noise(gmp::as.bigz(2)^100), 2^53 - 1)
  expect_identical(with_noise(-(gmp::as.bigz(2)^100)), 0)
  expect_identical(with_noise(-(gmp::as.bigz(2)^53) + 2L, 2^53 - 1), 1)
  expect_identical(with_noise(gmp::as.bigz(1L), 2^53 - 1), 2^53 - 1)
  expect_identical(with_noise(gmp::as.bigz(-200L), lower = -10), -10)
  expect_identical(with_noise(gmp::as.bigz(200L), upper = 120), 120)
})

.dp_sampler_distribution <- function(epsilon, sensitivity, n = 10000L) {
  context <- list(query = "exact-sampler-distribution",
                  epsilon = epsilon, sensitivity = sensitivity)
  sample <- vapply(seq_len(n), function(coordinate) as.numeric(
    .dsomopDpDiscreteLaplace(
      .dp_sampler_key, context, coordinate, epsilon, sensitivity
    )
  ), numeric(1))
  rate <- epsilon / sensitivity
  q <- exp(-rate)
  # Paired integer boundaries retain the centre and both aggregated tails.
  # Broad bins also cover the large public contribution/grid sensitivities.
  boundaries <- unique(c(-1, 0, 1, floor((-8:8) / (2 * rate))))
  boundaries <- sort(unique(c(boundaries, -boundaries - 1)))
  cdf <- ifelse(boundaries < 0,
                exp(rate * boundaries) / (1 + q),
                1 - exp(-rate * (boundaries + 1)) / (1 + q))
  probability <- diff(c(0, cdf, 1))
  observed <- tabulate(findInterval(sample - 0.5, boundaries) + 1L,
                       nbins = length(probability))
  # Exact marginal binomial checks remain valid when a high-epsilon tail
  # has fewer than five expected observations; no asymptotic tail pooling.
  bin_p <- pmin(1, 2 * pmin(
    stats::pbinom(observed, n, probability),
    stats::pbinom(observed - 1L, n, probability, lower.tail = FALSE)
  ))
  nonzero <- sum(sample != 0)
  symmetry_p <- if (nonzero == 0L) 1 else stats::binom.test(
    sum(sample > 0), nonzero, p = 0.5
  )$p.value
  expected_abs <- 1 / sinh(rate)
  variance_abs <- 2 * q / (1 - q)^2 - expected_abs^2
  abs_z <- abs(mean(abs(sample)) - expected_abs) / sqrt(variance_abs / n)
  list(n = n, min_bin_p = min(bin_p), symmetry_p = symmetry_p,
       mean_abs = mean(abs(sample)), expected_abs = expected_abs,
       abs_z = abs_z, tail_count = sum(observed[c(1L, length(observed))]))
}

test_that("exact keyed samples match the two-sided geometric pmf and moments", {
  cases <- data.frame(
    epsilon = c(0.1, 0.25, 1, 4, 8, 0.1, 1, 8),
    sensitivity = c(1, 1, 1, 1, 1, 3, 65535, 3),
    n = c(10000L, 10000L, 10000L, 10000L, 40000L, 10000L, 10000L, 10000L)
  )
  for (i in seq_len(nrow(cases))) {
    result <- .dp_sampler_distribution(
      cases$epsilon[[i]], cases$sensitivity[[i]], cases$n[[i]]
    )
    info <- sprintf("epsilon=%g sensitivity=%g n=%d",
                     cases$epsilon[[i]], cases$sensitivity[[i]], result$n)
    expect_true(result$min_bin_p > 1e-7, info = info)
    expect_true(result$symmetry_p > 1e-7, info = info)
    expect_true(result$abs_z < 6, info = info)
    cat(sprintf(paste0(
      "\nSampler validation %s: min_bin_p=%.6g symmetry_p=%.6g ",
      "mean_abs=%.9g expected_abs=%.9g abs_z=%.6g tails=%d\n"
    ), info, result$min_bin_p, result$symmetry_p, result$mean_abs,
    result$expected_abs, result$abs_z, result$tail_count))
  }
})
