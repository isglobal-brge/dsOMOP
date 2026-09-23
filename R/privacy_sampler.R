# Exact discrete-Laplace sampling, following Canonne, Kamath and Steinke
# (2020), https://arxiv.org/abs/2004.00010, section 5, Algorithms 1 and 2.
# All arithmetic affecting sampling probabilities or unbounded integers uses
# GMP. The source of randomness is the domain-separated keyed bit stream below.

.dsomopDpBitStream <- function(key, context, coordinate, draw = 0L) {
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
      # rawToBits uses least-significant bit first within each byte. Consume
      # all 256 bits, then advance an arbitrary-precision block counter.
      buffer <<- rawToBits(digest)
      position <<- 1L
      draw <<- draw + 1L
    }
    bit <- as.integer(buffer[[position]])
    position <<- position + 1L
    bit
  }
}

# Uniform integer in [0, n), using bit rejection rather than reduction modulo n.
.dsomopDpUniformInteger <- function(n, bits) {
  n <- gmp::as.bigz(n)
  if (n == 1L) return(gmp::as.bigz(0L))
  width <- gmp::sizeinbase(n - 1L, 2L)
  repeat {
    value <- gmp::as.bigz(0L)
    for (i in seq_len(width)) value <- 2L * value + bits()
    if (value < n) return(value)
  }
}

# Compare a lazy uniform binary expansion to the rational numerator/denominator.
# After each bit the undecided interval is rescaled exactly; its probability
# halves. This avoids drawing a large denominator-sized integer for each coin.
.dsomopDpBernoulli <- function(numerator, denominator, bits) {
  numerator <- gmp::as.bigz(numerator)
  denominator <- gmp::as.bigz(denominator)
  repeat {
    if (numerator <= 0L) return(FALSE)
    if (numerator >= denominator) return(TRUE)
    numerator <- 2L * numerator
    if (bits() == 1L) numerator <- numerator - denominator
  }
}

# CKS Algorithm 1: the parity of the first failed rational coin realizes the
# alternating exponential series exactly. For x > 1, multiply exp(-1) coins.
.dsomopDpBernoulliExp <- function(numerator, denominator, bits) {
  numerator <- gmp::as.bigz(numerator)
  denominator <- gmp::as.bigz(denominator)
  while (numerator > denominator) {
    if (!.dsomopDpBernoulliExp(1L, 1L, bits)) return(FALSE)
    numerator <- numerator - denominator
  }
  k <- gmp::as.bigz(1L)
  while (.dsomopDpBernoulli(numerator, denominator * k, bits)) {
    k <- k + 1L
  }
  k %% 2L == 1L
}

# CKS Algorithm 2's magnitude: P[Y = y] = (1-exp(-s/t))*exp(-s*y/t).
# Its expected number of iterations does not grow with sensitivity/epsilon.
.dsomopDpGeometric <- function(numerator, denominator, bits) {
  numerator <- gmp::as.bigz(numerator)
  denominator <- gmp::as.bigz(denominator)
  repeat {
    u <- .dsomopDpUniformInteger(denominator, bits)
    if (.dsomopDpBernoulliExp(u, denominator, bits)) break
  }
  v <- gmp::as.bigz(0L)
  while (.dsomopDpBernoulliExp(1L, 1L, bits)) v <- v + 1L
  (u + denominator * v) %/% numerator
}

.dsomopDpDiscreteLaplace <- function(key, context, coordinate, epsilon,
                                     sensitivity) {
  if (!is.numeric(epsilon) || length(epsilon) != 1L || !is.finite(epsilon) ||
      epsilon <= 0 || epsilon > 8 || !is.numeric(sensitivity) ||
      length(sensitivity) != 1L || !is.finite(sensitivity) ||
      sensitivity <= 0) {
    stop("The discrete-Laplace allocation is invalid.", call. = FALSE)
  }
  # Canonicalisation is the exact binary rational represented by each R number.
  # For example 0.1 = 3602879701896397/36028797018963968. Convert BEFORE
  # division; decimal formatting or a floating-point quotient changes the rate.
  rate <- gmp::as.bigq(epsilon) / gmp::as.bigq(sensitivity)
  numerator <- gmp::numerator(rate)
  denominator <- gmp::denominator(rate)
  bits <- .dsomopDpBitStream(key, context, coordinate)
  repeat {
    magnitude <- .dsomopDpGeometric(numerator, denominator, bits)
    if (bits() == 1L) {
      if (magnitude == 0L) next
      return(-magnitude)
    }
    return(magnitude)
  }
}
