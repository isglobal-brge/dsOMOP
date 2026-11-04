# Exact discrete-Laplace sampling, following Canonne, Kamath and Steinke
# (2020), https://arxiv.org/abs/2004.00010, section 5, Algorithms 1 and 2.
# All arithmetic affecting sampling probabilities or unbounded integers is
# exact (privacy_bigint.R). The source of randomness is the domain-separated
# keyed bit stream below.

.dsomopDpBitStream <- function(key, context, coordinate, draw = 0L) {
  coordinate <- .dsomopBigDecimal(coordinate)
  draw <- .dsomopBigInteger(draw)
  buffer <- raw(0L)
  position <- 1L
  function() {
    if (position > length(buffer)) {
      digest <- .dsomopDpHmacRaw(key, .dsomopDpCanonicalJson(list(
        protocol = "dsomop-dp-hmac-bit-stream-v1",
        context = context,
        coordinate = coordinate,
        draw = .dsomopBigDecimal(draw)
      )))
      # rawToBits uses least-significant bit first within each byte. Consume
      # all 256 bits, then advance an arbitrary-precision block counter.
      buffer <<- rawToBits(digest)
      position <<- 1L
      draw <<- .dsomopBigAdd(draw, 1L)
    }
    bit <- as.integer(buffer[[position]])
    position <<- position + 1L
    bit
  }
}

# Uniform integer in [0, n), using bit rejection rather than reduction modulo n.
.dsomopDpUniformInteger <- function(n, bits) {
  n <- .dsomopBigInteger(n)
  if (.dsomopBigCompare(n, 1L) == 0L) return(.dsomopBigInteger(0L))
  width <- .dsomopBigBitLength(.dsomopBigSubtract(n, 1L))
  repeat {
    # Most significant bit first, as value <- 2 * value + bit.
    value <- .dsomopBigFromBits(
      vapply(seq_len(width), function(i) bits(), integer(1L))
    )
    if (.dsomopBigCompare(value, n) < 0L) return(value)
  }
}

# Compare a lazy uniform binary expansion to the rational numerator/denominator.
# After each bit the undecided interval is rescaled exactly; its probability
# halves. This avoids drawing a large denominator-sized integer for each coin.
.dsomopDpBernoulli <- function(numerator, denominator, bits) {
  numerator <- .dsomopBigInteger(numerator)
  denominator <- .dsomopBigInteger(denominator)
  repeat {
    if (numerator$sign <= 0L) return(FALSE)
    if (.dsomopBigCompare(numerator, denominator) >= 0L) return(TRUE)
    numerator <- .dsomopBigAdd(numerator, numerator)
    if (bits() == 1L) numerator <- .dsomopBigSubtract(numerator, denominator)
  }
}

# CKS Algorithm 1: the parity of the first failed rational coin realizes the
# alternating exponential series exactly. For x > 1, multiply exp(-1) coins.
.dsomopDpBernoulliExp <- function(numerator, denominator, bits) {
  numerator <- .dsomopBigInteger(numerator)
  denominator <- .dsomopBigInteger(denominator)
  while (.dsomopBigCompare(numerator, denominator) > 0L) {
    if (!.dsomopDpBernoulliExp(1L, 1L, bits)) return(FALSE)
    numerator <- .dsomopBigSubtract(numerator, denominator)
  }
  k <- .dsomopBigInteger(1L)
  while (.dsomopDpBernoulli(
    numerator, .dsomopBigMultiply(denominator, k), bits
  )) {
    k <- .dsomopBigAdd(k, 1L)
  }
  .dsomopBigIsOdd(k)
}

# CKS Algorithm 2's magnitude: P[Y = y] = (1-exp(-s/t))*exp(-s*y/t).
# Its expected number of iterations does not grow with sensitivity/epsilon.
.dsomopDpGeometric <- function(numerator, denominator, bits) {
  numerator <- .dsomopBigInteger(numerator)
  denominator <- .dsomopBigInteger(denominator)
  repeat {
    u <- .dsomopDpUniformInteger(denominator, bits)
    if (.dsomopDpBernoulliExp(u, denominator, bits)) break
  }
  v <- .dsomopBigInteger(0L)
  while (.dsomopDpBernoulliExp(1L, 1L, bits)) v <- .dsomopBigAdd(v, 1L)
  .dsomopBigDivide(
    .dsomopBigAdd(u, .dsomopBigMultiply(denominator, v)), numerator
  )
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
  rate <- .dsomopDpExactRate(epsilon, sensitivity)
  bits <- .dsomopDpBitStream(key, context, coordinate)
  repeat {
    magnitude <- .dsomopDpGeometric(rate$numerator, rate$denominator, bits)
    if (bits() == 1L) {
      if (magnitude$sign == 0L) next
      return(.dsomopBigNegate(magnitude))
    }
    return(magnitude)
  }
}
