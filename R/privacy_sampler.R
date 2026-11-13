# Exact discrete-Laplace sampling, following Canonne, Kamath and Steinke
# (2020), https://arxiv.org/abs/2004.00010, section 5, Algorithms 1 and 2.
# All arithmetic affecting sampling probabilities or unbounded integers is
# exact (privacy_bigint.R). The source of randomness is the domain-separated
# keyed bit stream below.

.dsomopDpBitStream <- function(key, context, coordinate, draw = 0L) {
  coordinate <- .dsomopBigDecimal(coordinate)
  draw <- .dsomopBigInteger(draw)
  buffer <- integer(0L)
  position <- 1L
  refill <- function() {
    digest <- .dsomopDpHmacRaw(key, .dsomopDpCanonicalJson(list(
      protocol = "dsomop-dp-hmac-bit-stream-v1",
      context = context,
      coordinate = coordinate,
      draw = .dsomopBigDecimal(draw)
    )))
    # Preserve the original least-significant-bit-first order in each byte.
    buffer <<- as.integer(rawToBits(digest))
    position <<- 1L
    draw <<- .dsomopBigAdd(draw, 1L)
  }
  next_bit <- function() {
    if (position > length(buffer)) refill()
    bit <- buffer[[position]]
    position <<- position + 1L
    bit
  }
  # Uniform-integer sampling consumes a known number of bits. Copy the same
  # consecutive bits in blocks, avoiding an R closure call for every bit.
  # This request-local cursor is shared with scalar Bernoulli draws; neither
  # unused bits nor the arbitrary-precision block counter are discarded.
  attr(next_bit, "take") <- function(n) {
    out <- integer(n)
    filled <- 0L
    while (filled < n) {
      if (position > length(buffer)) refill()
      count <- min(n - filled, length(buffer) - position + 1L)
      out[filled + seq_len(count)] <- buffer[position - 1L + seq_len(count)]
      position <<- position + count
      filled <- filled + count
    }
    out
  }
  next_bit
}

# Uniform integer in [0, n), using bit rejection rather than reduction modulo n.
.dsomopDpUniformInteger <- function(n, bits) {
  n <- .dsomopBigInteger(n)
  if (.dsomopBigCompare(n, 1L) == 0L) return(.dsomopBigInteger(0L))
  width <- .dsomopBigBitLength(.dsomopBigSubtract(n, 1L))
  take <- attr(bits, "take", exact = TRUE)
  repeat {
    # Most significant bit first, as value <- 2 * value + bit.
    value <- .dsomopBigFromBits(if (is.function(take)) take(width) else
      vapply(seq_len(width), function(i) bits(), integer(1L)))
    if (.dsomopBigCompare(value, n) < 0L) return(value)
  }
}

# Compare a lazy uniform binary expansion to the rational numerator/denominator.
# After each bit the undecided interval is rescaled exactly; its probability
# halves. This avoids drawing a large denominator-sized integer for each coin.
.dsomopDpBernoulli <- function(numerator, denominator, bits) {
  numerator <- .dsomopBigInteger(numerator)
  denominator <- .dsomopBigInteger(denominator)
  # Two base-2^32 digits cover the common rates without allocating bigint
  # objects for every bit. Each intermediate is an integer of magnitude below
  # 2^33, exactly representable by an R double. Larger inputs retain the
  # arbitrary-precision path below; sampling probabilities never round.
  if (numerator$sign >= 0L && denominator$sign > 0L &&
      length(numerator$limbs) <= 4L && length(denominator$limbs) <= 4L) {
    a <- c(numerator$limbs, rep(0, 4L - length(numerator$limbs)))
    b <- c(denominator$limbs, rep(0, 4L - length(denominator$limbs)))
    lo <- a[[1L]] + 65536 * a[[2L]]
    hi <- a[[3L]] + 65536 * a[[4L]]
    dlo <- b[[1L]] + 65536 * b[[2L]]
    dhi <- b[[3L]] + 65536 * b[[4L]]
    base <- 4294967296
    repeat {
      if (hi < 0 || (hi == 0 && lo <= 0)) return(FALSE)
      if (hi > dhi || (hi == dhi && lo >= dlo)) return(TRUE)
      lo <- 2 * lo
      carry <- as.numeric(lo >= base)
      lo <- lo - carry * base
      hi <- 2 * hi + carry
      if (bits() == 1L) {
        lo <- lo - dlo
        borrow <- as.numeric(lo < 0)
        lo <- lo + borrow * base
        hi <- hi - dhi - borrow
      }
    }
  }
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
