# Exact integer arithmetic for the DP sampler, in base R.
#
# dsOMOP 2.7.0 and 2.7.1 used the gmp package here. Compiling gmp from source
# needs the GNU MP headers (libgmp-dev), which many R servers lack (for example
# Rock images that install packages from CRAN sources), so dsOMOP could not be
# installed there. These helpers provide exactly the operations the sampler
# needs, with the same results as gmp. An integer is a sign (-1, 0 or 1) and a
# little-endian vector of base-2^16 limbs stored as doubles: limb products,
# column sums and carries stay far below 2^53, so every step is exact.

.DSOMOP_BIG_BASE <- 65536

.dsomopBigNew <- function(sign, limbs) {
  used <- which(limbs != 0)
  if (length(used) == 0L) {
    return(structure(list(sign = 0L, limbs = numeric(0L)),
                     class = "dsomop_bigint"))
  }
  structure(list(sign = if (sign < 0) -1L else 1L,
                 limbs = limbs[seq_len(used[[length(used)]])]),
            class = "dsomop_bigint")
}

#' Exact integer from an R value
#'
#' @param x A \code{dsomop_bigint}, an integer-valued finite number of any
#'   magnitude, or a decimal string such as \code{"-123"}.
#' @return A \code{dsomop_bigint}.
#' @keywords internal
.dsomopBigInteger <- function(x) {
  if (inherits(x, "dsomop_bigint")) return(x)
  if (is.character(x) && length(x) == 1L && !is.na(x) &&
      grepl("^-?[0-9]+$", x)) {
    return(.dsomopBigFromDecimal(x))
  }
  if (!is.numeric(x) || length(x) != 1L || !is.finite(x) || x != floor(x)) {
    stop("An exact DP integer is invalid.", call. = FALSE)
  }
  x <- as.numeric(x)
  sign <- if (x < 0) -1L else 1L
  x <- abs(x)
  limbs <- numeric(0L)
  # Dividing by a power of two is exact, so each remainder is an exact limb.
  while (x > 0) {
    high <- floor(x / .DSOMOP_BIG_BASE)
    limbs <- c(limbs, x - high * .DSOMOP_BIG_BASE)
    x <- high
  }
  .dsomopBigNew(sign, limbs)
}

.dsomopBigFromDecimal <- function(text) {
  negative <- startsWith(text, "-")
  digits <- sub("^-", "", text)
  digits <- paste0(strrep("0", (4L - nchar(digits) %% 4L) %% 4L), digits)
  starts <- seq.int(1L, nchar(digits), by = 4L)
  limbs <- numeric(0L)
  for (chunk in as.numeric(substring(digits, starts, starts + 3L))) {
    limbs <- .dsomopBigCarry(
      c(limbs * 10000, 0) + c(chunk, numeric(length(limbs)))
    )
  }
  .dsomopBigNew(if (negative) -1L else 1L, limbs)
}

# Normalise nonnegative limb values below 2^52 into base-2^16 digits.
.dsomopBigCarry <- function(values) {
  carry <- 0
  for (i in seq_along(values)) {
    total <- values[[i]] + carry
    carry <- floor(total / .DSOMOP_BIG_BASE)
    values[[i]] <- total - carry * .DSOMOP_BIG_BASE
  }
  while (carry > 0) {
    high <- floor(carry / .DSOMOP_BIG_BASE)
    values <- c(values, carry - high * .DSOMOP_BIG_BASE)
    carry <- high
  }
  values
}

#' Decimal digits of an exact integer, as gmp prints them
#'
#' @param x A value accepted by \code{.dsomopBigInteger()}.
#' @return One string, for example \code{"-1267650600228229401496703205377"}.
#' @keywords internal
.dsomopBigDecimal <- function(x) {
  x <- .dsomopBigInteger(x)
  if (x$sign == 0L) return("0")
  limbs <- x$limbs
  groups <- character(0L)
  while (length(limbs) > 0L) {
    # Short division by 10^4; every intermediate value is below 2^30.
    remainder <- 0
    for (i in rev(seq_along(limbs))) {
      current <- remainder * .DSOMOP_BIG_BASE + limbs[[i]]
      limbs[[i]] <- floor(current / 10000)
      remainder <- current - limbs[[i]] * 10000
    }
    groups <- c(sprintf("%04d", as.integer(remainder)), groups)
    used <- which(limbs != 0)
    limbs <- limbs[seq_len(if (length(used)) used[[length(used)]] else 0L)]
  }
  text <- sub("^0+", "", paste(groups, collapse = ""))
  if (x$sign < 0L) paste0("-", text) else text
}

#' Exact integer as an R number
#'
#' @param x A value accepted by \code{.dsomopBigInteger()}.
#' @return The double value; exact when the magnitude is at most 2^53.
#' @keywords internal
.dsomopBigNumeric <- function(x) {
  x <- .dsomopBigInteger(x)
  value <- 0
  for (limb in rev(x$limbs)) value <- value * .DSOMOP_BIG_BASE + limb
  if (x$sign < 0L) -value else value
}

.dsomopBigMagnitudeCompare <- function(a, b) {
  if (length(a) != length(b)) return(if (length(a) < length(b)) -1L else 1L)
  differ <- which(a != b)
  if (length(differ) == 0L) return(0L)
  top <- differ[[length(differ)]]
  if (a[[top]] < b[[top]]) -1L else 1L
}

# Magnitude a - b, for magnitudes with a >= b.
.dsomopBigMagnitudeSubtract <- function(a, b) {
  difference <- a - c(b, numeric(length(a) - length(b)))
  borrow <- 0
  for (i in seq_along(difference)) {
    value <- difference[[i]] - borrow
    borrow <- if (value < 0) 1 else 0
    difference[[i]] <- value + borrow * .DSOMOP_BIG_BASE
  }
  difference
}

#' Exact integer comparison
#'
#' @param a,b Values accepted by \code{.dsomopBigInteger()}.
#' @return -1L, 0L or 1L as \code{a} is below, equal to or above \code{b}.
#' @keywords internal
.dsomopBigCompare <- function(a, b) {
  a <- .dsomopBigInteger(a)
  b <- .dsomopBigInteger(b)
  if (a$sign != b$sign) return(if (a$sign < b$sign) -1L else 1L)
  if (a$sign == 0L) return(0L)
  a$sign * .dsomopBigMagnitudeCompare(a$limbs, b$limbs)
}

.dsomopBigAdd <- function(a, b) {
  a <- .dsomopBigInteger(a)
  b <- .dsomopBigInteger(b)
  if (a$sign == 0L) return(b)
  if (b$sign == 0L) return(a)
  if (a$sign == b$sign) {
    n <- max(length(a$limbs), length(b$limbs))
    total <- c(a$limbs, numeric(n - length(a$limbs))) +
      c(b$limbs, numeric(n - length(b$limbs)))
    return(.dsomopBigNew(a$sign, .dsomopBigCarry(total)))
  }
  order <- .dsomopBigMagnitudeCompare(a$limbs, b$limbs)
  if (order == 0L) return(.dsomopBigNew(0L, numeric(0L)))
  if (order > 0L) {
    .dsomopBigNew(a$sign, .dsomopBigMagnitudeSubtract(a$limbs, b$limbs))
  } else {
    .dsomopBigNew(b$sign, .dsomopBigMagnitudeSubtract(b$limbs, a$limbs))
  }
}

.dsomopBigNegate <- function(x) {
  x <- .dsomopBigInteger(x)
  .dsomopBigNew(-x$sign, x$limbs)
}

.dsomopBigSubtract <- function(a, b) {
  .dsomopBigAdd(a, .dsomopBigNegate(b))
}

.dsomopBigMultiply <- function(a, b) {
  a <- .dsomopBigInteger(a)
  b <- .dsomopBigInteger(b)
  if (a$sign == 0L || b$sign == 0L) return(.dsomopBigNew(0L, numeric(0L)))
  product <- numeric(length(a$limbs) + length(b$limbs))
  for (i in seq_along(a$limbs)) {
    columns <- seq.int(i, length.out = length(b$limbs))
    product[columns] <- product[columns] + a$limbs[[i]] * b$limbs
  }
  .dsomopBigNew(a$sign * b$sign, .dsomopBigCarry(product))
}

# Binary digits of |x|, most significant first (0L for zero).
.dsomopBigBits <- function(x) {
  x <- .dsomopBigInteger(x)
  if (x$sign == 0L) return(0L)
  n <- length(x$limbs)
  bits <- integer(16L * n)
  for (j in 0:15) {
    bits[seq.int(j + 1L, by = 16L, length.out = n)] <-
      as.integer(floor(x$limbs / 2^j) %% 2)
  }
  bits <- rev(bits)
  bits[seq.int(which(bits == 1L)[[1L]], length(bits))]
}

# Nonnegative integer from binary digits, most significant first.
.dsomopBigFromBits <- function(bits) {
  bits <- rev(as.numeric(bits))
  bits <- c(bits, numeric((16L - length(bits) %% 16L) %% 16L))
  .dsomopBigNew(1L, colSums(matrix(bits, nrow = 16L) * 2^(0:15)))
}

# Number of binary digits of |x|; 1L for zero, as gmp::sizeinbase(x, 2).
.dsomopBigBitLength <- function(x) {
  x <- .dsomopBigInteger(x)
  if (x$sign == 0L) return(1L)
  top <- x$limbs[[length(x$limbs)]]
  width <- 0L
  while (top >= 1) {
    top <- floor(top / 2)
    width <- width + 1L
  }
  16L * (length(x$limbs) - 1L) + width
}

.dsomopBigIsOdd <- function(x) {
  x <- .dsomopBigInteger(x)
  x$sign != 0L && x$limbs[[1L]] %% 2 == 1
}

# x * 2^bits.
.dsomopBigShiftLeft <- function(x, bits) {
  x <- .dsomopBigInteger(x)
  if (bits == 0L || x$sign == 0L) return(x)
  shifted <- .dsomopBigNew(x$sign, c(numeric(bits %/% 16L), x$limbs))
  .dsomopBigMultiply(shifted, 2^(bits %% 16L))
}

#' Exact floor division of nonnegative integers
#'
#' @param a Dividend, at least zero.
#' @param b Divisor, above zero.
#' @return \code{floor(a / b)} as a \code{dsomop_bigint}.
#' @keywords internal
.dsomopBigDivide <- function(a, b) {
  a <- .dsomopBigInteger(a)
  b <- .dsomopBigInteger(b)
  if (a$sign < 0L || b$sign <= 0L) {
    stop("Exact DP division needs a nonnegative dividend and a positive ",
         "divisor.", call. = FALSE)
  }
  if (.dsomopBigMagnitudeCompare(a$limbs, b$limbs) < 0L) {
    return(.dsomopBigNew(0L, numeric(0L)))
  }
  # Binary long division: shift in one dividend bit at a time.
  bits <- .dsomopBigBits(a)
  quotient <- integer(length(bits))
  if (.dsomopBigBitLength(b) <= 52L) {
    # The usual case. The remainder stays below 2 * b <= 2^53, so the same
    # steps are exact in plain doubles.
    divisor <- .dsomopBigNumeric(b)
    remainder <- 0
    for (i in seq_along(bits)) {
      remainder <- 2 * remainder + bits[[i]]
      if (remainder >= divisor) {
        remainder <- remainder - divisor
        quotient[[i]] <- 1L
      }
    }
    return(.dsomopBigFromBits(quotient))
  }
  remainder <- .dsomopBigNew(0L, numeric(0L))
  for (i in seq_along(bits)) {
    remainder <- .dsomopBigAdd(.dsomopBigAdd(remainder, remainder), bits[[i]])
    if (.dsomopBigCompare(remainder, b) >= 0L) {
      remainder <- .dsomopBigSubtract(remainder, b)
      quotient[[i]] <- 1L
    }
  }
  .dsomopBigFromBits(quotient)
}

# A positive finite double as odd * 2^exponent exactly; odd is below 2^53.
.dsomopDpDyadic <- function(x) {
  exponent <- 0L
  while (x != floor(x)) {
    x <- x * 2
    exponent <- exponent - 1L
  }
  while (x / 2 == floor(x / 2)) {
    x <- x / 2
    exponent <- exponent + 1L
  }
  list(odd = x, exponent = exponent)
}

# Binary gcd of two odd integers below 2^53; every step is exact in doubles.
.dsomopDpOddGcd <- function(a, b) {
  while (a != b) {
    if (a > b) {
      a <- a - b
      while (a / 2 == floor(a / 2)) a <- a / 2
    } else {
      b <- b - a
      while (b / 2 == floor(b / 2)) b <- b / 2
    }
  }
  a
}

#' Exact reduced rate epsilon / sensitivity
#'
#' Both inputs are taken as the exact binary rationals their R numbers
#' represent: R's 0.1 is 3602879701896397/36028797018963968, not 1/10.
#'
#' @param epsilon,sensitivity Positive finite numbers.
#' @return A list with the reduced \code{numerator} and \code{denominator}
#'   (both \code{dsomop_bigint}), equal to gmp's
#'   \code{as.bigq(epsilon) / as.bigq(sensitivity)}.
#' @keywords internal
.dsomopDpExactRate <- function(epsilon, sensitivity) {
  e <- .dsomopDpDyadic(epsilon)
  s <- .dsomopDpDyadic(sensitivity)
  # Odd parts share no factor of two, so their gcd reduces the fraction fully.
  common <- .dsomopDpOddGcd(e$odd, s$odd)
  shift <- e$exponent - s$exponent
  list(
    numerator = .dsomopBigShiftLeft(e$odd / common, max(shift, 0L)),
    denominator = .dsomopBigShiftLeft(s$odd / common, max(-shift, 0L))
  )
}

#' Exact DP integers as R values
#'
#' Exact integers of the DP sampler convert to decimal strings and to doubles
#' (exact up to 2^53).
#'
#' @param x A \code{dsomop_bigint}.
#' @param ... Unused.
#' @return A string or a double.
#' @name dsomop_bigint
#' @keywords internal
NULL

#' @rdname dsomop_bigint
#' @export
as.character.dsomop_bigint <- function(x, ...) .dsomopBigDecimal(x)

#' @rdname dsomop_bigint
#' @export
as.double.dsomop_bigint <- function(x, ...) .dsomopBigNumeric(x)
