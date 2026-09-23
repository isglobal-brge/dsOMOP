# Discrete-Laplace sampler diagnosis (before implementation)

Baseline: dsOMOP 2.6.0, commit `3e7e1ce`; dsOMOPClient 2.7.2,
commit `a5e95af`. Recorded on 2026-09-23 before replacing the sampler.

## Reproduction

Sourcing the original `R/privacy_state.R` in an isolated R environment and
substituting all-zero/all-255 HMAC bytes exercises the actual uniform endpoint
calculation. It gives `u_min = 2.2204460492503126e-16` and
`u_max = 0.99999999999999978`. The actual inverse transform has these bounds
at sensitivity one:

| Epsilon | Minimum geometric | Maximum geometric | Noise support |
| --- | ---: | ---: | --- |
| 0.1 | 0 | 360 | [-360, 360] |
| 0.25 | 0 | 144 | [-144, 144] |
| 1 | 0 | 36 | [-36, 36] |
| 8 | 0 | 4 | [-4, 4] |

Calling the original `.dsomopDpDiscreteLaplace` with opposite endpoint digests
for its two draws returns +4 and -4 at epsilon 8. Therefore the output 96 is
possible for count 100, but impossible for its add-one neighbour 101. The
default nonnegative output clipping does not remove this counterexample.
The 52-bit grid also rounds bin probabilities. Declaring delta zero is not
justified for this implemented sampler even though the ideal mechanism is pure
DP. The original known-answer draw is 5; its `dsomop-dp-sampler-kat-v1` hash is
`f41d43840fc89a32939f8413d825b14d5900a13ae80e0f37b35111294e4acaf0`.

## Noise and allocation inventory

`R/privacy_state.R` passes the server-owned `release_epsilon` to each release
payload. The actual admitted range is [1e-6, 8], default 0.1. Every production
noise draw goes through `.dsomopDpNoisyInteger` into
`.dsomopDpDiscreteLaplace`; its only other caller is the known-answer guard.
The only production caller of `.dsomopDpUniform` is the old sampler.

| Path in R/privacy.R | Component epsilon | Person sensitivity |
| --- | --- | --- |
| Distinct-person count | epsilon | 1 |
| Bounded record count | epsilon | contribution cap |
| Bounded distinct cardinality | epsilon | contribution cap |
| Categorical histogram, each coordinate | epsilon | vector L1 cap; 1 for mode/first/last |
| Numeric histogram, each coordinate | epsilon | records cap; otherwise 1 |
| Bounded mean: count, grid sum | epsilon/2 each | 1, numeric_grid respectively |
| Binary rate: denominator, numerator | epsilon/2 each | 1 each |

Longitudinal reducers perform deterministic person-bounding preprocessing for
these paths and introduce no separate noise source. Coordinates currently use
index 1 with separate component contexts (including histogram bin labels).
`numeric_grid` can be 2^24-1 and contribution caps can be 10000: a geometric
loop taking sensitivity/epsilon steps would be impractical. Use the efficient
CKS rational geometric construction instead.

## Contracts and implementation constraints

The old sampler identifier is `hmac-inverse-cdf-52bit-v1`. Policy schema version
2, the mechanism identifier, sampler identifier and other contract fields enter
the policy hash. The sampler is exposed in status and release metadata.

`dsOMOPClient/R/privacy.R` accepts arbitrary nonempty sampler/mechanism strings,
requires equality across selected sites and against preflight metadata, and
requires `release_delta == 0`. It pins the release-envelope protocol, privacy
contract and guarantee labels, whose field structure and semantics need not
change for this repair. Updating the sampler, mechanism implementation version
and hashed policy schema distinguishes repaired releases without a client
change. Compatibility will be tested with the unchanged client.

Parameters will be the exact binary rationals represented by the validated R
numbers, not rounded decimal strings. In particular epsilon/2 is an exact
binary division throughout the admitted policy range. Arbitrary-precision
integer/rational arithmetic must continue through adding noise and applying the
existing clipping bounds; conversion back to an R number is safe only after
clipping. Stream counters must not wrap or round. The documented DP claim is
for ideal independent stream bits, with the deployed keyed HMAC stream relying
on a cryptographic pseudorandomness assumption; finite-key cryptography is not
an information-theoretic source of infinitely many random bits.

Algorithm reference: Canonne, Kamath and Steinke (2020),
[The Discrete Gaussian for Differential Privacy, section 5](https://arxiv.org/html/2004.00010v4#S5),
exact exponential Bernoulli and rational two-sided geometric sampling.
