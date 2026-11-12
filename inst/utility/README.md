# p0r3 protected survival utility study

Run from the dsOMOP source directory on the test pod (R packages from the test
suite are required):

```
Rscript inst/utility/p0r3-survival-utility.R 100 inst/utility/results/all law
Rscript inst/utility/p0r3-survival-utility.R 1 inst/utility/results/crypto-smoke crypto 1
```

The optional fourth argument selects comma-separated scenario IDs. Every
scenario uses the shipped default reference epsilon 0.1 and true HR/IRR 1.5.
The seed is `2026111200 + scenario * 10000 + replicate`; each replicate also
gets its own synthetic corpus identity under the fixed test root. These are
synthetic data, not clinical validation.

| ID | Model | Size | Design |
| --- | --- | --- | --- |
| 1–3 | Ordinary Cox | 500, 2,000, 10,000 per arm | Exponential baseline event time with mean 730 days, target hazard 1.5 times baseline, 366-day administrative follow-up |
| 4–5 | Interaction Cox | 2,000, 10,000 per arm | Same event process, three disjoint subgroups, common arm and subgroup hierarchy |
| 6 | SCCS | 500 persons | Exposed fractions logit-uniform over [.001,.02], one observation-period stratum per person |
| 7–8 | SCCS | 2,000, 10,000 persons | Exposed fractions logit-uniform over [.02,.5], one stratum per person |
| 9 | Ordinary Cox | 2,000 per arm | Baseline mean 7,300 days and 366-day follow-up: about 95.1% comparator censoring |
| 10 | Ordinary Cox | 2,000 per arm | Baseline mean 730 days and seven-day follow-up: about 99.0% comparator censoring |
| 11 | SCCS | 2,000 persons | Exposed fractions logit-uniform over [.95,.999]; informative fractions above .99 are retained and clamped to .99 |
| 12 | SCCS | 2,000 persons | Two informative observation-period strata per person, fractions over [.02,.5], one shared cap of ten events per person |

Ordinary follow-up implies about 60.6% comparator and 47.1% target censoring;
raw files from the extended script record realized proportions. SCCS samples
exposed counts conditionally on retained per-stratum totals. One-period totals
are `min(10, 1 + Poisson(3))`. For two periods, raw totals are
`1 + Poisson(7)`, the first period is bounded at eight, and the second receives
only the remaining part of the person's shared cap of ten. Both periods retain
positive counts. This studies the bounded conditional model; it does not
measure bias from truncating the first ten events of a full event-time process.

The retained raw files include the replicate, seed, estimate, standard error,
interval, oracle estimate, finite/informative flags, and coverage. Summaries
report finite and informative rates, coverage conditional on finite intervals,
unconditional coverage (unavailable intervals count as uncovered), and bias
and RMSE **on the log-HR/log-IRR scale, conditional on finite intervals**. A
finite fit requires a finite estimate, standard error and finite positive 95%
interval. An informative fit additionally requires upper/lower interval ratio
at most four. With 100 independent replicates a nominal 95% coverage estimate
has Monte Carlo standard error about 2.2 percentage points; fewer finite fits
increase uncertainty. The three subgroup estimates within a replicate are
correlated and are summarized separately.

The `law` mode is explicitly a utility approximation: it samples the calibrated
discrete-Laplace law using two inverse-geometric draws and deterministic 28-bit
uniforms. It retains the production contribution fingerprints, complete HMAC
contexts, equal-map noise sharing, complete-basis projections, fits, and
200-refit uncertainty procedure. It does not validate the production exact
arbitrary-precision sampler or establish privacy. The optional `crypto` mode
uses that production sampler. The retained one-replicate crypto smoke verifies
execution only and is not an empirical coverage study.

The uncertainty calculation perturbs released components independently and
adds its noise variance to model variance. It does not recover private map
equivalences. The empirical law runs retain the actual sticky correlations,
including identical empty Cox atoms shared by arms, so their coverage assesses
that limitation. Neither finite intervals nor a wide interval covering the
truth establishes useful inference. SCCS may refuse a fit when a unique
positive-information signed-score root cannot be certified. Grid rounding and
endpoint clipping change its estimand; the oracle column separates the
noise-free grouped estimate from the protected estimate. Near-boundary q can
have severe approximation bias, and no general coverage or utility guarantee
is implied by these selected scenarios.

Results are retained by run directory with raw CSV, summary CSV and R session
information. Baseline directories were generated before diagnostic columns
were added for the extended scenarios; the formulas and seeds of IDs 1–8 are
unchanged. `crypto-smoke` contains one replicate, while law directories contain
100 replicates per selected scenario.
