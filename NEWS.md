# dsOMOP 2.10.3 (2026-11-13)

## Date presentation (A-28 / CR-29)

- Corrected (DATES-B4): year-bearing custom factors include every year in a
  declared `level_range`, including unused middle years and ISO boundary weeks.
  Empty or disjoint observed dates no longer cause valid public keys to refuse
  publication. Re-execute affected recipes; no declaration changes are needed.

- Corrected (DATES-B1–B3): native date reducers and raw date columns apply the
  effective invalid/domain/future policy after selection and before presentation.
  A selected date beyond `as_of` becomes NA; no earlier event is substituted.
  Yearless custom factors use first appearance in exactly 2000–2399, and escaped
  percent directives are literal text in validation, rendering and label keys.
  Re-execute affected recipes to refresh values and ordered-factor contrasts;
  no declaration changes are needed. Active `%Y-W%V` remains refused: use `%G-W%V`.

- Newly permitted: `omop_date_spec()`, `omop_date()`, `omop_date_components()`,
  `omop_date_parts()`, `omop_time_between()` and `omop_age_at()` compose native
  calendar dates, periods, partial dates, date parts and independently anchored
  ages. Variable fields override recipe defaults. Calendar, fiscal, ISO/CDC and
  explicitly profiled epi weeks support ordered factors, numeric coordinates
  and inclusive period bounds. English names and validated custom patterns are
  locale independent. Date factors reconcile through the existing controlled
  factor transport, including public zero-count categories.
- Corrected: exact-date assignment needs no `dsomop.allow_absolute_dates`
  setting. All date-derived forms retain class-2 provenance, and publication,
  protected lineage and aggregate admission remain enforced. Native presentation
  cannot be erased by an omitted legacy date policy. Numeric measures stay
  numeric. Legacy `first_date`/`last_date` offsets and `omop_age()` annual ages
  keep their existing meaning when the new APIs are absent.
- Newly refused: unsafe/custom patterns (use `%G-W%V` for ISO weeks), incomplete
  epi profiles, irrelevant spec fields, duplicate part names and overlapping
  explicit legacy/native date policies. Express native policies in `date_spec`.
  Formats without a roster date slot refuse a date variable rather than
  encoding a Date/factor into a numeric sparse matrix.
- Migration: replace fixed-anchor offsets plus `mutate()`/`cut()` with
  `omop_date("first_visit", omop_date_spec(precision="quarter"))`; replace
  annual ages with `omop_age_at("death")` for completed years. Incomplete birth
  dates give NA unless an explicit imputation or `unit="annual_years"` is chosen.
  Coarsened labels are ordered factors; request `representation="number"` for
  trends or `"start"`/`"end"` for Date bounds. Native interval/panel/survival
  endpoint columns are added alongside unchanged internal day offsets when a
  recipe date spec requests presentation.

Security release: the P0 fixes of the OHDSI-tools audit.

- Release 2.10.3 pairs both packages, including recipe language 2 and the
  publication protocol. Existing valid secrets are reused; ordinary operation
  needs no privacy settings, state provisioning, epoch bump, ETL hook or TLS
  change. Opal's installed package and published profile remain distinct;
  the version check identifies a stale profile. README upgrade notes describe
  compatible pooling, legacy execution and changed-answer behavior.

- Reduce protected-release overhead by reusing canonical person text and fixed
  SHA-256 framing, consuming consecutive sampler bits in blocks, and using exact
  two-digit arithmetic for small rational coins. Larger coins retain arbitrary
  precision. Fingerprints, random bits and signed release components are unchanged;
  no cross-request cache or additional dependency is introduced.
- Corrected concurrent DP-root creation logging: only the worker that durably
  creates the private root reports creation. Other workers reuse the same root
  automatically; no administrator provisioning is needed.
- Contribution HMACs avoid copying UTF-8 input through R raw vectors; the
  cryptographic bytes, signed components and deterministic answers are unchanged.

- Corrected DP-only admission for analysis assign loaders: private server-side
  preparation can now create the sealed inputs needed by fixed typed releases.
  The tools-disabled gate, publication controls and final protected-release
  checks still apply; no administrator setup is needed.
- Large fixed histograms avoid repeated whole-population cell scans and factor
  construction during fingerprint chunking. The signed components, byte framing,
  noise and per-request generation are unchanged; no answer cache is added.

- Protected OHDSI-tool outputs now share canonical components across
  equivalent automatic/typed analyses, public-grid views, statistic aliases and
  validated lineage/rename variants. Complete registered domains and common
  histogram/moment bases remove fresh draws from dummy levels, clamp-only outer
  breaks and harmless cap variants. Flexible scopes reuse matching realised
  bounded contributions; supported fixed-synopsis views use only their common
  protected basis.
- No privacy accountant, total-epsilon budget, rate limit, query cap, or
  refusal based on the number of questions asked is introduced. Reference
  calibration remains 0.1; component allocations and approximation metadata are
  explicit. Public grids and clipping may change released precision. Old
  clients must not pool unsupported protected payloads.
- Automatic protection now covers the demonstrated disclosive distribution and
  flexible-window routes, including direct readers and aliases. Known-disclosive
  exact fallbacks are unavailable. Achilles observation-period support and
  2102/506 contracts, SCCS exposed-case statistic support, overlap/MDRR
  dependencies, model cell/complement admission, and PostgreSQL grouping for
  Achilles 2/4/5/10/12 are corrected. Typed releases now honor the
  community-tool enabled gate and redacted audit.
- Residual risk: different overlapping scopes, data/mechanism changes and other
  permitted exact methods can still support inference. Realised-contribution
  sharing has data-dependent equality/correlation leakage and is not a general
  transcript-DP guarantee. Exact support gates remain DataSHIELD-style
  controls. See DISCLOSURE_CONTROL.md for the threat model, supported
  fixed-synopsis guarantees and utility costs.

## Corrected

- Snapshot source certification now checks generated-column expressions and
  their column dependencies, including behind nested definer views. Volatile,
  stateful or unverifiable expressions refuse the whole request; deterministic
  generated columns and stored timestamp defaults remain supported. Normalized
  source definitions and session expression settings are rechecked on exit.

- MySQL and MariaDB snapshot requests certify the transitive sources of views,
  including external schemas, and refuse unverifiable or nontransactional
  dependencies. Snapshot failures revoke pending plan publications.
- Native population and matching compilers preserve those request snapshots.
  Active MySQL/MariaDB snapshots freeze populations through the existing
  private R transfer, requiring memory for the complete relation without
  imposing a row cap. Failed transfers retain temporary-table cleanup ownership.
- Nested Achilles histogram quantile intervals and tail flags follow the final
  reconciled histogram. Empty eligible incidence populations use the complete
  protected basis under the existing cohort-parent gate.
- Streamed membership digests preserve the minimum signed 64-bit identifier
  by selecting decimal text before driver conversion. This does not extend
  the separate person-token contract to that identifier.
- Achilles 2, 4, 5, 10 and 12 group by every selected concept name and run on
  PostgreSQL, SQL Server, DuckDB, Oracle and BigQuery (before, only SQLite and
  MySQL/MariaDB accepted their SQL). Verified on PostgreSQL 14, MariaDB 10.6
  and DuckDB, together with every live Achilles analysis.
- Achilles reader: record analyses 220/420/620/720/820/1820, 505 and 1818 count
  persons only over records inside an observation period, as upstream does,
  through a correlated `EXISTS` (duplicate or overlapping periods add no
  support). A missing observation_period table or stratum column withholds
  these cells; contracted stratum keys are never shortened.
- Achilles 2102 is persons by device exposure start month, by device concept:
  the reader serves its rows (it returned none) and the live entry returns
  persons instead of the monthly device record count (new output columns
  `calendar_month, concept_id, concept_name, count_value`). The reader keeps
  refusing 506; the live 506 counts one death (the earliest) per person.
- The OHDSI results reader no longer releases SCCS effect statistics (rr, CI
  bounds, p, log_rr, se_log_rr) resting on fewer than `nfilter.subset` exposed
  cases: `sccs_result` statistics also need `covariate_subjects` and enough
  outcomes inside and outside exposed time; `cm_result` (both arms) and
  `plp_performances` (both outcome classes) get the same private statistic
  basis, read even when only `rr` is selected. Tables without the basis return
  NA statistics and keep their counts.
- `cohortdx.cohort_overlap` no longer returns the redundant `either` row, which
  next to `a_only` and `b_only` revealed a suppressed `both` region; sum the
  released regions for the union.
- The MDRR of `cm.mdrr`, `ohdsi.cohort_method.cm_result` and the diagnostics
  summary is derived from the protected arm components in auto mode (one owner:
  the three entries show the same value). On standard routes it is
  unavailable, like the person-time it depends on; its standard kernel needs
  each arm's persons, outcome persons, outcomes and person-time separately and
  uses the released arm values (band(target) + band(comparator)), never the
  band of their total.
- PLP and propensity models admit a binary covariate only with `nfilter.tab`
  persons with and without it within each outcome class or arm; an
  inadmissible response releases no model (no intercept-only fallback, except
  the propensity model's explicit intercept-only contract). PLP calibration
  means need supported outcomes and non-outcomes per bin and are released at
  banded precision. Interaction subgroup families are complete partitions
  (with unknown gender and unknown age) released only when every level and its
  complement pass in both arms.
- CohortMethod, incidence, Kaplan-Meier and interaction ports intersect the
  time at risk with the observation period containing the index (coalesced
  periods, half-open windows): outcomes outside observation no longer count and
  person-days count whole days of `[start, end + 1)`.
- MariaDB servers with a distribution suffix in their version (for example
  Ubuntu's `10.6.23-MariaDB-0ubuntu0.22.04.1`) are recognized. Before, the
  last dotted number (`0.22.04`) was read as the server version, so every
  connection to such a server was refused as older than MariaDB 10.2.

## Newly protected (auto mode)

- Release contracts (`R/tools_release_contracts.R`) cover Achilles 3, 10,
  101-107, 203/403/603/703/803/1803, 206/406/606/706/806/1806/2106, 506 and
  715-717; the reviewed QueryLibrary histograms and partitions,
  `person.age_distribution_decade` and `observation_period.long_period_count`;
  `fe.continuous`, `fe.comorbidity_index`, `cohortdx.time_distribution`,
  `cm.followup_distribution`, the temporal covariate value distribution,
  `txpath.duration_eras` and `char.time_to_event`; the distribution rows of
  `fe.table1` and `char.target_covariates` and the MDRR of the CohortMethod
  diagnostics summary (mixed frames); `incidence.rate`,
  `cm.mdrr`/`cm_result`, `cm.effect_estimate` and the interaction estimate
  (person-time status and time components; Poisson rate ratios derived from
  them), and every alias of these entries. Every other scope is
  realised-contribution protected.
- Fixed synopses compute a canonical person-membership digest within each
  request's pinned snapshot. Clinical COUNT/MAX scans and cross-request
  generation/failure caches are removed. Membership insertions, removals and
  balanced replacements redraw fixed components; unrelated clinical inserts
  leave unchanged components stable. Clinical edits with unchanged membership
  retain the documented component-change residual and no evolving-transcript
  DP guarantee. `snapshot_id` affects only display metadata, including restored
  sealed tables. Epoch rotation is optional hardening.
- Aggregate requests use backend snapshot transactions where supported, pin the
  connection through support/generation/components, and roll back on exit.
  Snapshot loss fails the entire request. PostgreSQL permits temporary-table
  DDL under REPEATABLE READ; MySQL/MariaDB use consistent snapshots, and
  DuckDB/SQLite use explicit transactions. Other capabilities are labelled.
- CohortMethod Cox and admitted interaction Cox use one full-horizon protected
  exit-time/status basis with shared C6 event parents (Breslow ties, 3650 days,
  labelled as such, not the raw Efron estimator); the SCCS incidence rate
  ratio is estimated from a protected histogram of the q-gridded conditional
  sufficient statistics (each person's first 10 events); both report standard
  errors that include the release noise. In standard mode with the DP
  service, incidence and CohortMethod person-time, rates, MDRRs and effects
  are filled from the same protected run (mixed frames).
- Client pooling preserves Cox status parents and complete interaction
  families before age-view coarsening. Signed family exports include public
  atom/view identities; supplemental pooled rows and cells retain their view
  labels. Effect estimates continue to use per-site inverse-variance
  meta-analysis rather than a pooled Cox refit.
- Continuous values of `fe.continuous`, target covariates and temporal value
  distributions are reduced per person within concept × unit (unknown units a
  separate stratum; only registry-declared conversions); registered
  concept × unit scales (built-ins and `dsomop.release.measurement_scales`)
  give bounded moments, other strata a coarse histogram. The outputs gain
  `unit_concept_id` and `unit_name`.
- The eight QueryLibrary record summaries release capped record-weighted
  moments in auto mode (K = 10 records per person by date and
  key; persons, records and lattice sums; days [0, 10957], calendar months
  [0, 360]; quantity and cost scales through `dsomop.release.quantity_scales`
  and `dsomop.release.cost_scales`). Unregistered scales retain weighted
  value histograms and interval quantiles; moments stay unavailable. Observed
  strata are honestly labelled realised-sticky and DP-exclusive mode refuses
  them. Standard mode retains its established governance exception.
- Capped recurrent incidence counts and event-time histograms are restored.
  Event-time 1/30/365-day views share one public hierarchy with a whole-person
  cap across episodes and bins; first-event requests remain distinct. Recurrent
  `persons` counts distinct contributors among those same kept occurrences via
  a complete 998-interval presence vector with sensitivity 3K. Static fractions
  are N/R .2 each, A/B/C .1 each and P .3; displayed R lies in [N,K N]. The
  versioned radial projection enforces event/person bounds and interval
  constraints but can add bias toward its event-mass anchor. Coarse persons
  select presence coordinates and are never sums of daily person counts.
- Typed binary pooling validates complete semantic domains and numerator/
  denominator masks. Weighted numeric views project the complete atom basis
  before coarsening on server and client. Catalogue guarantee scopes are checked
  alongside epsilon/delta. New clients warn about old servers and refuse mixed
  protected pooling; old typed clients receive a clear upgrade refusal.
- Quantiles include bin intervals and open/censored flags. SCCS documentation
  distinguishes signed estimating equations from likelihood fits, states q-grid
  clipping bias, and reports simulation uncertainty as approximate.
- The public marginal hierarchy: one canonical parent person total per
  eligible population, shared by its stratifications; strata are reconciled to
  it and projected onto its displayed total, so a child request never moves
  its parent. Person-time components belong to scientific owners (one outcome
  definition, one time contract), never to labels or spellings. Protected
  person-time is capped at the 3650-day horizon before event selection.
- Free numeric parameters of every built-in aggregate entry snap to public
  grids before evaluation (time-at-risk, windows, look-backs, ages, age bands,
  `min_days`, reference year, alpha/power; model feature counts and calibration
  bins) and the label reports requested and effective values. Day parameters
  use the clinical grids (anchors 0, 1, 7, 14, 21, 28, 30, 60, 90, 120, 180,
  270, 365, 545, 730, 1095, 1460, 1825, 3650): standard windows such as
  [1, 365], look-back 365 and [0, 0] are unchanged.
- The reviewed QueryLibrary histograms use the public grids of the release
  layer (durations on the clinical duration grid up to 10957 days, counts per
  person 0-100+); unbounded scales have an explicit underflow bin.
- Typed releases are views of registered complete public bases (complete
  categorical domains with Other and Unknown, fixed numeric atoms); the
  release-person map is sealed with the table, aliases of one eligible
  population share its person total, and compound identities use injective
  length-framed encodings.
- Every protected release carries a public `source$logical_source_id`;
  realised and mixed releases state no `delta`.
- Pooling: `release_pooling` descriptors carry the established contract;
  dsOMOPClient 2.10.3 pools record summaries by their signed components and
  effect estimates by inverse-variance meta-analysis of the per-site
  protected estimates.

## Newly refused or unavailable

- Standard routes (standard mode, or auto mode without the DP service) release
  exact scoped moments and quantiles of per-person distributions as NA
  (`protected_output_required` / `protected_mechanism_unavailable`). Exact
  flexible-window person-days, rates, MDRRs and effect estimates/CI/SE are
  never released: without the DP service they are NA, with it (also in
  standard mode) they come from the protected run.
- The Achilles support of the reader's companion cells is recomputed on every
  read (no per-handle cache), and private SCCS person support uses
  `max(nfilter.subset, nfilter.tab)` for every person basis and complement.
- The long observation-period count is released only from its protected
  histogram; without the DP service it returns a stable unavailable row.
- In auto mode the precomputed Achilles distribution reader returns its
  summaries as NA (`protected_basis_unavailable`); the live analyses release
  protected summaries.
- DP-only mode accepts only fixed-synopsis releases: cohort- or table-scoped
  protected questions, unscoped ones whose data generation cannot be derived,
  and typed releases other than counts, capped record counts and
  registered-scale means of unscoped pinned loader tables of a connected
  database are refused with a stable message.
- Typed `bounded_mean` needs one registered public scale
  (`public_scale_unavailable` otherwise; custodians can add scales with
  `dsomop.release.public_scales`). Typed counts now use the shared person-total
  allocation (0.2 of the reference calibration), so a standalone typed count is
  noisier than its former full-epsilon release.
- `omopDpReleaseDS` requires the community tools (`dsomop.tools.enabled`) and
  logs every attempt; it returns the protected payload of
  `dsomop-protected-release-v1` and needs dsOMOPClient 2.10.3. The previous
  per-request semantic identities and the private database version (largest
  clinical primary keys) are removed: realised releases re-draw exactly the
  components whose contributions changed, and fixed synopses follow the
  automatic data generation.
- DP status advertises the protected-release contract
  (`privacy_contract = "protected_release_fixed_basis_or_realised_v1"`,
  `history_based_admission = FALSE`); clients older than 2.10.3 refuse typed
  releases from it.

## Recipe language 2 and concept presentation

Recipe language 2 adds a typed plan graph, explicit coverage, events, eras,
anchors, matching, reducers and composable outputs. This release includes the
recipe-language-2 work. Required support is documented in README; the installed
capability block remains authoritative while compilers are integrated.

## Corrected

- Protected concept reconciliation retains the authenticated release-person
  map and snapshot provenance when resealing. Outputs without concept fields
  or generated concept names retain their existing metadata and values.
- BIGINT reducers preserve exact decimal values, including empty and all-NULL
  streams. Existing recipes need no driver options or changes.

- (A-27 review F2/P1) `ds.omop.table()` preserves arbitrary permitted labels,
  including commas, through temporary protected analysis copies with lossless
  category tokens. Counts map back to the exact public level contract, including
  unused declarations and deliberate merges; actual missing values stay missing.
  Existing disclosure checks remain in force. Use matching server/client builds
  and this helper for free-text pooled tables; direct dsBase label transport
  remains comma-delimited. No recipe changes are needed.

- (A-27 review fixes) CSV staging preserves literal `"NA"` labels separately
  from actual missing cells, including reconciliation rewrites. New CSV
  descriptors declare `csv_encoding = "backslash-v1"`; server-side readers
  must decode through `.publishReadChunks()` (a leading backslash escapes
  literal `NA` and leading backslashes). Existing descriptors retain their
  legacy decoding. Unicode whitespace-only recodes are now rejected on both
  boundaries; replace them with a nonempty label. Valid text is unchanged.

- (A-27) Concept factors preserve their federation level contract during
  model-frame subsetting, including locally absent categories in `ds.glm`.
  Translated legacy dictionaries use the declared concept schema in both
  memory and staged execution; they do not expose an observed ID inventory.

- (A-27/CR-28) Concept-valued results are unordered factors by declared
  semantic class, including aliased reducers and nested/staged references.
  Vocabulary labels and generated concept names reconcile by ID across
  retained sites; missing names fall back to decimal IDs, duplicate automatic
  labels/names are disambiguated, and absent categories retain zero-count
  levels. Computations and joins still use internal IDs. For numeric IDs
  and untranslated generated names, use
  `options = list(translate_concepts = FALSE)`.


- Publication counts distinct genuine persons on the final materialisation,
  validates every row's direct or roster lineage, and commits complete output
  families atomically. Dynamic inventories reserve fresh symbols; tokenisation,
  DP sealing, staged files and manifests have verified rollback. Status is read
  through `omopPlanStatusDS` before cleanup; revoked staged components cannot
  be opened. A partial `out` mapping publishes only mapped roots unless
  `publish` is explicit. Explicitly empty publication roots remain private.
- Parent populations restrict membership. A child without its own index
  inherits the parent's episode roster before evaluating its criteria. Unions
  retain distinct same-date events by full source identity. A specified concept
  set resolving to no concepts matches no rows, never every concept. Concept
  exclusions and domain concept-column sentinels resolve consistently.
- Candidate eligibility, observation coverage and index selection have explicit
  order. Ambiguous coverage defaults to private dropping; missing coverage does
  not manufacture observation. CDM 5.4 procedures use their recorded end role.
- Event values, units and ranges are normalized before predicates and reducers.
  Missing tables use explicit unavailable semantics. SQL predicates retain
  three-valued logic, including NOT. Episode-member and cost adapters validate
  person agreement and currency; typed adapter failures travel through status.
- Eras distinguish recorded coverage, course envelopes and effective stockpiled
  supply. `min_days` uses recorded span; persistence and carry do not satisfy
  it. Direct-event coverage follows the interval quality policy. Observed-day
  segments, visit/episode associations and unavailable reducers retain their
  declared semantics. MySQL consumers can reuse stockpile relations without
  reopening a temporary table. Private fetches and cleanup are bounded.
- Native score history ends at the reference date (index by default); the v1
  adapter keeps its historical all-time condition selection. Native survival
  uses explicit censoring, while v1 retains its cohort-end cap and old component
  shapes. Dense panels retain source/concept identities and carry-forward keys.
- One closed validation and normalisation pipeline handles v1 compatibility and
  native v2. The shared semantic digest binds result-affecting legacy variants,
  version tuple and native defaults, excluding only declared advisory fields.
  Typed SQL helpers probe runtime capabilities and refuse unsupported constructs
  before execution. Runtime/driver evidence is separate from SQL snapshots.

## Newly permitted

- (A-27/CR-28) Append `labels` and `column_names` to `omop_variable()` and
  `omop_recipe()`. Variable maps override recipe maps. Equal explicit value
  labels deliberately merge categories; explicit column names address stable
  untranslated keys and never merge columns. Maps require native rendering.
  Presentation-only federations may use different vocabulary versions;
  comparable newest releases win, otherwise UTF-8 byte order breaks ties.


- Small or empty private intermediates, narrow windows and age ranges, numeric
  row predicates and off-grid assigned age bands. Only final independently
  accessible patient components need `nfilter.subset` persons. Aggregate
  endpoints retain their admission, suppression, grid and banding controls.
- Assigned tables may carry exact birth components and exact age. An assigned
  `age_breaks = c(0, 18, 19, 65)` grid may retain a one-person `18-18` band;
  informational quasi-identifier lint does not change admission.
- Index sources cover every reviewed event-role table, including visit detail,
  observation periods, death, specimen, note, payer periods, eras and CDM 5.4
  episodes. Point-event interval outputs use start=end. No start role still
  means refusal. Indexed windowed features may default to episode grain;
  an unindexed population or explicit incompatible person grain stays invalid.
- Advanced event/reducer recipes and the equivalent simple study layer, typed
  parameters and composition, explicit publication roots, and protected
  server-side mutation/filter expressions, subject to advertised operations.
- `on_small_site = "skip"` keeps successful sites and commits nothing at a site
  with a small family. Non-strict `"error"` returns passing families with a
  partial warning. Only `insufficient_support` is skippable.

## Newly refused

- (A-27/CR-28) Translation with `factor_concepts = FALSE`, malformed maps,
  invalid or colliding effective explicit names, old servers without
  `output.concept_labels`, and incomplete/disclosure-unsafe shared factor
  contracts fail clearly. Remove `factor_concepts = FALSE` or explicitly
  disable translation. Reconciliation failures roll back the federation;
  `on_small_site = "skip"` cannot bypass them.


### Structural

- Unknown fields at every nesting level, mixed population shapes, unsupported
  versions, incompatible grain/index policies and native-wire legacy variants
  fail before queries. Existing execution-time refusals are validated earlier
  with typed errors. `scope.tables_frames` cannot be supplied on the wire.
- A v1 results-cohort selector of zero is refused; an optional inline cohort
  label of zero remains distinct and permitted. Historical empty v1 criteria
  populations keep their private empty semantics (A-21), rather than acquiring
  an invented population. Handle-owned cohort names retain their historical,
  population-local compatibility representation (A-25).
- Ignored set-operation modifiers, unknown population kinds and missing output
  population/reference dependencies now fail validation. Unknown baseline
  derived fields and absolute-date fields outside the date policy are refused.
  Class-1 sources and incompatible predicate/concept-column types fail closed.
- `tie_policy = "error"` remains a refusal. Missing tables, join cardinality,
  requested coverage ambiguity errors and bounded resource failures are never
  treated as small-site skips. Adapter errors include the closed reasons
  `adapter_person_mismatch`, `cost_currency_mixed`, `stockpile_row_cap` and
  `reducer_row_cap` under `validation_error`.

### Publication and verbs

- Small final patient components, including zero-row components, refuse their
  entire root family; reference companions cannot escape. Malformed lineage is
  an error; `set_id` alone cannot establish person support. Invalid or missing
  status and failed cleanup are technical failures with federation rollback.
- Verbs revalidate class-1 columns in their results. Mutation cannot read or
  write protected keys, use class-1 references or create class-1 aliases. Its
  closed AST retains operational caps and invalidates DP provenance. In-place
  federation replacement requires verified restoration on failures, including
  missing acknowledgements; unresolved rollback copies are retained for retry.
- DP preparation rejects matching descendants, unsealed external frames and
  persistent-cohort scopes as `not_person_local`. Eligible outputs seal before
  commit. No query budget, accountant or persistent answer store is introduced.

### Dialect

- A plan needing an unavailable typed helper or cross-statement temporary
  materialisation refuses with `dialect_unsupported`. MySQL ≥8.0 and MariaDB
  ≥10.2 are required; SQLite window-frame support is probed at runtime.
  SQL-contract-only backends do not imply executable recipe parity.

## Upgrade and release verification

Install matching dsOMOP/dsOMOPClient 2.10.3 versions. On Opal, republish both
methods and Options after installation (Administration > DataSHIELD > Packages
> dsOMOP > Publish), then check `ds.omop.server.version()`. The client requires
published `omopPlanStatusDS` and `omopStagedRevokeDS`; 2.10.1 lacks them. A
2.10.1 client against this server retains safe v1 plans, but a strict refusal
can appear as missing requested outputs after status `refused`.

The O1–O12 harness checks fresh/upgrade paths on pinned Opal 6.1.3 and 5.7.6,
including the strict/non-strict × error/skip matrix and unchanged aggregate
policy. Native acceptance and two-version Opal results are release gates, not
claims made by these documentation changes. Roxygen remains 7.3.3.

# dsOMOP 2.10.2

- Reads every OMOP CDM version label. `cdm_source.cdm_version` values with a
  patch number ("v5.3.1", as in Eunomia GiBleed and OHDSI Broadsea's demo CDM,
  or "5.4.2") use their minor version's specification, as the OHDSI tools read
  them; 2.10.1 and earlier refused such databases with "No supported OHDSI CDM
  specification is available for version 'v5.3.1'". A label with no available
  specification (an older 5.x, a custom label, 6.0 without CommonDataModel) is
  read with the specification its table structure matches (5.3 or 5.4), and an
  empty label is inferred from the structure.
- PostgreSQL through RPostgreSQL works: R servers without RPostgres fell back
  to RPostgreSQL, which does not implement `DBI::dbIsValid()`, and every OMOP
  handle failed to initialise and to close. Connections of such drivers are now
  checked with a trivial query.
- README: on Opal 6, "Allow R packages management" (Administration > General
  Settings > Configuration) must be enabled before installing DataSHIELD
  packages from the GUI. Verified end to end on Opal 6.1.3 with Rock and
  Eunomia GiBleed.

# dsOMOP 2.10.1

Plug and play on Opal, Armadillo and DSLite: install, create a resource,
connect.

- No database connection requires TLS any more. A resource that leaves its
  TLS settings empty connects the way the driver does by default, as dsOMOP
  1.x did: PostgreSQL receives no `sslmode` (libpq's "prefer": TLS only when
  the database offers it), MySQL/MariaDB do not require TLS, and Microsoft's
  SQL Server ODBC drivers get `Encrypt=no` and `TrustServerCertificate=yes`.
  2.10.0 required verified TLS (`verify-full`) for every database that was not
  on localhost, so a database on the same machine or Docker network without
  TLS (OHDSI Broadsea's atlasdb, a plain postgres container) failed with
  "server does not support SSL, but SSL was required". A resource that names
  a TLS mode still gets exactly that mode.
- Resources created with the dsOMOP 1.x form (format `omop.cdm.db`,
  `postgresql://host:port/db//dsomop::/schema:cdm/vocabulary_schema:vocab`,
  also mysql and mariadb) open again: the resolver rewrites their URL into the
  2.x form and they are validated and connected exactly like 2.x resources.
  The resource and its pseudonymization identity are unchanged. The resource
  form still only creates 2.x resources (`omop.dbi.db`).
- A resource that names no CDM schema uses the engine's default schema when
  that holds the `person` table, and otherwise the single schema that does
  (Broadsea keeps its CDM in `demo_cdm`); the vocabulary follows, or the single
  schema holding `concept`. Several candidates are never guessed between: the
  error lists them so the data owner can name one.
- Upgrading on Opal: Opal publishes a package's DataSHIELD methods when it is
  first installed, not when an installed package is upgraded. After upgrading
  dsOMOP, publish its settings again (Administration > DataSHIELD > Packages >
  dsOMOP > Publish) or analysts get "No such DataSHIELD 'ASSIGN' method with
  name: omopInitDS". dsOMOPClient 2.10.1 reports this case by name
  (`ds.omop.server.version()`).

# dsOMOP 2.10.0

- Seed-only DP: the first-answer store of 2.7.1 to 2.9.x is removed. Every
  answer is a deterministic function of the secret noise root, the canonical
  question, the protected statistic and a private automatic database version
  (the largest identifier of each present clinical table, keyed with the root
  and never released). Nothing but the root is stored: no store, capacity
  limit, lock or scaling cost. Answers are identical after restarts and on
  every R server that shares the root (`DSOMOP_DP_NOISE_ROOT` /
  `dsomop.dp.noise_root`), and a reload that adds rows re-draws every answer
  at once, without an epoch bump. In-place edits that add no row with a larger
  identifier still need an epoch bump (README, isglobal-brge/dsOMOP#20).
- Status reports `release_binding = "seed_data_versioned_v1"`,
  `persistent_state = "noise_root_only"`, `service_capacity = "none"`,
  `history_dependent = FALSE` and privacy contract
  `fixed_per_release_seed_data_versioned_v1` (protocol `dsomop-dp-release-v3`,
  sampler and mechanism unchanged). Explicit DP releases need dsOMOPClient
  2.10.0; the analysis-catalog DP questions work with any client.
- Upgrading re-draws previously asked questions once. Existing
  `release-bindings/` files are no longer read and can be archived.
  `omopInitializeReleaseStore()` is deprecated and does nothing; the options
  `dsomop.dp.release_store_id` and `dsomop.dp.release_store_bytes` and their
  DataSHIELD profile defaults are removed.
- The noise root is created on first use with a custodian message, and a
  damaged, unsafe or substituted root fails closed (one stable analyst error,
  the reason in the server log) instead of being replaced.
- Fallback snapshot identities no longer warn: the database version tracks
  reloads.
- `omopDpReleaseDS()` binds the session's OMOP resource when no community
  tool has, so the database version is always available.

# dsOMOP 2.9.2

- Fix federated pooling of the reviewed QueryLibrary questions (2.9.0 and
  2.9.1): their pooling contracts declared every non-count column as a label,
  so a federation whose servers returned more than one row (every DP
  histogram and partition, and multi-row summaries) failed with "group keys
  are not unique". Concept, unit, currency, gender and visit ids, bins and
  partition cells are now grouping keys; names and bin bounds remain labels.
  Pooled DP counts are the sums of the per-server noisy counts.

# dsOMOP 2.9.1

- New community-tool assign method `omopDpPlanExecuteDS()`: prepares
  person-local tables for sticky DP releases from the same typed plans and
  recipes as `omopPlanExecuteDS()`. It runs the unchanged core plan engine and
  then seals each person-local output with the authenticated DP provenance
  `omopDpReleaseDS()` requires (canonical plan lineage and resource dataset
  identity). Ordinary loading with `omopPlanExecuteDS()` still never involves
  DP. It needs the community tools and an available DP service, creates
  in-memory tables only, and refuses outputs that are not person-local, with
  the reason (for example a baseline `age_at_index`, whose protection depends
  on other persons' ages: use an age at a public reference year instead).
  Filtering or otherwise manipulating a prepared table drops its provenance,
  so the release population must be defined in the plan.

# dsOMOP 2.9.0

- Automatic protection for the OHDSI community tools, with nothing to
  configure: the new custodial option `dsomop.tools.mode` (default `"auto"`).
  The core is unchanged and never uses DP.
- The 18 query-library questions that were blocked because they release exact
  person-level distributions are answered again, by reviewed server-owned
  replacements (their raw SQL still never runs; `person.load` stays blocked):
  - per-person durations and counts (`condition.duration_stats`,
    `drug.duration_stats`, `drug.days_supply_stats`,
    `drug_era.exposure_count_stats`, `condition_era.eras_per_person_stats`,
    `visit.duration_stats`, `observation_period.length_by_gender`) are released
    in auto mode as histograms of one value per person (the mean of the
    person's records, or their count) over fixed public bins, with sticky
    discrete-Laplace noise of sensitivity 1 person;
  - demographic distributions (`person.year_of_birth_distribution`,
    `person.birth_month_distribution`, `person.gender_by_age_decade`) as
    complete public partitions with the same noise;
  - values, quantities and costs (`measurement.value_stats`, now per unit;
    `drug.quantity_stats`; `drug_cost.*`, now per currency) and the
    all-concept or by-year length summaries use the standard person-supported
    summary (both the record and the person count must reach
    `dsomop.nfilter.dist`).
  When the DP service is disabled or unavailable, auto mode releases the same
  tables under the standard controls (`standard_fallback`). `"standard"` mode
  returns the original summaries under the standard controls only, and `"dp"`
  mode (also selected by `dsomop.dp.exclusive = TRUE` with DP enabled) refuses
  every standard statistic of the tools.
- Every analysis-catalog result carries the attribute `dsomop_protection`
  (`"standard"`, `"dp"` or `"standard_fallback"`, the estimand and, for DP,
  epsilon, sensitivity, adjacency and the noise standard deviation).
- Disclosure repairs in the community tools:
  - released rows are ordered from released (banded) values and public keys,
    so the order of QueryLibrary and live Achilles results no longer reveals
    the order of counts inside one band;
  - Patient-Level Prediction false-positive and false-negative cells are
    suppressed below `nfilter.tab` like the other confusion cells;
  - CohortMethod outcome counts and the MDRR, the CohortMethod interaction
    estimates, and the SCCS incidence-rate ratio require `nfilter.tab`
    distinct persons with an outcome (per arm or window), not only enough
    persons in the arm; the first SCCS attrition step requires
    `nfilter.tab` persons with an observation period for its observation
    metrics;
  - the Patient-Level Prediction and propensity models apply the DataSHIELD
    `glm` admission rules before fitting: each binary covariate needs
    `nfilter.tab` persons with and without it, and the number of parameters
    stays within `nfilter.glm` (default 0.33) times the number of persons;
  - Achilles status and catalog report the reviewed analyses the present
    result tables can serve instead of which analyses a site populated, and
    the OHDSI status and summary no longer release result-table row counts.
- New DataSHIELD options: `default.dsomop.tools.mode="auto"` and
  `nfilter.glm=0.33`. `omopDpStatusDS()` reports `tools_mode`.

# dsOMOP 2.8.0

- The core no longer depends on differential privacy (DP). Connecting, loading
  tables through plans and recipes, filters, concept factors, merges and row
  binding, cohorts and dsOMOP's own exploration statistics work as they did
  before DP was added, under the standard DataSHIELD disclosure controls only.
  On the same database, plans, filters, selections, merges and row binds load
  tables identical to dsOMOP 2.2.34 (values, column types and person tokens).
  Removed from the core: the DP-exclusive gate on dsOMOP's own statistics,
  plan preview, cohort list and definitions, factor-level discovery and the
  capabilities population count; DP provenance sealing of plan outputs and of
  filter, select, merge, bind and factor results; the DP runtime bootstrap in
  `omopInitDS()`; the merge DP preflight; the DP-era restrictions on merge keys
  (the episode aliases `row_id`, `rowid` and `rowId`, and compatible key
  storage types, work again) and on row binding (compatible column types and
  differently ordered factor levels bind again); the DP lineage canonicalizer
  in derived-column execution (extra fields such as `label`, and policies in
  any case, work again); and DP helpers in staged-output descriptors, which now
  use their own canonical fingerprint. Disclosure settings report the pre-DP
  constant flags again.
- Integer `*_concept_id` columns can be filtered by equality and membership,
  like factor columns and under the same categorical controls.
- The OHDSI community tools (Achilles results, OHDSI results tables, the
  analysis catalog and the query library) stay enabled by default under the
  standard controls; custodians can disable them with
  `dsomop.tools.enabled = FALSE`. `dsomop.dp.exclusive` now defaults to
  `FALSE` and applies only to these tools. DP releases accept only
  outputs of the community tools. Their DP lineage keeps the 2.7.x canonical
  identities, so existing first answers are unchanged.
- DP-era fixes to the core are kept: concept-factor safety checks, merge
  suffix protection, the filter's missing-value exclusion, identifier
  sanitization, staged streaming and the Opal cohort transport fixes.

# dsOMOP 2.7.2

- Install on any R server without extra system libraries. The exact
  discrete-Laplace sampler no longer uses `gmp`, whose source build needs the
  GNU MP headers (`libgmp-dev`). On R servers that install CRAN source
  packages (for example Rock images whose repository is
  `https://cloud.r-project.org`), 2.7.0 and 2.7.1 could not be installed
  (`configure: error: Header file gmp.h not found`, then `dependency 'gmp' is
  not available`), and the Opal administration UI showed no error. The exact
  arithmetic is now plain R (`R/privacy_bigint.R`) and draws exactly the same
  noise as before: the sampler protocol, the self-test answers and sticky
  release identities are unchanged. `gmp` is only a suggested package, used by
  tests that compare both implementations.
- Package loading no longer runs the DP self-test or validates pseudonym
  settings. R test-loads every package while installing it, so any failing
  check there made the package impossible to install. The DP bootstrap still
  runs the canonical-encoder and sampler self-test before the first release and
  refuses releases if it fails; pseudonym settings are validated when first
  used.

# dsOMOP 2.7.1

- Resolve the conditional temporal equality oracle in
  [isglobal-brge/dsOMOP#20](https://github.com/isglobal-brge/dsOMOP/issues/20)
  with one permanent first answer per public request, snapshot and privacy
  epoch. Valid repeats return the entire stored canonical response, even after
  resealing one or many changed persons or restarting R; hits do not fingerprint
  private contents or evaluate the payload again. Provenance/MAC and typed
  admission remain mandatory.
- Validate public DP specifications before private input processing or stored
  answer lookup. First/last reducers require `order_by` even on empty or
  all-missing data; public column types, levels, bounds and reducer requirements
  are checked during parsing for every primitive. Empty categorical inputs now
  prepare zero counts instead of failing before lookup.
- Add an authenticated, atomically committed persistent binding store under
  `DSOMOP_STATE_DIR`. Fresh state and 2.7.0-style state automatically initialize
  on first use when no store, configured UUID or local pin exists, with a
  custodian message and a synced owner-only `release-store-id` pin outside the
  store directory. Keep `omopInitializeReleaseStore()` for explicit setup.
  Configured UUIDs take precedence and must match the pin and authenticated
  header. Missing/corrupt established state or root fails closed; roots are no
  longer silently regenerated. Injected roots require bindings too. Replicas
  and backups must retain the consistent root/pin/store/lock set.
- Recommend external UUID pinning: without it, deleting the whole state root
  permits a fresh installation (new root/domain with the file-backed provider),
  and deleting both store and pin while retaining the noise root is not detected.
  Whole-state rollback remains an operator/storage trust assumption.
- Reserve public capacity once per new request identity, default 1 GiB through
  `dsomop.dp.release_store_bytes` / `DSOMOP_DP_RELEASE_STORE_BYTES`. Keep existing
  answers readable at capacity with no eviction or privacy call quota.
- Known scaling limitation: every request authenticates the full first-answer
  inventory in O(entries). A review benchmark measured about 25 seconds for an
  existing-answer replay at 40,000 entries; concurrent requests may time out.
  Size reservation capacity accordingly, or lower `dsomop.dp.release_store_bytes`
  before the inventory grows. Incremental authentication is future work.
- Answers stay stale until explicit custodian snapshot/epoch rotation and session
  restart. Different requests against a mutable source need not reflect one
  coherent database snapshot. This repeated-payload guarantee is not a finite
  cumulative budget or a full temporal-transcript DP guarantee.
- Publish release protocol v3 and the snapshot-first-answer status contract;
  pair with dsOMOPClient 2.7.4, whose legacy-server transition option is opt-in
  with warnings and per-server contract metadata. Old stateless answers cannot
  be reconstructed; plan the publication migration even though store setup is
  automatic. Keep all seven
  primitives, QueryLibrary paths, exact sampler, sensitivities, contribution
  bounds, epsilon/delta defaults, component allocation and clipping unchanged.

# dsOMOP 2.7.0

- Make the typed DP channel exclusive by default while DP is enabled. The
  custodial `dsomop.dp.exclusive` option and its DataSHIELD fallback
  `default.dsomop.dp.exclusive` default to `TRUE`; custodians can set `FALSE`
  to restore standard population statistics. Analysts cannot bypass this
  policy through a client option.
- A shared interface policy refuses 28 standard statistical routes, including
  profiling, query/analysis results, result inventories and observed factor
  levels, and directs analysts to `ds.omop.dp.release`. Structural capabilities
  remain available without `total_persons`; schema, vocabulary metadata and
  server-side DP input preparation remain available.
- Report the configured `exclusive` flag in DP status, including when DP is
  disabled. Switching exclusivity does not change the mechanism, policy hash
  or sticky releases. dsOMOPClient 2.7.3 automatically skips observed factor
  discovery during exclusive-mode plan preparation and displays exclusivity.
- Replace the finite-precision inverse-CDF noise sampler with the exact
  integer/rational discrete-Laplace algorithm of Canonne, Kamath and Steinke
  (2020), driven by the deterministic keyed HMAC bit stream. The sampler
  protocol is `dsomop-dp-exact-discrete-laplace-v1`, the mechanism identifier is
  `dsomop-sticky-discrete-laplace-prf-v2`, and the hashed policy schema is 3.
- DP noise calibration, contribution bounds and output clipping are unchanged.
  The implemented sampler now has the exact ideal
  distribution: pure DP with delta 0 holds with independent random bits;
  deployment through keyed HMAC relies on its cryptographic pseudorandomness
  assumption. This is still a per-release guarantee, without a cumulative
  privacy budget.
- Add `gmp` for exact arithmetic. Epsilon and sensitivity retain the exact
  binary rational values of the supplied R numbers, with no decimal rounding.
  New sampler metadata changes sticky release identities; upgrading can draw
  new noise and does not repair previous disclosures. dsOMOPClient 2.7.2 accepts
  the new metadata and delta 0 unchanged, and rejects mixed sampler versions
  within one federated release. See `DISCLOSURE_CONTROL.md` and
  `SAMPLER_DIAGNOSIS.md` for the algorithm and reproduced defect.

# dsOMOP 2.6.0

- The DP release channel is enabled by default since 2.6.0. Custodians may opt
  out with `dsomop.dp.enabled = FALSE` or `DSOMOP_DP_ENABLED=0`.
- Unconfigured domain and snapshot identifiers are derived from hashed canonical
  database coordinates and CDM source metadata; explicit configuration wins.
  Precedence: explicit configuration → complete `cdm_source` → fallback.
  Multiple complete source rows are canonically sorted and hashed; the original
  single-row hash is unchanged. Missing/empty/incomplete source metadata falls
  back to sorted vocabulary ID/version pairs plus resource identity, or resource
  identity alone when vocabulary metadata is unavailable. One warning per R
  process reminds custodians to bump `dsomop.dp.privacy_epoch` after each reload:
  unchanged snapshots allow differencing releases across reloads. Bump the epoch
  whenever data change without metadata changes, then restart sessions.
  Unresolvable resource identity and conflicting configuration still fail closed.
- The default noise root requires a persistent private state directory
  (`DSOMOP_STATE_DIR` / `dsomop.state_dir`), shared across replicas. The ephemeral
  state override remains test-only. DSLite sessions isolate their DP policies.
- Epsilon (default 0.1, maximum 8), delta (0), discrete-Laplace mechanism and
  contribution caps are unchanged. Ordinary aggregate contracts are unchanged.
