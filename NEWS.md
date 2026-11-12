# dsOMOP 2.10.3 (in development)

Recipe language 2.0: a typed plan graph (`plan-v2.json`), publication as the
only disclosure gate on assignment paths, explicit coverage, eras, anchors,
matching and adapters. This section is the WS-0 skeleton; each workstream
fills its entries. Classes follow design-2.10.3 section 5.

## Corrected (results that were wrong and now follow the documented rule)

- Publication validates one genuine person per final row through direct keys or
  an immutable roster. Composite families commit together; support is counted
  on the materialized component, including streamed staged files.
- Without an explicit `publish`, only outputs named in `out` are released;
  other computed outputs remain private. For example,
  `out = c(conditions = "cond_df")` publishes only `cond_df`, preserving the
  2.10.2 behavior and agreeing with client status validation.
- Roster keys use the same decimal representation for doubles, integers,
  BIGINT values and CSV-staged characters. Large keys such as `100000` no
  longer cause a false `lineage_malformed` refusal through scientific notation.
- Output names come from the committed inventory and never overwrite occupied
  symbols. Tokenization, DP sealing, assignments and manifest publication share
  verified rollback; staged readers require exact committed membership.
- `omopPlanStatusDS()` transports bounded execution status before client cleanup;
  `omopStagedRevokeDS()` revokes execution-owned symbols and staged files.
  Strict/non-strict behavior and small-site skipping are decided before commit.
- DP preparation follows the full normalized graph and authenticated scopes;
  matching taints descendants and sealing completes before publication.
- Publication contexts carry the probed SQL dialect profile. Native compiled
  statements pass the typed SQL check before execution, and shared dialect
  refusals use the bounded publication status transport. Invalid publication
  policy errors name the `commit_policy` argument.

- (B-8) Before 2.10.3, `parent_id` recorded provenance but did not restrict
  membership; from 2.10.3 a child population is a subset of its parent, as
  `print()` has always shown.
- (A-E1) A specified concept set that resolves to no concept now matches no
  row (`{concepts = 123, exclude = 123}` yields zero counts); it was treated
  as unrestricted in feature extraction. A v1 feature specification whose
  declared set resolves to nothing selects no row of the shared stream and
  counts zero; the language-2.0 concept-set node has the explicit states
  `all` (no set declared), `set` and `none` (SQL `1 = 0`, never unrestricted).
- (A-E4) Cohort unions keep distinct same-date index events of different
  sources or keys; they collapsed on (subject, start, end).
- (A-E3) Index-event concept sets accept `exclude`, resolved by the shared
  concept-set resolver like every other concept set; they were refused with
  "Unknown index-event concept-set field(s): exclude".
- (B-20) On CDM 5.4, an index event on `procedure_occurrence` with an
  `EndDate` end strategy ends at `procedure_end_date` when it is recorded (role
  registry, design section 6.2); 2.10.2 always used `procedure_date`.
- (A-9) The plan code no longer claims that windowed population filters
  without a cohort index fall back to the server wall clock: they need an
  explicit `reference_date` and fail closed without one (behaviour unchanged).
- The `base` population of a multi-population v1 plan, declared without
  criteria, is every person, as in a single-population plan; without a plan
  cohort and a scope it restricted its outputs to no person.

## Newly permitted (previously refused by an intermediate or granularity gate)

- (B-20) Index events may come from any table with an event role record in
  the role registry (`inst/ohdsi/roles-5.3.csv`, `roles-5.4.csv`): besides the
  seven tables of 2.10.2, `visit_detail`, `observation_period`, `death`,
  `specimen`, `note`, `payer_plan_period`, the era tables and, on CDM 5.4,
  `episode`. Tables that are never event sources (`cost`, `episode_event`,
  `provider`, `care_site`, `location`, `fact_relationship`) stay refused.
- A baseline `age_breaks` grid is validated by shape only: any strictly
  increasing integer grid starting at 0 (for example `c(0, 18, 19, 65)`) is a
  valid assigned grid; the aggregate on-grid/coarsening rule of
  `.computeAgeGroups()` no longer applies to it at validation (design 4.5 b).

- Small private intermediates (populations, set-operation members, candidate
  rosters) run; only the released object's minimum persons is checked.
- Windows under 30 days, age ranges under 5 years and off-grid age bands on
  assignment paths; numeric filter operators; point tables in interval
  outputs; exact birth components and exact age in assigned tables
  (`year_of_birth`, `month_of_birth`, `day_of_birth`, `birth_datetime`);
  aggregate endpoints keep their own controls.
- (F-1) A row concept-set filter without a concept column (the
  table-agnostic `concept_id` sentinel emitted by 2.10.1 clients) matches the
  table's domain concept column, resolved like the `start_date`/`end_date`
  sentinels; it was refused as an unknown column.
- NOT groups (`{"not": ...}`) in v1 row filters and population filter trees,
  as in the language-2.0 predicate and criteria grammars (design 6.6), with
  SQL three-valued logic: a negated UNKNOWN stays UNKNOWN and only TRUE rows
  and persons are kept. The closed v1 envelope, the v1-to-IR lowering and the
  DP lineage accept them.
- `omopFilterDS()` no longer applies the `nfilter_levels_max` and
  `nfilter_levels_density` cardinality gates, following the publication-only
  disclosure rule in design section 7.2. Filtered results still need the
  minimum number of distinct persons at publication.

## Newly refused

### Structural
- Unknown fields anywhere in a plan, mixed population shapes, legacy
  constructs on the wire, `tie_policy = "error"`.
- Every plan passes one validation sequence before any query (design 4.5):
  the closed envelope of its version, normalisation (a v1 plan is lowered to
  the compatibility IR), `ir-v2.json` and the semantic rules. A v1 plan with an
  unknown field at the root, in an output (including `concept_dictionary` and
  `cohort_membership` outputs) or in any nested object is refused with the
  field's path; `already_gated` was accepted at the root and in a dictionary
  output. A null-valued known field still counts as absent; an unknown member
  is refused even when its value is null. Every nested object is closed
  (output `options`, visit filters, concept-set objects of survival outcomes,
  `value_bin` operands, safe scopes, inclusion criteria, ...).
- v1 plans are refused before any query, with the typed codes the status
  transports, for what the v1 executors refused only while running: invalid
  temporal specs, filter trees, custom filters, index end strategies,
  multi-state graphs, interval windows and selections, survival censoring and
  TAR, date handling and person_period grain; person_level raw tables other
  than person/death and feature time windows there; event_level
  wide/features/sparse grain and index-window couplings; set-op members
  declared after the set operation; `episode_policy` with `index_event`;
  tables, results-cohort tables and custom-filter columns this site does not
  provide (`unavailable_table`, `validation_error`), the custom filter of a
  person_level output over its raw person/death tables, unknown or blocked
  baseline person columns (with the executors' own messages: "Baseline
  person column(s) not found", "Disclosive: ... are blocked"); and the
  session temporary tables the v1 executors need on SQL families without
  them (`dialect_unsupported`). Typed site checks refuse what 2.10.2 left to
  SQL coercion: a predicate literal whose type does not match its column (a
  text literal on a numeric or concept column, a non-ISO date) and a
  `concept_col` that is not a filterable concept column (2.10.2 accepted any
  existing column, class-1 columns included). An unknown v1 output type
  keeps the executor's "Unsupported output type" wording. The plan-declaration error
  of an output that needs a cohort keeps the executor's untyped error.
- (A-15) A v1 inclusion `occurrence` is the executor's `{type, count}`
  object, its type matched exactly (`at_least`, `at_most`, `exactly`): any
  other member or type is refused at validation, where 2.10.2 refused it
  only while running.
- (A-10) A v1 results-cohort `cohort_definition_id` of 0 is refused, naming
  the field; the optional identifier of an inline cohort spec may be 0
  (A-16).
- A plan cannot carry resolved workspace frames: `scope.tables_frames` is
  refused; scope tables come only from the endpoint's scope arguments.
- A v1 baseline `derived` value other than `age_at_index`,
  `prior_observation` or `future_observation` is refused: unknown names were
  dropped silently, and `cohort_start_date`, `cohort_end_date`,
  `observation_period_start_date` and `observation_period_end_date` released
  exact dates outside the absolute-date policy.
- A v1 criteria population other than `base` without `filter_tree`,
  `cohort_definition_id` or `index_event` (it resolved to no person) and a
  difference of a population with itself (always empty) are refused.
- (A-E2) v1 set-operation populations carrying `filter_tree`,
  `cohort_definition_id`, `index_event` or `episode_policy` are refused (the
  first two were silently ignored), and a population of an unknown `kind` is
  refused instead of being read as a criteria population.
- A v1 output targeting a population the multi-population plan does not
  declare (`base` included), and a dictionary whose `source_outputs` name
  missing outputs, are refused at validation (the output was skipped with a
  warning).
- A plan `version` other than none, 1 or 2 is refused. Language 2.0 plans are
  validated and explained (`omopPlanExplainDS`) but not executed by this build
  until the 2.0 compilers are installed; `recipe_language.operations` is empty
  until then, so 2.10.3 clients emit v1.
- Validation failures are plan-level errors under `dsomop.query_strict = FALSE`
  as well (design 7.3); per-output failures of the v1 executors keep their
  non-strict behaviour.

### Publication
- Orphan, conflicting or nonunique roster mappings fail closed. A set identifier
  alone is never person lineage; reference companions cannot escape a refused
  patient family. Uncommitted, altered and revoked staged descriptors are refused.
- Final patient components with fewer than `nfilter.subset` persons,
  including empty sparse or recurrent components, are refused at
  publication; safe-result equivalence applies only to results that satisfy
  the final publication contract.
- The in-memory row-filter mask follows the SQL truth table (design 6.6):
  `in` / `not_in` over a missing value are UNKNOWN rather than FALSE, `not`
  keeps UNKNOWN, and a filter column the evaluated rows do not carry is
  refused instead of matching nothing (the kept rows of 2.10.2 filters are
  unchanged; only TRUE rows are kept).
- Manipulation verbs revalidate class-1 sensitive columns in their results.
  If a frame was loaded with `dsomop.allow_sensitive_columns = TRUE` and the
  administrator later disables that option, a verb result retaining blocked
  columns is now refused (design section 7.6).

### Dialect
- A v2 plan that needs an SQL construct the server's database family cannot
  run is refused at preflight, before any query, with the typed
  `dialect_unsupported` rejection naming the output and the construct:
  session temporary tables for a roster reused by several statements on SQL
  Server/Synapse/PDW, Oracle, Redshift, BigQuery, Snowflake and
  Spark/Databricks; window functions on a SQLite runtime that cannot execute
  the 1-PRECEDING frame (the runtime is probed, never inferred from the
  package version); MySQL below 8.0 and MariaDB below 10.2. The SQL SHA-256
  push-down is never required: matching computes its order in R where the
  family lacks it, as on SQLite.

## Upgrading clients and servers

- Pair dsOMOP 2.10.3 with dsOMOPClient 2.10.3 for the publication-status
  protocol. The new client requires `omopPlanStatusDS` and
  `omopStagedRevokeDS`; a 2.10.1 server lacks them.
  `ds.omop.server.version()` reports missing methods. After upgrading an
  Opal server, publish dsOMOP's settings again so the profile exposes them.
- A 2.10.1 client against a 2.10.3 server still fails closed on a strict
  publication refusal: no output is published. The assign call now returns
  status `refused`, so the old client reports that requested outputs did not
  materialize instead of receiving the previous constant `Disclosive` error.
- A federation that still has a server older than 2.10.4 receives, from
  dsOMOPClient 2.10.4, the legacy execution call of dsOMOPClient 2.10.2 on
  every site for a v1 plan (language-2 plans are refused before any
  assignment). This server runs that call, positional and without
  `commit_policy`, under the documented default `commit_policy = "server"`:
  its own strict mode decides, as for the DP preparation (WS-8 review B1).
- `omopPlanExecuteDS()` and `omopDpPlanExecuteDS()` accept an empty out
  mapping exactly when the plan's `publish` is an explicit empty array:
  the full plan is validated, nothing is materialized or assigned and the
  status is `ok` with empty inventories (A-22; WS-8 review B6). Invalid unused
  declarations still refuse the plan. Any other plan still needs a non-empty
  mapping.

## Design amendments applied by WS-0 (design/2.10.3/amendments.md)

- A-1 results-cohort index sources (namespace `cohort:<n>`, `event_end` =
  `cohort_end_date`), A-2 `omop_score()` conditions on or before the reference
  date (v1 adapter all-time, IR flag `legacy_all_time_conditions`), A-3
  `person_period` carry-forward keyed by source and per-identity column names,
  A-4 reviewed `episode_event` field concepts (visit 1147070, specimen 1147049),
  A-5 native survival / recurrent component schemas, A-6 one identifier grammar
  in the mutation AST, A-7 `omop_censor(when_index_source =)`.

- A-9 recipe 12 uses the importer's deterministic Circe names
  (`type_2_diabetes`, `type_2_diabetes_dx`); A-10 nine closed IR-only variants
  carry the v1 semantics the wire cannot express (`legacy_columns`,
  `legacy_temporal`, `legacy_automatic_features`, `legacy_single_outcome`,
  `legacy_states`, `legacy_state_step`, `legacy_episodes`/`legacy_index_anchor`,
  `observed_days.legacy_containing_period`, `legacy_names`,
  `legacy_concept_scope`); A-11 the pinned v2 normal form
  (`R/plan_normalize_v2.R`, vendored byte-identically, with the normalised
  fixtures of `inst/schema/normalized/`); A-12/A-13 client signature lines;
  A-14 execution plan selection for 2.10.1-style recipes, `source_analyses`
  on temporal and person-period outputs, and the windowed-features corpus row
  reclassified `newly_permitted`.
- A-15 the `legacy_spec` inclusion `occurrence` is the executor's historical
  `{type: at_least | at_most | exactly, count >= 0}` object (defaults
  `at_least`, 1), not `{min, max}`; A-16 five further IR-only variants carry
  the v1 semantics the executor audit found (`legacy_raw_filter`, the
  nearest-only non-zero `legacy_anchor_offset`, `legacy_spec.cohort_definition_id
  >= 0`, scope `legacy_cohort_sources` with the materialised `combine`,
  `legacy_automatic_features` format `sparse`); probes I41-I60 and the wire
  refusals P64-P66.
- A-18 a rejected status entry may carry a third closed field `reason`
  (`adapter_person_mismatch`, `cost_currency_mixed`), present only with
  `validation_error`: every status writer builds its entry with
  `.rejectionEntry()`, which copies the reason from the condition only under
  those conditions and never its raw message, field path or state;
  `.validatePlanStatus()` admits exactly that field and vocabulary.

## Concept sets, events, adapters and predicates (WS-4)

- `R/compile_concept.R`: `.compileConceptSet()` resolves a concept-set node
  once per plan (memoised on its canonical JSON): concepts, then descendants
  (`concept_ancestor`), then the `'Maps to'` sources of the set, minus the
  exclusion; (A-15) an object-valued `exclude` is expanded with its own flags
  before it is removed, a bare vector still means exact ids. The RxNorm
  ingredient map of drug concepts (fragment identity `("map", "ingredient")`)
  serves eras grouped `by = "ingredient"`.
- `R/compile_event.R`: one event compiler in three stages: the table's roles
  from the role registry (the event's own end policy, the original recorded
  end and supply kept for era overrides and supply reducers, the scoping
  concept, value/unit/operator/range sources, the visit link, the other
  filterable columns), unit normalisation (one affine map per source unit
  applied to the value and the reference ranges; unknown units `missing`
  (NA, `unit_unknown = 1`) or `exclude`; reviewed inequality operators
  (`inst/ohdsi/operator_classes.csv`) kept or excluded, NULL and unknown
  operators counting as equality), then the `where` predicate over the
  normalised columns. The configured value column and `value` bind to the
  normalised value; `value_source` / `unit_source` keep the source values.
  Predicates are typed against the column (ISO dates, integer concept ids,
  finite numbers, text in equality and IN only) and refuse identifiers,
  class-1 columns and roles the table lacks. `on_missing_table =
  "unavailable"` compiles to a typed unavailable source; `"error"` refuses
  with `unavailable_table`. Visit-type restrictions use the visit link;
  index-visit events are joined by the consumer.
- `R/compile_adapters.R`: `episode_members` (rows linked through
  `episode_event` by the reviewed field mapping of amendment A-4; one row per
  episode-member link; a member of another person is refused,
  `adapter_person_mismatch`) and `cost` (cost rows of a table event by the
  reviewed `cost_domain_id` mapping; one amount field; a source mixing
  currencies without `currency` is refused, `cost_currency_mixed`); both
  carry the parent event identity (`parent_source_ns`, `parent_event_id`).
  These refusals are plan rejections transported as `validation_error` with
  their closed `reason` (A-18). An unavailable adapter keeps the declared
  columns, unique key, source identity, ordering and private columns of the
  available one (A-17).
- `R/compile_predicate.R` (the language-2.0 predicate grammar in SQL, with
  the Kleene `any`/`all` table) and `R/compile_statement.R` (the fragment
  registry of the execution context and `.buildStatement()`: one WITH chain
  in dependency order, typed-SQL checked). (A-17) Fragment SQL identifiers
  are allocated per execution context from the structured identity (prefix,
  name, stage): every identity of the plan is preallocated in canonical
  order, so the allocation is deterministic and independent of compilation
  order; a readable name is kept only when no other identity reads the same
  under the dialect's identifier comparison and limit (PostgreSQL 63 bytes,
  Oracle 30), otherwise a stem plus a ten-digit SHA-256 prefix and the first
  free `_2`, `_3`, ... are used. A final fragment never shares an identifier
  with a stage, and events differing only by case never collide.
- Language-2.0 plans compile their event sources (adapters included) in the
  private execution context before the refusal that they are not executed
  yet, so the section 11 adapter refusals reach the status, and the
  analyst's error, with their reason; nothing is published. Covered end to
  end on PostgreSQL through `omopPlanExecuteDS` under both commit policies
  and through `ds.omop.plan.execute()` over DSLite under both
  `on_small_site` settings.

## Plan IR, role registry, explain and capabilities (WS-2)

- `R/ir.R`: `.irPrepare()` (the frozen sequence), `.irNormalize()`,
  `.irValidate()`, `.irOrder()`, `.irNode()`; failures are typed
  `dsomop_plan_rejection` conditions naming fields, outputs and constructs
  only, with the codes the publication status transports: `validation_error`
  (plan findings), `unavailable_table` (an event source this site cannot
  serve under `on_missing_table = "error"`) and `dialect_unsupported`
  (preflight, naming the output). Sources served under
  `on_missing_table = "unavailable"` are reported with the outputs that read
  them, never refused. `omopPlanPreviewDS()` and `ds.omop.plan.validate()`
  report the same findings in `validation$errors`. Native and lowered v1
  plans pass every stage, the site checks and the dialect preflight
  included. Each plan meets the site rules of the engine that executes it: a
  native plan the compilers' own metadata rules (WS-4's `.eventSiteCheck()`
  for every event and adapter source with the reducer predicates over its
  rows, `.predicateTableCatalogue()` for criteria person predicates); a
  lowered v1 plan first the v1 executors' own checks, with their messages
  (tables, results cohort, filter trees, custom filters including the raw
  person/death tables of a person_level output, the baseline person-column
  contract), then the IR's typed checks over the column names the v1
  executors accept. A v1 event over a present table without an event role
  (`event_level` over `person`) is a legacy row source; it is never a native
  event table. A v1 plan-declaration error (an output that needs a cohort)
  keeps the executor's untyped error.
- (A-11) Every default is expanded by the frozen normaliser shared with
  dsOMOPClient, `.planNormalizeV2()` (`R/plan_normalize_v2.R`), for language
  2.0 plans and lowered v1 plans alike; grain and collapse rules use its
  indexedness rule (`intersect`/`setdiff` follow their first member). WS-2's
  interim `R/ir_defaults.R` is removed from both packages.
- (A-10) v1 plans lower to the compatibility IR of design section 5 with
  every result-affecting v1 field in the IR document: the legacy variants of
  section 4.5 plus `legacy_columns`, `legacy_temporal`,
  `legacy_automatic_features`, `legacy_single_outcome`, `legacy_states` /
  `legacy_state_step`, `legacy_episodes` / `legacy_index_anchor`,
  `legacy_containing_period`, `legacy_names` and `legacy_concept_scope`, and
  amendments A-15 (the inline-spec occurrence: the effective `{type, count}`
  object, the type matched exactly as the executor does) and A-16
  (`legacy_raw_filter`; `legacy_anchor_offset` only for a non-zero offset
  under `event_select = "nearest"`, other offsets advisory; the inline-spec
  `cohort_definition_id >= 0` as supplied, passed to `.cohortCreate()` as
  `cohort_id` by the legacy-spec adapter; `scope.legacy_cohort_sources` with
  the materialised `combine` and `scope.cohort` as its first source; the
  sparse automatic builder). One shared `.planSemanticDigest()` binds them; advisory
  v1 fields go to a server-private annex that explain lists
  (`legacy_annex`) and no digest covers. The `legacy_residue` attribute and
  `.irSemanticDigest()` are retired. v1 plans keep running on the v1
  executors in this release stage; a lowered v1 plan names no publication
  roots (they are the outputs the execution maps, as in 2.10.2).
- Validation runs on the plan as it arrived, before the server injects the
  resolved scope frames and before the execution context and the DP seal
  exist. The context of a language 2.0 plan carries its validated,
  normalised IR (`ctx$ir`, read by the native publication and DP
  contracts); a v1 plan's carries the plan its executors and the legacy
  publication contracts read, with the lowered IR in `ctx$validation`; the
  resolved frames are `ctx$scopes`.
- `R/roles.R`: `.roleOf(handle, table)`, the role registry of design 6.2;
  role records keep each column's value type for typed site checks.
- New aggregate method `omopPlanExplainDS(omop_symbol, plan)`: the normalised
  plan, role records, column classes (exact-date-preserving reducers are
  class 2), effective defaults, publication roots, the advisory annex and
  the dialect preflight; no counts, no SQL.
- (A-E7) `omopGetCapabilitiesDS()` reports `recipe_language`: version 2,
  semantic version 2.0.0, the schema id, the operations and reducers this
  server executes and the dialect block. New option
  `dsomop.recipe_operations_disabled` (none by default) withdraws operations.
- `tests/testthat/fixtures/ir-v1-corpus.json`: every distinct v1 plan of the
  pinned 2.10.2 suites, bound to its frozen corpus class; `ir-v1-golden.json`
  pins their normalised IR documents as regression snapshots.
  `ir-v1-results.json` pins the 2.10.2 results of the safe-class plans the
  baseline suite executed on the default fixture; `test-ir-results.R`
  re-executes them with this code and requires identical results, except
  the two reviewed F-26 age-band corrections, checked against the design's
  rule (`tools/ws2/capture_results.R` and `compare_results.py` capture and
  compare every suite call against the baseline).

## Contracts shipped by WS-0

- `inst/schema/plan-v2.json`, `ir-v2.json`, `mutate-v1.json`, the canonical
  examples, the probe corpus, the semantic-digest vectors and the normalised
  fixtures (`normalized/`, `normalized-vectors.json`); `R/plan_normalize_v2.R`;
  `inst/contracts/reducers.csv`, `column_classes.csv`;
  `inst/ohdsi/operator_classes.csv`, `episode_event_fields.csv`,
  `cost_domains.csv`; `inst/boundaries.csv` (audited WS-1 publication boundaries;
  WS-2/WS-9 entries remain pending);
  `inst/plans/acceptance/` (the fifteen acceptance recipes with validated
  plans and hand-computed expected results); `R/types.R`,
  `R/json_schema.R`, `R/semantic_digest.R`; the adversarial extension of
  the reach fixture and the synthetic CDM generator under `tests/fixtures/`
  (PostgreSQL loads with typed dates through `conn`/`schema`).
- `inst/plans/acceptance/score-reference.json`: paired v1/v2 Charlson, CHADS2
  and CHA2DS2-VASc expectations with fixed and index-resolved reference dates
  (CR-2 / A-2); `tests/testthat/fixtures/v1-corpus-classes.csv`: one explicit
  reviewed disposition per test block of both 2.10.2 suites (helper-built
  plans included, mixed blocks split; `tools/ws0/corpus_decisions.csv`), keyed
  by `package:file:<test description>` against the pinned 2.10.2 commits of
  `tools/ws0/corpus_baseline.json` (CR-7): live test edits never move or break
  the corpus; a reclassification is a decisions-file change.
- Semantic digest: the ECMAScript shortest round-trip decimal is found for
  every finite double (powers of two have asymmetric rounding intervals;
  `2^-24` serialises as `5.960464477539063e-8`), checked against the Python
  reference on 11,045 doubles including every power of two and its neighbours.
- `R/types.R`: roster-mode publication candidates must agree with the
  fragment's declared person lineage and key roles; the roster fragment name
  (`person_lineage$via_roster`) and the roster namespace (`lineage$roster_id`)
  stay distinct.

## SQL dialect layer

- (A-E8) New SQL never relies on regex translation of `DATEADD`/`DATEDIFF`,
  which handles only literal offsets and identifier operands and passes
  column or arithmetic forms through untranslated (SQLite then fails on
  `DATEADD(day, t.days_supply - 1, t.start)`). The typed helpers of
  `R/sql_expr.R` (`.sqlDateAddDays`, `.sqlDateDiffDays`, `.sqlYear`,
  `.sqlMonthKey`, `.sqlCalendarFloor`, `.sqlCalendarAdvance`, `.sqlMakeDate`,
  `.sqlDateLiteral`, `.sqlIntegerLiteral`, `.sqlLeast`, `.sqlGreatest`,
  `.sqlNullsLast`, `.sqlRowNumber`, `.sqlLosslessKey`, `.sqlSha256Hex`, with
  `.sqlWindow`, `.sqlMonth`, `.sqlDateText` and `.sqlLosslessKeyText`)
  accept expressions as operands and have one semantics in every family:
  NULL-ignoring `LEAST`/`GREATEST`, NULL (never an error or a normalised
  date) for invalid date components, explicit window frames only. The
  built-in translator leaves their output unchanged, and
  `.sqlAssertTypedSql()` refuses the OHDSI spellings in new statements. The
  v1 SQL paths, their helpers and the translator are unchanged.
- Required support (design section 9): executable on PostgreSQL, SQLite,
  DuckDB, MySQL >= 8.0 and MariaDB >= 10.2; SQL Server/Synapse/PDW, Oracle,
  Redshift, BigQuery, Snowflake and Spark/Databricks are implemented and
  verified by rendered-SQL snapshots (SQL contract only). The dialect profile
  (`.sqlDialectProfile()`) gives the capability `dialect` block its family,
  temporary materialisation, SHA-256 and window-frame availability and the
  runtime version, and records the driver for the conformance report.
- MariaDB servers whose version string carries a distribution suffix, such as
  Ubuntu 22.04's `10.6.23-MariaDB-0ubuntu0.22.04.1`, are no longer refused
  as below MariaDB 10.2: the version tagged `-MariaDB` is read instead of the
  last dotted number (`0.22.04`). This affected every MariaDB connection and
  the longitudinal SQL floor.

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
