
- Corrected (date/category schema review): prospective codebooks include native
  date endpoints before final selection, with their effective presentation types
  and order, including nested survival and joined outputs. Rebuild affected
  codebooks; recipe declarations and published data do not change.

- Corrected (categorical schema review): prospective codebooks enumerate closed
  panel source columns and apply the same required numeric-role rules as
  execution for selected panel and survival copies, including nested components.
  Rebuild affected codebooks; no recipe declaration changes are needed.

- Corrected (categorical composition review): final column selection and order
  apply to output-local copies and expanded variables in both execution and
  prospective codebooks. Structural columns remain present.
- Corrected: output-local category copies accept categorical selector reductions
  (including mode and vector nth), retaining consumed identity, missingness and
  admission contracts while allowing independent presentation. Re-run affected
  native recipes; no declaration changes are needed.

- Corrected (categorical review): prospective codebooks expand native nth-value
  columns and honor effective numeric, date and category presentation defaults,
  overrides and untranslated concepts. Native outputs can select expanded
  keys such as `answer_3` in the requested order.
- Corrected: recipe hierarchy defaults are inactive for semantic flags and
  numeric/date categories, including `multiple="all"`. Explicit incompatible
  hierarchy requests still refuse; use concept inputs and category indicators
  when multiple hierarchy targets are intended.

- Native language-2 releases now carry codebook metadata for every real table,
  including plain numeric and untranslated plans. They require
  `output.codebook_metadata`; upgrade participating servers to 2.10.5.
  Existing v1 plans retain their compatibility path.

- Newly permitted (A-30 / CR-31): native category specifications add typed
  identity order, nominal recodes, hierarchy maps, language and code labels,
  explicit Missing/Unknown levels and semantic-flag representations. Category
  event projections, indicators and reductions preserve explicit identity.
- Newly permitted: declared repeated time-point/visit columns and output-local
  category copies; client codebooks use public plans and admitted release
  metadata. These constructs require native language 2 and matching 2.10.5
  capabilities. Use wide/long typed columns for factor analyses, with a numeric
  sibling for quantitative arithmetic.
- Newly refused: ambiguous hierarchy mappings require a declared priority or
  membership indicators. A mixed zero/nonzero user-label merge cannot become
  an Unknown sentinel: retain unknown=keep, use separate labels, or explicitly
  recode its identities before sentinel handling.

- Corrected: native matrix and person-period outputs no longer implicitly
  consume unrelated recipe variables. Select analytic columns explicitly;
  use a typed wide companion for factor/logical matrix covariates.
- Corrected: explicit outer vocabulary styling and hierarchy stages use the
  effective concept retained by category reduction, preserving original
  admission evidence and upstream equivalences.
- Corrected: undefined anchors remain NA through repeated subsetting and
  Missing presentation. Staged empty category contracts preserve nullable
  references, declared levels and contrast specifications.
- Newly permitted: prospective codebooks describe structural schema and
  survival/reference component families locally; published codebooks retain
  reconciled names, category provenance and mapping contracts.
- Categorical binary presentation of abnormal-event reducers means at least
  one abnormal event. Keep an unwrapped numeric sibling for event counts.

- Corrected (A-29 review): independently normalized numeric siblings retain
  their original event-row binding in long and intervals outputs. BIGINT
  longitudinal arithmetic now checks exact conversion before calculation;
  retain raw/count identity when a binary64 conversion is refused.
- Corrected: expression types follow each selected canonical/presented input
  prefix, preserving fractional values and exact integers. Slope reducers and
  generated relation-day columns retain class-2 provenance through copies,
  categories and transport. Supply roles use days; unrelated numeric roles
  do not inherit measurement units. Re-run affected numeric recipes.
- Newly refused: scalar formulas in long output, including nested formulas,
  require explicit `broadcast = TRUE`, just like direct scalar reducers.


- Newly permitted (A-29 / CR-30): native numeric presentation with declared
  source and fixed units, ordered transforms, exact binary64 decimal rounding,
  public bins and clinical categories, flags and value presence/missingness.
  Numeric rows, counts, longitudinal changes, closed multi-input expressions
  and output-local canonical column copies run before protection and release.
  Numeric capabilities are negotiated before assignment; new declarations
  require native language 2 and dsOMOP 2.10.5 at every selected site.
- Newly permitted: numeric factor reconciliation retains the complete ordered
  public domain, including unused levels. Memory and Parquet carry numeric
  units, expression identity, domains and privacy lineage. Incompatible retained
  contracts fail and roll back; a numeric sibling is required for arithmetic.
- Corrected: A-29 CV results are dimensionless SD/mean. The compatibility route
  without numeric declarations retains its percentage CV. To display a native
  CV as a percent, explicitly apply an affine factor of 100.
- Corrected: exact BIGINT selectors and integer count promotion avoid silent
  binary64/32-bit loss. Identity and round(none) retain integer64; operations
  requiring an inexact double conversion refuse with a bounded remedy.
- Newly refused: explicit numeric presentation of identifiers, concept values,
  Dates/factors, model weights or structural codes; factor-valued matrix cells;
  implicit event broadcasts; presentations that change implicit matrix zeros;
  and nonnumeric/foreign expression bindings.
  Use registered numeric measures, explicit numeric date representations,
  declared broadcasts/reducers, and typed wide/long companions as applicable.
