# Module: recipe-language capability block and plan explain (design-2.10.3
# sections 4.4, 6.15 and defect A-E7).

# The frozen operation and reducer vocabularies of section 4.4.
.RECIPE_OPERATIONS <- c(
  output.codebook_metadata = ".categoryReleaseSchema",
  layout.repeated = ".repeatedRecord",
  category.reduce = ".categoryReduceRecord",
  category.rollup = ".categoryRollupMap",
  category.indicators = ".categoryIndicatorRecord",
  category.presentation = c(".categoryPresent", ".categoryResolveRecords"),
  number.presentation = c(".numberPresent", ".numberOutputColumns"),
  number.rows = ".numberEventRecord", number.count = ".numberEventRecord",
  number.change = c(".numberEventRecord", ".numberChangeValues"),
  number.expression = c(".numberExpression", ".numberDerivedRecord", ".numberAstCheck"),
  number.columns = ".numberOutputColumns",
