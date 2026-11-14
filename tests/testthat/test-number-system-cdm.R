.numeric_definitions()
test_that("N02 N03 N36 N40 N65 Synthea numeric pipelines publish once in memory and Parquet", {
  hs<-.numeric_backends();.ws7_keys(hs)
  for(h in hs) {
    d<-numeric_measurements(h$conn,h$cdm_schema);oracle<-numeric_oracle(d)
    p<-numeric_plan();p$variables<-p$variables[c("mean","last","first")]
    p$variables$mean$number_spec<-list(unit=list(kind="event_unit",spec=list(target=8840L,convert=list(`8713`=list(factor=1000,offset=0)))),rounding=list(method="decimals",digits=1))
    p$variables$last$number_spec<-list(representation="category",thresholds=list(12,15))
    p$variables$first$number_spec<-list(representation="bins",breaks=list("-Inf",12,15,"Inf"))
    copy<-list(derived=list(kind="number_column",column="mean"))
    raw_copy<-copy;raw_copy$number_spec<-list(pipeline=list(list(kind="transform",method="identity")))
    p$outputs$wide$number_columns<-list(copy=copy,raw_copy=raw_copy)
    p$outputs$wide$columns<-as.list(c("mean","last","first","copy","raw_copy"))
    previous<-NULL
    for(mode in c("memory","staged")) {
      got<-.numeric_run(h,p,mode);f<-got$values$wide$wide
      at<-match(as.numeric(got$raw$wide$wide$person_id),oracle$pid)
      expect_equal(f$mean,vapply(oracle$mean[at]*1000,function(x).numberRoundOne(x,"decimals",1),0),tolerance=1e-8)
      expect_equal(f$copy,f$mean);expect_equal(f$raw_copy,oracle$mean[at]*1000,tolerance=1e-8)
      expect_true(is.ordered(f$last));expect_identical(levels(f$last),c("low","normal","high"))
      expect_true(is.ordered(f$first));expect_identical(levels(f$first),c("(-Inf,12)","[12,15)","[15,Inf)"))
      if(!is.null(previous)) for(n in c("mean","last","first"))expect_equal(f[[n]],previous[[n]])
      previous<-f
    }
  }
})
test_that("N39 N40 N65 Synthea recipe defaults cover every existing analytic format", {
  hs<-.numeric_backends();.ws7_keys(hs)
  for(h in hs) {
    p<-numeric_formats();p$options<-list(number_spec=list(rounding=list(method="decimals",digits=1)))
    previous<-NULL
    for(mode in c("memory","staged")) {
      got<-.numeric_run(h,p,mode)
      for(root in names(got$values))for(component in names(got$values[[root]])) {
        f<-got$values[[root]][[component]]
        expect_gt(nrow(f),0L)
        if(!is.null(previous)) for(n in names(f))expect_equal(f[[n]],previous[[root]][[component]][[n]])
      }
      previous<-got$values
    }
  }
})

test_that("N02 N39 N65 recipe source maps apply to raw output streams before presentation", {
  hs<-.numeric_backends();.ws7_keys(hs)
  for(h in hs) {
    p<-numeric_formats()
    if(!"long" %in% names(p$outputs))stop("numeric fixture must retain long output")
    baseline_all<-.numeric_run(h,p,"memory")$values
    baseline<-baseline_all$long$long
    p$options<-list(number_spec=list(unit=list(kind="event_unit",spec=list(target=8840L,convert=list(`8713`=list(factor=1000,offset=0))))))
    for(mode in c("memory","staged")) {
      all<-.numeric_run(h,p,mode)$values;got<-all$long$long
      expect_equal(all$temporal$temporal$covariate_value,baseline_all$temporal$temporal$covariate_value*1000)
      for(n in intersect(c("lab_mean","lab_last"),names(all$panel$panel)))expect_equal(all$panel$panel[[n]],baseline_all$panel$panel[[n]]*1000)
      expect_equal(all$survival_covariates$survival_covariates$mean,baseline_all$survival_covariates$survival_covariates$mean*1000)
      expect_equal(got$value,baseline$value*1000)
      expect_identical(as.character(got$unit_concept_id),rep("milligram per deciliter",nrow(got)))
      expect_equal(names(got),names(baseline))
    }
  }
})
