test_that("C29 C32 C46 C48 category plans preserve types on supported SQL backends", {
  bs<-.ws7_scenario_backends();withr::defer(.ws4_cleanup(bs))
  for(dbms in c("postgresql","mariadb","mysql"))if(!is.null(.dsomopVendorTestConfig(dbms)))expect_true(dbms %in% names(bs),info=dbms)
  api<-asNamespace("dsOMOPClient")
  make<-function() {
    lab<-omop_event("measurement",.WS7$LAB,name="lab")
    binary<-omop_variable(event=lab,reducer=omop_reduce("binary"))
    value<-omop_number_bins(omop_number_rows(lab),breaks=c(-Inf,5,10,Inf))
    omop_recipe(events=list(lab),populations=omop_population("p",index=omop_index(lab)),variables=list(
      logical=omop_category(binary,omop_category_spec(binary=omop_category_binary("logical"))),
      factor=omop_category(binary,omop_category_spec(binary=omop_category_binary("factor"))),
      repeated=omop_repeated(lab,list(value=value),timepoints=omop_timepoints(list(baseline=omop_timepoint(),m6=omop_timepoint(6,"months"))))),
      outputs=omop_output("wide",format="wide",rows="p",grain="episode"),category_spec=omop_category_spec(ordered=FALSE))
  }
  environment(make)<-list2env(list(.WS7=.WS7),parent=api)
  p<-dsOMOPClient::recipe_to_plan(make(),version=2)
  .ws7_keys(lapply(bs,function(b)b$handle))
  for(backend in names(bs)) {
    prior<-NULL
    for(mode in c("memory","staged")) {
      check<-function(chunk) {
        for(n in names(prior))if(is.factor(prior[[n]])) {
          expect_identical(levels(chunk[[n]]),levels(prior[[n]]));expect_identical(is.ordered(chunk[[n]]),is.ordered(prior[[n]]))
          expect_equal(contrasts(chunk[[n]]),contrasts(prior[[n]]))
        }
      }
      f<-.numeric_run(bs[[backend]]$handle,p,mode,chunk_check=check)$values$wide$wide
      expect_type(f$logical,"logical");expect_s3_class(f$factor,"factor")
      expect_s3_class(f$repeated_value_baseline,"factor");expect_identical(levels(f$repeated_value_m6),levels(f$repeated_value_baseline))
      if(!is.null(prior))for(n in names(f)) {
        if(is.factor(f[[n]]))expect_identical(as.character(f[[n]]),as.character(prior[[n]]))
        else expect_equal(f[[n]],prior[[n]],info=paste(backend,n))
      }
      prior<-f
    }
  }
})
