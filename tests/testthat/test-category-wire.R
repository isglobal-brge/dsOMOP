test_that("C02 C57 C58 independent category normal forms and wire probes agree", {
  vectors<-jsonlite::fromJSON(system.file("schema/category-vectors.json",package="dsOMOP"),simplifyVector=FALSE)
  for(v in vectors) {
    n<-.planNormalizeV2(v$instance)
    expect_identical(.jcsCanonical(n),.jcsCanonical(v$normalized),info=v$label)
    expect_identical(.planNormalizedDigest(v$instance)$sha256,v$digest_sha256,info=v$label)
    expect_identical(.jcsCanonical(.planNormalizeV2(n)),.jcsCanonical(n),info=v$label)
  }
  probes<-jsonlite::fromJSON(system.file("schema/probes/category-probes.json",package="dsOMOP"),simplifyVector=FALSE)
  schema<-.jsonSchemaPackaged("plan-v2")
  for(p in probes)expect_identical(.jsonSchemaValidate(schema,p$instance)$valid,p$expect=="valid",info=p$id)
})
