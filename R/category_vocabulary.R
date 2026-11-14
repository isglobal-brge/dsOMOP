# Vocabulary-only hierarchy adapter A30-1. No clinical table participates.
.CATEGORY_ADAPTER <- "A30-1:ATC-class;ICD10-reverse-Maps-to-Is-a;CA-exact-distance"
.categoryRollupMap <- function(handle,spec) {
  .categoryRollupCheck(spec)
  if(spec$kind=="none")return(NULL)
  bp<-.buildBlueprint(handle)
  tables<-bp$tables$table_name[bp$tables$present_in_db]
  required<-c("concept",if(spec$kind=="icd10")"concept_relationship" else "concept_ancestor")
  if(!all(required %in% tables)) .categoryReject("Required public hierarchy table is missing.","category_domain_incompatible")
  read<-function(table,fields,where) {
    if(!all(fields %in% bp$columns[[table]]$column_name)) .categoryReject("Malformed public hierarchy schema.","category_domain_incompatible")
    target<-.qualifyTable(handle,table,.resolveTableSchema(handle,table,"Vocabulary"))
    .executeQuery(handle,paste0("SELECT ",paste(fields,collapse=", ")," FROM ",target," WHERE ",where))
  }
  domain<-if(is.list(spec$domain)&&!is.null(names(spec$domain)))as.character(.resolveConceptSetNode(handle,spec$domain)$ids) else sub("^concept:","",.categoryKeys(spec$domain,FALSE,TRUE))
  if(!length(domain)) .categoryReject("Public rollup domain expands to no concepts.","category_domain_incompatible")
  concept<-function(ids) {
    rows<-unique(read("concept",c("concept_id","vocabulary_id","concept_class_id","invalid_reason"),paste0("concept_id IN (",.sqlIdList(ids),")")))
    valid<-rows[is.na(rows$invalid_reason),,drop=FALSE]
    if(anyNA(valid[c("concept_id","vocabulary_id","concept_class_id")]) || anyDuplicated(valid$concept_id) || any(!nzchar(valid$vocabulary_id)|!nzchar(valid$concept_class_id))) .categoryReject("Malformed required public concept/class adapter records.","category_domain_incompatible")
    rows
  }
  inputs<-concept(domain);valid_input<-as.character(inputs$concept_id[is.na(inputs$invalid_reason)])
  relation<-setNames(rep(list(character()),length(domain)),domain)
  if(spec$kind!="icd10") {
    # Expand the required public ancestor subgraph, including ancestors of
    # declared inputs. A cycle need not contain two declared input nodes.
    fields<-c("ancestor_concept_id","descendant_concept_id","min_levels_of_separation","max_levels_of_separation")
    frontier<-domain;seen<-character();parts<-list()
    while(length(frontier)) {
      rows<-read("concept_ancestor",fields,paste0("descendant_concept_id IN (",.sqlIdList(frontier),")"))
      parts[[length(parts)+1L]]<-rows;seen<-union(seen,frontier)
      frontier<-setdiff(as.character(rows$ancestor_concept_id),seen)
    }
    ca<-unique(do.call(rbind,parts))
    if(anyNA(ca$min_levels_of_separation)||anyNA(ca$max_levels_of_separation)||
       any(ca$min_levels_of_separation<0|ca$max_levels_of_separation<ca$min_levels_of_separation)||
       any(ca$min_levels_of_separation==0 & as.character(ca$ancestor_concept_id)!=as.character(ca$descendant_concept_id))) .categoryReject("Malformed public ancestor distances.","category_domain_incompatible")
    positive<-ca[ca$max_levels_of_separation>0,,drop=FALSE]
    graph<-split(as.character(positive$ancestor_concept_id),as.character(positive$descendant_concept_id))
    finished<-character()
    visit<-function(id,path=character()) {
      if(id %in% path) .categoryReject("Cyclic public hierarchy.","category_domain_incompatible")
      if(id %in% finished)return(NULL)
      for(parent in graph[[id]])visit(parent,c(path,id))
      finished<<-union(finished,id);NULL
    }
    for(id in names(graph))visit(id)
    targets<-concept(unique(c(domain,as.character(ca$ancestor_concept_id))))
    eligible<-is.na(targets$invalid_reason)
    if(spec$kind=="atc")eligible<-eligible & targets$vocabulary_id=="ATC" & targets$concept_class_id==paste("ATC",c("1st","2nd","3rd","4th","5th")[spec$level])
    else eligible<-eligible & targets$vocabulary_id==(spec$vocabulary %||% "SNOMED")
    allowed<-as.character(targets$concept_id[eligible])
    if(!is.null(spec$ancestors))allowed<-intersect(allowed,sub("^concept:","",unlist(spec$ancestors)))
    for(id in intersect(domain,valid_input)) {
      rows<-ca[as.character(ca$descendant_concept_id)==id,,drop=FALSE]
      if(!is.null(spec$separation))rows<-rows[rows[[paste0(spec$distance,"_levels_of_separation")]]==spec$separation,,drop=FALSE]
      candidates<-as.character(rows$ancestor_concept_id)
      if(is.null(spec$separation)||spec$separation==0)candidates<-union(candidates,id)
      relation[[id]]<-intersect(candidates,allowed)
    }
  } else {
    fields<-c("concept_id_1","concept_id_2","relationship_id","invalid_reason")
    maps<-read("concept_relationship",fields,paste0("relationship_id = 'Maps to' AND concept_id_2 IN (",.sqlIdList(domain),") AND invalid_reason IS NULL"))
    starts<-unique(c(domain,as.character(maps$concept_id_1)))
    nodes<-concept(starts);nodes<-nodes[is.na(nodes$invalid_reason)&nodes$vocabulary_id=="ICD10",,drop=FALSE]
    frontier<-as.character(nodes$concept_id);edges<-list();seen<-character()
    while(length(frontier)) {
      seen<-union(seen,frontier)
      rel<-read("concept_relationship",fields,paste0("invalid_reason IS NULL AND ((relationship_id = 'Is a' AND concept_id_1 IN (",.sqlIdList(frontier),")) OR (relationship_id = 'Subsumes' AND concept_id_2 IN (",.sqlIdList(frontier),")))"))
      child<-ifelse(rel$relationship_id=="Is a",as.character(rel$concept_id_1),as.character(rel$concept_id_2))
      parent<-ifelse(rel$relationship_id=="Is a",as.character(rel$concept_id_2),as.character(rel$concept_id_1))
      new<-setdiff(unique(parent),as.character(nodes$concept_id))
      if(length(new))nodes<-rbind(nodes,concept(new))
      valid<-as.character(nodes$concept_id[is.na(nodes$invalid_reason)&nodes$vocabulary_id=="ICD10"])
      for(i in seq_along(child))if(parent[i] %in% valid)edges[[child[i]]]<-union(edges[[child[i]]],parent[i])
      frontier<-setdiff(intersect(parent,valid),seen)
    }
    class<-if(spec$level=="chapter")"ICD10 Chapter" else "ICD10 SubChapter"
    allowed<-as.character(nodes$concept_id[is.na(nodes$invalid_reason)&nodes$vocabulary_id=="ICD10"&nodes$concept_class_id==class])
    ascend<-function(id,path=character()) {
      if(id %in% path) .categoryReject("Cyclic public ICD10 hierarchy.","category_domain_incompatible")
      union(if(id %in% allowed)id else character(),unlist(lapply(edges[[id]],ascend,path=c(path,id)),use.names=FALSE))
    }
    for(id in intersect(domain,valid_input)) {
      start<-if(id %in% as.character(nodes$concept_id))id else as.character(maps$concept_id_1[as.character(maps$concept_id_2)==id])
      relation[[id]]<-unique(unlist(lapply(start,ascend),use.names=FALSE))
    }
  }
  for(id in domain) {
    if(id=="0")relation[[id]]<-"0"
    if(!length(relation[[id]]) && spec$unmatched=="keep")relation[[id]]<-id
  }
  if(spec$multiple=="error" && any(lengths(relation)>1L)) .categoryReject("Public hierarchy domain is ambiguous; declare complete priority or membership indicators.","category_rollup_ambiguous")
  if(spec$multiple=="priority") {
    priority<-sub("^concept:","",unlist(spec$priority,use.names=FALSE))
    if(!all(unlist(relation,use.names=FALSE) %in% priority)) .categoryReject("Priority must cover every possible public target.")
    relation<-lapply(relation,function(x)if(length(x))priority[which(priority %in% x)[1L]] else character())
  }
  relation<-lapply(relation,function(x)if(length(x))paste0("concept:",x[.conceptIdOrder(x)]) else character())
  names(relation)<-paste0("concept:",names(relation))
  relation<-relation[order(names(relation),method="radix")]
  list(adapter=.CATEGORY_ADAPTER,mapping=relation,
    digest=paste0(as.character(openssl::sha256(charToRaw(.jcsCanonical(relation))))))
}
.categoryPreflight <- function(ir,handle) {
  walk<-function(x, defaults=NULL) {
    if(!is.list(x))return(invisible(NULL))
    s<-.categoryEffective(x$category_spec,defaults)
    s<-.categoryApplicableVocabulary(s,x$category_spec,.categoryDeclaredConcept(x))
    if(identical(s$label_style,"source_code")) .categoryReject("Source-code presentation needs an independently admitted source-concept policy; use standard code.","category_source_forbidden")
    if(!is.null(s$rollup)&&s$rollup$kind!="none") .categoryRollupMap(handle,s$rollup)
    if(!is.null(s$missing)&&s$missing$unknown!="keep") {
      labels<-.conceptMapOverlay(.conceptMap(ir$options$labels,"recipe",wire=TRUE),.conceptMap(x$labels,"variable",wire=TRUE))
      map<-unlist(s$recode$map,use.names=TRUE)
      if("0" %in% names(labels)&&!"concept:0" %in% names(map)&&
         any(names(labels)!="0" & labels==labels[["0"]] & !paste0("concept:",names(labels)) %in% names(map)))
        .categoryReject("Mixed zero/nonzero user-label merge cannot become Unknown; use separate labels or an explicit recode.")
    }
    for(y in x)if(is.list(y))walk(y)
  }
  # Recipe defaults apply to final variables, never arbitrary nested operands.
  for (v in ir$variables) walk(v,ir$options$category_spec)
  for (o in ir$outputs) {
    for (v in o$category_columns) walk(v,ir$options$category_spec)
    for (v in o$number_columns) walk(v,ir$options$category_spec)
  }
  invisible(TRUE)
}
