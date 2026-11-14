# Category presentation is applied before publication and its protection seal.
# It preserves original concept identities for the controlled metadata gate.
.categoryFlag <- function(node, column) {
  flag <- isTRUE(attr(column,"semantic_flag")) || (node$reducer$fn %||% "") %in% c("binary","any","all","abnormal_high","abnormal_low") ||
    identical(node$derived$kind,"died")
  pipeline <- attr(column,"number_contract")$pipeline
  if(is.null(pipeline)&&!is.null(node$number_spec))pipeline<-.numberEffective(node$number_spec)$pipeline
  for(p in pipeline %||% list()) {
    if(p$kind %in% c("flag","present","missing")) flag <- TRUE
    else if(!(p$kind=="transform" && p$method=="identity" ||
      p$kind=="round" && p$method=="none" ||
      p$kind=="fixed_unit" && p$factor==1 && (p$offset %||% 0)==0)) flag <- FALSE
  }
  flag
}
.categoryConceptProducer <- function(contract) {
  if(length(contract$spec$recode$map) && any(!grepl("^concept:",unlist(contract$spec$recode$levels,use.names=FALSE))))return(FALSE)
  producer<-contract$producer
  if(identical(producer$kind,"concept"))return(TRUE)
  if((producer$kind %||% "") %in% c("category","category_reduce"))return(.categoryConceptProducer(producer$input))
  FALSE
}
.categoryPresent <- function(ctx,frame,columns) {
  contracts <- attr(frame,"omop_concepts") %||% list()
  for(n in intersect(names(frame),names(columns))) {
    col <- columns[[n]];owner <- attr(col,"concept_owner")
    node <- if(length(owner))ctx$ir$variables[[owner]] else list()
    explicit <- .categoryEffective(attr(col,"category_spec"),node$category_spec)
    if(!length(explicit)&&is.null(attr(col,"category_spec"))&&is.null(node$category_spec))explicit<-NULL
    s <- .categoryEffective(explicit,ctx$ir$options$category_spec)
    if(!length(s))next
    c <- contracts[[n]]; flag <- .categoryFlag(node,col)
    if(is.null(c) && !flag) {
      if(!is.null(explicit)) .categoryReject("Category presentation requires a declared concept, category or semantic flag.","category_type_unsupported")
      next
    }
    if(!is.null(s$binary) && !flag) {
      if(!is.null(explicit$binary)) .categoryReject("binary requires semantic flag lineage; use an explicit threshold flag.","category_type_unsupported")
      s$binary <- NULL
    }
    concept <- !flag && (is.null(c$kind) || identical(c$kind,"category") && .categoryConceptProducer(c$category))
    s <- .categoryApplicableVocabulary(s, explicit, concept)
    if(!is.null(s$rollup) && s$rollup$kind!="none" && identical(s$rollup$multiple,"all")) .categoryReject("Multiple hierarchy targets require category_indicators.","category_type_unsupported")
    available <- attr(col,"category_available") %||% attr(col,"number_origin")$available %||% TRUE
    defined<-attr(col,"category_defined") %||% attr(frame[[n]],"category_defined")
    if(!is.null(defined))available<-available & defined
    if(length(available)==1L)available<-rep(available,nrow(frame))
    if(length(available)!=nrow(frame)) .categoryReject("Incomplete private availability lineage.")
    if(flag) {
      flag_value<-as.integer(as.logical(frame[[n]]))
      as <- s$binary$as %||% "integer"
      if(as!="factor") {
        if(identical(s$missing$na,"level") && !is.null(explicit$missing)) .categoryReject("Explicit Missing levels require binary factor representation.")
        irrelevant <- intersect(names(explicit),c("levels","order","ordered","reference","recode","rollup","label_style","language"))
        if(length(irrelevant)) .categoryReject("Factor operations require binary(as=factor).")
        frame[[n]] <- if(as=="logical")as.logical(flag_value) else flag_value
        frame[[n]][!available] <- NA
        next
      }
      ids <- ifelse(is.na(flag_value),NA_character_,paste0("flag:",flag_value))
      lev <- c("flag:0","flag:1");lab<-unlist(s$binary$labels,use.names=FALSE)
      records <- setNames(lapply(seq_along(lev),function(i)list(id=lev[i],label=lab[i],origin="producer")),lev)
      declared <- lev;producer<-list(kind="flag");producer_order<-lev;producer_ordered<-FALSE
    } else {
      old_ids <- names(c$records)
      prefix <- if(identical(c$kind,"number")) {
        pipeline <- c$number$pipeline
        if(any(vapply(pipeline,function(p)p$kind=="bins",logical(1))))"bin:" else "category:"
      } else if(identical(c$kind,"date"))"period:" else "concept:"
      lev <- if(!length(old_ids))character() else if(identical(c$kind,"category"))old_ids else paste0(prefix,if(identical(c$kind,"number"))seq_along(old_ids) else old_ids)
      ids <- lev[match(c$ids,old_ids)];declared<-lev[match(c$declared,old_ids)]
      records <- setNames(lapply(seq_along(lev),function(i) {
        r<-c$records[[i]];r$id<-lev[i]
        if((c$kind %||% "") %in% c("date","number"))r$origin<-"producer"
        r
      }),lev)
      producer<-if(identical(c$kind,"category"))list(kind="category",input=c$category) else list(kind=c$kind %||% "concept",calendar=c$calendar,number=c$number)
      producer<-producer[!vapply(producer,is.null,logical(1))]
      producer_order<-if(identical(c$kind,"category"))unlist(c$category$producer_order,use.names=FALSE) else if(is.null(c$kind) || identical(c$kind,"date"))character() else lev;producer_ordered<-is.ordered(frame[[n]])
      if(is.null(c$kind)) {
        if(isFALSE(ctx$ir$options$translate_concepts)) .categoryReject("Enable translate_concepts for concept presentation.")
        public_inputs<-c(if(!length(s$recode$map))unlist(s$levels,use.names=FALSE),names(s$recode$map))
        extra<-setdiff(public_inputs,lev)
        if(length(extra)) {
          .categoryKeys(extra,concept=TRUE)
          rr<-.conceptVocabulary(ctx$handle,sub("^concept:","",extra))
          for(k in seq_along(extra)) {r<-rr[[k]];r$id<-extra[k];records[[extra[k]]]<-r}
          declared<-union(declared,extra)
        }
        records<-.categoryVocabularyLabels(ctx$handle,records,s)
      }
    }
    if(!flag && identical(c$kind,"category") && (!is.null(s$label_style)||!is.null(s$language))) {
      # A consumed display is not a fresh vocabulary name. Rebind automatic
      # names to the effective concept before applying an explicit outer style.
      for(key in names(records)) {
        r<-records[[key]];identity<-r$effective_id %||% key
        if(!grepl("^concept:",identity)||r$origin %in% c("variable","recipe","recode","producer","sentinel"))next
        fresh<-.conceptVocabulary(ctx$handle,sub("^concept:","",identity))[[1L]]
        fresh$id<-key;fresh$effective_id<-identity;records[[key]]<-fresh
      }
      records<-.categoryVocabularyLabels(ctx$handle,records,s)
    }
    rollup <- NULL
    if(!is.null(s$rollup) && s$rollup$kind!="none") {
      if(!.categoryConceptProducer(list(producer=producer))) .categoryReject("Hierarchy rollup requires concept identities.")
      rollup <- .categoryRollupMap(ctx$handle,s$rollup)
      targets <- unique(unlist(rollup$mapping,use.names=FALSE))
      target_records <- if(length(targets)).conceptVocabulary(ctx$handle,sub("^concept:","",targets)) else list()
      names(target_records) <- targets
      map <- .conceptMapOverlay(.conceptMap(ctx$ir$options$labels,"recipe",wire=TRUE),.conceptMap(node$labels,"variable",wire=TRUE))
      for(key in targets) {
        target_records[[key]]$id <- key
        id <- sub("^concept:","",key)
        if(id %in% names(map)) {target_records[[key]]$origin<-"variable";target_records[[key]]$label<-map[[id]]}
      }
      target_records <- .categoryVocabularyLabels(ctx$handle,target_records,s)
      # The declared hierarchy domain is public. Carry all its mapped source
      # identities, so consumed alias contracts cannot depend on local rows.
      for(key in setdiff(names(rollup$mapping),names(records)))if(length(rollup$mapping[[key]])) {
        r<-target_records[[rollup$mapping[[key]][1L]]]
        r$id<-key;r$effective_id<-key;records[[key]]<-r
      }
      effective<-vapply(records,function(r)r$effective_id %||% r$id,"")
      targets_for<-lapply(effective,function(key)if(grepl("^special:",key))key else rollup$mapping[[key]])
      names(targets_for)<-names(records)
      # Preserve original admission keys while mapping the consumed identity.
      ids <- vapply(ids,function(id)if(is.na(id)||!length(targets_for[[id]]))NA_character_ else id,"")
      records <- records[lengths(targets_for)>0L]
      for(key in names(records)) {
        target <- targets_for[[key]][1L]
        r<-if(grepl("^special:",target))records[[key]] else target_records[[target]]
        r$id<-key;r$effective_id<-target;records[[key]]<-r
      }
      for(key in setdiff(targets,names(records)))records[[key]]<-target_records[[key]]
      declared <- union(intersect(declared,names(records)),union(targets,intersect(names(rollup$mapping),names(records))))
    }
    effective_spec<-s
    if(is.list(s[["order"]]$metadata))effective_spec[["order"]]<-list(kind="category_order",keys=as.list(.categoryOrderCheck(s[["order"]])))
    contract<-list(spec=effective_spec,producer=producer,producer_order=as.list(producer_order),producer_ordered=producer_ordered)
    if(!is.null(rollup))contract$rollup<-rollup
    if(!flag && identical(c$kind,"category")) {
      inherited<-.categoryNullableStrings(.categoryMergedAliases(c$resolved$aliases))
      if(!is.null(rollup))inherited<-vapply(inherited,function(key) {
        if(is.na(key))return(NA_character_)
        if(grepl("^special:",key))return(key)
        rollup$mapping[[key]][1L] %||% NA_character_
      },"")
      contract$producer_aliases<-.categoryMergedAliases(inherited)
    }

    # Open concept/period order is resolved only from admitted metadata later.
    # Keep observed identities private here; failure never includes their keys.
    sentinel<-character()
    if(identical(s$missing$na,"level")) sentinel<-c(sentinel,"special:missing")
    if(identical(s$missing$unknown,"level")) sentinel<-c(sentinel,"special:unknown")
    for(key in sentinel)records[[key]]<-list(id=key,label=as.character(s$missing$labels[[sub("^special:","",key)]]),origin="sentinel")
    declared<-union(declared,sentinel)
    if(identical(s$missing$na,"level"))ids[is.na(ids)&available]<-"special:missing"
    if(length(s$recode$map)) {
      target_levels<-unlist(s$recode$levels,use.names=FALSE)
      target_labels<-unlist(s$recode$labels %||% sub("^[^:]+:","",target_levels),use.names=FALSE)
      for(key in setdiff(target_levels,names(records)))records[[key]]<-list(id=key,label=target_labels[match(key,target_levels)],origin="recode",domain_only=TRUE)
      declared<-union(declared,target_levels)
    }
    records<-lapply(records,function(r){r$kind<-"category";r$category<-contract;r})
    resolved<-.categoryResolveRecords(unname(records),complete=FALSE,identity=contract)
    if(!is.null(explicit$levels) && (producer$kind!="concept" || length(s$recode$map)) && !setequal(unlist(explicit$levels,use.names=FALSE),resolved$keys)) .categoryReject("Declare the complete existing output domain; new literal levels require an explicit producer or recode label.","category_domain_incompatible")
    values<-resolved$labels[match(ids,resolved$ids)]
    values[!available]<-NA_character_
    frame[[n]]<-.categoryFactor(values,resolved)
    contracts[[n]]<-list(kind="category",category=contract,ids=ids,records=records,
      declared=declared,resolved=resolved,owner=owner)
    if(!is.null(c$upstream_safe)) {
      contracts[[n]]$upstream_safe<-c$upstream_safe
      contracts[[n]]$upstream_admitted<-intersect(c$upstream_admitted,names(records))
    }
  }
  if(length(contracts))attr(frame,"omop_concepts")<-contracts
  for(n in names(frame)) {
    attr(frame[[n]],"category_defined")<-NULL
    attr(frame[[n]],"category_canonical_missing")<-NULL
    if(inherits(frame[[n]],"dsomop_category_value")) {
      cl<-setdiff(class(frame[[n]]),"dsomop_category_value")
      class(frame[[n]])<-if(identical(cl,typeof(frame[[n]])))NULL else cl
    }
  }
  frame
}
.categoryVocabularyLabels <- function(handle,records,s) {
  style<-s$label_style %||% "name"
  if(style=="source_code") .categoryReject("source_code needs an independently admitted source-concept operand; use standard code.","category_source_forbidden")
  if(style=="name" && (is.null(s$language)||identical(s$language,"en")))return(records)
  bp<-.buildBlueprint(handle);tables<-bp$tables$table_name[bp$tables$present_in_db]
  if(!"concept" %in% tables)return(records)
  identities<-vapply(records,function(r)r$effective_id %||% r$id,"")
  ids<-sub("^concept:","",identities);ids<-ids[grepl("^[0-9]+$",ids)]
  if(!length(ids))return(records)
  table<-.qualifyTable(handle,"concept",.resolveTableSchema(handle,"concept","Vocabulary"))
  fields<-intersect(c("concept_id","concept_code","vocabulary_id"),bp$columns$concept$column_name)
  data<-.executeQuery(handle,paste0("SELECT ",paste(fields,collapse=", ")," FROM ",table," WHERE concept_id IN (",.sqlIdList(ids),")"))
  synonyms<-NULL
  if(is.list(s$language) && "concept_synonym" %in% tables) {
    table<-.qualifyTable(handle,"concept_synonym",.resolveTableSchema(handle,"concept_synonym","Vocabulary"))
    synonyms<-.executeQuery(handle,paste0("SELECT concept_id, concept_synonym_name FROM ",table," WHERE language_concept_id = ",s$language$language_concept_id," AND concept_id IN (",.sqlIdList(ids),")"))
  }
  for(k in names(records)) {
    r<-records[[k]];if(r$origin %in% c("variable","recipe","recode","producer","sentinel"))next
    identity<-identities[[k]]
    if(!grepl("^concept:",identity))next
    i<-match(sub("^concept:","",identity),as.character(data$concept_id))
    name<-r$name %||% sub("^concept:","",identity)
    candidates<-as.character(synonyms$concept_synonym_name[as.character(synonyms$concept_id)==sub("^concept:","",identity)])
    candidates<-candidates[!is.na(candidates)&nzchar(trimws(candidates))]
    if(length(candidates)){name<-sort(unique(enc2utf8(candidates)),method="radix")[1L];r$label_origin<-"synonym"}
    code<-if(!is.na(i)&&"concept_code" %in% names(data))as.character(data$concept_code[i]) else NA_character_
    vocab<-if(!is.na(i)&&"vocabulary_id" %in% names(data))as.character(data$vocabulary_id[i]) else NA_character_
    if(is.na(code)||!nzchar(code))code<-sub("^concept:","",identity)
    if(is.na(vocab)||!nzchar(vocab))vocab<-sub("^concept:","",identity)
    r$name<-switch(style,name=name,code=code,code_name=paste(code,name,sep=" - "),vocabulary_code=paste(vocab,code,sep=":"))
    r$label<-r$name;records[[k]]<-r
  }
  records
}
#' @export
`[.dsomop_category_factor` <- function(x,...,drop=FALSE) {
  contract<-attr(x,"omop_category_contract");out<-NextMethod("[",drop=FALSE)
  attr(out,"omop_category_contract")<-contract;out
}
