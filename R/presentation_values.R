# Shared execution presentation on typed vectors, including empty public prototypes.
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
.categoryConceptProducer <- function(contract, recoded=TRUE) {
  if(recoded && length(contract$spec$recode$map) && any(!grepl("^concept:",unlist(contract$spec$recode$levels,use.names=FALSE))))return(FALSE)
  producer<-contract$producer
  if(identical(producer$kind,"concept"))return(TRUE)
  if((producer$kind %||% "") %in% c("category","category_reduce"))return(.categoryConceptProducer(producer$input,recoded))
  FALSE
}
.categoryPresent <- function(ctx,frame,columns) {
  # Vocabulary access exists only in server execution. Resolve those backend
  # functions on demand; empty public prototypes never enter these branches.
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
    concept <- !flag && (is.null(c$kind) || identical(c$kind,"category") && .categoryConceptProducer(c$category,recoded=FALSE))
    s <- .categoryApplicableVocabulary(s, explicit, concept)
    # Styling can retain concept labels in a mixed recode; hierarchy requires
    # an entirely concept-valued input. Defaults do not reinterpret that recode.
    if (identical(c$kind,"category") && !.categoryConceptProducer(c$category) && is.null(explicit$rollup)) s$rollup <- NULL
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
          rr<-get(".conceptVocabulary",mode="function")(ctx$handle,sub("^concept:","",extra))
          for(k in seq_along(extra)) {r<-rr[[k]];r$id<-extra[k];records[[extra[k]]]<-r}
          declared<-union(declared,extra)
        }
        records<-get(".categoryVocabularyLabels",mode="function")(ctx$handle,records,s)
      }
    }
    if(!flag && identical(c$kind,"category") && (!is.null(s$label_style)||!is.null(s$language))) {
      # A consumed display is not a fresh vocabulary name. Rebind automatic
      # names to the effective concept before applying an explicit outer style.
      for(key in names(records)) {
        r<-records[[key]];identity<-r$effective_id %||% key
        if(!grepl("^concept:",identity)||r$origin %in% c("variable","recipe","recode","producer","sentinel"))next
        fresh<-get(".conceptVocabulary",mode="function")(ctx$handle,sub("^concept:","",identity))[[1L]]
        fresh$id<-key;fresh$effective_id<-identity;records[[key]]<-fresh
      }
      records<-get(".categoryVocabularyLabels",mode="function")(ctx$handle,records,s)
    }
    rollup <- NULL
    if(!is.null(s$rollup) && s$rollup$kind!="none") {
      if(!.categoryConceptProducer(list(producer=producer))) .categoryReject("Hierarchy rollup requires concept identities.")
      rollup <- get(".categoryRollupMap",mode="function")(ctx$handle,s$rollup)
      targets <- unique(unlist(rollup$mapping,use.names=FALSE))
      target_records <- if(length(targets))get(".conceptVocabulary",mode="function")(ctx$handle,sub("^concept:","",targets)) else list()
      names(target_records) <- targets
      map <- .conceptMapOverlay(.conceptMap(ctx$ir$options$labels,"recipe",wire=TRUE),.conceptMap(node$labels,"variable",wire=TRUE))
      for(key in targets) {
        target_records[[key]]$id <- key
        id <- sub("^concept:","",key)
        if(id %in% names(map)) {target_records[[key]]$origin<-"variable";target_records[[key]]$label<-map[[id]]}
      }
      target_records <- get(".categoryVocabularyLabels",mode="function")(ctx$handle,target_records,s)
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

# Date metadata is a distinct variant of the existing controlled A-27 route.
.dateCapture <- function(frame,columns) {
  contracts<-attr(frame,"omop_concepts") %||% list()
  for(col in intersect(names(frame),names(columns))) {
    c<-attr(frame[[col]],"omop_date_contract")
    if(is.null(c)) {
      s<-attr(columns[[col]],"date_spec")
      if(is.null(s)||!is.factor(frame[[col]])) next
      labels<-levels(frame[[col]]);keys<-labels;map<-unlist(s$labels)
      if(length(map)) keys[labels %in% map]<-names(map)[match(labels[labels %in% map],map)]
      c<-list(spec=s,levels=keys,labels=labels,order=seq_along(keys))
    }
    # Subsetting data frames can retain vector attributes; reconstruct keys from
    # the actual labels so collapse/join never leaves row-misaligned identity.
    ids<-c$levels[match(as.character(frame[[col]]),c$labels)]
    s<-c$spec
    identity<-s;identity$labels<-NULL;identity$level_range<-NULL
    identity$operation<-attr(columns[[col]],"date_operation")
    # Lexical built-in keys are chronology-safe. Custom and fixed parts use
    # their public calendar order, never patient insertion order.
    ranks<-if(!is.null(s$precision)&&s$precision!="custom") match(c$levels,sort(c$levels)) else c$order
    records<-setNames(lapply(seq_along(c$levels),function(i)list(kind="date",id=c$levels[i],label=c$labels[i],calendar=identity,order=ranks[i])),c$levels)
    resolved<-.dateResolveRecords(unname(records),identity)
    declared<-c$declared %||% setdiff(c$levels,unique(ids[!is.na(ids)]))
    contracts[[col]]<-list(kind="date",calendar=identity,ids=ids,records=records,declared=declared,resolved=resolved)
  }
  if(length(contracts)) attr(frame,"omop_concepts")<-contracts
  frame
}


.numberCapture <- function(frame,columns) {
  contracts<-attr(frame,"omop_concepts") %||% list();numbers<-attr(frame,"omop_numbers") %||% list()
  for(n in intersect(names(columns),names(frame))) {
    c<-attr(columns[[n]],"number_contract")
    if(is.null(c))next
    numbers[[n]]<-c
    if(!is.factor(frame[[n]]))next
    labels<-levels(frame[[n]]);ids<-paste0("N",seq_along(labels))
    records<-setNames(lapply(seq_along(labels),function(i)list(kind="number",id=ids[i],label=labels[i],number=c,order=i)),ids)
    contracts[[n]]<-list(kind="number",number=c,ids=ids[match(as.character(frame[[n]]),labels)],records=records,declared=ids,resolved=.numberResolveRecords(unname(records),c))
  }
  if(length(contracts))attr(frame,"omop_concepts")<-contracts
  if(length(numbers))attr(frame,"omop_numbers")<-numbers
  frame
}

# The same descriptor is used for actual released vectors and empty prototypes.
# No values, counts or observed support are read here.
.presentationDescribe <- function(value, category=NULL, domains=TRUE) {
  type <- if(is.ordered(value)) "ordered_factor" else if(is.factor(value)) "factor" else
    if(inherits(value,"Date")) "Date" else if(inherits(value,"integer64")) "integer64" else typeof(value)
  if(!domains)return(list(r_type=type,storage_type=typeof(value)))
  resolved <- attr(value,"omop_category_contract") %||% category$resolved
  calendar <- attr(value,"omop_date_contract")
  keys <- resolved$keys %||% resolved$ids %||% calendar$levels
  labels <- resolved$levels %||% calendar$labels %||% if(is.factor(value))levels(value)
  list(r_type=type,storage_type=typeof(value),
    levels=if(length(keys))list(keys=keys,labels=labels,completeness="declared"),
    ordered=if(is.factor(value))is.ordered(value),
    reference=if(is.factor(value))resolved$reference,
    contrasts=if(is.factor(value))resolved$contrasts %||% list(kind=if(is.ordered(value))"polynomial" else "treatment"))
}

# Membership's public producer inventory is also consumed by its compiler.
.presentationMembershipColumns <- function(population, options=list(), legacy=FALSE) {
  columns <- c(if("legacy_cohort_definition_id" %in% names(options) || legacy)
    c(cohort_definition_id="integer"),cohort_start_date="Date",cohort_end_date="Date")
  if((population$kind %||% "") %in% c("index","criteria","setop","match"))
    columns <- c(columns,index_source="character")
  columns
}

# Decorators attach date, number and category covariates in separate phases.
# Their final slots follow the declared selection, independent of those phases;
# structural/generated columns keep their producer positions.
.presentationVariableOrder <- function(keys, variables, selected) {
  selected <- unlist(selected,use.names=FALSE)
  declared <- unlist(lapply(names(variables),function(k).pnv2VariableColumns(k,variables[[k]])),use.names=FALSE)
  requested <- unique(unlist(lapply(selected,function(k) {
    if(k %in% names(variables)).pnv2VariableColumns(k,variables[[k]]) else k
  }),use.names=FALSE))
  slots <- which(keys %in% intersect(declared,requested))
  keys[slots] <- requested[requested %in% keys[slots]]
  keys
}
