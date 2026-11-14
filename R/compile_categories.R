.categoryInputContract <- function(ctx,input) {
  maps<-list()
  walk<-function(x,path="input") {
    if(!is.list(x))return(NULL)
    if(!is.null(x$category_spec$rollup)&&x$category_spec$rollup$kind!="none")maps[[path]]<<-.categoryRollupMap(ctx$handle,x$category_spec$rollup)
    for(i in seq_along(x))if(is.list(x[[i]]))walk(x[[i]],paste0(path,"/",names(x)[i] %||% i))
  }
  walk(input)
  list(variable=input,maps=maps)
}

.categorySetIndicator <- function(ctx,raw,spec,key,missing="propagate",labels=NULL) {
      s<-spec;mapping<-.categoryRollupMap(ctx$handle,s$rollup)$mapping
      targets<-unique(unlist(mapping,use.names=FALSE))
      public_labels<-.conceptMapOverlay(.conceptMap(ctx$ir$options$labels,"recipe",wire=TRUE),.conceptMap(labels,"input",wire=TRUE))
      s$rollup<-NULL;s[["order"]]<-NULL;s$ordered<-NULL;s$reference<-NULL;s$binary<-NULL;s$label_style<-NULL;s$language<-NULL
      meta<-list(spec=s,producer=list(kind="hierarchy_set"),producer_order=list(),producer_ordered=FALSE)
      rr<-setNames(lapply(targets,function(key) {
        id<-sub("^concept:","",key)
        list(id=key,label=if(id %in% names(public_labels))public_labels[[id]] else key,
          origin=if(id %in% names(public_labels))"variable" else "vocabulary",category=meta)
      }),targets)
      if(identical(s$missing$na,"level"))rr[["special:missing"]]<-list(id="special:missing",label=as.character(s$missing$labels[["missing"]]),origin="sentinel",category=meta)
      if(identical(s$missing$unknown,"level"))rr[["special:unknown"]]<-list(id="special:unknown",label=as.character(s$missing$labels[["unknown"]]),origin="sentinel",category=meta)
      resolution<-.categoryResolveRecords(unname(rr),identity=meta);aliases<-unlist(resolution$aliases,use.names=TRUE)
      key<-if(key %in% names(aliases))aliases[[key]] else key
      if(grepl("^special:",key)&&!key %in% resolution$keys) .categoryReject("Declare a sentinel before testing it.")
      v<-vapply(seq_along(raw),function(i) {
        set<-if(is.na(raw[i]))character() else mapping[[paste0("concept:",as.character(raw[i]))]]
        effective<-unname(aliases[set])
        if(!length(effective)||all(is.na(effective))) {
          if(identical(s$missing$na,"level"))return(as.integer(key=="special:missing"))
          return(if(missing=="zero")0L else NA_integer_)
        }
        as.integer(key %in% effective)
      },1L)
  v
}

.categoryEventRecord <- function(ctx,node,name,cons) {
  d<-node$derived;src<-.reduceSource(ctx,d$source)
  res<-.predicateResolve(src$catalogue,d$value,"category value")
  if(!isTRUE(res$concept) && !identical(d$value,"concept_id"))
    .categoryReject("Select a registered admitted concept role; free text and numeric quantities are not categories.","category_source_forbidden")
  column<-.fragmentColumn(res$type,TRUE,"concept");attr(column,"concept")<-TRUE
  columns<-setNames(list(column),name)
  if(!src$available) {
    rec<-.reduceUnavailable(ctx,cons,name,columns);rec$event_grain<-TRUE;rec$event_source<-d$source
    rec$evaluate<-function(){out<-data.frame(row_key=character(),event_id=character(),source_ns=character(),start_date=as.Date(character()));out[[name]]<-integer();out}
    return(rec)
  }
  extra<-list(v=list(expr=paste0("e.",res$column),type=res$type))
  if("visit_id" %in% names(src$fragment$columns))extra$visit_id<-list(expr="e.visit_id",type="bigint",role="private")
  stream<-.reduceStream(ctx,cons,src,NULL,.reduceName(ctx,cons,"s",name),extra=extra,dated=FALSE)
  evaluate<-function() {
    rows<-.reduceStreamRows(ctx,cons,stream,NULL,"category source")$rows
    rows<-rows[.reduceOrderRows(rows,c("row_key",intersect(.reduceSourceOrdering(src),names(rows)))),,drop=FALSE]
    out<-rows[intersect(c("row_key","event_id","source_ns","start_date","end_date","visit_id"),names(rows))]
    out[[name]]<-rows$v;out
  }
  list(stream=stream,reducer=NULL,declared_empty_value=NA,columns=columns,evaluate=evaluate,
    available=TRUE,event_grain=TRUE,event_source=d$source)
}
.categoryIndicatorRecord <- function(ctx,node,name,cons) {
  d<-node$derived;input<-d$variable;dep<-.numberDependencyName(name,"category")
  attr(input,"name")<-dep;rec<-.compileVariable(ctx,input,cons$roster,cons)
  if(length(rec$columns)!=1L) .categoryReject("Indicators require exactly one typed operand.")
  cn<-names(rec$columns)[1L];column<-.fragmentColumn("integer",TRUE,"measure")
  attr(column,"semantic_flag")<-TRUE
  attr(column,"category_input_contract")<-.categoryInputContract(ctx,input)
  attr(column,"privacy_class")<-attr(rec$columns[[cn]],"privacy_class")
  evaluate<-function() {
    rows<-rec$evaluate()
    if(identical(input$category_spec$rollup$multiple,"all")) {
      raw<-rows[[cn]]
      v<-.categorySetIndicator(ctx,raw,input$category_spec,d$key,d$missing,input$labels)
      defined<-attr(raw,"category_defined") %||% rep(TRUE,length(v))
      available<-isTRUE(rec$available) & defined
      v[!available]<-NA_integer_
      out<-rows[setdiff(names(rows),cn)];out[[name]]<-.categoryValue(v,available)
      attr(out[[name]],"category_canonical_missing")<-is.na(raw);return(out)
    }
    # An isolated private context prevents outer defaults from changing the
    # explicitly requested identity test on the input.
    local<-list2env(as.list(ctx,all.names=TRUE),parent=parent.env(ctx));local$ir<-ctx$ir;local$ir$options$category_spec<-NULL
    if(.categoryFlag(input,rec$columns[[cn]]))input$category_spec<-.categoryEffective(input$category_spec,list(binary=list(kind="category_binary",as="factor",labels=c("no","yes"))))
    if(.categoryFlag(input,rec$columns[[cn]]))input$category_spec$binary$as<-"factor"
    else if(is.null(input$category_spec))input$category_spec<-list(order=list(kind="category_order",metadata="producer"))
    local$ir$variables[[dep]]<-input
    cols<-rec$columns;attr(cols[[cn]],"concept_owner")<-dep
    numeric<-.numberOutputColumns(rows,cols,list(format="long"),"input","input",recipe=NULL)
    presented<-.conceptPresent(local,numeric$frame,numeric$columns)
    c<-attr(presented,"omop_concepts")[[cn]]
    if(is.null(c)) .categoryReject("Indicators require statically declared category identity.")
    if(identical(c$kind,"category")) {
      alias<-unlist(c$resolved$aliases,use.names=TRUE);key<-d$key
      if(key %in% names(alias))key<-alias[[key]]
      if(grepl("^special:",key)&&!key %in% unlist(c$resolved$keys)) .categoryReject("Declare a Missing/Unknown sentinel before testing it.")
      identities<-unname(alias[c$ids])
    } else {
      prefix<-if(identical(c$kind,"number"))"bin:" else if(identical(c$kind,"date"))"period:" else "concept:"
      identities<-if(identical(c$kind,"number"))paste0(prefix,match(c$ids,names(c$records))) else paste0(prefix,c$ids)
      identities[is.na(c$ids)]<-NA_character_;key<-d$key
      if(grepl("^special:",key)) .categoryReject("Declare a Missing/Unknown sentinel before testing it.")
    }
    v<-as.integer(identities==key)
    if(d$missing=="zero"&&isTRUE(rec$available))v[is.na(v)]<-0L
    defined<-isTRUE(rec$available) & (attr(rows[[cn]],"category_defined") %||% rep(TRUE,length(v)))
    v[!defined]<-NA_integer_
    out<-rows[setdiff(names(rows),cn)];out[[name]]<-.categoryValue(v,defined);attr(out[[name]],"category_canonical_missing")<-attr(rows[[cn]],"category_canonical_missing") %||% is.na(rows[[cn]]);out
  }
  list(stream=rec$stream,reducer=NULL,declared_empty_value=NA,columns=setNames(list(column),name),
    evaluate=evaluate,available=rec$available,event_grain=rec$event_grain,event_source=rec$event_source)
}

.categoryReduceRecord <- function(ctx,node,name,cons) {
  d<-node$derived;input<-d$variable;dep<-.numberDependencyName(name,"operand")
  attr(input,"name")<-dep;rec<-.compileVariable(ctx,input,cons$roster,cons)
  if(!isTRUE(rec$event_grain)||length(rec$columns)!=1L) .categoryReject("category_reduce needs one same-event category/flag projection, not a nested reduction.")
  r<-d$reducer
  if(!r$fn %in% c("first_value","last_value","nth_value","closest_value","mode","count","n_distinct","any","all","sum")||!is.null(r$value)||!is.null(r$predicate)) .categoryReject("Category reducers consume the typed operand; omit value/predicate.")
  for(k in c("reference_date","agg","p"))if(!is.null(r[[k]])) .categoryReject("Inapplicable category reducer argument.")
  if(!is.null(r$unit)&&!identical(r$unit,"day")) .categoryReject("Category reducers do not convert units.")
  input_column<-rec$columns[[1L]];flag<-.categoryFlag(input,input_column)
  if(flag&&!r$fn %in% c("any","all","sum","count","n_distinct")) .categoryReject("Semantic flags support any/all/sum/count/n_distinct; keep a categorical sibling for selectors.")
  if(r$fn %in% c("any","all","sum")&&!flag) .categoryReject("any/all/sum require explicit semantic indicators.")
  numeric<-r$fn %in% c("count","n_distinct","any","all","sum")
  column<-.fragmentColumn(if(numeric)"integer" else "text",TRUE,"measure")
  attr(column,"privacy_class")<-attr(input_column,"privacy_class")
  attr(column,"category_input_contract")<-.categoryInputContract(ctx,input)
  # Input lineage also exists on counts and indicators; only selectors return
  # a categorical cell. Eligibility must be known before evaluating values.
  if(!numeric)attr(column,"category_result")<-TRUE
  if(r$fn %in% c("any","all"))attr(column,"semantic_flag")<-TRUE
  columns<-setNames(rep(list(column),length(.reduceColumnNames(name,r))),.reduceColumnNames(name,r))
  evaluate<-function() {
    rows<-rec$evaluate();cn<-names(rec$columns)[1L]
    local<-list2env(as.list(ctx,all.names=TRUE),parent=parent.env(ctx));local$ir<-ctx$ir
    local$ir$options$category_spec<-NULL;local$ir$options$number_spec<-NULL
    if(!flag&&is.null(input$category_spec))input$category_spec<-list(order=list(kind="category_order",metadata="producer"))
    local$ir$variables[[dep]]<-input
    cols<-rec$columns;attr(cols[[cn]],"concept_owner")<-dep
    presented<-.numberOutputColumns(rows,cols,list(format="long"),"input","input",recipe=NULL)
    presented<-.conceptPresent(local,presented$frame,presented$columns)
    contract<-attr(presented,"omop_concepts")[[cn]]
    upstream_safe<-TRUE
    if(!is.null(contract)) {
      roster_support<-.reduceRosterFrame(ctx,cons$roster)
      person_key<-intersect(.PERSON_KEY_COLS(),names(roster_support))
      if(length(person_key))presented[[person_key[1L]]]<-roster_support[[person_key[1L]]][match(rows$row_key,roster_support$cohort_row_id)]
      upstream_safe<-isTRUE(.conceptAssess(presented,cn,contract)$safe)
    }
    if(flag) {
      keys<-if(is.factor(presented[[cn]])) {
        identity<-.categoryNullableStrings(contract$resolved$aliases)[contract$ids]
        ifelse(identity=="flag:0",0L,ifelse(identity=="flag:1",1L,NA_integer_))
      } else as.integer(presented[[cn]])
    } else {
      if(is.null(contract)) .categoryReject("Category reduction requires a declared categorical producer, not a quantity.")
      if(identical(contract$kind,"category"))keys<-unname(unlist(contract$resolved$aliases,use.names=TRUE)[contract$ids])
      else {
        prefix<-if(identical(contract$kind,"number"))"bin:" else if(identical(contract$kind,"date"))"period:" else "concept:"
        keys<-if(identical(contract$kind,"number"))paste0(prefix,match(contract$ids,names(contract$records))) else paste0(prefix,contract$ids)
        keys[is.na(contract$ids)]<-NA_character_
      }
    }
    rows$v<-keys
    rows$concept_id<-keys
    if(flag)rows$pred<-keys
    bounds<-.reduceBoundsR(ctx,cons,r$window,"category reduction window")
    rows<-.reduceWindowFilter(rows,bounds)
    roster<-.reduceRosterFrame(ctx,cons$roster);out<-data.frame(row_key=roster$cohort_row_id)
    if(r$fn=="mode") {
      groups<-.reduceGroups(rows$row_key,roster$cohort_row_id)
      value<-lapply(groups,function(ix) {
        v<-rows$v[ix];v<-v[!is.na(v)]
        if(!length(v))return(NA_character_)
        domain<-unique(v);counts<-tabulate(match(v,domain),nbins=length(domain))
        ties<-domain[counts==max(counts)];ties[.categoryKeyOrder(ties)][1L]
      });result<-list(value)
    } else {
      anchor<-if(r$fn=="closest_value").reduceAnchorDates(ctx,cons,.reduceAnchor(r)) else NULL
      identity_domain<-unique(rows$concept_id[!is.na(rows$concept_id)])
      identity_domain<-identity_domain[.categoryKeyOrder(identity_domain)]
      if(!numeric)rows$v<-match(rows$concept_id,identity_domain)
      result<-.reduceApply(r$fn,rows,roster$cohort_row_id,r,c("start_date","source_ns","event_id"),anchor,if(flag)"integer" else "text")
    }
    for(i in seq_along(columns)) {
      value<-.reduceCoerce(result[[i]],if(numeric)"integer" else "text")
      if(!numeric&&r$fn!="mode")value<-identity_domain[as.integer(value)]
      defined<-bounds$defined & isTRUE(rec$available)
      if(r$fn=="closest_value")defined<-defined & !is.na(anchor)
      value[!defined]<-NA
      if(!numeric) {
        # Carry already-consumed identity semantics; no automatic second recode.
        if(identical(contract$kind,"category")) {
          aliases<-unlist(contract$resolved$aliases,use.names=TRUE)
          effective<-unlist(contract$resolved$keys,use.names=FALSE)
          labels<-unlist(contract$resolved$levels,use.names=FALSE)
        } else {
          old<-names(contract$records)
          effective<-if(identical(contract$kind,"number"))paste0("bin:",seq_along(old)) else paste0(if(identical(contract$kind,"date"))"period:" else "concept:",old)
          labels<-contract$resolved$labels;aliases<-setNames(effective,effective)
        }
        producer_order<-contract$category$producer_order %||% list()
        if(length(contract$category$spec$recode$map))producer_order<-contract$category$spec$recode$levels
        declared_order<-contract$category$spec[["order"]]
        if(!is.null(declared_order)) {
          requested<-.categoryOrderCheck(declared_order)
          if(!identical(requested,"producer")&&!identical(requested,"identity")) {
            hit<-requested %in% names(aliases);requested[hit]<-aliases[requested[hit]]
            producer_order<-as.list(unique(requested[!is.na(requested)]))
          }
        }
        metadata<-list(spec=list(),producer=list(kind="category_reduce",input=contract$category %||% contract$number %||% contract$calendar),producer_order=producer_order,producer_ordered=is.ordered(presented[[cn]]),producer_aliases=.categoryMergedAliases(aliases))
        record_keys<-union(effective,names(aliases)[!is.na(aliases)])
        records<-setNames(lapply(record_keys,function(key) {
          identity<-if(key %in% names(aliases))aliases[[key]] else key
          r<-contract$records[[key]] %||% list(origin="producer")
          r$kind<-"category";r$id<-key;r$effective_id<-identity;r$label<-labels[match(identity,effective)];r$category<-metadata
          recoded<-length(contract$category$spec$recode$map)>0L && identity %in% unlist(contract$category$spec$recode$levels,use.names=FALSE)
          if(recoded || !grepl("^concept:",identity)) {
            r$origin<-if(grepl("^special:",identity))"sentinel" else if(recoded)"recode" else "producer"
            r$name<-NULL;r$vocabulary<-NULL;r$version<-NULL
          }
          r
        }),record_keys)
        c<-list(kind="category",category=metadata,ids=value,records=records,declared=character(),upstream_safe=upstream_safe,upstream_admitted=record_keys,resolved=.categoryResolveRecords(unname(records),identity=metadata))
        value<-.categoryFactor(c$resolved$labels[match(value,c$resolved$ids)],c$resolved)
        attr(value,"category_consumed")<-c
      }
      out[[names(columns)[i]]]<-.categoryValue(value,defined)
    }
    out
  }
  list(stream=rec$stream,reducer=r,declared_empty_value=if(numeric)0L else NA,
    columns=columns,evaluate=evaluate,available=rec$available,event_grain=FALSE)
}
