# Pure A-30 identity resolution, shared with the client. Records supplied here
# have already passed the existing controlled factor-metadata protocol.
.categoryResolveRecords <- function(records,complete=TRUE,identity=NULL) {
  if (!length(records)&&is.null(identity)) .categoryReject("Missing category contract.", "category_domain_incompatible")
  contracts <- if(length(records))lapply(records, function(r) r$category) else list(identity)
  if (length(unique(vapply(unique(contracts),.jcsCanonical,""))) != 1L)
    .categoryReject("Category semantics differ across retained sites.","category_domain_incompatible")
  contract <- contracts[[1L]]; s <- contract$spec
  ids <- unique(vapply(records,function(r)r$id,"")); ids <- ids[.categoryKeyOrder(ids)]
  # One pass groups each key's records (linear in the list), by position.
  by_id <- split(records,factor(match(vapply(records,function(r)r$id,""),ids),levels=seq_along(ids)))
  names(by_id) <- ids
  # Resolve vocabulary names before presentation. Language candidates win over
  # English fallback; automatic equal names never form an equivalence group.
  rows <- lapply(seq_along(ids),function(i) {
    id <- ids[[i]]; rr <- by_id[[i]]
    user <- Filter(function(r)r$origin %in% c("variable","recipe","producer","recode","sentinel"),rr)
    recoded <- Filter(function(r)identical(r$origin,"recode"),user)
    if(length(recoded))user<-recoded
    if (length(user)) {
      labels <- unique(vapply(user,function(r)r$label,""))
      if (length(labels)!=1L) .categoryReject("Conflicting public category labels.","category_domain_incompatible")
      return(list(id=id,label=labels[[1L]],user=TRUE,origin=user[[1L]]$origin))
    }
    translated <- Filter(function(r)identical(r$label_origin,"synonym"),rr)
    chosen <- .conceptChooseName(if(length(translated))translated else rr)
    list(id=id,label=chosen$name %||% sub("^concept:","",id),user=FALSE,
      origin=if(length(translated))"synonym" else "concept_name_fallback",version=chosen$version,rule=chosen$rule)
  })
  names(rows) <- ids
  keys <- vapply(by_id,function(rr)rr[[1L]]$effective_id %||% rr[[1L]]$id,"")
  inherited <- .categoryNullableStrings(contract$producer_aliases)
  hit <- ids %in% names(inherited); keys[hit] <- inherited[ids[hit]]
  labels <- setNames(vapply(rows,function(r)r$label,""),ids)
  recode <- s$recode
  if (!is.null(recode) && length(recode$map)) {
    map <- unlist(recode$map,use.names=TRUE)
    canonical <- names(map); alias_hit <- canonical %in% names(inherited)
    canonical[alias_hit] <- inherited[canonical[alias_hit]]
    if(anyNA(canonical)) .categoryReject("An absent producer identity cannot be recoded.")
    for(key in unique(canonical))if(length(unique(map[canonical==key]))>1L) .categoryReject("Conflicting recodes for aliases of an upstream merged category.")
    names(map)<-canonical;map<-map[!duplicated(names(map))]
    hit <- keys %in% names(map); keys[hit] <- unname(map[keys[hit]])
    if (recode$unmatched=="missing") keys[!hit & !grepl("^special:",ids) & !vapply(by_id,function(rr)isTRUE(rr[[1L]]$domain_only),logical(1))] <- NA_character_
    lev <- unlist(recode$levels,use.names=FALSE)
    lab <- unlist(recode$labels %||% sub("^[^:]+:","",lev),use.names=FALSE)
    labels[hit] <- lab[match(keys[hit],lev)]
  }
  # Equal explicit A-27 concept labels merge only after the current recode.
  mergeable <- vapply(rows,function(r)r$origin %in% c("variable","recipe"),logical(1)) & grepl("^concept:",keys)
  for (label in unique(labels[mergeable])) {
    ix <- which(mergeable & labels==label)
    canonical <- keys[ix][.categoryKeyOrder(keys[ix])][1L]
    if ("concept:0" %in% keys[ix] && length(unique(keys[ix]))>1L &&
        !is.null(s$missing) && s$missing$unknown!="keep")
      .categoryReject("Mixed zero/nonzero equivalence cannot become Unknown. Use distinct labels, unknown=keep, or recode the whole group.")
    keys[ix] <- canonical
  }
  m <- s$missing
  if (!is.null(m) && m$unknown!="keep" && "concept:0" %in% names(inherited)) {
    group<-names(inherited)[!is.na(inherited) & inherited==inherited[["concept:0"]]]
    if(length(grep("^concept:[1-9]",group)) && any(keys[ids %in% group]=="concept:0",na.rm=TRUE))
      .categoryReject("Inherited zero/nonzero equivalence cannot become Unknown; recode the whole group or use unknown=keep.")
  }
  if (!is.null(m)) {
    zero <- which(keys=="concept:0")
    if(m$unknown=="level") keys[zero] <- "special:unknown"
    if(m$unknown=="missing") keys[zero] <- NA_character_
    if(m$na=="level") keys[is.na(keys)] <- "special:missing"
  }
  domain <- unique(keys[!is.na(keys)])
  if (!is.null(recode) && length(recode$map)) domain <- union(unlist(recode$levels,use.names=FALSE),domain)
  # Sentinel levels exist even when globally unused.
  if (!is.null(m) && m$na=="level") domain <- union(domain,"special:missing")
  if (!is.null(m) && m$unknown=="level") domain <- union(domain,"special:unknown")
  effective_labels <- setNames(vapply(domain,function(key) {
    if (key=="special:missing") return(as.character(m$labels[["missing"]] %||% "Missing"))
    if (key=="special:unknown") return(as.character(m$labels[["unknown"]] %||% "Unknown"))
    if (!is.null(recode) && length(recode$map) && key %in% unlist(recode$levels)) {
      lev <- unlist(recode$levels,use.names=FALSE)
      return(unlist(recode$labels %||% sub("^[^:]+:","",lev),use.names=FALSE)[match(key,lev)])
    }
    labels[which(keys==key)[1L]]
  },""),domain)
  explicit_keys<-unique(keys[vapply(rows,function(r)r$origin %in% c("variable","recipe","recode"),logical(1)) & !is.na(keys)])
  repeat {
    dup <- duplicated(effective_labels)|duplicated(effective_labels,fromLast=TRUE)
    if(!any(dup))break
    collision_labels<-effective_labels
    for(key in names(effective_labels)[dup]) {
      suffix <- if(grepl("^special:",key))sub("^special:","",key) else key
      # Ordinary explicitly supplied labels are never rewritten for a sentinel.
      if(!grepl("^special:",key) && (key %in% explicit_keys || any(grepl("^special:",names(collision_labels)[collision_labels==collision_labels[[key]]]))))next
      effective_labels[[key]] <- paste0(effective_labels[[key]]," [",suffix,"]")
    }
    if(identical(collision_labels,effective_labels)) .categoryReject("Distinct explicitly labelled categories need distinct labels.","category_domain_incompatible")
  }
  order <- if(!is.null(s[["order"]])) .categoryOrderCheck(s[["order"]]) else NULL
  if (is.null(order) || identical(order,"producer")) {
    base <- unlist(contract$producer_order,use.names=FALSE)
    if(!is.null(recode) && length(recode$map))base<-unlist(recode$levels,use.names=FALSE)
    domain <- union(intersect(base,domain),domain[.categoryKeyOrder(domain)])
  } else if(identical(order,"identity")) domain <- domain[.categoryKeyOrder(domain)]
  else {
    # Explicit order covers premerge keys. Merged member blocks must be contiguous.
    required <- union(ids[!is.na(keys) & !grepl("^special:",ids) & keys==ids],domain)
    merged <- which(!is.na(keys) & keys!=ids & (mergeable | ids %in% names(inherited)))
    required <- union(required,ids[merged])
    if((complete&&!setequal(order,required))||!all(required %in% order)) .categoryReject("Explicit order must cover the complete permitted domain; supply a complete public order.","category_domain_incompatible")
    mapped <- if(complete)order else intersect(order,union(ids,domain)); mapped[mapped %in% ids] <- keys[match(mapped[mapped %in% ids],ids)]
    runs <- rle(mapped)$values
    if(anyDuplicated(runs)) .categoryReject("Merged category members must be contiguous in explicit order.")
    domain <- unique(mapped[!is.na(mapped)])
  }
  reference <- s$reference
  if(!is.null(reference) && reference %in% ids) reference <- keys[match(reference,ids)]
  if(is.null(reference)) reference <- domain[!grepl("^special:",domain)][1L]
  if(complete && length(reference) && !is.na(reference) && !reference %in% domain) .categoryReject("Reference must identify an effective category.")
  ordered <- s$ordered %||% if(!is.null(recode) && length(recode$map))recode$ordered else isTRUE(contract$producer_ordered)
  list(kind="category",category=contract,ids=ids,labels=unname(effective_labels[keys]),
    levels=unname(effective_labels[domain]),keys=domain,aliases=setNames(keys,ids),ordered=ordered,
    reference=unname(reference),contrasts=list(kind=if(ordered)"polynomial" else "treatment",reference=unname(reference)),
    decisions=lapply(seq_along(ids),function(i)list(id=ids[i],label=unname(effective_labels[keys[i]]),rule=rows[[i]]$rule %||% "declared category identity",origin=rows[[i]]$origin,version=rows[[i]]$version,language=s$language %||% "en")))
}
.categoryFactor <- function(values,resolved) {
  out <- factor(values,levels=unlist(resolved$levels,use.names=FALSE),ordered=isTRUE(resolved$ordered))
  class(out) <- c("dsomop_category_factor",class(out))
  keys <- unlist(resolved$keys,use.names=FALSE)
  if(length(keys)>1L) {
    contrast <- if(isTRUE(resolved$ordered)) stats::contr.poly(length(keys)) else {
      base <- match(resolved$reference,keys)
      stats::contr.treatment(length(keys),base=if(length(base)!=1L||is.na(base))1L else base)
    }
    rownames(contrast)<-levels(out);attr(out,"contrasts")<-contrast
  }
  attr(out,"omop_category_contract") <- resolved
  out
}
.categoryValidateResolution <- function(shared,records) {
  if(!identical(shared$kind,"category") || length(records) &&
     !identical(.jcsCanonical(shared$category),.jcsCanonical(records[[1L]]$category)))
    .categoryReject("Changed shared category contract.","category_domain_incompatible")
  # Local records cannot attest foreign vocabulary labels; they can attest all
  # semantic declarations and their own identity coverage.
  ids <- as.character(unlist(shared$ids,use.names=FALSE))
  keys <- as.character(unlist(shared$keys,use.names=FALSE))
  labels <- unname(.categoryNullableStrings(shared$labels))
  levels <- as.character(unlist(shared$levels,use.names=FALSE))
  aliases <- .categoryNullableStrings(shared$aliases)
  .categoryKeys(ids);.categoryKeys(keys)
  if(anyDuplicated(ids) || !all(names(records) %in% ids) ||
     length(ids)!=length(labels) || anyDuplicated(levels) || length(keys)!=length(levels) ||
     (length(ids)>0L&&!identical(names(aliases),ids)) || !all(aliases[!is.na(aliases)] %in% keys) ||
     !identical(unname(levels[match(aliases,keys)]),labels))
    .categoryReject("Incomplete shared category coverage.","category_domain_incompatible")
  local <- .categoryResolveRecords(unname(records),complete=FALSE,identity=shared$category)
  expected_alias <- .categoryNullableStrings(local$aliases)
  if(!identical(unname(aliases[names(expected_alias)]),unname(expected_alias)) ||
     !identical(isTRUE(shared$ordered),isTRUE(local$ordered)))
    .categoryReject("Changed shared category identities or ordinal semantics.","category_domain_incompatible")
  for(r in records)if(r$origin %in% c("variable","recipe","recode")) {
    at<-match(r$id,ids);local_at<-match(r$id,local$ids)
    if(!identical(labels[at],unname(local$labels[local_at]))) .categoryReject("Shared labels conflict with the declared recode.","category_domain_incompatible")
  }
  s <- shared$category$spec
  order <- if(!is.null(s[["order"]])) .categoryOrderCheck(s[["order"]]) else NULL
  if(is.null(order)||identical(order,"producer")) {
    producer <- unlist(shared$category$producer_order,use.names=FALSE)
    if(length(s$recode$map))producer<-unlist(s$recode$levels,use.names=FALSE)
    expected <- union(intersect(producer,keys),keys[.categoryKeyOrder(keys)])
  } else if(identical(order,"identity"))expected<-keys[.categoryKeyOrder(keys)]
  else {
    expected<-order
    hit<-expected %in% names(aliases);expected[hit]<-aliases[expected[hit]]
    expected<-unique(expected[!is.na(expected)])
  }
  if(!identical(keys,expected)) .categoryReject("Changed shared category order.","category_domain_incompatible")
  reference<-s$reference
  if(!is.null(reference)&&reference %in% names(aliases))reference<-aliases[[reference]]
  if(is.null(reference))reference<-keys[!grepl("^special:",keys)][1L]
  actual_reference<-unname(.categoryNullableStrings(shared$reference))
  if(!length(actual_reference))actual_reference<-NA_character_
  if(!identical(actual_reference,unname(reference)) ||
     !identical(shared$contrasts$kind,if(isTRUE(shared$ordered))"polynomial" else "treatment"))
    .categoryReject("Changed shared category reference/contrasts.","category_domain_incompatible")
  invisible(TRUE)
}

# Only real equivalences belong in the producer declaration. Unmerged admitted
# identities remain in records so an open local domain can be federated.
.categoryMergedAliases <- function(aliases) {
  aliases<-.categoryNullableStrings(aliases)
  targets<-unique(aliases[!is.na(aliases)&aliases!=names(aliases)])
  as.list(aliases[!is.na(aliases)&aliases %in% targets])
}
