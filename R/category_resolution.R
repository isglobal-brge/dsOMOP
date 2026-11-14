# Pure A-30 identity resolution, shared with the client. Records supplied here
# have already passed the existing controlled factor-metadata protocol.
.categoryResolveRecords <- function(records) {
  if (!length(records)) .categoryReject("Missing category contract.", "category_domain_incompatible")
  contracts <- lapply(records, function(r) r$category)
  if (length(unique(vapply(contracts,.jcsCanonical,""))) != 1L)
    .categoryReject("Category semantics differ across retained sites.","category_domain_incompatible")
  contract <- contracts[[1L]]; s <- contract$spec
  ids <- unique(vapply(records,function(r)r$id,"")); ids <- ids[.categoryKeyOrder(ids)]
  by_id <- setNames(lapply(ids,function(id)Filter(function(r)identical(r$id,id),records)),ids)
  # Resolve vocabulary names before presentation. Language candidates win over
  # English fallback; automatic equal names never form an equivalence group.
  rows <- lapply(ids,function(id) {
    rr <- by_id[[id]]
    user <- Filter(function(r)r$origin %in% c("variable","recipe","producer","recode","sentinel"),rr)
    if (length(user)) {
      labels <- unique(vapply(user,function(r)r$label,""))
      if (length(labels)!=1L) .categoryReject("Conflicting public category labels.","category_domain_incompatible")
      return(list(id=id,label=labels[[1L]],user=TRUE,origin=user[[1L]]$origin))
    }
    translated <- Filter(function(r)identical(r$label_origin,"synonym"),rr)
    chosen <- .conceptChooseName(if(length(translated))translated else rr)
    list(id=id,label=chosen$name %||% sub("^concept:","",id),user=FALSE,
      origin=if(length(translated))"synonym" else "concept_name_fallback")
  })
  names(rows) <- ids
  keys <- vapply(by_id,function(rr)rr[[1L]]$effective_id %||% rr[[1L]]$id,"")
  labels <- setNames(vapply(rows,function(r)r$label,""),ids)
  recode <- s$recode
  if (!is.null(recode) && length(recode$map)) {
    map <- unlist(recode$map,use.names=TRUE)
    hit <- keys %in% names(map); keys[hit] <- unname(map[keys[hit]])
    if (recode$unmatched=="missing") keys[!hit & !grepl("^special:",ids) & !vapply(by_id,function(rr)isTRUE(rr[[1L]]$domain_only),logical(1))] <- NA_character_
    lev <- unlist(recode$levels,use.names=FALSE)
    lab <- unlist(recode$labels %||% sub("^[^:]+:","",lev),use.names=FALSE)
    if (recode$unmatched=="keep" && !all(keys[!is.na(keys)] %in% c(lev,grep("^special:",ids,value=TRUE))))
      .categoryReject("Recode levels must cover all retained identities.","category_domain_incompatible")
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
  repeat {
    dup <- duplicated(effective_labels)|duplicated(effective_labels,fromLast=TRUE)
    if(!any(dup))break
    collision_labels<-effective_labels
    for(key in names(effective_labels)[dup]) {
      suffix <- if(grepl("^special:",key))sub("^special:","",key) else key
      # Ordinary explicitly supplied labels are never rewritten for a sentinel.
      if(!grepl("^special:",key) && any(grepl("^special:",names(collision_labels)[collision_labels==collision_labels[[key]]])))next
      effective_labels[[key]] <- paste0(effective_labels[[key]]," [",suffix,"]")
    }
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
    merged <- which(!is.na(keys) & keys!=ids & mergeable)
    required <- union(required,ids[merged])
    if(!setequal(order,required)) .categoryReject("Explicit order must cover the complete permitted domain; supply a complete public order.","category_domain_incompatible")
    mapped <- order; mapped[order %in% ids] <- keys[match(order[order %in% ids],ids)]
    runs <- rle(mapped)$values
    if(anyDuplicated(runs)) .categoryReject("Merged category members must be contiguous in explicit order.")
    domain <- unique(mapped[!is.na(mapped)])
  }
  reference <- s$reference
  if(!is.null(reference) && reference %in% ids) reference <- keys[match(reference,ids)]
  if(is.null(reference)) reference <- domain[!grepl("^special:",domain)][1L]
  if(length(reference) && !is.na(reference) && !reference %in% domain) .categoryReject("Reference must identify an effective category.")
  ordered <- s$ordered %||% if(!is.null(recode) && length(recode$map))recode$ordered else isTRUE(contract$producer_ordered)
  list(kind="category",category=contract,ids=ids,labels=unname(effective_labels[keys]),
    levels=unname(effective_labels[domain]),keys=domain,aliases=setNames(keys,ids),ordered=ordered,
    reference=reference,contrasts=list(kind=if(ordered)"polynomial" else "treatment",reference=reference),
    decisions=lapply(seq_along(ids),function(i)list(id=ids[i],label=unname(effective_labels[keys[i]]),rule="declared category identity")))
}
.categoryFactor <- function(values,resolved) {
  out <- factor(values,levels=unlist(resolved$levels,use.names=FALSE),ordered=isTRUE(resolved$ordered))
  class(out) <- c("dsomop_category_factor",class(out))
  keys <- unlist(resolved$keys,use.names=FALSE)
  if(length(keys)>1L) {
    contrast <- if(isTRUE(resolved$ordered)) stats::contr.poly(length(keys)) else {
      base <- match(resolved$reference,keys)
      stats::contr.treatment(length(keys),base=if(is.na(base))1L else base)
    }
    rownames(contrast)<-levels(out);attr(out,"contrasts")<-contrast
  }
  attr(out,"omop_category_contract") <- resolved
  out
}
.categoryValidateResolution <- function(shared,records) {
  if(!identical(shared$kind,"category") || !length(records) ||
     !identical(.jcsCanonical(shared$category),.jcsCanonical(records[[1L]]$category)))
    .categoryReject("Changed shared category contract.","category_domain_incompatible")
  # Local records cannot attest foreign vocabulary labels; they can attest all
  # semantic declarations and their own identity coverage.
  ids <- unlist(shared$ids,use.names=FALSE)
  keys <- unlist(shared$keys,use.names=FALSE)
  labels <- unname(.categoryNullableStrings(shared$labels))
  levels <- unlist(shared$levels,use.names=FALSE)
  aliases <- .categoryNullableStrings(shared$aliases)
  .categoryKeys(ids);.categoryKeys(keys)
  if(anyDuplicated(ids) || !all(names(records) %in% ids) ||
     length(ids)!=length(labels) || anyDuplicated(levels) || length(keys)!=length(levels) ||
     !identical(names(aliases),ids) || !all(aliases[!is.na(aliases)] %in% keys) ||
     !identical(unname(levels[match(aliases,keys)]),labels))
    .categoryReject("Incomplete shared category coverage.","category_domain_incompatible")
  local <- .categoryResolveRecords(unname(records))
  expected_alias <- .categoryNullableStrings(local$aliases)
  if(!identical(unname(aliases[names(expected_alias)]),unname(expected_alias)) ||
     !identical(isTRUE(shared$ordered),isTRUE(local$ordered)))
    .categoryReject("Changed shared category identities or ordinal semantics.","category_domain_incompatible")
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
