.repeatedRecord <- function(ctx,node,name,cons) {
  d<-node$derived
  values<-setNames(lapply(d$values,function(v)v$variable),vapply(d$values,function(v)v$name,""))
  if(!d$value %in% names(values)) .categoryReject("Unknown repeated value key.","repeated_spec_invalid")
  records<-setNames(lapply(names(values),function(key) {
    v<-values[[key]];attr(v,"name")<-.numberDependencyName(name,key)
    rec<-.compileVariable(ctx,v,cons$roster,cons)
    if(!isTRUE(rec$event_grain)||length(rec$columns)!=1L||!identical(.jcsCanonical(rec$event_source),.jcsCanonical(d$source)))
      .categoryReject("Repeated operands require one identical event source and candidate stream; use shared event normalization or separate bundles.","repeated_grain_incompatible")
    rec
  }),names(values))
  available<-all(vapply(records,function(r)isTRUE(r$available),logical(1)))
  selected<-records[[d$value]];column<-selected$columns[[1L]]
  attr(column,"privacy_class")<-2L
  # Static dependency and selection policies travel with the result contract.
  attr(column,"repeated_contract")<-d
  visit_stream<-NULL;visit_src<-NULL
  if(!is.null(d$visits)) {
    ev<-ctx$ir$events[[d$visits$source$event]]
    if(!identical(ev$table,"visit_occurrence")) .categoryReject("Visit ranking requires registered visit_occurrence rows.","repeated_spec_invalid")
    visit_src<-.reduceSource(ctx,d$visits$source)
    available<-available&&isTRUE(visit_src$available)
    if(visit_src$available)visit_stream<-.reduceStream(ctx,cons,visit_src,d$visits$window,.reduceName(ctx,cons,"s",paste0(name,"visits")),dated=TRUE)
  }
  evaluate<-function() {
    roster<-.reduceRosterFrame(ctx,cons$roster);out<-data.frame(row_key=roster$cohort_row_id)
    out[[name]]<-.reduceTypedNA(column$type,nrow(roster))
    if(!available)return(out)
    frames<-lapply(records,function(rec)rec$evaluate())
    rows<-frames[[1L]]
    identity<-function(f)do.call(paste,c(lapply(f[c("row_key","source_ns","event_id")],as.character),sep="\r"))
    keys<-identity(rows);aligned<-list()
    for(k in names(frames)) {
      at<-match(keys,identity(frames[[k]]))
      if(anyNA(at)||nrow(frames[[k]])!=length(keys)||anyDuplicated(identity(frames[[k]])))
        .categoryReject("Repeated values do not have identical source-qualified rows.","repeated_grain_incompatible")
      aligned[[k]]<-frames[[k]][[names(records[[k]]$columns)[1L]]][at]
    }
    missing<-vapply(aligned,function(x)attr(x,"category_canonical_missing") %||% is.na(x),logical(nrow(rows)))
    if(!is.matrix(missing))missing<-matrix(missing,ncol=length(aligned))
    keep<-if(d$missing=="skip_any")rowSums(missing)==0L else if(d$missing=="skip_all")rowSums(!missing)>0L else rep(TRUE,nrow(rows))
    rows$.candidate<-seq_len(nrow(rows));rows<-rows[keep&!is.na(rows$start_date),,drop=FALSE]
    chosen<-rep(NA_integer_,nrow(roster));defined<-rep(TRUE,nrow(roster))
    if(!is.null(d$timepoints)) {
      points<-d$timepoints$points
      point_names<-vapply(points,function(p)p$name,"")
      if(!d$point %in% point_names) .categoryReject("Unknown repeated point key.","repeated_spec_invalid")
      used<-vector("list",nrow(roster))
      for(entry in points) {
        point<-entry$point
        anchors<-.dateExact(.dateSourceValues(ctx,cons,point$anchor %||% d$timepoints$anchor,.dateEffective()))
        target<-.repeatedTarget(anchors,point)
        selection<-rep(NA_integer_,nrow(roster))
        tolerance<-unlist(point$tolerance,use.names=FALSE)
        for(i in seq_len(nrow(roster))) {
          if(is.na(target[i]))next
          ix<-which(as.character(rows$row_key)==as.character(roster$cohort_row_id[i]) & rows$start_date>=target[i]+tolerance[1L] & rows$start_date<=target[i]+tolerance[2L])
          if(!isTRUE(d$timepoints$reuse))ix<-ix[!rows$.candidate[ix] %in% used[[i]]]
          picked<-.repeatedSelect(rows[ix,,drop=FALSE],target[i],point$select)
          if(!is.na(picked)){selection[i]<-rows$.candidate[ix[picked]];used[[i]]<-c(used[[i]],selection[i])}
        }
        if(entry$name==d$point){chosen<-selection;defined<-!is.na(target);break}
      }
    } else {
      visits<-.reduceStreamRows(ctx,cons,visit_stream,d$visits$window,"repeated visits")$rows
      defined<-.reduceBoundsR(ctx,cons,d$visits$window,"visit window")$defined
      visits<-visits[!is.na(visits$start_date),,drop=FALSE]
      vid<-identity(visits)
      if(anyDuplicated(vid)) {
        canon<-visits[intersect(c("row_key","source_ns","event_id","start_date","end_date"),names(visits))]
        if(anyDuplicated(vid[!duplicated(canon)])) .categoryReject("Conflicting registered visit identity.","repeated_spec_invalid")
        visits<-visits[!duplicated(vid),,drop=FALSE]
      }
      visits<-visits[.reduceOrderRows(visits,c("row_key","start_date","source_ns","event_id")),,drop=FALSE]
      n<-as.integer(sub("^visit_","",d$point))
      for(i in seq_len(nrow(roster))) {
        group<-which(as.character(visits$row_key)==as.character(roster$cohort_row_id[i]))
        if(length(group)<n)next
        visit<-visits[group[n],,drop=FALSE]
        ix<-which(as.character(rows$row_key)==as.character(roster$cohort_row_id[i]) & !is.na(rows$visit_id) & as.character(rows$visit_id)==as.character(visit$event_id))
        picked<-.repeatedSelect(rows[ix,,drop=FALSE],visit$start_date,d$visits$select)
        if(!is.na(picked))chosen[i]<-rows$.candidate[ix[picked]]
      }
    }
    out[[name]]<-aligned[[d$value]][chosen]
    indicator<-values[[d$value]]$derived
    if(identical(indicator$kind,"category_indicators")) {
      empty<-is.na(chosen)&defined
      spec<-.categoryEffective(indicator$variable$category_spec,ctx$ir$options$category_spec)
      if(identical(spec$missing$na,"level"))out[[name]][empty]<-as.integer(indicator$key=="special:missing")
      else if(indicator$missing=="zero")out[[name]][empty]<-0L
    }
    # Preserve unavailable/undefined target independently of ordinary no-row NA.
    out[[name]]<-.categoryValue(out[[name]],defined)
    out
  }
  list(stream=NULL,reducer=NULL,declared_empty_value=NA,columns=setNames(list(column),name),
    evaluate=evaluate,available=available,event_grain=FALSE,event_source=NULL)
}
