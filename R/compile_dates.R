# Native date sources are evaluated on private typed materialisations; no SQL
# date constructor narrows the 0001..9999 domain and no patient rows leave here.
.dateSourceValues <- function(ctx,cons,source,s) {
  roster<-.reduceRosterFrame(ctx,cons$roster);n<-nrow(roster)
  if(is.list(source)&&identical(source$kind,"date_components")) {
    operands<-source[c("year","month","day")]
    cols<-unique(vapply(Filter(function(x)!is.null(x)&&identical(x$kind,"column"),operands),function(x)x$name,""))
    if(is.null(source$event)) {
      # Person columns use the registered catalogue and the same class-1 guard.
      have<-.reduceTableColumns(ctx,"person")
      for(col in cols) {
        row<-have[match(col,tolower(have$column_name)),,drop=FALSE]
        if(!col %in% tolower(have$column_name) || .assignmentBlockedColumn(col, have)) .dateReject("Component source is not a permitted registered integer column.","date_source_invalid")
      }
      p<-.reducePersonFrame(ctx,cons,extra_columns=cols)
      data<-p[match(as.character(roster$cohort_row_id),as.character(p$row_key)),,drop=FALSE]
    } else {
      src<-.reduceSource(ctx,source$event)
      extra<-list()
      for(col in cols) {
        res<-.predicateResolve(src$catalogue,col,"date components")
        if(!res$type %in% c("integer","bigint")) .dateReject("Component columns must have integer type.","date_source_invalid")
        extra[[col]]<-list(expr=paste0("e.",res$column),type=res$type)
      }
      stream<-.reduceStream(ctx,cons,src,source$search,.reduceName(ctx,cons,"dc",.dateSourceKey(source)),extra=extra)
      rows<-.reduceStreamRows(ctx,cons,stream,source$search,"date components")$rows
      ordering<-.reduceSourceOrdering(src);rows<-rows[.reduceOrderRows(rows,c("row_key",intersect(ordering,names(rows)))),,drop=FALSE]
      groups<-.reduceGroups(rows$row_key,roster$cohort_row_id)
      at<-vapply(groups,function(ix) { if(!length(ix)) return(NA_integer_);k<-switch(source$which,first=1L,last=length(ix),nth=source$n);if(k>length(ix))NA_integer_ else ix[k]},1L)
      data<-rows[at,,drop=FALSE]
    }
    values<-lapply(operands,function(x) {
      if(is.null(x)) return(rep(NA_integer_,n))
      if(x$kind=="integer") return(rep(x$value,n))
      v<-data[[x$name]]
      if(!is.numeric(v)&&!inherits(v,"integer64")) .dateReject("Component columns must have integer type.","date_source_invalid")
      as.numeric(v)
    })
    return(.dateComponents(values$year,values$month,values$day,s))
  }
  if(is.list(source)&&!is.null(source$date)) {
    if(!.dateISO(source$date)) .dateReject("Invalid fixed date source.","date_source_invalid")
    d<-rep(as.Date(source$date),n)
  } else {
    # Missing builtin anchors are missing values, not a reason to lose a person.
    if(is.character(source)&&! .ANCHOR_BUILTINS[[source]] %in% names(roster)) d<-as.Date(rep(NA_character_,n)) else d<-.reduceAnchorDates(ctx,cons,source,"date source")
  }
  d[!is.na(d)&(d<as.Date("0001-01-01")|d>as.Date("9999-12-31"))]<-NA
  if(s$future=="na") d[!is.na(d)&d>as.Date(s$as_of)]<-NA
  list(lo=d,hi=d)
}
.dateBirthSource <- function() list(kind="date_components",year=list(kind="column",name="year_of_birth"),month=list(kind="column",name="month_of_birth"),day=list(kind="column",name="day_of_birth"),which="first",n=1L,scope="person")
.dateExact <- function(x) { d<-x$lo;d[is.na(x$hi)|d!=x$hi]<-NA;d }
.dateResultType <- function(s,d) {
  if(d$kind %in% c("time_between","age_at")) return(if(isTRUE(d$bands))"text" else if(d$unit %in% c("weeks","decimal_years"))"numeric" else "integer")
  probe<-list(lo=as.Date("2024-02-29"),hi=as.Date("2024-02-29"))
  val<-.datePresent(probe,s,d$part,as.Date("1970-01-01"))
  if(inherits(val,"Date"))"date" else if(is.factor(val))"text" else if(is.integer(val))"integer" else "numeric"
}
.reduceDateVariable <- function(ctx,node,name,cons) {
  d<-node$derived;s<-node$date_spec %||% .dateEffective(NULL,ctx$ir$options$date_spec,d$kind,d$part)
  type<-.dateResultType(s,d);column<-.fragmentColumn(type,TRUE,"measure")
  attr(column,"date_spec")<-s;attr(column,"privacy_class")<-2L
  evaluate<-function() {
    roster<-.reduceRosterFrame(ctx,cons$roster)
    if(d$kind %in% c("calendar_date","date_part")) {
      source<-.dateSourceValues(ctx,cons,d$reference,s)
      anchor<-if(identical(s$precision,"relative_days")) .dateExact(.dateSourceValues(ctx,cons,s$anchor,s)) else NULL
      value<-.datePresent(source,s,d$part,anchor)
    } else {
      a<-.dateSourceValues(ctx,cons,if(d$kind=="age_at") .dateBirthSource() else d$from,s)
      b<-.dateSourceValues(ctx,cons,if(d$kind=="age_at") d$reference else d$to,s)
      from<-.dateExact(a);to<-.dateExact(b)
      if(d$kind=="age_at"&&d$unit=="annual_years") {
        value<-.dateY(to)-.dateY(a$lo);value[!is.na(to)&!is.na(a$lo)&to<a$lo]<-NA_integer_
      } else value<-.dateElapsed(from,to,d$unit,isTRUE(d$absolute),d$rounding %||% "none")
      if(d$kind=="age_at") {
        value[!is.na(value)&value<0]<-NA
        if(isTRUE(d$bands)) {
          breaks<-as.integer(unlist(d$age_breaks));labels<-c(paste0(head(breaks,-1),"-",tail(breaks,-1)-1L),paste0(tail(breaks,1),"+"))
          keys<-as.character(cut(value,c(breaks,Inf),right=FALSE,labels=labels))
          value<-.dateFactor(keys,labels,s)
        }
      }
    }
    out<-data.frame(row_key=roster$cohort_row_id);out[[name]]<-value;out
  }
  list(stream=NULL,reducer=NULL,declared_empty_value=NA,columns=setNames(list(column),name),evaluate=evaluate,available=TRUE)
}

.datePresentRecord <- function(record,node,ctx,name,cons) {
  if(is.null(node$date_spec)||(node$derived$kind %||% "") %in% .DATE_KINDS) return(record)
  if(!all(vapply(record$columns,function(c) identical(c$type,"date"),logical(1)))) .dateReject("date_spec requires a date-valued source; numeric measures are not timestamps.")
  s<-node$date_spec;old<-record$evaluate
  record$evaluate<-function() {
    out<-old();anchor<-if(identical(s$precision,"relative_days")) .dateExact(.dateSourceValues(ctx,cons,s$anchor,s)) else NULL
    for(col in names(record$columns)) out[[col]]<-.datePresent(list(lo=out[[col]],hi=out[[col]]),s,anchor=anchor)
    out
  }
  for(col in names(record$columns)) {
    record$columns[[col]]$type<-.dateResultType(s,list(kind="calendar_date"))
    attr(record$columns[[col]],"date_spec")<-s;attr(record$columns[[col]],"privacy_class")<-2L
  }
  record
}

.dateSourceKey <- function(source) substr(as.character(openssl::sha256(charToRaw(.jcsCanonical(source)))),1L,16L)
.dateComponentSources <- function(x) {
  if(!is.list(x)) return(list())
  if(identical(x$kind,"date_components")) return(list(x))
  unlist(lapply(x,.dateComponentSources),recursive=FALSE)
}
.datePersonColumns <- function(ir) {
  sources<-Filter(function(x)is.null(x$event),.dateComponentSources(ir))
  unique(unlist(lapply(sources,function(x) unlist(lapply(x[c("year","month","day")],function(v)if(identical(v$kind,"column"))v$name))),use.names=FALSE))
}
