# A-28. Shared closed date declarations and private Gregorian arithmetic.
.DATE_PRECISIONS <- c("day", "year", "semester", "quarter", "month", "iso_week", "cdc_week", "epi_week", "fiscal_year", "fiscal_semester", "fiscal_quarter", "fiscal_month", "relative_days", "custom")
.DATE_PARTS <- c("year", "semester", "quarter", "month", "month_name", "month_abbr", "day", "weekday", "weekday_name", "weekday_abbr", "iso_week", "iso_year", "day_of_year", "epi_week", "epi_year", "fiscal_year", "fiscal_quarter", "season", "month_sin", "month_cos", "weekday_sin", "weekday_cos", "season_sin", "season_cos")
.DATE_KINDS <- c("calendar_date", "date_part", "time_between", "age_at")
.dateReject <- function(message, reason = "date_spec_invalid") {
  stop(structure(list(message = paste0(reason, ": ", message), call = NULL,
    code = "validation_error", reason = reason), class = c("omop_date_error", "dsomop_plan_rejection", "error", "condition")))
}
.dateISO <- function(x) {
  if (!is.character(x) || anyNA(x) || any(!grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}$", x))) return(FALSE)
  y <- suppressWarnings(as.Date(x, format = "%Y-%m-%d"))
  all(!is.na(y) & sprintf("%04d-%02d-%02d",as.integer(format(y,"%Y")),as.integer(format(y,"%m")),as.integer(format(y,"%d"))) == x & substr(x, 1, 4) != "0000")
}
.datePatternTokens <- function(p) {
  bad <- function() .dateReject("Use allowed calendar directives and ASCII separators; ISO weeks require %G-W%V.", "date_pattern_invalid")
  if (!is.character(p) || length(p) != 1L || is.na(p) || nchar(p, type="bytes") > 128L || !nzchar(p) || !grepl("%", p, fixed=TRUE)) bad()
  chars <- strsplit(p,"",fixed=TRUE)[[1L]]; tokens <- character(); i <- 1L
  while(i <= length(chars)) {
    ch <- chars[i]
    if(ch == "%") {
      i <- i+1L
      if(i > length(chars) || !chars[i] %in% c("Y","m","d","j","G","V","u","b","B","a","A","%")) bad()
      tokens <- c(tokens,setNames(chars[i],"directive"))
    } else {
      if(!grepl("^[a-zA-Z0-9 _/.,:()-]$",ch)) bad()
      tokens <- c(tokens,setNames(ch,"literal"))
    }
    i <- i+1L
  }
  directives <- tokens[names(tokens)=="directive"]
  if(("V" %in% directives) != ("G" %in% directives) ||
     ("V" %in% directives && "Y" %in% directives)) bad()
  tokens
}
.datePattern <- function(p) { .datePatternTokens(p); p }
.dateSpecCheck <- function(s) {
  if (is.null(s)) return(NULL)
  if (!is.list(s) || (length(s) && (is.null(names(s)) || anyDuplicated(names(s))))) .dateReject("date_spec must be a named declaration.")
  enums <- list(precision=.DATE_PRECISIONS, representation=c("auto","factor","number","start","end"), numeric=c("index","decimal_year"),
    hemisphere=c("north","south"), partial=c("none","start","mid","end"), invalid="na", future=c("allow","na"))
  allowed <- c(names(enums), "origin","anchor","pattern","labels","level_range","fiscal_start","week_start","week_min_days","as_of")
  if (length(setdiff(names(s),allowed))) .dateReject("Unknown date_spec field.")
  for (k in names(s)) {
    v <- s[[k]]
    if (is.null(v)) .dateReject("Omit NULL date_spec fields.")
    if (k %in% names(enums) && (!is.character(v) || length(v)!=1L || is.na(v) || !v %in% enums[[k]])) .dateReject(paste("Invalid", k, "option."))
    if (k %in% c("origin","as_of") && (length(v)!=1L || !.dateISO(v))) .dateReject(paste(k,"must be a valid fixed ISO date."))
    if (k %in% c("fiscal_start","week_start","week_min_days") && (!is.numeric(v) || length(v)!=1L || is.na(v) || v!=trunc(v) || v<1 || v>if(k=="fiscal_start")12 else 7)) .dateReject(paste("Invalid",k))
    if (k=="pattern") .datePattern(v)
    if (k=="level_range" && (length(v)!=2L || !.dateISO(unlist(v)) || v[[1]]>v[[2]])) .dateReject("level_range needs two ascending ISO dates.")
    if (k=="labels") {
      v <- unlist(v)
      if (!is.character(v) || is.null(names(v)) || anyNA(v) || any(!nzchar(v)) || any(!nzchar(names(v))) || anyDuplicated(names(v))) .dateReject("labels must map unique calendar keys to nonempty labels.")
    }
  }
  s
}
.dateEffective <- function(variable=NULL, recipe=NULL, kind="calendar_date", part=NULL) {
  variable <- .dateSpecCheck(variable); recipe <- .dateSpecCheck(recipe)
  overlay <- function(a,b) { for(k in names(b)) a[[k]] <- b[[k]]; a }
  raw <- overlay(recipe %||% list(), variable %||% list())
  p <- raw$precision %||% "day"; r <- raw$representation %||% "auto"
  applicable <- c("partial","invalid","future","as_of")
  if (kind=="calendar_date") {
    applicable <- c(applicable,"precision","representation")
    if(p=="relative_days") applicable <- c(applicable,"anchor")
    if(p=="custom") applicable <- c(applicable,"pattern")
    if(grepl("^fiscal_",p)) applicable <- c(applicable,"fiscal_start")
    if(p=="epi_week") applicable <- c(applicable,"week_start","week_min_days")
    if(r=="number" && !p %in% c("day","relative_days","custom")) {
      applicable <- c(applicable,"numeric")
      if(!p %in% c("year","fiscal_year") && (raw$numeric %||% "index")=="index") applicable <- c(applicable,"origin")
    }
    if(r=="factor" || (r=="auto" && !p %in% c("day","relative_days"))) applicable <- c(applicable,"labels","level_range")
    if(p=="day" && !r %in% c("auto","factor","start","end") || p=="relative_days" && !r %in% c("auto","number") || p=="custom" && !r %in% c("auto","factor")) .dateReject("Representation is incompatible with precision.")
  } else if(kind=="date_part") {
    applicable <- c(applicable,"representation")
    if(grepl("^epi_",part)) applicable <- c(applicable,"week_start","week_min_days")
    if(grepl("^fiscal_",part)) applicable <- c(applicable,"fiscal_start")
    if(grepl("^season",part)) applicable <- c(applicable,"hemisphere")
    seasonal <- part %in% c("month","month_name","month_abbr","weekday","weekday_name","weekday_abbr","season")
    # Calendar recipe representation is a default only when applicable to a part.
    r <- variable$representation %||% if((recipe$representation %||% "auto") %in% c("auto","number")) recipe$representation else "auto"
    r <- r %||% "auto"; raw$representation <- r
    if(!r %in% c("auto","number",if(seasonal)"factor")) .dateReject("Date parts support auto/number; seasonality also supports factor.")
    if(seasonal && r!="number") applicable <- c(applicable,"labels")
  }
  irrelevant <- setdiff(names(variable), applicable)
  if(length(irrelevant)) .dateReject(paste("Irrelevant date_spec field:",paste(irrelevant,collapse=", ")))
  s <- list(partial="none",invalid="na",future="allow")
  if(kind=="calendar_date") {
    s$precision <- p; s$representation <- r
    if("numeric" %in% applicable) s$numeric <- "index"
    if("origin" %in% applicable) s$origin <- "1970-01-01"
    if("anchor" %in% applicable) s$anchor <- "index"
  }
  if(kind=="date_part") s$representation <- r
  if("fiscal_start" %in% applicable) s$fiscal_start <- 1L
  if("hemisphere" %in% applicable) s$hemisphere <- "north"
  s <- overlay(s,raw[intersect(names(raw),applicable)])
  if(s$future=="na" && is.null(s$as_of) || s$future=="allow" && !is.null(s$as_of)) .dateReject("future=na requires as_of; future=allow must omit as_of.")
  if("week_start" %in% applicable && (is.null(s$week_start)||is.null(s$week_min_days))) .dateReject("Epidemiological weeks require week_start and week_min_days.")
  if(kind=="calendar_date" && p=="custom" && is.null(s$pattern)) .dateReject("custom precision requires a pattern.","date_pattern_invalid")
  s
}
.dateMake <- function(y,m=1L,d=1L) {
  y[!is.finite(y)|y<1|y>9999|y!=trunc(y)] <- NA_real_
  m[!is.finite(m)|m<1|m>12|m!=trunc(m)] <- NA_real_
  d[!is.finite(d)|d<1|d>31|d!=trunc(d)] <- NA_real_
  x <- sprintf("%04d-%02d-%02d",as.integer(y),as.integer(m),as.integer(d))
  out <- suppressWarnings(as.Date(x, format="%Y-%m-%d"))
  out[is.na(y)|is.na(m)|is.na(d)|y<1|y>9999|m<1|m>12|d<1|d>31|is.na(out)|sprintf("%04d-%02d-%02d",.dateY(out),.dateM(out),.dateD(out))!=x] <- NA
  out
}
.dateY <- function(d) as.integer(format(d,"%Y"))
.dateM <- function(d) as.integer(format(d,"%m"))
.dateD <- function(d) as.integer(format(d,"%d"))
.dateW <- function(d) as.integer((as.numeric(d)+3) %% 7+1)
.dateMonthEnd <- function(y,m) {
  days <- c(31L,28L,31L,30L,31L,30L,31L,31L,30L,31L,30L,31L)[pmax(1L,pmin(12L,m))]
  days <- days + as.integer(m==2 & y%%4==0 & (y%%100!=0 | y%%400==0))
  .dateMake(y,m,days)
}
.dateYearStart <- function(y) {
  # Internal adjacent years are needed for week-year/anniversary arithmetic;
  # they never become a public Date outside the native domain.
  d <- .dateMake(pmax(1L,pmin(9999L,y)),1L,1L)
  d + ifelse(y==0L,-366L,ifelse(y==10000L,365L,0L))
}
.dateComponents <- function(y,m,d,s) {
  n <- max(length(y),length(m),length(d)); y<-rep_len(y,n);m<-rep_len(m,n);d<-rep_len(d,n)
  lo <- .dateMake(y,ifelse(is.na(m),1,m),ifelse(is.na(d),1,d))
  hi <- lo; unknown <- is.na(d)
  hi[unknown] <- .dateMonthEnd(y[unknown],ifelse(is.na(m[unknown]),12,m[unknown]))
  # Date 9999-12-31 does not need constructing the out-of-domain next year.
  hi[unknown & y==9999 & (is.na(m)|m==12)] <- as.Date("9999-12-31")
  bad <- is.na(y)|(!is.na(d)&is.na(m))|(!is.na(y)&y!=trunc(y))|(!is.na(m)&m!=trunc(m))|(!is.na(d)&d!=trunc(d))|is.na(lo)|is.na(hi)
  lo[bad]<-NA;hi[bad]<-NA
  if(s$partial!="none") {
    lo <- switch(s$partial,start=lo,end=hi,mid=lo+floor(as.numeric(hi-lo)/2)); hi<-lo
  }
  if(s$future=="na") { bad<-!is.na(hi)&hi>as.Date(s$as_of);lo[bad]<-NA;hi[bad]<-NA }
  list(lo=lo,hi=hi)
}
.dateWeek <- function(d,start=1L,min_days=4L) {
  first <- function(y) { jan <- .dateYearStart(y); jan-((.dateW(jan)-start)%%7)+ifelse(7-(.dateW(jan)-start)%%7<min_days,7,0) }
  y <- .dateY(d); f<-first(y); prev<-!is.na(d)&d<f; y[prev]<-y[prev]-1L
  nextf<-first(y+1L); nexty<-!is.na(d)&!is.na(nextf)&d>=nextf;y[nexty]<-y[nexty]+1L
  f<-first(y); num<-as.integer(as.numeric(d-f)%/%7+1L)
  list(year=y,number=num,start=d-(.dateW(d)-start)%%7,weeks=as.integer((first(y+1L)-f)/7))
}
.datePeriod <- function(d,s) {
  p<-s$precision;y<-.dateY(d);m<-.dateM(d);fy<-y;fm<-m
  fiscal<-grepl("^fiscal_",p)
  if(fiscal) { fs<-s$fiscal_start;fy<-y-as.integer(m<fs);fm<-(m-fs)%%12+1L }
  kind<-sub("^fiscal_","",p)
  if(kind %in% c("year","semester","quarter","month")) {
    size<-switch(kind,year=12L,semester=6L,quarter=3L,month=1L);num<-(fm-1L)%/%size+1L
    sm<-if(fiscal) s$fiscal_start+(num-1L)*size else (num-1L)*size+1L
    sy<-fy+(sm-1L)%/%12L;sm<-(sm-1L)%%12L+1L
    st<-.dateMake(sy,sm,1);em<-sm+size-1L;ey<-sy+(em-1L)%/%12L;em<-(em-1L)%%12L+1L
    en<-.dateMonthEnd(ey,em);en[ey==9999 & em==12 & !is.na(ey)]<-as.Date("9999-12-31")
    key<-if(fiscal) switch(kind,year=sprintf("%04d-FY",fy),semester=sprintf("%04d-FS%d",fy,num),quarter=sprintf("%04d-FQ%d",fy,num),month=sprintf("%04d-FM%02d",fy,num)) else
      switch(kind,year=sprintf("%04d",y),semester=sprintf("%04d-S%d",y,num),quarter=sprintf("%04d-Q%d",y,num),month=sprintf("%04d-%02d",y,m))
    out<-list(key=key,start=st,end=en,year=fy,number=num,periods=12/size)
  } else if(p %in% c("iso_week","cdc_week","epi_week")) {
    w<-.dateWeek(d,if(p=="iso_week")1L else if(p=="cdc_week")7L else s$week_start,if(p=="epi_week")s$week_min_days else 4L)
    out<-list(key=sprintf("%04d-W%02d",w$year,w$number),start=w$start,end=w$start+6,year=w$year,number=w$number,periods=w$weeks)
  } else out<-list(key=sprintf("%04d-%02d-%02d",y,m,.dateD(d)),start=d,end=d,year=y,number=1L,periods=1L)
  out$key[is.na(d)]<-NA_character_
  for(k in c("start","end")) out[[k]][!is.na(out[[k]]) & (out[[k]]<as.Date("0001-01-01")|out[[k]]>as.Date("9999-12-31"))]<-NA
  out
}
.DATE_MONTHS <- c("January","February","March","April","May","June","July","August","September","October","November","December")
.DATE_DAYS <- c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday")
.dateRender <- function(d,p,tokens=.datePatternTokens(p)) {
  if(!length(d)) return(character())
  w<-.dateWeek(d);m<-.dateM(d);u<-.dateW(d)
  values<-list(Y=sprintf("%04d",.dateY(d)),m=sprintf("%02d",m),d=sprintf("%02d",.dateD(d)),j=sprintf("%03d",as.integer(d-.dateMake(.dateY(d),1,1))+1L),G=sprintf("%04d",w$year),V=sprintf("%02d",w$number),u=as.character(u),b=substr(.DATE_MONTHS[m],1,3),B=.DATE_MONTHS[m],a=substr(.DATE_DAYS[u],1,3),A=.DATE_DAYS[u],"%"=rep("%",length(d)))
  out<-rep("",length(d))
  for(i in seq_along(tokens)) out<-paste0(out,if(names(tokens)[i]=="directive") values[[tokens[i]]] else tokens[i])
  out[is.na(d)]<-NA_character_;out
}
.dateFactor <- function(keys,levels,s,order=NULL,public=levels) {
  levels<-unique(levels[!is.na(levels)]);map<-unlist(s$labels %||% list())
  if(length(setdiff(names(map),levels))) .dateReject("Label keys are outside the declared date category domain.")
  labels<-levels;if(length(map)) labels[levels %in% names(map)]<-unname(map[levels[levels %in% names(map)]])
  if(anyDuplicated(labels)) .dateReject("Date labels must remain unique, including unmapped levels.")
  out<-ordered(labels[match(keys,levels)],levels=labels)
  class(out)<-c("dsomop_date_factor","ordered","factor")
  attr(out,"omop_date_contract")<-list(keys=keys,levels=levels,labels=labels,spec=s,order=order %||% seq_along(levels),declared=unique(public))
  out
}
.datePart <- function(d,part,s) {
  y<-.dateY(d);m<-.dateM(d);w<-.dateW(d);season<-(m%%12)%/%3+1L
  iso<-.dateWeek(d)
  val<-switch(part,year=y,semester=(m-1L)%/%6L+1L,quarter=(m-1L)%/%3L+1L,day=.dateD(d),
    month=m,month_name=m,month_abbr=m,weekday=w,weekday_name=w,weekday_abbr=w,season=season,
    iso_week=iso$number,iso_year=iso$year,day_of_year=as.integer(d-.dateMake(y,1,1))+1L,
    epi_week=.dateWeek(d,s$week_start,s$week_min_days)$number,epi_year=.dateWeek(d,s$week_start,s$week_min_days)$year,
    fiscal_year=y-as.integer(m<s$fiscal_start),fiscal_quarter=((m-s$fiscal_start)%%12)%/%3L+1L,
    month_sin=sin(2*pi*(m-1)/12),month_cos=cos(2*pi*(m-1)/12),weekday_sin=sin(2*pi*(w-1)/7),weekday_cos=cos(2*pi*(w-1)/7),season_sin=sin(2*pi*(season-1)/4),season_cos=cos(2*pi*(season-1)/4))
  lv<-switch(part,month=sprintf("%02d",1:12),month_name=.DATE_MONTHS,month_abbr=substr(.DATE_MONTHS,1,3),weekday=as.character(1:7),weekday_name=.DATE_DAYS,weekday_abbr=substr(.DATE_DAYS,1,3),season=if((s$hemisphere %||% "north")=="north")c("Winter","Spring","Summer","Autumn") else c("Summer","Autumn","Winter","Spring"),NULL)
  if(!is.null(lv) && s$representation!="number") return(.dateFactor(lv[val],lv,s))
  if(grepl("_(sin|cos)$",part)) val else as.integer(val)
}
.datePresent <- function(interval,s,part=NULL,anchor=NULL) {
  lo<-interval$lo;hi<-interval$hi
  if(!is.null(part)) {
    out<-.datePart(lo,part,s)
    # Parts may cycle within a partial interval, so equality of endpoints alone is insufficient.
    partial<-which(!is.na(lo)&!is.na(hi)&lo!=hi)
    for(i in partial) { all<-.datePart(seq(lo[i],hi[i],by="day"),part,s);if(length(unique(as.character(all)))!=1L) out[i]<-NA }
    return(out)
  }
  p<-s$precision;r<-s$representation
  if(p=="relative_days") {out<-as.integer(lo-anchor);out[is.na(hi)|lo!=hi]<-NA_integer_;return(out)}
  if(p=="day" && r!="factor") {lo[is.na(hi)|lo!=hi]<-NA;return(lo)}
  period<-.datePeriod(lo,s);endperiod<-.datePeriod(hi,s)
  tokens<-if(p=="custom") .datePatternTokens(s$pattern) else NULL
  keys<-if(p=="custom") .dateRender(lo,tokens=tokens) else period$key
  invalid<-is.na(lo)|is.na(hi)
  if(p=="custom") {
    for(i in which(!invalid & lo!=hi)) if(length(unique(.dateRender(seq(lo[i],hi[i],by="day"),tokens=tokens)))!=1L) invalid[i]<-TRUE
  } else invalid<-invalid|is.na(period$key)|period$key!=endperiod$key
  keys[invalid]<-NA_character_
  if(r %in% c("start","end")) {out<-period[[r]];out[invalid]<-NA;return(out)}
  if(r=="number") {
    out<-if(p %in% c("year","fiscal_year")) as.integer(period$year) else if(s$numeric=="decimal_year") period$year+(period$number-1)/period$periods else {
      origin<-.datePeriod(as.Date(s$origin),s)
      if(grepl("week",p)) as.integer((period$start-origin$start)/7) else as.integer(((.dateY(period$start)-.dateY(origin$start))*12+.dateM(period$start)-.dateM(origin$start))/(12/period$periods))
    };out[invalid]<-NA;return(out)
  }
  public<-character()
  if(!is.null(s$level_range)) {
    days<-seq(as.Date(s$level_range[[1]]),as.Date(s$level_range[[2]]),by="day")
    public<-if(p=="custom") .dateRender(days,tokens=tokens) else .datePeriod(days,s)$key
  }
  levels<-unique(c(keys[!is.na(keys)],public,names(unlist(s$labels))))
  if(p=="custom") {
    hasyear<-any(tokens[names(tokens)=="directive"] %in% c("Y","G"))
    if(hasyear) {
      years<-unique(c(.dateY(lo),.dateY(hi)))
      # Public levels include every intervening year, even with no observed rows.
      if(!is.null(s$level_range)) {
        range_years<-.dateY(as.Date(unlist(s$level_range)))
        years<-union(years,seq.int(range_years[1L],range_years[2L]))
      }
      if(length(s$labels)) years<-union(years,.dateKeyYears(names(unlist(s$labels)),tokens=tokens))
      # ISO week-years can cross calendar boundaries; keep this calendar sorted.
      years<-sort(unique(c(years-1L,years,years+1L)))
      years<-years[!is.na(years)&years>=1L&years<=9999L]
    } else {
      # A-28 fixes the first-appearance order to this exact Gregorian cycle.
      years<-2000:2399
      if(length(s$labels)) .dateKeyYears(names(unlist(s$labels)),tokens=tokens)
    }
    calendar<-if(length(years)) do.call(c,lapply(years,function(y) seq(.dateMake(y,1,1),.dateMake(y,12,31),by="day"))) else as.Date(character())
    rendered<-.dateRender(calendar,tokens=tokens);reference<-unique(rendered);levels<-levels[order(match(levels,reference),levels)]
    ranks<-as.numeric(calendar[match(levels,rendered)])
    if(anyNA(ranks)) .dateReject("Custom label keys are not valid rendered calendar dates.")
  } else {
    if(length(s$labels)) {
      mapkeys<-names(unlist(s$labels));years<-suppressWarnings(as.integer(substr(mapkeys,1,4)))
      if(anyNA(years)||any(years<1|years>9999)) .dateReject("Invalid canonical calendar label key.")
      years<-unique(pmax(1L,pmin(9999L,c(years-1L,years,years+1L))))
      calendar<-do.call(c,lapply(years,function(y)seq(.dateMake(y,1,1),.dateMake(y,12,31),by="day")))
      if(!all(mapkeys %in% .datePeriod(calendar,s)$key)) .dateReject("Invalid canonical calendar label key.")
    }
    levels<-sort(levels,method="radix")
  }
  .dateFactor(keys,levels,s,if(p=="custom")ranks else NULL,unique(c(public,names(unlist(s$labels)))))
}
.dateElapsed <- function(from,to,unit="days",absolute=FALSE,rounding="none") {
  delta<-as.numeric(to-from);lo<-pmin(from,to);hi<-pmax(from,to);sgn<-sign(delta)
  anniversary<-function(y) {
    m<-.dateM(lo);d<-.dateD(lo);leap<-y%%4==0&(y%%100!=0|y%%400==0)
    feb<-m==2&d==29&!leap;m[feb]<-3L;d[feb]<-1L;.dateMake(y,m,d)
  }
  out<-switch(unit,days=delta,weeks=delta/7,months={
    n<-(.dateY(hi)-.dateY(lo))*12+.dateM(hi)-.dateM(lo)
    ann<-.dateMake(.dateY(hi),.dateM(hi),pmin(.dateD(lo),.dateD(.dateMonthEnd(.dateY(hi),.dateM(hi)))))
    (n-as.integer(hi<ann))*sgn
  }, years=,decimal_years={
    n<-.dateY(hi)-.dateY(lo);ann<-anniversary(.dateY(lo)+n);n<-n-as.integer(hi<ann)
    if(unit=="years") n*sgn else {last<-anniversary(.dateY(lo)+n);last[n==0&!is.na(n)]<-lo[n==0&!is.na(n)];nexta<-anniversary(.dateY(lo)+n+1);(n+as.numeric(hi-last)/as.numeric(nexta-last))*sgn}
  })
  if(absolute) out<-abs(out)
  out<-switch(rounding,none=out,floor=floor(out),ceiling=ceiling(out),trunc=trunc(out),nearest=round(out))
  if(unit %in% c("days","months","years")) as.integer(out) else as.numeric(out)
}
.datePlanNormalize <- function(x) {
  .dateSourceCheck(x$variables)
  recipe<-x$options$date_spec
  .dateSpecCheck(recipe)
  for(nm in names(x$variables)) {
    v<-x$variables[[nm]];k<-v$derived$kind %||% "calendar_date"
    native<-k %in% .DATE_KINDS && !is.null(v$derived) || !is.null(v$date_spec) || (!is.null(recipe)&&(v$reducer$fn %||% "") %in% c("first_date","last_date"))
    if(!native) next
    v$date_spec<-.dateEffective(v$date_spec,recipe,k,v$derived$part)
    if(identical(k,"age_at")) {
      d<-v$derived
      if(isTRUE(d$bands)) {
        if(!d$unit %in% c("years","annual_years")) .dateReject("Bands require years or annual_years.")
        b<-unlist(d$age_breaks %||% as.list(seq(0L,100L,5L)))
        if(length(b)<2L||b[1]!=0||anyNA(b)||any(diff(b)<=0)||any(b!=trunc(b))) .dateReject("Invalid age band grid.")
        v$derived$age_breaks<-unname(as.list(b))
      } else if(!is.null(d$age_breaks)) .dateReject("age_breaks requires bands=TRUE.")
    }
    x$variables[[nm]]<-v
  }
  for(nm in names(x$outputs)) {
    o<-x$outputs[[nm]];dh<-o$options$date_handling
    vars<-as.character(unlist(o$columns %||% names(x$variables)))
    native<-vars[vapply(x$variables[vars],function(v)!is.null(v$date_spec),logical(1))]
    if(!is.null(dh) && (length(native)||!is.null(recipe))) {
      targets<-unlist(dh$date_columns)
      if(!is.null(recipe)||!length(targets)||length(intersect(targets,native))) .dateReject("Explicit date_handling overlaps native presentation; express that policy in date_spec.","date_policy_conflict")
    }
    if(is.null(dh)&&!length(native)&&is.null(recipe)&&o$format %in% c("wide","long","membership")) {
      o$options$date_handling<-list(mode="relative",reference="index",bin_width=NULL,date_columns=NULL)
    }
    x$outputs[[nm]]<-o
  }
  x
}

.dateResolveRecords <- function(records, calendar = NULL) {
  if(!all(vapply(records,function(x)identical(x$kind,"date"),logical(1)))) .dateReject("Mixed calendar and concept metadata.")
  specs<-c(if(!is.null(calendar))list(calendar),lapply(records,function(x)x$calendar))
  if(!length(specs)) .dateReject("Missing public calendar identity.")
  canonical<-vapply(specs,.jcsCanonical,"")
  if(length(unique(canonical))>1L) .dateReject("Calendar/policy mismatch across retained sites.")
  keys<-unique(vapply(records,function(x)x$id,""))
  labels<-vapply(keys,function(k) {ls<-unique(vapply(Filter(function(x)x$id==k,records),function(x)x$label,""));if(length(ls)!=1L).dateReject("Inconsistent date labels.");ls},"")
  order<-vapply(keys,function(k) min(vapply(Filter(function(x)x$id==k,records),function(x)as.numeric(x$order),0)),0)
  p<-specs[[1L]]$precision
  ix<-if(!is.null(p)&&p!="custom") order(keys,method="radix") else order(order,keys,method="radix");keys<-keys[ix];labels<-labels[ix]
  if(anyDuplicated(labels)) .dateReject("Date labels cannot merge periods.")
  list(ids=keys,labels=unname(labels),levels=unname(labels),vocabulary=unname(labels),kind="date",calendar=specs[[1L]],decisions=list())
}
.dateValidateResolution <- function(shared,records) {
  if(!identical(shared$kind,"date") || is.null(shared$calendar)) .dateReject("Missing shared calendar identity.")
  if(any(!vapply(records,function(r)identical(.jcsCanonical(r$calendar),.jcsCanonical(shared$calendar)),logical(1)))) .dateReject("Changed shared calendar identity.")
  ids<-unlist(shared$ids);labels<-unlist(shared$labels);levels<-unlist(shared$levels)
  if(length(ids)!=length(labels)||anyNA(ids)||anyNA(labels)||anyDuplicated(ids)||anyDuplicated(labels)||!identical(unname(labels),unname(levels))) .dateReject("Invalid shared calendar levels.")
  for(r in records) {
    i<-match(r$id,ids)
    if(is.na(i)||labels[i]!=r$label) .dateReject("Incomplete or changed calendar identities.")
  }
  invisible(TRUE)
}

.dateKeyYears <- function(keys,pattern,tokens=.datePatternTokens(pattern)) {
  re<-"^";has<-FALSE
  expressions<-c(Y="([0-9]{4})",G="([0-9]{4})",m="[0-9]{2}",d="[0-9]{2}",j="[0-9]{3}",V="[0-9]{2}",u="[1-7]",b="[A-Za-z]{3}",B="[A-Za-z]+",a="[A-Za-z]{3}",A="[A-Za-z]+","%"="%")
  for(i in seq_along(tokens)) {
    ch<-tokens[i]
    if(names(tokens)[i]=="directive") {
      re<-paste0(re,expressions[[ch]])
      if(ch %in% c("Y","G")) has<-TRUE
    } else re<-paste0(re,if(ch %in% c(".","(",")"))paste0("\\",ch) else ch)
  }
  re<-paste0(re,"$")
  if(any(!grepl(re,keys))) .dateReject("Label keys must match the declared calendar pattern.")
  if(!has) return(2000:2399)
  unique(as.integer(unlist(lapply(regmatches(keys,regexec(re,keys)),function(x)x[-1]))))
}
.dateSourceCheck <- function(x) {
  if(!is.list(x)) return(invisible(TRUE))
  if(identical(names(x),"date") && !.dateISO(x$date)) .dateReject("Use a valid Gregorian ISO date.","date_source_invalid")
  if(identical(x$kind,"date_components")) {
    if((x$which %||% "first")!="nth"&&(x$n %||% 1L)!=1L) .dateReject("Nondefault component n requires nth.","date_source_invalid")
    if(is.null(x$year)) .dateReject("A year component is required.","date_source_invalid")
  }
  for(v in x) .dateSourceCheck(v)
  invisible(TRUE)
}

.dateNeedsPresentation <- function(plan) {
  !is.null(plan$options$date_spec) || any(vapply(plan$variables,function(v)!is.null(v$date_spec)||(v$derived$kind %||% "") %in% .DATE_KINDS,logical(1)))
}
