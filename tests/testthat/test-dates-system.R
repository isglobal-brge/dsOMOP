.ds <- function(...) .dateEffective(list(...))
.dp <- function(d,s=.ds(),part=NULL) .datePresent(list(lo=as.Date(d),hi=as.Date(d)),s,part)
test_that("D01-D14 Gregorian built-ins, boundaries and all representations", {
  expect_identical(.dp(c("0001-01-01","0004-02-29","0999-12-31","1000-01-01",NA)),as.Date(c("0001-01-01","0004-02-29","0999-12-31","1000-01-01",NA)))
  fixtures<-list(year=c("2024-02-29","2024"),semester=c("2024-07-01","2024-S2"),quarter=c("2024-04-01","2024-Q2"),month=c("2024-02-29","2024-02"),iso_week=c("2021-01-01","2020-W53"),cdc_week=c("2024-12-29","2025-W01"),epi_week=c("2021-01-01","2020-W53"),fiscal_year=c("2024-03-31","2023-FY"),fiscal_semester=c("2024-04-01","2024-FS1"),fiscal_quarter=c("2024-04-01","2024-FQ1"),fiscal_month=c("2024-04-01","2024-FM01"))
  for(p in names(fixtures)) {
    args<-list(precision=p)
    if(p=="epi_week") args<-c(args,list(week_start=1L,week_min_days=4L))
    if(grepl("^fiscal",p)) args$fiscal_start<-4L
    spec<-.dateEffective(args)
    value<-.dp(fixtures[[p]][1],spec)
    expect_true(is.ordered(value),info=p);expect_identical(as.character(value),fixtures[[p]][2],info=p)
    for(r in c("start","end","number")) {
      a<-args;a$representation<-r;v<-.dp(fixtures[[p]][1],.dateEffective(a))
      if(r=="number") expect_true(is.integer(v),info=p) else expect_s3_class(v,"Date")
    }
    args$representation<-"number";args$numeric<-"decimal_year"
    expect_true(is.numeric(.dp(fixtures[[p]][1],.dateEffective(args))))
  }
  expect_identical(.dp("2024-02-29",.ds(precision="quarter",representation="start")),as.Date("2024-01-01"))
  expect_identical(.dp("2024-02-01",.ds(precision="month",representation="end")),as.Date("2024-02-29"))
  expect_identical(.dp(c("1970-01-01","1969-12-31","1971-01-01"),.ds(precision="month",representation="number",origin="1970-01-01")),c(0L,-1L,12L))
  expect_equal(.dp("2024-04-01",.ds(precision="quarter",representation="number",numeric="decimal_year")),2024.25)
  expect_error(.ds(precision="year",representation="number",origin="1970-01-01"),"Irrelevant")
})
test_that("D15-D19 all date parts use fixed English and cyclic conventions", {
  expected<-c(year=2024,semester=1,quarter=1,month=2,month_name=2,month_abbr=2,day=29,weekday=4,weekday_name=4,weekday_abbr=4,iso_week=9,iso_year=2024,day_of_year=60,epi_week=9,epi_year=2024,fiscal_year=2023,fiscal_quarter=4,season=1)
  for(p in names(expected)) {
    s<-list(representation="number")
    if(grepl("^epi",p)) s<-c(s,list(week_start=1L,week_min_days=4L))
    if(grepl("^fiscal",p)) s$fiscal_start<-4L
    expect_equal(.dp("2024-02-29",.dateEffective(s,kind="date_part",part=p),p),unname(expected[p]),info=p)
  }
  for(p in c("month_sin","month_cos","weekday_sin","weekday_cos","season_sin","season_cos")) {
    s<-.dateEffective(kind="date_part",part=p)
    value<-.dp("2024-01-01",s,p)
    expect_equal(value,if(grepl("sin$",p))0 else 1)
  }
  expect_identical(as.character(.dp("2024-02-29",.dateEffective(kind="date_part",part="weekday_name"),"weekday_name")),"Thursday")
  expect_identical(as.character(.dp("2024-12-01",.dateEffective(list(hemisphere="south"),kind="date_part",part="season"),"season")),"Summer")
})
test_that("D20-D23 patterns, labels and relative days are closed", {
  s<-.ds(precision="custom",pattern="%Y %m %d %j %u %b %B %a %A %%")
  expect_identical(as.character(.dp("2024-02-29",s)),"2024 02 29 060 4 Feb February Thu Thursday %")
  expect_identical(as.character(.dp("2021-01-01",.ds(precision="custom",pattern="%G-W%V"))),"2020-W53")
  for(p in c("%Y-W%V","%n","%t","%s","%Z","%c","%OY","%4Y","%Y;system()","%Y\n")) expect_error(.ds(precision="custom",pattern=p),"date_pattern_invalid")
  expect_identical(as.character(.dp("2024-02-29",.ds(precision="quarter",labels=c("2024-Q1"="First quarter 2024")))),"First quarter 2024")
  expect_error(.dp(c("2024-01-01","2024-04-01"),.ds(precision="quarter",labels=c("2024-Q1"="2024-Q2"))),"unique")
  expect_identical(.datePresent(list(lo=as.Date("2020-06-15"),hi=as.Date("2020-06-15")),.ds(precision="relative_days"),anchor=as.Date("1970-01-01")),18428L)
})
test_that("D25-D28 signed elapsed calendar arithmetic and rounding", {
  f<-function(a,b,...) .dateElapsed(as.Date(a),as.Date(b),...)
  expect_identical(f("2020-06-15","2024-06-01"),1447L)
  expect_identical(f("0999-12-31","1000-01-01"),1L)
  expect_identical(f("2023-01-31","2023-02-28","months"),1L)
  expect_identical(f("2024-01-31","2024-02-28","months"),0L)
  expect_identical(f("2023-02-28","2023-01-31","months"),-1L)
  expect_identical(f("2000-02-29","2001-02-28","years"),0L)
  expect_identical(f("2000-02-29","2001-03-01","years"),1L)
  expect_equal(f("2000-02-29","2001-02-28","decimal_years"),365/366)
  expect_equal(f("2024-01-11","2024-01-01","weeks"),-10/7)
  expect_equal(f("2024-01-11","2024-01-01","weeks",TRUE,"floor"),1)
  for(r in c("floor","ceiling","trunc","nearest","none")) expect_equal(f("2024-01-11","2024-01-01","weeks",FALSE,r),switch(r,floor=-2,ceiling=-1,trunc=-1,nearest=-1,none=-10/7))
})
test_that("D33-D37 partial, invalid, future and missing values", {
  for(policy in c("start","mid","end")) {
    s<-.ds(partial=policy)
    expect_identical(.dateComponents(2024,2,NA,s)$lo,as.Date(switch(policy,start="2024-02-01",mid="2024-02-15",end="2024-02-29")))
    expect_identical(.dateComponents(2023,NA,NA,s)$lo,as.Date(switch(policy,start="2023-01-01",mid="2023-07-02",end="2023-12-31")))
  }
  s<-.ds();i<-.dateComponents(c(2024,2024,2024),c(NA,2,2),c(NA,NA,30),s)
  expect_true(all(is.na(.datePresent(i,s))))
  expect_identical(as.character(.datePresent(i,.ds(precision="year"))),c("2024","2024",NA))
  expect_identical(as.character(.datePresent(i,.ds(precision="month"))),c(NA,"2024-02",NA))
  expect_true(is.na(.dateComponents(2024,NA,10,s)$lo))
  expect_true(is.na(.dateComponents(2027,NA,NA,.ds(future="na",as_of="2026-11-13"))$lo))
  expect_error(.ds(future="na"),"as_of")
})
test_that("D39-D40 inheritance and public missing periods", {
  expect_identical(.dateEffective(list(precision="day"),list(precision="custom",pattern="%b %Y")),.ds())
  expect_error(.dateEffective(list(precision="day",pattern="%Y")),"Irrelevant")
  s<-.ds(precision="month",level_range=c("2024-01-01","2024-03-31"))
  a<-.dp("2024-01-01",s);b<-.dp("2024-03-01",s)
  expect_identical(levels(a),c("2024-01","2024-02","2024-03"));expect_identical(levels(a),levels(b))
  expect_identical(as.integer(table(a)),c(1L,0L,0L))
})
