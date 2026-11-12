# .sqlDateAddDays renders per family

    Code
      .ws3_snapshot(function(f) .sqlDateAddDays(f, "t.start_date", 30L))
    Output
      postgresql (CAST(t.start_date AS DATE) + 30)
      sqlite     date(t.start_date, '+30 days')
      duckdb     CAST(CAST(t.start_date AS DATE) + 30 AS DATE)
      mysql      DATE_ADD(CAST(t.start_date AS DATE), INTERVAL 30 DAY)
      sqlserver  DATEADD(day, 30, CAST(t.start_date AS DATE))
      oracle     (CAST(t.start_date AS DATE) + 30)
      redshift   CAST(DATEADD(day, 30, CAST(t.start_date AS DATE)) AS DATE)
      bigquery   DATE_ADD(CAST(t.start_date AS DATE), INTERVAL 30 DAY)
      snowflake  DATEADD(day, 30, CAST(t.start_date AS DATE))
      spark      DATE_ADD(CAST(t.start_date AS DATE), 30)
    Code
      .ws3_snapshot(function(f) .sqlDateAddDays(f, "t.start_date", -7L))
    Output
      postgresql (CAST(t.start_date AS DATE) + (-7))
      sqlite     date(t.start_date, '-7 days')
      duckdb     CAST(CAST(t.start_date AS DATE) + (-7) AS DATE)
      mysql      DATE_ADD(CAST(t.start_date AS DATE), INTERVAL (-7) DAY)
      sqlserver  DATEADD(day, (-7), CAST(t.start_date AS DATE))
      oracle     (CAST(t.start_date AS DATE) + (-7))
      redshift   CAST(DATEADD(day, (-7), CAST(t.start_date AS DATE)) AS DATE)
      bigquery   DATE_ADD(CAST(t.start_date AS DATE), INTERVAL (-7) DAY)
      snowflake  DATEADD(day, (-7), CAST(t.start_date AS DATE))
      spark      DATE_ADD(CAST(t.start_date AS DATE), (-7))
    Code
      .ws3_snapshot(function(f) .sqlDateAddDays(f, "t.start_date",
        "t.days_supply - 1"))
    Output
      postgresql (CAST(t.start_date AS DATE) + CAST(t.days_supply - 1 AS INTEGER))
      sqlite     date(t.start_date, CAST(CAST(t.days_supply - 1 AS INTEGER) AS TEXT) || ' days')
      duckdb     CAST(CAST(t.start_date AS DATE) + CAST(t.days_supply - 1 AS INTEGER) AS DATE)
      mysql      DATE_ADD(CAST(t.start_date AS DATE), INTERVAL CAST(t.days_supply - 1 AS SIGNED) DAY)
      sqlserver  DATEADD(day, CAST(t.days_supply - 1 AS INTEGER), CAST(t.start_date AS DATE))
      oracle     (CAST(t.start_date AS DATE) + CAST(t.days_supply - 1 AS NUMBER(19)))
      redshift   CAST(DATEADD(day, CAST(t.days_supply - 1 AS INTEGER), CAST(t.start_date AS DATE)) AS DATE)
      bigquery   DATE_ADD(CAST(t.start_date AS DATE), INTERVAL CAST(t.days_supply - 1 AS INT64) DAY)
      snowflake  DATEADD(day, CAST(t.days_supply - 1 AS INTEGER), CAST(t.start_date AS DATE))
      spark      DATE_ADD(CAST(t.start_date AS DATE), CAST(t.days_supply - 1 AS INTEGER))
    Code
      .ws3_snapshot(function(f) .sqlDateAddDays(f, "r.end_date", "r.persistence"))
    Output
      postgresql (CAST(r.end_date AS DATE) + CAST(r.persistence AS INTEGER))
      sqlite     date(r.end_date, CAST(CAST(r.persistence AS INTEGER) AS TEXT) || ' days')
      duckdb     CAST(CAST(r.end_date AS DATE) + CAST(r.persistence AS INTEGER) AS DATE)
      mysql      DATE_ADD(CAST(r.end_date AS DATE), INTERVAL CAST(r.persistence AS SIGNED) DAY)
      sqlserver  DATEADD(day, CAST(r.persistence AS INTEGER), CAST(r.end_date AS DATE))
      oracle     (CAST(r.end_date AS DATE) + CAST(r.persistence AS NUMBER(19)))
      redshift   CAST(DATEADD(day, CAST(r.persistence AS INTEGER), CAST(r.end_date AS DATE)) AS DATE)
      bigquery   DATE_ADD(CAST(r.end_date AS DATE), INTERVAL CAST(r.persistence AS INT64) DAY)
      snowflake  DATEADD(day, CAST(r.persistence AS INTEGER), CAST(r.end_date AS DATE))
      spark      DATE_ADD(CAST(r.end_date AS DATE), CAST(r.persistence AS INTEGER))

# .sqlDateDiffDays renders per family

    Code
      .ws3_snapshot(function(f) .sqlDateDiffDays(f, "t.start_date", "t.end_date"))
    Output
      postgresql (CAST(t.end_date AS DATE) - CAST(t.start_date AS DATE))
      sqlite     CAST(julianday(date(t.end_date)) - julianday(date(t.start_date)) AS INTEGER)
      duckdb     CAST(DATE_DIFF('day', CAST(t.start_date AS DATE), CAST(t.end_date AS DATE)) AS INTEGER)
      mysql      DATEDIFF(CAST(t.end_date AS DATE), CAST(t.start_date AS DATE))
      sqlserver  DATEDIFF(day, CAST(t.start_date AS DATE), CAST(t.end_date AS DATE))
      oracle     CAST(TRUNC(CAST(t.end_date AS DATE)) - TRUNC(CAST(t.start_date AS DATE)) AS INTEGER)
      redshift   CAST(DATEDIFF(day, CAST(t.start_date AS DATE), CAST(t.end_date AS DATE)) AS INTEGER)
      bigquery   DATE_DIFF(CAST(t.end_date AS DATE), CAST(t.start_date AS DATE), DAY)
      snowflake  CAST(DATEDIFF(day, CAST(t.start_date AS DATE), CAST(t.end_date AS DATE)) AS INTEGER)
      spark      DATEDIFF(CAST(t.end_date AS DATE), CAST(t.start_date AS DATE))
    Code
      .ws3_snapshot(function(f) .sqlDateDiffDays(f, "a.anchor_date", .sqlDateAddDays(
        f, "e.start_date", 1L)))
    Output
      postgresql (CAST((CAST(e.start_date AS DATE) + 1) AS DATE) - CAST(a.anchor_date AS DATE))
      sqlite     CAST(julianday(date(date(e.start_date, '+1 days'))) - julianday(date(a.anchor_date)) AS INTEGER)
      duckdb     CAST(DATE_DIFF('day', CAST(a.anchor_date AS DATE), CAST(CAST(CAST(e.start_date AS DATE) + 1 AS DATE) AS DATE)) AS INTEGER)
      mysql      DATEDIFF(CAST(DATE_ADD(CAST(e.start_date AS DATE), INTERVAL 1 DAY) AS DATE), CAST(a.anchor_date AS DATE))
      sqlserver  DATEDIFF(day, CAST(a.anchor_date AS DATE), CAST(DATEADD(day, 1, CAST(e.start_date AS DATE)) AS DATE))
      oracle     CAST(TRUNC(CAST((CAST(e.start_date AS DATE) + 1) AS DATE)) - TRUNC(CAST(a.anchor_date AS DATE)) AS INTEGER)
      redshift   CAST(DATEDIFF(day, CAST(a.anchor_date AS DATE), CAST(CAST(DATEADD(day, 1, CAST(e.start_date AS DATE)) AS DATE) AS DATE)) AS INTEGER)
      bigquery   DATE_DIFF(CAST(DATE_ADD(CAST(e.start_date AS DATE), INTERVAL 1 DAY) AS DATE), CAST(a.anchor_date AS DATE), DAY)
      snowflake  CAST(DATEDIFF(day, CAST(a.anchor_date AS DATE), CAST(DATEADD(day, 1, CAST(e.start_date AS DATE)) AS DATE)) AS INTEGER)
      spark      DATEDIFF(CAST(DATE_ADD(CAST(e.start_date AS DATE), 1) AS DATE), CAST(a.anchor_date AS DATE))

# .sqlYear, .sqlMonth and .sqlMonthKey render per family

    Code
      .ws3_snapshot(function(f) .sqlYear(f, "t.d"))
    Output
      postgresql CAST(EXTRACT(YEAR FROM t.d) AS INTEGER)
      sqlite     CAST(strftime('%Y', t.d) AS INTEGER)
      duckdb     CAST(EXTRACT(YEAR FROM t.d) AS INTEGER)
      mysql      YEAR(t.d)
      sqlserver  YEAR(t.d)
      oracle     CAST(EXTRACT(YEAR FROM t.d) AS NUMBER(19))
      redshift   CAST(EXTRACT(YEAR FROM t.d) AS INTEGER)
      bigquery   EXTRACT(YEAR FROM t.d)
      snowflake  CAST(EXTRACT(YEAR FROM t.d) AS INTEGER)
      spark      YEAR(t.d)
    Code
      .ws3_snapshot(function(f) .sqlMonth(f, "t.d"))
    Output
      postgresql CAST(EXTRACT(MONTH FROM t.d) AS INTEGER)
      sqlite     CAST(strftime('%m', t.d) AS INTEGER)
      duckdb     CAST(EXTRACT(MONTH FROM t.d) AS INTEGER)
      mysql      MONTH(t.d)
      sqlserver  MONTH(t.d)
      oracle     CAST(EXTRACT(MONTH FROM t.d) AS NUMBER(19))
      redshift   CAST(EXTRACT(MONTH FROM t.d) AS INTEGER)
      bigquery   EXTRACT(MONTH FROM t.d)
      snowflake  CAST(EXTRACT(MONTH FROM t.d) AS INTEGER)
      spark      MONTH(t.d)
    Code
      .ws3_snapshot(function(f) .sqlMonthKey(f, "t.d"))
    Output
      postgresql (CAST(EXTRACT(YEAR FROM t.d) AS INTEGER) * 100 + CAST(EXTRACT(MONTH FROM t.d) AS INTEGER))
      sqlite     (CAST(strftime('%Y', t.d) AS INTEGER) * 100 + CAST(strftime('%m', t.d) AS INTEGER))
      duckdb     (CAST(EXTRACT(YEAR FROM t.d) AS INTEGER) * 100 + CAST(EXTRACT(MONTH FROM t.d) AS INTEGER))
      mysql      (YEAR(t.d) * 100 + MONTH(t.d))
      sqlserver  (YEAR(t.d) * 100 + MONTH(t.d))
      oracle     (CAST(EXTRACT(YEAR FROM t.d) AS NUMBER(19)) * 100 + CAST(EXTRACT(MONTH FROM t.d) AS NUMBER(19)))
      redshift   (CAST(EXTRACT(YEAR FROM t.d) AS INTEGER) * 100 + CAST(EXTRACT(MONTH FROM t.d) AS INTEGER))
      bigquery   (EXTRACT(YEAR FROM t.d) * 100 + EXTRACT(MONTH FROM t.d))
      snowflake  (CAST(EXTRACT(YEAR FROM t.d) AS INTEGER) * 100 + CAST(EXTRACT(MONTH FROM t.d) AS INTEGER))
      spark      (YEAR(t.d) * 100 + MONTH(t.d))

# .sqlCalendarFloor renders per family

    Code
      .ws3_snapshot(function(f) .sqlCalendarFloor(f, "t.d", "month"))
    Output
      postgresql CAST(DATE_TRUNC('month', CAST(t.d AS DATE)) AS DATE)
      sqlite     date(t.d, 'start of month')
      duckdb     CAST(DATE_TRUNC('month', CAST(t.d AS DATE)) AS DATE)
      mysql      DATE_ADD(MAKEDATE(YEAR(t.d), 1), INTERVAL (MONTH(t.d) - 1) MONTH)
      sqlserver  DATEFROMPARTS(YEAR(t.d), MONTH(t.d), 1)
      oracle     TRUNC(CAST(t.d AS DATE), 'MM')
      redshift   CAST(DATE_TRUNC('month', CAST(t.d AS DATE)) AS DATE)
      bigquery   DATE_TRUNC(CAST(t.d AS DATE), MONTH)
      snowflake  CAST(DATE_TRUNC('month', CAST(t.d AS DATE)) AS DATE)
      spark      CAST(DATE_TRUNC('month', CAST(t.d AS DATE)) AS DATE)
    Code
      .ws3_snapshot(function(f) .sqlCalendarFloor(f, "t.d", "quarter"))
    Output
      postgresql CAST(DATE_TRUNC('quarter', CAST(t.d AS DATE)) AS DATE)
      sqlite     date(t.d, 'start of month', '-' || ((CAST(strftime('%m', t.d) AS INTEGER) - 1) % 3) || ' months')
      duckdb     CAST(DATE_TRUNC('quarter', CAST(t.d AS DATE)) AS DATE)
      mysql      DATE_ADD(MAKEDATE(YEAR(t.d), 1), INTERVAL ((QUARTER(t.d) - 1) * 3) MONTH)
      sqlserver  DATEFROMPARTS(YEAR(t.d), (DATEPART(quarter, t.d) - 1) * 3 + 1, 1)
      oracle     TRUNC(CAST(t.d AS DATE), 'Q')
      redshift   CAST(DATE_TRUNC('quarter', CAST(t.d AS DATE)) AS DATE)
      bigquery   DATE_TRUNC(CAST(t.d AS DATE), QUARTER)
      snowflake  CAST(DATE_TRUNC('quarter', CAST(t.d AS DATE)) AS DATE)
      spark      CAST(DATE_TRUNC('quarter', CAST(t.d AS DATE)) AS DATE)
    Code
      .ws3_snapshot(function(f) .sqlCalendarFloor(f, "t.d", "year"))
    Output
      postgresql CAST(DATE_TRUNC('year', CAST(t.d AS DATE)) AS DATE)
      sqlite     date(t.d, 'start of year')
      duckdb     CAST(DATE_TRUNC('year', CAST(t.d AS DATE)) AS DATE)
      mysql      MAKEDATE(YEAR(t.d), 1)
      sqlserver  DATEFROMPARTS(YEAR(t.d), 1, 1)
      oracle     TRUNC(CAST(t.d AS DATE), 'YYYY')
      redshift   CAST(DATE_TRUNC('year', CAST(t.d AS DATE)) AS DATE)
      bigquery   DATE_TRUNC(CAST(t.d AS DATE), YEAR)
      snowflake  CAST(DATE_TRUNC('year', CAST(t.d AS DATE)) AS DATE)
      spark      CAST(DATE_TRUNC('year', CAST(t.d AS DATE)) AS DATE)

# .sqlCalendarAdvance renders per family

    Code
      .ws3_snapshot(function(f) .sqlCalendarAdvance(f, "t.d", 1L, "month"))
    Output
      postgresql CAST(CAST(DATE_TRUNC('month', CAST(t.d AS DATE)) AS DATE) + 1 * INTERVAL '1 month' AS DATE)
      sqlite     date(date(t.d, 'start of month'), '+1 months')
      duckdb     CAST(CAST(DATE_TRUNC('month', CAST(t.d AS DATE)) AS DATE) + 1 * INTERVAL '1 month' AS DATE)
      mysql      DATE_ADD(DATE_ADD(MAKEDATE(YEAR(t.d), 1), INTERVAL (MONTH(t.d) - 1) MONTH), INTERVAL 1 MONTH)
      sqlserver  DATEADD(month, 1, DATEFROMPARTS(YEAR(t.d), MONTH(t.d), 1))
      oracle     ADD_MONTHS(TRUNC(CAST(t.d AS DATE), 'MM'), 1)
      redshift   CAST(DATEADD(month, 1, CAST(DATE_TRUNC('month', CAST(t.d AS DATE)) AS DATE)) AS DATE)
      bigquery   DATE_ADD(DATE_TRUNC(CAST(t.d AS DATE), MONTH), INTERVAL 1 MONTH)
      snowflake  DATEADD(month, 1, CAST(DATE_TRUNC('month', CAST(t.d AS DATE)) AS DATE))
      spark      ADD_MONTHS(CAST(DATE_TRUNC('month', CAST(t.d AS DATE)) AS DATE), 1)
    Code
      .ws3_snapshot(function(f) .sqlCalendarAdvance(f, "t.d", "b.n", "quarter"))
    Output
      postgresql CAST(CAST(DATE_TRUNC('quarter', CAST(t.d AS DATE)) AS DATE) + (CAST(b.n AS INTEGER) * 3) * INTERVAL '1 month' AS DATE)
      sqlite     date(date(t.d, 'start of month', '-' || ((CAST(strftime('%m', t.d) AS INTEGER) - 1) % 3) || ' months'), CAST((CAST(b.n AS INTEGER) * 3) AS TEXT) || ' months')
      duckdb     CAST(CAST(DATE_TRUNC('quarter', CAST(t.d AS DATE)) AS DATE) + (CAST(b.n AS INTEGER) * 3) * INTERVAL '1 month' AS DATE)
      mysql      DATE_ADD(DATE_ADD(MAKEDATE(YEAR(t.d), 1), INTERVAL ((QUARTER(t.d) - 1) * 3) MONTH), INTERVAL (CAST(b.n AS SIGNED) * 3) MONTH)
      sqlserver  DATEADD(month, (CAST(b.n AS INTEGER) * 3), DATEFROMPARTS(YEAR(t.d), (DATEPART(quarter, t.d) - 1) * 3 + 1, 1))
      oracle     ADD_MONTHS(TRUNC(CAST(t.d AS DATE), 'Q'), (CAST(b.n AS NUMBER(19)) * 3))
      redshift   CAST(DATEADD(month, (CAST(b.n AS INTEGER) * 3), CAST(DATE_TRUNC('quarter', CAST(t.d AS DATE)) AS DATE)) AS DATE)
      bigquery   DATE_ADD(DATE_TRUNC(CAST(t.d AS DATE), QUARTER), INTERVAL (CAST(b.n AS INT64) * 3) MONTH)
      snowflake  DATEADD(month, (CAST(b.n AS INTEGER) * 3), CAST(DATE_TRUNC('quarter', CAST(t.d AS DATE)) AS DATE))
      spark      ADD_MONTHS(CAST(DATE_TRUNC('quarter', CAST(t.d AS DATE)) AS DATE), (CAST(b.n AS INTEGER) * 3))
    Code
      .ws3_snapshot(function(f) .sqlCalendarAdvance(f, "t.d", -2L, "year"))
    Output
      postgresql CAST(CAST(DATE_TRUNC('year', CAST(t.d AS DATE)) AS DATE) + (-24) * INTERVAL '1 month' AS DATE)
      sqlite     date(date(t.d, 'start of year'), '-24 months')
      duckdb     CAST(CAST(DATE_TRUNC('year', CAST(t.d AS DATE)) AS DATE) + (-24) * INTERVAL '1 month' AS DATE)
      mysql      DATE_ADD(MAKEDATE(YEAR(t.d), 1), INTERVAL (-24) MONTH)
      sqlserver  DATEADD(month, (-24), DATEFROMPARTS(YEAR(t.d), 1, 1))
      oracle     ADD_MONTHS(TRUNC(CAST(t.d AS DATE), 'YYYY'), (-24))
      redshift   CAST(DATEADD(month, (-24), CAST(DATE_TRUNC('year', CAST(t.d AS DATE)) AS DATE)) AS DATE)
      bigquery   DATE_ADD(DATE_TRUNC(CAST(t.d AS DATE), YEAR), INTERVAL (-24) MONTH)
      snowflake  DATEADD(month, (-24), CAST(DATE_TRUNC('year', CAST(t.d AS DATE)) AS DATE))
      spark      ADD_MONTHS(CAST(DATE_TRUNC('year', CAST(t.d AS DATE)) AS DATE), (-24))

# .sqlMakeDate renders per family

    Code
      .ws3_snapshot(function(f) .sqlMakeDate(f, "p.year_of_birth", "p.month_of_birth",
        "p.day_of_birth"))
    Output
      postgresql CASE WHEN CAST(p.year_of_birth AS INTEGER) BETWEEN 1000 AND 9999 AND CAST(p.month_of_birth AS INTEGER) BETWEEN 1 AND 12 AND CAST(p.day_of_birth AS INTEGER) BETWEEN 1 AND 31 THEN CASE WHEN CAST(EXTRACT(MONTH FROM (CAST(MAKE_DATE(CAST(p.year_of_birth AS INTEGER), CAST(p.month_of_birth AS INTEGER), 1) AS DATE) + CAST(CAST(p.day_of_birth AS INTEGER) - 1 AS INTEGER))) AS INTEGER) = CAST(p.month_of_birth AS INTEGER) THEN (CAST(MAKE_DATE(CAST(p.year_of_birth AS INTEGER), CAST(p.month_of_birth AS INTEGER), 1) AS DATE) + CAST(CAST(p.day_of_birth AS INTEGER) - 1 AS INTEGER)) END END
      sqlite     CASE WHEN CAST(p.year_of_birth AS INTEGER) BETWEEN 1000 AND 9999 AND CAST(p.month_of_birth AS INTEGER) BETWEEN 1 AND 12 AND CAST(p.day_of_birth AS INTEGER) BETWEEN 1 AND 31 THEN CASE WHEN CAST(strftime('%m', date(date(printf('%04d-%02d-01', CAST(p.year_of_birth AS INTEGER), CAST(p.month_of_birth AS INTEGER))), CAST(CAST(CAST(p.day_of_birth AS INTEGER) - 1 AS INTEGER) AS TEXT) || ' days')) AS INTEGER) = CAST(p.month_of_birth AS INTEGER) THEN date(date(printf('%04d-%02d-01', CAST(p.year_of_birth AS INTEGER), CAST(p.month_of_birth AS INTEGER))), CAST(CAST(CAST(p.day_of_birth AS INTEGER) - 1 AS INTEGER) AS TEXT) || ' days') END END
      duckdb     CASE WHEN CAST(p.year_of_birth AS INTEGER) BETWEEN 1000 AND 9999 AND CAST(p.month_of_birth AS INTEGER) BETWEEN 1 AND 12 AND CAST(p.day_of_birth AS INTEGER) BETWEEN 1 AND 31 THEN CASE WHEN CAST(EXTRACT(MONTH FROM CAST(CAST(MAKE_DATE(CAST(p.year_of_birth AS INTEGER), CAST(p.month_of_birth AS INTEGER), 1) AS DATE) + CAST(CAST(p.day_of_birth AS INTEGER) - 1 AS INTEGER) AS DATE)) AS INTEGER) = CAST(p.month_of_birth AS INTEGER) THEN CAST(CAST(MAKE_DATE(CAST(p.year_of_birth AS INTEGER), CAST(p.month_of_birth AS INTEGER), 1) AS DATE) + CAST(CAST(p.day_of_birth AS INTEGER) - 1 AS INTEGER) AS DATE) END END
      mysql      CASE WHEN CAST(p.year_of_birth AS SIGNED) BETWEEN 1000 AND 9999 AND CAST(p.month_of_birth AS SIGNED) BETWEEN 1 AND 12 AND CAST(p.day_of_birth AS SIGNED) BETWEEN 1 AND 31 THEN CASE WHEN MONTH(DATE_ADD(CAST(DATE_ADD(MAKEDATE(CAST(p.year_of_birth AS SIGNED), 1), INTERVAL (CAST(p.month_of_birth AS SIGNED) - 1) MONTH) AS DATE), INTERVAL CAST(CAST(p.day_of_birth AS SIGNED) - 1 AS SIGNED) DAY)) = CAST(p.month_of_birth AS SIGNED) THEN DATE_ADD(CAST(DATE_ADD(MAKEDATE(CAST(p.year_of_birth AS SIGNED), 1), INTERVAL (CAST(p.month_of_birth AS SIGNED) - 1) MONTH) AS DATE), INTERVAL CAST(CAST(p.day_of_birth AS SIGNED) - 1 AS SIGNED) DAY) END END
      sqlserver  CASE WHEN CAST(p.year_of_birth AS INTEGER) BETWEEN 1000 AND 9999 AND CAST(p.month_of_birth AS INTEGER) BETWEEN 1 AND 12 AND CAST(p.day_of_birth AS INTEGER) BETWEEN 1 AND 31 THEN CASE WHEN MONTH(DATEADD(day, CAST(CAST(p.day_of_birth AS INTEGER) - 1 AS INTEGER), CAST(DATEFROMPARTS(CAST(p.year_of_birth AS INTEGER), CAST(p.month_of_birth AS INTEGER), 1) AS DATE))) = CAST(p.month_of_birth AS INTEGER) THEN DATEADD(day, CAST(CAST(p.day_of_birth AS INTEGER) - 1 AS INTEGER), CAST(DATEFROMPARTS(CAST(p.year_of_birth AS INTEGER), CAST(p.month_of_birth AS INTEGER), 1) AS DATE)) END END
      oracle     CASE WHEN CAST(p.year_of_birth AS NUMBER(19)) BETWEEN 1000 AND 9999 AND CAST(p.month_of_birth AS NUMBER(19)) BETWEEN 1 AND 12 AND CAST(p.day_of_birth AS NUMBER(19)) BETWEEN 1 AND 31 THEN CASE WHEN CAST(EXTRACT(MONTH FROM (CAST(ADD_MONTHS(TO_DATE(TO_CHAR(CAST(p.year_of_birth AS NUMBER(19)), 'FM0000') || '-01-01', 'YYYY-MM-DD'), CAST(p.month_of_birth AS NUMBER(19)) - 1) AS DATE) + CAST(CAST(p.day_of_birth AS NUMBER(19)) - 1 AS NUMBER(19)))) AS NUMBER(19)) = CAST(p.month_of_birth AS NUMBER(19)) THEN (CAST(ADD_MONTHS(TO_DATE(TO_CHAR(CAST(p.year_of_birth AS NUMBER(19)), 'FM0000') || '-01-01', 'YYYY-MM-DD'), CAST(p.month_of_birth AS NUMBER(19)) - 1) AS DATE) + CAST(CAST(p.day_of_birth AS NUMBER(19)) - 1 AS NUMBER(19))) END END
      redshift   CASE WHEN CAST(p.year_of_birth AS INTEGER) BETWEEN 1000 AND 9999 AND CAST(p.month_of_birth AS INTEGER) BETWEEN 1 AND 12 AND CAST(p.day_of_birth AS INTEGER) BETWEEN 1 AND 31 THEN CASE WHEN CAST(EXTRACT(MONTH FROM CAST(DATEADD(day, CAST(CAST(p.day_of_birth AS INTEGER) - 1 AS INTEGER), CAST(CAST(DATEADD(month, CAST(p.month_of_birth AS INTEGER) - 1, TO_DATE(CAST(CAST(p.year_of_birth AS INTEGER) AS VARCHAR(4)) || '-01-01', 'YYYY-MM-DD')) AS DATE) AS DATE)) AS DATE)) AS INTEGER) = CAST(p.month_of_birth AS INTEGER) THEN CAST(DATEADD(day, CAST(CAST(p.day_of_birth AS INTEGER) - 1 AS INTEGER), CAST(CAST(DATEADD(month, CAST(p.month_of_birth AS INTEGER) - 1, TO_DATE(CAST(CAST(p.year_of_birth AS INTEGER) AS VARCHAR(4)) || '-01-01', 'YYYY-MM-DD')) AS DATE) AS DATE)) AS DATE) END END
      bigquery   CASE WHEN CAST(p.year_of_birth AS INT64) BETWEEN 1000 AND 9999 AND CAST(p.month_of_birth AS INT64) BETWEEN 1 AND 12 AND CAST(p.day_of_birth AS INT64) BETWEEN 1 AND 31 THEN CASE WHEN EXTRACT(MONTH FROM DATE_ADD(CAST(DATE(CAST(p.year_of_birth AS INT64), CAST(p.month_of_birth AS INT64), 1) AS DATE), INTERVAL CAST(CAST(p.day_of_birth AS INT64) - 1 AS INT64) DAY)) = CAST(p.month_of_birth AS INT64) THEN DATE_ADD(CAST(DATE(CAST(p.year_of_birth AS INT64), CAST(p.month_of_birth AS INT64), 1) AS DATE), INTERVAL CAST(CAST(p.day_of_birth AS INT64) - 1 AS INT64) DAY) END END
      snowflake  CASE WHEN CAST(p.year_of_birth AS INTEGER) BETWEEN 1000 AND 9999 AND CAST(p.month_of_birth AS INTEGER) BETWEEN 1 AND 12 AND CAST(p.day_of_birth AS INTEGER) BETWEEN 1 AND 31 THEN CASE WHEN CAST(EXTRACT(MONTH FROM DATEADD(day, CAST(CAST(p.day_of_birth AS INTEGER) - 1 AS INTEGER), CAST(DATE_FROM_PARTS(CAST(p.year_of_birth AS INTEGER), CAST(p.month_of_birth AS INTEGER), 1) AS DATE))) AS INTEGER) = CAST(p.month_of_birth AS INTEGER) THEN DATEADD(day, CAST(CAST(p.day_of_birth AS INTEGER) - 1 AS INTEGER), CAST(DATE_FROM_PARTS(CAST(p.year_of_birth AS INTEGER), CAST(p.month_of_birth AS INTEGER), 1) AS DATE)) END END
      spark      CASE WHEN CAST(p.year_of_birth AS INTEGER) BETWEEN 1000 AND 9999 AND CAST(p.month_of_birth AS INTEGER) BETWEEN 1 AND 12 AND CAST(p.day_of_birth AS INTEGER) BETWEEN 1 AND 31 THEN CASE WHEN MONTH(DATE_ADD(CAST(MAKE_DATE(CAST(p.year_of_birth AS INTEGER), CAST(p.month_of_birth AS INTEGER), 1) AS DATE), CAST(CAST(p.day_of_birth AS INTEGER) - 1 AS INTEGER))) = CAST(p.month_of_birth AS INTEGER) THEN DATE_ADD(CAST(MAKE_DATE(CAST(p.year_of_birth AS INTEGER), CAST(p.month_of_birth AS INTEGER), 1) AS DATE), CAST(CAST(p.day_of_birth AS INTEGER) - 1 AS INTEGER)) END END
    Code
      .ws3_snapshot(function(f) .sqlMakeDate(f, 2020L, 2L, 29L))
    Output
      postgresql DATE '2020-02-29'
      sqlite     '2020-02-29'
      duckdb     DATE '2020-02-29'
      mysql      DATE '2020-02-29'
      sqlserver  CAST('2020-02-29' AS DATE)
      oracle     DATE '2020-02-29'
      redshift   CAST('2020-02-29' AS DATE)
      bigquery   DATE '2020-02-29'
      snowflake  DATE '2020-02-29'
      spark      DATE '2020-02-29'
    Code
      .ws3_snapshot(function(f) .sqlMakeDate(f, 2019L, 2L, 29L))
    Output
      postgresql CAST(NULL AS DATE)
      sqlite     NULL
      duckdb     CAST(NULL AS DATE)
      mysql      CAST(NULL AS DATE)
      sqlserver  CAST(NULL AS DATE)
      oracle     CAST(NULL AS DATE)
      redshift   CAST(NULL AS DATE)
      bigquery   CAST(NULL AS DATE)
      snowflake  CAST(NULL AS DATE)
      spark      CAST(NULL AS DATE)

# .sqlDateLiteral and .sqlIntegerLiteral render per family

    Code
      .ws3_snapshot(function(f) .sqlDateLiteral(f, "2020-01-01"))
    Output
      postgresql DATE '2020-01-01'
      sqlite     '2020-01-01'
      duckdb     DATE '2020-01-01'
      mysql      DATE '2020-01-01'
      sqlserver  CAST('2020-01-01' AS DATE)
      oracle     DATE '2020-01-01'
      redshift   CAST('2020-01-01' AS DATE)
      bigquery   DATE '2020-01-01'
      snowflake  DATE '2020-01-01'
      spark      DATE '2020-01-01'
    Code
      .ws3_snapshot(function(f) .sqlIntegerLiteral(f, 42L))
    Output
      postgresql 42
      sqlite     42
      duckdb     42
      mysql      42
      sqlserver  42
      oracle     42
      redshift   42
      bigquery   42
      snowflake  42
      spark      42
    Code
      .ws3_snapshot(function(f) .sqlIntegerLiteral(f, -3L))
    Output
      postgresql (-3)
      sqlite     (-3)
      duckdb     (-3)
      mysql      (-3)
      sqlserver  (-3)
      oracle     (-3)
      redshift   (-3)
      bigquery   (-3)
      snowflake  (-3)
      spark      (-3)
    Code
      .ws3_snapshot(function(f) .sqlIntegerLiteral(f, "9007199254740993"))
    Output
      postgresql 9007199254740993
      sqlite     9007199254740993
      duckdb     9007199254740993
      mysql      9007199254740993
      sqlserver  9007199254740993
      oracle     9007199254740993
      redshift   9007199254740993
      bigquery   9007199254740993
      snowflake  9007199254740993
      spark      9007199254740993

# .sqlLeast and .sqlGreatest render per family

    Code
      .ws3_snapshot(function(f) .sqlLeast(f, "a.end_date", "b.op_end"))
    Output
      postgresql LEAST(a.end_date, b.op_end)
      sqlite     MIN(COALESCE(a.end_date, b.op_end), COALESCE(b.op_end, a.end_date))
      duckdb     LEAST(a.end_date, b.op_end)
      mysql      LEAST(COALESCE(a.end_date, b.op_end), COALESCE(b.op_end, a.end_date))
      sqlserver  CASE WHEN (a.end_date) IS NOT NULL AND ((b.op_end) IS NULL OR (a.end_date) <= (b.op_end)) THEN (a.end_date) ELSE (b.op_end) END
      oracle     LEAST(COALESCE(a.end_date, b.op_end), COALESCE(b.op_end, a.end_date))
      redshift   LEAST(COALESCE(a.end_date, b.op_end), COALESCE(b.op_end, a.end_date))
      bigquery   LEAST(COALESCE(a.end_date, b.op_end), COALESCE(b.op_end, a.end_date))
      snowflake  LEAST(COALESCE(a.end_date, b.op_end), COALESCE(b.op_end, a.end_date))
      spark      LEAST(COALESCE(a.end_date, b.op_end), COALESCE(b.op_end, a.end_date))
    Code
      .ws3_snapshot(function(f) .sqlGreatest(f, "x.a", "x.b", "x.c"))
    Output
      postgresql GREATEST(x.a, x.b, x.c)
      sqlite     MAX(COALESCE(x.a, x.b, x.c), COALESCE(x.b, x.a, x.c), COALESCE(x.c, x.a, x.b))
      duckdb     GREATEST(x.a, x.b, x.c)
      mysql      GREATEST(COALESCE(x.a, x.b, x.c), COALESCE(x.b, x.a, x.c), COALESCE(x.c, x.a, x.b))
      sqlserver  CASE WHEN (x.a) IS NOT NULL AND ((x.b) IS NULL OR (x.a) >= (x.b)) AND ((x.c) IS NULL OR (x.a) >= (x.c)) THEN (x.a) WHEN (x.b) IS NOT NULL AND ((x.c) IS NULL OR (x.b) >= (x.c)) THEN (x.b) ELSE (x.c) END
      oracle     GREATEST(COALESCE(x.a, x.b, x.c), COALESCE(x.b, x.a, x.c), COALESCE(x.c, x.a, x.b))
      redshift   GREATEST(COALESCE(x.a, x.b, x.c), COALESCE(x.b, x.a, x.c), COALESCE(x.c, x.a, x.b))
      bigquery   GREATEST(COALESCE(x.a, x.b, x.c), COALESCE(x.b, x.a, x.c), COALESCE(x.c, x.a, x.b))
      snowflake  GREATEST(COALESCE(x.a, x.b, x.c), COALESCE(x.b, x.a, x.c), COALESCE(x.c, x.a, x.b))
      spark      GREATEST(COALESCE(x.a, x.b, x.c), COALESCE(x.b, x.a, x.c), COALESCE(x.c, x.a, x.b))
    Code
      .ws3_snapshot(function(f) .sqlGreatest(f, 0L, "x.overlap"))
    Output
      postgresql GREATEST(0, x.overlap)
      sqlite     MAX(COALESCE(0, x.overlap), COALESCE(x.overlap, 0))
      duckdb     GREATEST(0, x.overlap)
      mysql      GREATEST(COALESCE(0, x.overlap), COALESCE(x.overlap, 0))
      sqlserver  CASE WHEN (0) IS NOT NULL AND ((x.overlap) IS NULL OR (0) >= (x.overlap)) THEN (0) ELSE (x.overlap) END
      oracle     GREATEST(COALESCE(0, x.overlap), COALESCE(x.overlap, 0))
      redshift   GREATEST(COALESCE(0, x.overlap), COALESCE(x.overlap, 0))
      bigquery   GREATEST(COALESCE(0, x.overlap), COALESCE(x.overlap, 0))
      snowflake  GREATEST(COALESCE(0, x.overlap), COALESCE(x.overlap, 0))
      spark      GREATEST(COALESCE(0, x.overlap), COALESCE(x.overlap, 0))
    Code
      .ws3_snapshot(function(f) .sqlLeast(f, "x.a"))
    Output
      postgresql (x.a)
      sqlite     (x.a)
      duckdb     (x.a)
      mysql      (x.a)
      sqlserver  (x.a)
      oracle     (x.a)
      redshift   (x.a)
      bigquery   (x.a)
      snowflake  (x.a)
      spark      (x.a)

# .sqlNullsLast renders per family

    Code
      .ws3_snapshot(function(f) .sqlNullsLast(f, "e.value"))
    Output
      postgresql e.value ASC NULLS LAST
      sqlite     CASE WHEN (e.value) IS NULL THEN 1 ELSE 0 END, e.value ASC
      duckdb     e.value ASC NULLS LAST
      mysql      CASE WHEN (e.value) IS NULL THEN 1 ELSE 0 END, e.value ASC
      sqlserver  CASE WHEN (e.value) IS NULL THEN 1 ELSE 0 END, e.value ASC
      oracle     e.value ASC NULLS LAST
      redshift   e.value ASC NULLS LAST
      bigquery   e.value ASC NULLS LAST
      snowflake  e.value ASC NULLS LAST
      spark      e.value ASC NULLS LAST
    Code
      .ws3_snapshot(function(f) .sqlNullsLast(f, "e.value", descending = TRUE))
    Output
      postgresql e.value DESC NULLS LAST
      sqlite     CASE WHEN (e.value) IS NULL THEN 1 ELSE 0 END, e.value DESC
      duckdb     e.value DESC NULLS LAST
      mysql      CASE WHEN (e.value) IS NULL THEN 1 ELSE 0 END, e.value DESC
      sqlserver  CASE WHEN (e.value) IS NULL THEN 1 ELSE 0 END, e.value DESC
      oracle     e.value DESC NULLS LAST
      redshift   e.value DESC NULLS LAST
      bigquery   e.value DESC NULLS LAST
      snowflake  e.value DESC NULLS LAST
      spark      e.value DESC NULLS LAST

# .sqlRowNumber and .sqlWindow render per family

    Code
      .ws3_snapshot(function(f) .sqlRowNumber(f, "r.person_id", c("e.start_date",
        "e.event_id")))
    Output
      postgresql ROW_NUMBER() OVER (PARTITION BY r.person_id ORDER BY e.start_date, e.event_id)
      sqlite     ROW_NUMBER() OVER (PARTITION BY r.person_id ORDER BY e.start_date, e.event_id)
      duckdb     ROW_NUMBER() OVER (PARTITION BY r.person_id ORDER BY e.start_date, e.event_id)
      mysql      ROW_NUMBER() OVER (PARTITION BY r.person_id ORDER BY e.start_date, e.event_id)
      sqlserver  ROW_NUMBER() OVER (PARTITION BY r.person_id ORDER BY e.start_date, e.event_id)
      oracle     ROW_NUMBER() OVER (PARTITION BY r.person_id ORDER BY e.start_date, e.event_id)
      redshift   ROW_NUMBER() OVER (PARTITION BY r.person_id ORDER BY e.start_date, e.event_id)
      bigquery   ROW_NUMBER() OVER (PARTITION BY r.person_id ORDER BY e.start_date, e.event_id)
      snowflake  ROW_NUMBER() OVER (PARTITION BY r.person_id ORDER BY e.start_date, e.event_id)
      spark      ROW_NUMBER() OVER (PARTITION BY r.person_id ORDER BY e.start_date, e.event_id)
    Code
      .ws3_snapshot(function(f) .sqlRowNumber(f, character(0), c(.sqlNullsLast(f,
        "e.value"), "e.event_id")))
    Output
      postgresql ROW_NUMBER() OVER (ORDER BY e.value ASC NULLS LAST, e.event_id)
      sqlite     ROW_NUMBER() OVER (ORDER BY CASE WHEN (e.value) IS NULL THEN 1 ELSE 0 END, e.value ASC, e.event_id)
      duckdb     ROW_NUMBER() OVER (ORDER BY e.value ASC NULLS LAST, e.event_id)
      mysql      ROW_NUMBER() OVER (ORDER BY CASE WHEN (e.value) IS NULL THEN 1 ELSE 0 END, e.value ASC, e.event_id)
      sqlserver  ROW_NUMBER() OVER (ORDER BY CASE WHEN (e.value) IS NULL THEN 1 ELSE 0 END, e.value ASC, e.event_id)
      oracle     ROW_NUMBER() OVER (ORDER BY e.value ASC NULLS LAST, e.event_id)
      redshift   ROW_NUMBER() OVER (ORDER BY e.value ASC NULLS LAST, e.event_id)
      bigquery   ROW_NUMBER() OVER (ORDER BY e.value ASC NULLS LAST, e.event_id)
      snowflake  ROW_NUMBER() OVER (ORDER BY e.value ASC NULLS LAST, e.event_id)
      spark      ROW_NUMBER() OVER (ORDER BY e.value ASC NULLS LAST, e.event_id)
    Code
      .ws3_snapshot(function(f) .sqlWindow(f, paste0("MAX(", .sqlDateAddDays(f,
        "r.end_date", 0L), ")"), c("r.person_id", "r.group_id"), c("r.start_date",
        "r.event_id"), frame = "preceding"))
    Output
      postgresql MAX((CAST(r.end_date AS DATE) + 0)) OVER (PARTITION BY r.person_id, r.group_id ORDER BY r.start_date, r.event_id ROWS BETWEEN UNBOUNDED PRECEDING AND 1 PRECEDING)
      sqlite     MAX(date(r.end_date, '+0 days')) OVER (PARTITION BY r.person_id, r.group_id ORDER BY r.start_date, r.event_id ROWS BETWEEN UNBOUNDED PRECEDING AND 1 PRECEDING)
      duckdb     MAX(CAST(CAST(r.end_date AS DATE) + 0 AS DATE)) OVER (PARTITION BY r.person_id, r.group_id ORDER BY r.start_date, r.event_id ROWS BETWEEN UNBOUNDED PRECEDING AND 1 PRECEDING)
      mysql      MAX(DATE_ADD(CAST(r.end_date AS DATE), INTERVAL 0 DAY)) OVER (PARTITION BY r.person_id, r.group_id ORDER BY r.start_date, r.event_id ROWS BETWEEN UNBOUNDED PRECEDING AND 1 PRECEDING)
      sqlserver  MAX(DATEADD(day, 0, CAST(r.end_date AS DATE))) OVER (PARTITION BY r.person_id, r.group_id ORDER BY r.start_date, r.event_id ROWS BETWEEN UNBOUNDED PRECEDING AND 1 PRECEDING)
      oracle     MAX((CAST(r.end_date AS DATE) + 0)) OVER (PARTITION BY r.person_id, r.group_id ORDER BY r.start_date, r.event_id ROWS BETWEEN UNBOUNDED PRECEDING AND 1 PRECEDING)
      redshift   MAX(CAST(DATEADD(day, 0, CAST(r.end_date AS DATE)) AS DATE)) OVER (PARTITION BY r.person_id, r.group_id ORDER BY r.start_date, r.event_id ROWS BETWEEN UNBOUNDED PRECEDING AND 1 PRECEDING)
      bigquery   MAX(DATE_ADD(CAST(r.end_date AS DATE), INTERVAL 0 DAY)) OVER (PARTITION BY r.person_id, r.group_id ORDER BY r.start_date, r.event_id ROWS BETWEEN UNBOUNDED PRECEDING AND 1 PRECEDING)
      snowflake  MAX(DATEADD(day, 0, CAST(r.end_date AS DATE))) OVER (PARTITION BY r.person_id, r.group_id ORDER BY r.start_date, r.event_id ROWS BETWEEN UNBOUNDED PRECEDING AND 1 PRECEDING)
      spark      MAX(DATE_ADD(CAST(r.end_date AS DATE), 0)) OVER (PARTITION BY r.person_id, r.group_id ORDER BY r.start_date, r.event_id ROWS BETWEEN UNBOUNDED PRECEDING AND 1 PRECEDING)
    Code
      .ws3_snapshot(function(f) .sqlWindow(f, "SUM(r.new_era)", c("r.person_id",
        "r.group_id"), c("r.start_date", "r.event_id"), frame = "cumulative"))
    Output
      postgresql SUM(r.new_era) OVER (PARTITION BY r.person_id, r.group_id ORDER BY r.start_date, r.event_id ROWS BETWEEN UNBOUNDED PRECEDING AND CURRENT ROW)
      sqlite     SUM(r.new_era) OVER (PARTITION BY r.person_id, r.group_id ORDER BY r.start_date, r.event_id ROWS BETWEEN UNBOUNDED PRECEDING AND CURRENT ROW)
      duckdb     SUM(r.new_era) OVER (PARTITION BY r.person_id, r.group_id ORDER BY r.start_date, r.event_id ROWS BETWEEN UNBOUNDED PRECEDING AND CURRENT ROW)
      mysql      SUM(r.new_era) OVER (PARTITION BY r.person_id, r.group_id ORDER BY r.start_date, r.event_id ROWS BETWEEN UNBOUNDED PRECEDING AND CURRENT ROW)
      sqlserver  SUM(r.new_era) OVER (PARTITION BY r.person_id, r.group_id ORDER BY r.start_date, r.event_id ROWS BETWEEN UNBOUNDED PRECEDING AND CURRENT ROW)
      oracle     SUM(r.new_era) OVER (PARTITION BY r.person_id, r.group_id ORDER BY r.start_date, r.event_id ROWS BETWEEN UNBOUNDED PRECEDING AND CURRENT ROW)
      redshift   SUM(r.new_era) OVER (PARTITION BY r.person_id, r.group_id ORDER BY r.start_date, r.event_id ROWS BETWEEN UNBOUNDED PRECEDING AND CURRENT ROW)
      bigquery   SUM(r.new_era) OVER (PARTITION BY r.person_id, r.group_id ORDER BY r.start_date, r.event_id ROWS BETWEEN UNBOUNDED PRECEDING AND CURRENT ROW)
      snowflake  SUM(r.new_era) OVER (PARTITION BY r.person_id, r.group_id ORDER BY r.start_date, r.event_id ROWS BETWEEN UNBOUNDED PRECEDING AND CURRENT ROW)
      spark      SUM(r.new_era) OVER (PARTITION BY r.person_id, r.group_id ORDER BY r.start_date, r.event_id ROWS BETWEEN UNBOUNDED PRECEDING AND CURRENT ROW)
    Code
      .ws3_snapshot(function(f) .sqlWindow(f, "LAG(r.end_date)", "r.person_id", c(
        "r.start_date", "r.event_id")))
    Output
      postgresql LAG(r.end_date) OVER (PARTITION BY r.person_id ORDER BY r.start_date, r.event_id)
      sqlite     LAG(r.end_date) OVER (PARTITION BY r.person_id ORDER BY r.start_date, r.event_id)
      duckdb     LAG(r.end_date) OVER (PARTITION BY r.person_id ORDER BY r.start_date, r.event_id)
      mysql      LAG(r.end_date) OVER (PARTITION BY r.person_id ORDER BY r.start_date, r.event_id)
      sqlserver  LAG(r.end_date) OVER (PARTITION BY r.person_id ORDER BY r.start_date, r.event_id)
      oracle     LAG(r.end_date) OVER (PARTITION BY r.person_id ORDER BY r.start_date, r.event_id)
      redshift   LAG(r.end_date) OVER (PARTITION BY r.person_id ORDER BY r.start_date, r.event_id)
      bigquery   LAG(r.end_date) OVER (PARTITION BY r.person_id ORDER BY r.start_date, r.event_id)
      snowflake  LAG(r.end_date) OVER (PARTITION BY r.person_id ORDER BY r.start_date, r.event_id)
      spark      LAG(r.end_date) OVER (PARTITION BY r.person_id ORDER BY r.start_date, r.event_id)

# .sqlLosslessKey, .sqlLosslessKeyText and .sqlDateText render per family

    Code
      .ws3_snapshot(function(f) .sqlLosslessKey(f, "e.event_id"))
    Output
      postgresql CAST(e.event_id AS BIGINT)
      sqlite     CAST(e.event_id AS BIGINT)
      duckdb     CAST(e.event_id AS BIGINT)
      mysql      CAST(e.event_id AS SIGNED)
      sqlserver  CAST(e.event_id AS BIGINT)
      oracle     CAST(e.event_id AS NUMBER(19))
      redshift   CAST(e.event_id AS BIGINT)
      bigquery   CAST(e.event_id AS INT64)
      snowflake  CAST(e.event_id AS BIGINT)
      spark      CAST(e.event_id AS BIGINT)
    Code
      .ws3_snapshot(function(f) .sqlLosslessKeyText(f, "e.event_id"))
    Output
      postgresql CAST(CAST(e.event_id AS BIGINT) AS VARCHAR(20))
      sqlite     CAST(CAST(e.event_id AS BIGINT) AS VARCHAR(20))
      duckdb     CAST(CAST(e.event_id AS BIGINT) AS VARCHAR(20))
      mysql      CAST(CAST(e.event_id AS SIGNED) AS CHAR(20))
      sqlserver  CAST(CAST(e.event_id AS BIGINT) AS VARCHAR(20))
      oracle     CAST(CAST(e.event_id AS NUMBER(19)) AS VARCHAR2(20))
      redshift   CAST(CAST(e.event_id AS BIGINT) AS VARCHAR(20))
      bigquery   CAST(CAST(e.event_id AS INT64) AS STRING)
      snowflake  CAST(CAST(e.event_id AS BIGINT) AS VARCHAR(20))
      spark      CAST(CAST(e.event_id AS BIGINT) AS STRING)
    Code
      .ws3_snapshot(function(f) .sqlDateText(f, "c.cohort_start_date"))
    Output
      postgresql TO_CHAR(CAST(c.cohort_start_date AS DATE), 'YYYY-MM-DD')
      sqlite     date(c.cohort_start_date)
      duckdb     STRFTIME(CAST(c.cohort_start_date AS DATE), '%Y-%m-%d')
      mysql      DATE_FORMAT(CAST(c.cohort_start_date AS DATE), '%Y-%m-%d')
      sqlserver  CONVERT(VARCHAR(10), CAST(c.cohort_start_date AS DATE), 23)
      oracle     TO_CHAR(CAST(c.cohort_start_date AS DATE), 'YYYY-MM-DD')
      redshift   TO_CHAR(CAST(c.cohort_start_date AS DATE), 'YYYY-MM-DD')
      bigquery   FORMAT_DATE('%Y-%m-%d', CAST(c.cohort_start_date AS DATE))
      snowflake  TO_CHAR(CAST(c.cohort_start_date AS DATE), 'YYYY-MM-DD')
      spark      DATE_FORMAT(CAST(c.cohort_start_date AS DATE), 'yyyy-MM-dd')

# .sqlSha256Hex renders per family (NULL = unsupported)

    Code
      .ws3_snapshot(function(f) .sqlSha256Hex(f, "t.label"))
    Output
      postgresql ENCODE(SHA256(CONVERT_TO(t.label, 'UTF8')), 'hex')
      sqlite     <unsupported: NULL>
      duckdb     SHA256(t.label)
      mysql      LOWER(SHA2(CONVERT(t.label USING utf8mb4), 256))
      sqlserver  LOWER(CONVERT(VARCHAR(64), HASHBYTES('SHA2_256', CAST(t.label AS VARCHAR(8000))), 2))
      oracle     LOWER(RAWTOHEX(STANDARD_HASH(t.label, 'SHA256')))
      redshift   SHA2(t.label, 256)
      bigquery   TO_HEX(SHA256(t.label))
      snowflake  SHA2(t.label, 256)
      spark      SHA2(t.label, 256)
    Code
      .ws3_snapshot(function(f) .sqlSha256Hex(f, "t.encoding_hex", input = "hex"))
    Output
      postgresql ENCODE(SHA256(DECODE(t.encoding_hex, 'hex')), 'hex')
      sqlite     <unsupported: NULL>
      duckdb     SHA256(UNHEX(t.encoding_hex))
      mysql      LOWER(SHA2(UNHEX(t.encoding_hex), 256))
      sqlserver  LOWER(CONVERT(VARCHAR(64), HASHBYTES('SHA2_256', CONVERT(VARBINARY(8000), t.encoding_hex, 2)), 2))
      oracle     LOWER(RAWTOHEX(STANDARD_HASH(HEXTORAW(t.encoding_hex), 'SHA256')))
      redshift   SHA2(FROM_HEX(t.encoding_hex), 256)
      bigquery   TO_HEX(SHA256(FROM_HEX(t.encoding_hex)))
      snowflake  SHA2(TO_BINARY(t.encoding_hex, 'HEX'), 256)
      spark      SHA2(UNHEX(t.encoding_hex), 256)

# the availability matrix renders per family

    Code
      print(noquote(.sqlHelperMatrix()))
    Output
                       postgresql sqlite      duckdb     mysql      sqlserver  
      date_add_days    executable executable  executable executable snapshot   
      date_diff_days   executable executable  executable executable snapshot   
      calendar         executable executable  executable executable snapshot   
      date_literal     executable executable  executable executable snapshot   
      integer_literal  executable executable  executable executable snapshot   
      least_greatest   executable executable  executable executable snapshot   
      nulls_last       executable executable  executable executable snapshot   
      window_functions executable executable  executable executable snapshot   
      window_frames    executable executable  executable executable snapshot   
      lossless_key     executable executable  executable executable snapshot   
      date_text        executable executable  executable executable snapshot   
      sha256_sql       executable unsupported executable executable snapshot   
      subqueries_cte   executable executable  executable executable snapshot   
      session_table    executable executable  executable executable unsupported
                       oracle      redshift    bigquery    snowflake   spark      
      date_add_days    snapshot    snapshot    snapshot    snapshot    snapshot   
      date_diff_days   snapshot    snapshot    snapshot    snapshot    snapshot   
      calendar         snapshot    snapshot    snapshot    snapshot    snapshot   
      date_literal     snapshot    snapshot    snapshot    snapshot    snapshot   
      integer_literal  snapshot    snapshot    snapshot    snapshot    snapshot   
      least_greatest   snapshot    snapshot    snapshot    snapshot    snapshot   
      nulls_last       snapshot    snapshot    snapshot    snapshot    snapshot   
      window_functions snapshot    snapshot    snapshot    snapshot    snapshot   
      window_frames    snapshot    snapshot    snapshot    snapshot    snapshot   
      lossless_key     snapshot    snapshot    snapshot    snapshot    snapshot   
      date_text        snapshot    snapshot    snapshot    snapshot    snapshot   
      sha256_sql       snapshot    snapshot    snapshot    snapshot    snapshot   
      subqueries_cte   snapshot    snapshot    snapshot    snapshot    snapshot   
      session_table    unsupported unsupported unsupported unsupported unsupported

