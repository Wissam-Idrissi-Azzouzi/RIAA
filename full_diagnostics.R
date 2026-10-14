df <- read.csv("data/Sales_Volume_Chart_data.csv", stringsAsFactors = FALSE)
names(df)[1] <- "Year"
df <- df[!is.na(df$Value..Actual.), ]
df$Value <- df$Value..Actual.

full_check <- function(fmt) {
  sub <- df[df$Format==fmt & df$Year>=2014 & df$Value>0, c("Year","Value")]
  sub <- sub[order(sub$Year),]
  fit <- lm(log(Value) ~ Year, data=sub)
  n <- nrow(sub); p <- 2

  rstud   <- rstudent(fit)
  lev     <- hatvalues(fit)
  cooksd  <- cooks.distance(fit)
  dffit   <- dffits(fit)
  cooks_p <- pf(cooksd, p, n-p)   # formal Cook's D percentile via F-distribution

  lev_thresh   <- 2*p/n
  dffit_thresh <- 2*sqrt(p/n)     # standard small-sample DFFITS cutoff (2*sqrt(p/n)); |DFFITS|>1 rule also checked

  out <- data.frame(
    Year = sub$Year,
    yOutlier   = abs(rstud) > 2,
    xOutlier_Leverage = lev > lev_thresh,
    PureLeverage = (lev > lev_thresh) & (abs(rstud) <= 2),   # high leverage but sits on the line
    DFFITS = round(dffit,2),
    DFFITS_flag = abs(dffit) > 1 | abs(dffit) > dffit_thresh,
    CooksD_pct = paste0(round(cooks_p*100,1), "%"),
    CooksD_call = ifelse(cooks_p>=0.5, "MAJOR", ifelse(cooks_p>=0.2,"moderate", ifelse(cooks_p>=0.1,"small","negligible")))
  )
  cat(sprintf("\n=== %s (n=%d) ===\n", fmt, n))
  print(out, row.names=FALSE)
}

for (f in c("CD","LP/EP","Download Single","Vinyl Single")) full_check(f)
