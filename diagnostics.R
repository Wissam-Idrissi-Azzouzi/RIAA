df <- read.csv("data/Sales_Volume_Chart_data.csv", stringsAsFactors = FALSE)
names(df)[1] <- "Year"
df <- df[!is.na(df$Value..Actual.), ]
df$Value <- df$Value..Actual.

check_diagnostics <- function(fmt) {
  sub <- df[df$Format==fmt & df$Year>=2014 & df$Value>0, c("Year","Value")]
  sub <- sub[order(sub$Year),]
  fit <- lm(log(Value) ~ Year, data=sub)
  n <- nrow(sub); p <- 2
  lev <- hatvalues(fit)
  cooksd <- cooks.distance(fit)
  rstud <- rstudent(fit)
  lev_thresh <- 2*p/n          # common leverage cutoff
  cooks_thresh <- 4/n          # common Cook's D cutoff
  out <- data.frame(Year=sub$Year, Value=round(sub$Value,1),
                     StudResid=round(rstud,2), Leverage=round(lev,3), CooksD=round(cooksd,3))
  out$FlagOutlier <- abs(out$StudResid) > 2
  out$FlagLeverage <- out$Leverage > lev_thresh
  out$FlagInfluential <- out$CooksD > cooks_thresh
  cat(sprintf("\n=== %s (n=%d, leverage cutoff=%.2f, Cook's D cutoff=%.2f) ===\n", fmt, n, lev_thresh, cooks_thresh))
  print(out, row.names=FALSE)
  cat(sprintf("Original slope CAGR: %.1f%%\n", (exp(coef(fit)[2])-1)*100))
  # Refit excluding the single most influential point to see how much the estimate moves
  worst <- which.max(cooksd)
  sub2 <- sub[-worst,]
  fit2 <- lm(log(Value) ~ Year, data=sub2)
  cat(sprintf("Refit excluding year %d (highest Cook's D): CAGR = %.1f%%  [shift of %.1f pts]\n",
              sub$Year[worst], (exp(coef(fit2)[2])-1)*100,
              ((exp(coef(fit2)[2])-1) - (exp(coef(fit)[2])-1))*100))
}

for (f in c("CD","LP/EP","Download Single","Vinyl Single")) check_diagnostics(f)
