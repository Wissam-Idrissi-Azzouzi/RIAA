df <- read.csv("data/Sales_Volume_Chart_data.csv", stringsAsFactors = FALSE)
names(df)[1] <- "Year"
df <- df[!is.na(df$Value..Actual.), ]
df$Value <- df$Value..Actual.

survivors <- c("Ringtones & Ringbacks","Download Single","Download Album",
               "Other Physical","Music Video (Physical)","CD","Vinyl Single","LP/EP")

# Recent-window log-linear trend (last 10 years where available) -> annualized % change, and
# projected year to cross a "functionally dead" threshold (1% of format's own 2024 value floor, min $0.1M)
results <- data.frame()
for (f in survivors) {
  sub <- df[df$Format == f, c("Year","Value")]
  sub <- sub[order(sub$Year), ]
  sub <- sub[sub$Value > 0, ]  # log requires positive
  recent <- tail(sub, 10)
  n <- nrow(recent)
  if (n < 4) {
    results <- rbind(results, data.frame(Format=f, N_years=n, AnnualPctChange=NA,
                                          Val2024=tail(sub$Value,1), ProjectedYearNearZero="insufficient data (<4 pts)"))
    next
  }
  fit <- lm(log(Value) ~ Year, data=recent)
  slope <- coef(fit)[2]
  annual_pct <- (exp(slope)-1)*100
  last_year <- max(recent$Year); last_val <- recent$Value[recent$Year==last_year]
  threshold <- 0.1  # $0.1M functionally irrelevant
  if (slope < 0 & last_val > threshold) {
    years_to_zero <- (log(threshold) - log(last_val)) / slope
    proj_year <- round(last_year + years_to_zero)
  } else if (slope < 0) {
    proj_year <- last_year
  } else {
    proj_year <- "GROWING - no crossing (trend positive)"
  }
  results <- rbind(results, data.frame(Format=f, N_years=n, AnnualPctChange=round(annual_pct,1),
                                        Val2024=round(last_val,1), ProjectedYearNearZero=as.character(proj_year)))
}
cat("=== 10-YEAR TREND EXTRAPOLATION (log-linear) ===\n")
print(results, row.names=FALSE)

# Deceleration check for the two "growing" formats: is growth rate itself slowing?
cat("\n=== YoY % change, LP/EP and Vinyl Single, last 8 years (is growth decelerating?) ===\n")
for (f in c("LP/EP","Vinyl Single")) {
  sub <- df[df$Format==f, c("Year","Value")]
  sub <- sub[order(sub$Year),]
  sub$pct <- c(NA, round(diff(sub$Value)/head(sub$Value,-1)*100,1))
  print(tail(sub,8))
}
