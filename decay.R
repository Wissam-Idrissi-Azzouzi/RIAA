df <- read.csv("data/Sales_Volume_Chart_data.csv", stringsAsFactors = FALSE)
names(df)[1] <- "Year"
df <- df[!is.na(df$Value..Actual.), ]
df$Value <- df$Value..Actual.

# For each format: peak year/value, and years-to-90%-decline from peak
formats <- unique(df$Format)
results <- data.frame()
for (f in formats) {
  sub <- df[df$Format == f, c("Year","Value")]
  sub <- sub[order(sub$Year), ]
  if (nrow(sub) < 3) next
  peak_idx <- which.max(sub$Value)
  peak_year <- sub$Year[peak_idx]
  peak_val <- sub$Value[peak_idx]
  post <- sub[sub$Year >= peak_year, ]
  threshold <- peak_val * 0.10
  below <- post[post$Value <= threshold, ]
  if (nrow(below) > 0) {
    year90 <- min(below$Year)
    years_to_90pct_decline <- year90 - peak_year
  } else {
    years_to_90pct_decline <- NA
  }
  last_year <- max(sub$Year)
  last_val <- sub$Value[sub$Year == last_year]
  n_years_post_peak <- last_year - peak_year
  results <- rbind(results, data.frame(Format=f, PeakYear=peak_year, PeakVal=round(peak_val,1),
                                        LastYear=last_year, LastVal=round(last_val,2),
                                        YearsToNeg90=years_to_90pct_decline,
                                        YearsSincePeak=n_years_post_peak))
}
results <- results[order(results$YearsToNeg90), ]
print(results, row.names=FALSE)
