df <- read.csv("data/Sales_Volume_Chart_data.csv", stringsAsFactors = FALSE)
names(df)[1] <- "Year"
df <- df[!is.na(df$Value..Actual.), ]
df$Value <- df$Value..Actual.

survivors <- c("Ringtones & Ringbacks","Download Single","Download Album","Other Physical",
               "Music Video (Physical)","CD","Vinyl Single","LP/EP")

for (f in survivors) {
  sub <- df[df$Format == f, c("Year","Value")]
  sub <- sub[order(sub$Year), ]
  n_pts <- nrow(sub)
  recent <- sub[sub$Year >= 2014, ]  # last decade for a cleaner regime
  cat(sprintf("\n--- %s ---\n", f))
  cat(sprintf("Total data points: %d | Last decade points: %d | 2024 value: %.2f\n", n_pts, nrow(recent), sub$Value[sub$Year==2024]))

  if (nrow(recent) < 4) {
    cat("Too few recent points to model responsibly.\n")
    next
  }
  # Trend direction: growing or shrinking in last decade?
  recent_clean <- recent[recent$Value > 0, ]
  if (nrow(recent_clean) < 4) { cat("Not enough positive values to log-fit.\n"); next }
  fit <- lm(log(Value) ~ Year, data = recent_clean)
  slope <- coef(fit)[2]
  r2 <- summary(fit)$r.squared
  cagr <- exp(slope) - 1
  direction <- ifelse(slope > 0, "GROWING", "DECLINING")
  cat(sprintf("Log-linear fit (2014-2024): %s at %.1f%%/yr, R^2=%.2f\n", direction, cagr*100, r2))

  # Project 5 years forward
  last_year <- max(recent_clean$Year)
  last_val <- recent_clean$Value[recent_clean$Year == last_year]
  proj_years <- (last_year+1):(last_year+5)
  proj_vals <- last_val * (1+cagr)^(1:5)
  cat("Projection 2025-2029:", paste(round(proj_vals,2), collapse=", "), "\n")
}
