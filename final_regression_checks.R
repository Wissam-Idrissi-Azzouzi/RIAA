df <- read.csv("data/Sales_Volume_Chart_data.csv", stringsAsFactors = FALSE)
names(df)[1] <- "Year"
df <- df[!is.na(df$Value..Actual.), ]
df$Value <- df$Value..Actual.

durbin_watson <- function(res) {
  n <- length(res)
  dw <- sum(diff(res)^2) / sum(res^2)
  dw
}

for (fmt in c("CD","LP/EP","Download Single","Vinyl Single")) {
  sub <- df[df$Format==fmt & df$Year>=2014 & df$Value>0, c("Year","Value")]
  sub <- sub[order(sub$Year),]
  fit <- lm(log(Value) ~ Year, data=sub)
  s <- summary(fit)
  res <- resid(fit)

  cat(sprintf("\n=== %s (n=%d) ===\n", fmt, nrow(sub)))

  # 1. Formal significance test of slope
  slope_row <- s$coefficients["Year", ]
  cat(sprintf("Slope significance test: t=%.2f, p=%.5f %s\n", slope_row["t value"], slope_row["Pr(>|t|)"],
              ifelse(slope_row["Pr(>|t|)"]<0.05, "-> trend IS statistically significant", "-> trend NOT significant")))

  # 2. Durbin-Watson for autocorrelated residuals
  dw <- durbin_watson(res)
  cat(sprintf("Durbin-Watson statistic: %.2f  (~2.0=no autocorrelation, <1.5=positive autocorrelation, >2.5=negative)\n", dw))

  # 3. Prediction interval for 2029
  newdata <- data.frame(Year=2029)
  pred <- predict(fit, newdata, interval="prediction", level=0.90)
  cat(sprintf("2029 prediction (90%% PI, log scale back-transformed): point=%.1f, range=[%.1f, %.1f]\n",
              exp(pred[1,"fit"]), exp(pred[1,"lwr"]), exp(pred[1,"upr"])))
}
