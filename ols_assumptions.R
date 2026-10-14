df <- read.csv("data/Sales_Volume_Chart_data.csv", stringsAsFactors = FALSE)
names(df)[1] <- "Year"
df <- df[!is.na(df$Value..Actual.), ]
df$Value <- df$Value..Actual.

skewness <- function(x) { n<-length(x); m<-mean(x); s<-sd(x); (sum((x-m)^3)/n)/s^3 }
excess_kurtosis <- function(x) { n<-length(x); m<-mean(x); s<-sd(x); (sum((x-m)^4)/n)/s^4 - 3 }

# manual Breusch-Pagan: regress squared residuals on X, test if that regression is significant
bp_test <- function(fit, x) {
  e2 <- resid(fit)^2
  aux <- lm(e2 ~ x)
  f <- summary(aux)$fstatistic
  pf(f[1], f[2], f[3], lower.tail=FALSE)
}

# manual RESET test: add fitted^2 to the model, test if it's significant (nonlinearity check)
reset_test <- function(fit, data) {
  data$fit2 <- fitted(fit)^2
  aug <- lm(update(formula(fit), . ~ . + fit2), data=data)
  anova(fit, aug)[2, "Pr(>F)"]
}

for (fmt in c("CD","LP/EP","Download Single","Vinyl Single")) {
  sub <- df[df$Format==fmt & df$Year>=2014 & df$Value>0, c("Year","Value")]
  sub <- sub[order(sub$Year),]
  fit <- lm(log(Value) ~ Year, data=sub)
  res <- resid(fit)

  cat(sprintf("\n=== %s (n=%d) ===\n", fmt, nrow(sub)))
  cat(sprintf("Raw Value  -> skewness=%.2f, excess kurtosis=%.2f\n", skewness(sub$Value), excess_kurtosis(sub$Value)))
  cat(sprintf("log(Value) -> skewness=%.2f, excess kurtosis=%.2f  (should be closer to 0 if log-transform helped)\n",
              skewness(log(sub$Value)), excess_kurtosis(log(sub$Value))))
  cat(sprintf("Residuals  -> skewness=%.2f, excess kurtosis=%.2f\n", skewness(res), excess_kurtosis(res)))

  sw <- shapiro.test(res)
  cat(sprintf("Shapiro-Wilk normality of residuals: W=%.3f, p=%.3f %s\n", sw$statistic, sw$p.value,
              ifelse(sw$p.value<0.05, "-> REJECT normality", "-> normality OK")))

  bp_p <- bp_test(fit, sub$Year)
  cat(sprintf("Breusch-Pagan homoscedasticity test: p=%.3f %s\n", bp_p,
              ifelse(bp_p<0.05, "-> REJECT homoscedasticity (heteroscedastic)", "-> homoscedasticity OK")))

  reset_p <- reset_test(fit, sub)
  cat(sprintf("RESET linearity test: p=%.3f %s\n", reset_p,
              ifelse(reset_p<0.05, "-> REJECT linearity (relationship may be nonlinear)", "-> linearity OK")))
}
