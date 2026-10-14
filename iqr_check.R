df <- read.csv("data/Sales_Volume_Chart_data.csv", stringsAsFactors = FALSE)
names(df)[1] <- "Year"
df <- df[!is.na(df$Value..Actual.), ]
df$Value <- df$Value..Actual.

iqr_flag <- function(x) {
  q <- quantile(x, c(0.25, 0.75), na.rm=TRUE)
  iqr <- q[2]-q[1]
  lower <- q[1]-1.5*iqr; upper <- q[2]+1.5*iqr
  list(lower=lower, upper=upper, flag=(x<lower | x>upper))
}

## ---- 1. IQR on regression residuals (cross-check vs rstudent method) ----
cat("=== IQR METHOD ON REGRESSION RESIDUALS (vs earlier rstudent method) ===\n")
for (fmt in c("CD","LP/EP","Download Single","Vinyl Single")) {
  sub <- df[df$Format==fmt & df$Year>=2014 & df$Value>0, c("Year","Value")]
  sub <- sub[order(sub$Year),]
  fit <- lm(log(Value) ~ Year, data=sub)
  resid_vals <- resid(fit)
  res <- iqr_flag(resid_vals)
  cat(sprintf("\n%s: IQR fence [%.3f, %.3f]\n", fmt, res$lower, res$upper))
  flagged_years <- sub$Year[res$flag]
  cat("  IQR-flagged years:", if(length(flagged_years)==0) "none" else paste(flagged_years, collapse=", "), "\n")
}

## ---- 2. IQR on YoY % change, across ALL formats/years ----
cat("\n\n=== IQR METHOD ON RAW YoY %CHANGE (whole dataset, all formats) ===\n")
raw <- read.csv("data/Sales_Volume_Chart_data.csv", stringsAsFactors = FALSE)
names(raw)[1] <- "Year"
pct <- raw$Change.vs..Prior.Year
pct_num <- suppressWarnings(as.numeric(gsub("[%+]", "", pct)))
raw$pct_num <- pct_num
valid <- raw[!is.na(raw$pct_num), c("Year","Format","pct_num")]
res <- iqr_flag(valid$pct_num)
cat(sprintf("N valid YoY%% values: %d | IQR fence: [%.1f%%, %.1f%%]\n", nrow(valid), res$lower, res$upper))
flagged <- valid[res$flag, ]
flagged <- flagged[order(-abs(flagged$pct_num)), ]
cat("Number of IQR-flagged extreme YoY changes:", nrow(flagged), "of", nrow(valid), "\n\n")
print(head(flagged, 20), row.names=FALSE)
