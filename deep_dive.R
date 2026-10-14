set.seed(42)
df <- read.csv("data/Sales_Volume_Chart_data.csv", stringsAsFactors = FALSE)
names(df)[1] <- "Year"
df <- df[!is.na(df$Value..Actual.), ]
df$Value <- df$Value..Actual.

## ============ 1. CLUSTERING FORMATS BY LIFECYCLE SHAPE ============
formats <- unique(df$Format)
feat <- data.frame()
for (f in formats) {
  sub <- df[df$Format == f, c("Year","Value")]
  sub <- sub[order(sub$Year), ]
  if (nrow(sub) < 8) next
  peak_idx <- which.max(sub$Value)
  first_year <- min(sub$Year); peak_year <- sub$Year[peak_idx]; last_year <- max(sub$Year)
  climb <- peak_year - first_year; decline <- last_year - peak_year
  ratio <- decline / max(climb, 0.5)
  peak_val <- sub$Value[peak_idx]
  # normalized post-peak decay rate (avg annual % decline from peak to last point)
  post <- sub[sub$Year >= peak_year & sub$Value > 0, ]
  decay_rate <- if(nrow(post) >= 3) coef(lm(log(Value) ~ Year, data=post))[2] else NA
  feat <- rbind(feat, data.frame(Format=f, climb, decline, ratio, log_peak=log(peak_val), decay_rate, era=peak_year))
}
rownames(feat) <- feat$Format
fmat <- scale(feat[, c("climb","decline","ratio","log_peak","decay_rate","era")])
fmat[is.na(fmat)] <- 0
d <- dist(fmat)
hc <- hclust(d, method="ward.D2")
k <- 4
clusters <- cutree(hc, k=k)
cat("=== CLUSTERS (k=4), by lifecycle shape features ===\n")
for (i in 1:k) {
  cat(sprintf("\nCluster %d: %s\n", i, paste(feat$Format[clusters==i], collapse=", ")))
}

## ============ 2. BOOTSTRAP CI ON GROWTH/DECAY RATES ============
survivors <- c("Ringtones & Ringbacks","Download Single","Download Album",
               "Music Video (Physical)","CD","Vinyl Single","LP/EP")
cat("\n\n=== BOOTSTRAP 90% CI ON 2014-2024 CAGR (2000 resamples) ===\n")
for (f in survivors) {
  sub <- df[df$Format==f & df$Year>=2014, c("Year","Value")]
  sub <- sub[sub$Value > 0, ]
  if (nrow(sub) < 5) { cat(sprintf("%s: too few points\n", f)); next }
  n <- nrow(sub)
  boot_cagrs <- numeric(2000)
  for (b in 1:2000) {
    idx <- sample(1:n, n, replace=TRUE)
    bs <- sub[idx, ]
    if (length(unique(bs$Year)) < 3) { boot_cagrs[b] <- NA; next }
    fit <- tryCatch(lm(log(Value) ~ Year, data=bs), error=function(e) NULL)
    boot_cagrs[b] <- if(!is.null(fit)) exp(coef(fit)[2])-1 else NA
  }
  boot_cagrs <- boot_cagrs[!is.na(boot_cagrs)]
  ci <- quantile(boot_cagrs, c(0.05, 0.5, 0.95))
  cat(sprintf("%-25s point-CAGR range across resamples: [%.1f%%, %.1f%%], median %.1f%%\n",
              f, ci[1]*100, ci[3]*100, ci[2]*100))
}

## ============ 3. CHANGEPOINT SEARCH (objective breakpoint, not eyeballed) ============
cat("\n\n=== OBJECTIVE CHANGEPOINT SEARCH (min combined RSS of 2-segment fit) ===\n")
find_breakpoint <- function(years, vals) {
  candidates <- years[years > min(years)+3 & years < max(years)-3]
  best_rss <- Inf; best_year <- NA
  for (cy in candidates) {
    seg1 <- data.frame(Year=years[years<=cy], Value=vals[years<=cy])
    seg2 <- data.frame(Year=years[years>cy], Value=vals[years>cy])
    if (nrow(seg1)<3 || nrow(seg2)<3) next
    rss <- sum(resid(lm(Value~Year,data=seg1))^2) + sum(resid(lm(Value~Year,data=seg2))^2)
    if (rss < best_rss) { best_rss <- rss; best_year <- cy }
  }
  best_year
}
tot <- aggregate(Value ~ Year, df, sum); tot <- tot[order(tot$Year),]
cat("Total market breakpoint:", find_breakpoint(tot$Year, tot$Value), "\n")
for (f in c("CD","Cassette","Download Single")) {
  sub <- df[df$Format==f,]; sub <- sub[order(sub$Year),]
  cat(sprintf("%s breakpoint: %s\n", f, find_breakpoint(sub$Year, sub$Value)))
}
