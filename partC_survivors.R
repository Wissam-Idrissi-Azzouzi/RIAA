source("R/07_lifecycle_models/common.R")

## ---- (1) Backtest restricted to LATE-decline origins (>= 8 yrs after peak), where the survivors actually are ----
rows <- read.csv("results/backtest_rows.csv", stringsAsFactors = FALSE)
pk <- sapply(unique(rows$Format), function(f) { s <- get_series(f); s$Year[which.max(s$Value)] })
rows$post <- rows$origin - pk[rows$Format]
late <- rows[rows$post >= 8, ]
pf <- aggregate(NMAE ~ Format + Model, late, mean)
cat("=== Late-decline origins only (>= 8 yrs past peak): macro-avg 5-yr error, % of peak ===\n")
print(round(100 * tapply(pf$NMAE, pf$Model, mean), 2))
bf <- pf[pf$Format %in% pf$Format[pf$Model == "Bass"], ]
cat("...same, Bass-eligible formats only:\n"); print(round(100 * tapply(bf$NMAE, bf$Model, mean), 2))
cat("Late-origin evaluations per model:\n"); print(table(late$Model))
cat("Best model per format (late origins):\n"); print(sapply(split(pf, pf$Format), function(d) d$Model[which.min(d$NMAE)]))

## ---- (2) 2029 projections for the survivors under each model ----
cat("\n\n=== 2029 projections for the surviving formats ($M) ===\n")
tab <- data.frame()
for (f in c("Ringtones & Ringbacks","Download Single","Download Album","Music Video (Physical)","CD")) {
  s <- get_series(f); yr <- s$Year; v <- s$Value; ip <- which.max(v); last <- tail(v, 1)
  ar <- s[s$Year >= 2014 & s$Value > 0, ]; art <- exp_fit(ar$Year, ar$Value)
  article <- last * (1 + art$cagr)^5
  pw <- ip:length(v); ok <- pw[v[pw] > 0]
  ex <- exp_fit(yr[ok], v[ok]); fl <- fit_floor(yr[ok], v[ok])
  nn <- length(ok)
  d_aicc <- aicc(ex$rss, nn, 2) - aicc(fl$rss, nn, 3)       # >0 => floor model preferred
  b <- fit_bass(yr, v, nstart = 80)
  tab <- rbind(tab, data.frame(Format = f, in_article = NA, article_method = round(article, 2),
    exp_from_peak = round(ex$predict(2029), 2), floor_model = round(fl$predict(2029), 2),
    floor_level = round(fl$floor, 2), floor_pref_dAICc = round(d_aicc, 1),
    bass = round(b$predict(2029), 2), last_2024 = round(last, 2)))
}
tab$in_article <- c(0.7, 38, 6, 0.15, 15)
options(width = 220); print(tab, row.names = FALSE)
cat("floor_pref_dAICc: > +2 => data prefer a floor over decay-to-zero; < -2 => prefer pure decay; between => can't tell\n")

## ---- Vinyl Single: recovery regime 2009-2024 ----
s <- get_series("Vinyl Single"); w <- s[s$Year >= 2009 & s$Value > 0, ]
ex <- exp_fit(w$Year, w$Value); fl <- fit_floor(w$Year, w$Value)
cat(sprintf("\nVinyl Single (2009-2024): exp trend %.1f%%/yr -> 2029 = %.2f | floor model: floor=%.2f, 2029 = %.2f | dAICc(floor pref) = %.1f\n",
  100*ex$cagr, ex$predict(2029), fl$floor, fl$predict(2029), aicc(ex$rss, nrow(w), 2) - aicc(fl$rss, nrow(w), 3)))
