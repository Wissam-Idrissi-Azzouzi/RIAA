source("R/07_lifecycle_models/common.R")
formats <- c("Cassette","Cassette Single","CD","CD Single","Download Album","Download Music Video","Download Single",
             "DVD Audio","Kiosk","Music Video (Physical)","Ringtones & Ringbacks","SACD","Vinyl Single")
bass_ok_formats <- c("Cassette","Cassette Single","CD","CD Single","Download Album","Download Music Video",
                     "Download Single","Kiosk","Music Video (Physical)","Ringtones & Ringbacks")
h <- 5; min_post <- 4
rows <- data.frame()
for (f in formats) {
  s <- get_series(f); yr <- s$Year; v <- s$Value; n <- length(v); ip <- which.max(v); peak <- v[ip]
  if (ip + min_post > n - h) next
  for (o in (ip + min_post):(n - h)) {
    te <- (o + 1):(o + h); actual <- v[te]
    post <- ip:o; ok <- post[v[post] > 0]
    preds <- list()
    if (length(ok) >= 4) preds$ExpPeak <- exp_fit(yr[ok], v[ok])$predict(yr[te])
    ok10 <- tail(ok, 10)
    if (length(ok10) >= 4) preds$ExpRecent10 <- exp_fit(yr[ok10], v[ok10])$predict(yr[te])
    if (length(ok) >= 5) preds$Floor <- fit_floor(yr[ok], v[ok])$predict(yr[te])
    if (f %in% bass_ok_formats && o >= 9) {
      b <- fit_bass(yr[1:o], v[1:o], nstart = 25); if (!is.null(b)) preds$Bass <- b$predict(yr[te])
    }
    for (mname in names(preds)) {
      rows <- rbind(rows, data.frame(Format = f, origin = yr[o], Model = mname,
        NMAE = mean(abs(preds[[mname]] - actual)) / peak, Bias = mean(preds[[mname]] - actual) / peak))
    }
  }
}
write.csv(rows, "results/backtest_rows.csv", row.names = FALSE)
cat("Total (format, origin, model) evaluations:", nrow(rows), " | origins per model:\n")
print(table(rows$Model) / 1)

# per-format mean NMAE by model, then macro average across formats (so long series do not dominate)
pf <- aggregate(NMAE ~ Format + Model, rows, mean)
wide <- reshape(pf, idvar = "Format", timevar = "Model", direction = "wide"); names(wide) <- sub("NMAE\\.", "", names(wide))
options(width = 200)
cat("\n=== Mean 5-yr-ahead forecast error, as % of the format's own peak (lower = better) ===\n")
w2 <- wide; w2[,-1] <- round(100 * w2[,-1], 2); print(w2, row.names = FALSE)

cat("\n=== Macro-average across formats (each format counts once) ===\n")
print(round(100 * tapply(pf$NMAE, pf$Model, mean), 2))
cat("\n=== Same, but only formats where Bass was available (apples-to-apples) ===\n")
bf <- pf[pf$Format %in% pf$Format[pf$Model == "Bass"], ]
print(round(100 * tapply(bf$NMAE, bf$Model, mean), 2))

# per-format winner
winners <- aggregate(NMAE ~ Format, pf, function(x) NA)  # placeholder
best <- sapply(split(pf, pf$Format), function(d) d$Model[which.min(d$NMAE)])
cat("\n=== Best model per format ===\n"); print(best); cat("\nWin counts:\n"); print(table(best))

bias <- aggregate(Bias ~ Format + Model, rows, mean)
cat("\n=== Average signed bias, % of peak (positive = over-forecasts, i.e. too optimistic) ===\n")
print(round(100 * tapply(bias$Bias, bias$Model, mean), 2))
