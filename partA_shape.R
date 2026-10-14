source("R/07_lifecycle_models/common.R")
formats <- c("Cassette","Cassette Single","CD","CD Single","Download Album","Download Music Video",
             "Download Single","DVD Audio","Kiosk","Music Video (Physical)","Ringtones & Ringbacks","8 - Track")
out <- data.frame()
for (f in formats) {
  s <- get_series(f); yr <- s$Year; v <- s$Value; n <- length(v)
  ip <- which.max(v); climb <- yr[ip] - yr[1]
  b <- fit_bass(yr, v); g <- fit_gauss(yr, v)
  sst <- sum((sqrt(v) - mean(sqrt(v)))^2)
  tpk <- if (b$q > b$p) b$t0 + log(b$q / b$p) / (b$p + b$q) else b$t0
  out <- rbind(out, data.frame(
    Format = f, n = n, climb_yrs = climb, actual_peak = yr[ip], bass_peak = round(tpk, 1),
    p = round(b$p, 4), q = round(b$q, 3), q_over_p = round(b$q / b$p, 1),
    R2_bass = round(1 - b$rss / sst, 3), R2_gauss = round(1 - g$rss / sst, 3),
    AICc_bass = round(aicc(b$rss, n, 4), 1), AICc_gauss = round(aicc(g$rss, n, 3), 1),
    bass_minus_gauss = round(aicc(b$rss, n, 4) - aicc(g$rss, n, 3), 1),
    m_over_observed = round(b$m / sum(v), 2), bound_flag = b$atbound))
}
options(width = 250); print(out, row.names = FALSE)
cat("\nbass_minus_gauss < -2 => Bass clearly better; > +2 => symmetric bell clearly better; in between => indistinguishable\n")
