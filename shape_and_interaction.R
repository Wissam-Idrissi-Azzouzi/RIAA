df <- read.csv("data/Sales_Volume_Chart_data.csv", stringsAsFactors = FALSE)
names(df)[1] <- "Year"
df <- df[!is.na(df$Value..Actual.), ]
df$Value <- df$Value..Actual.

# ---------- QUESTION 1: shape of each format's lifecycle ----------
# For each format with a real lifecycle (>=8 recorded years, peak not at an endpoint),
# compare "years climbing to peak" vs "years declining from peak to last recorded year"
formats <- unique(df$Format)
shape <- data.frame()
for (f in formats) {
  sub <- df[df$Format == f, c("Year","Value")]
  sub <- sub[order(sub$Year), ]
  if (nrow(sub) < 8) next
  peak_idx <- which.max(sub$Value)
  first_year <- min(sub$Year)
  peak_year <- sub$Year[peak_idx]
  last_year <- max(sub$Year)
  climb_years <- peak_year - first_year
  decline_years <- last_year - peak_year
  is_endpoint_peak <- (peak_year == first_year) | (peak_year == last_year)
  shape <- rbind(shape, data.frame(Format=f, FirstYear=first_year, PeakYear=peak_year,
                                    LastYear=last_year, ClimbYears=climb_years,
                                    DeclineYears=decline_years,
                                    Ratio_Decline_to_Climb = round(decline_years/max(climb_years,0.1),2),
                                    PeakAtEndpoint=is_endpoint_peak))
}
cat("=== LIFECYCLE SHAPE (climb vs decline duration) ===\n")
print(shape, row.names=FALSE)

# ---------- QUESTION 2: competition vs complementarity ----------
# Build wide matrix of value by year x format
wide <- reshape(df[,c("Year","Format","Value")], idvar="Year", timevar="Format", direction="wide")
names(wide) <- gsub("^Value\\.", "", names(wide))
names(wide) <- make.names(names(wide))
wide <- wide[order(wide$Year), ]

# Year-over-year CHANGE (delta) for each format, aligned by year
deltas <- wide
for (col in names(wide)[-1]) {
  deltas[[col]] <- c(NA, diff(wide[[col]]))
}

# Correlate YoY deltas pairwise (only using overlapping non-NA years) for a few key format pairs
pairs <- list(
  c("CD","Cassette"),
  c("Download.Single","CD"),
  c("Ringtones...Ringbacks","Download.Single"),
  c("LP.EP","CD"),
  c("LP.EP","Download.Single"),
  c("Download.Single","Download.Album")
)
cat("\n=== YoY DELTA CORRELATIONS (negative = one rises as other falls, i.e. substitution) ===\n")
for (p in pairs) {
  a <- deltas[[p[1]]]; b <- deltas[[p[2]]]
  ok <- complete.cases(a,b)
  if (sum(ok) >= 5) {
    r <- cor(a[ok], b[ok])
    cat(sprintf("%-25s vs %-25s : n=%2d, r=%.2f\n", p[1], p[2], sum(ok), r))
  } else {
    cat(sprintf("%-25s vs %-25s : insufficient overlap (n=%d)\n", p[1], p[2], sum(ok)))
  }
}

# ---------- Total market size over time: is the "pie" growing or just reshuffling? ----------
tot <- aggregate(Value ~ Year, df, sum)
tot <- tot[order(tot$Year), ]
cat("\n=== TOTAL MARKET (sum of all formats) SELECTED YEARS ===\n")
print(tot[tot$Year %in% c(1973,1978,1984,1988,1994,1999,2000,2004,2008,2012,2016,2020,2024), ])
