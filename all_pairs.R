df <- read.csv("data/Sales_Volume_Chart_data.csv", stringsAsFactors = FALSE)
names(df)[1] <- "Year"
df <- df[!is.na(df$Value..Actual.), ]
df$Value <- df$Value..Actual.

wide <- reshape(df[,c("Year","Format","Value")], idvar="Year", timevar="Format", direction="wide")
names(wide) <- gsub("^Value\\.", "", names(wide))
wide <- wide[order(wide$Year), ]
fmt_cols <- names(wide)[-1]

# YoY deltas for every format
deltas <- wide
for (col in fmt_cols) deltas[[col]] <- c(NA, diff(wide[[col]]))

pairs <- combn(fmt_cols, 2, simplify = FALSE)
cat("Total possible pairs:", length(pairs), "\n")

results <- data.frame()
for (p in pairs) {
  a <- deltas[[p[1]]]; b <- deltas[[p[2]]]
  ok <- complete.cases(a, b)
  n <- sum(ok)
  if (n >= 5) {
    r <- cor(a[ok], b[ok])
    results <- rbind(results, data.frame(F1=p[1], F2=p[2], n=n, r=round(r,3)))
  }
}
cat("Pairs with >=5 overlapping years (i.e. actually testable):", nrow(results), "\n")
cat("Pairs excluded (insufficient overlap):", length(pairs) - nrow(results), "\n\n")

results <- results[order(results$r), ]
cat("=== TOP 10 STRONGEST NEGATIVE (SUBSTITUTION) ===\n")
print(head(results, 10), row.names=FALSE)

cat("\n=== TOP 10 STRONGEST POSITIVE (MOVE TOGETHER) ===\n")
print(tail(results, 10)[order(-tail(results,10)$r),], row.names=FALSE)

write.csv(results, "results/all_pairs_results.csv", row.names=FALSE)
