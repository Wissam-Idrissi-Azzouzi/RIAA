df <- read.csv("data/Sales_Volume_Chart_data.csv", stringsAsFactors = FALSE)
names(df)[1] <- "Year"
df <- df[!is.na(df$Value..Actual.) & df$Value..Actual. >= 0, ]
df$Value <- df$Value..Actual.

formats <- unique(df$Format)
cat("Total formats in data:", length(formats), "\n\n")

## ============ REDO #1: first approach, now covering all 17 (no n<8 filter) ============
feat <- data.frame()
for (f in formats) {
  sub <- df[df$Format == f, c("Year","Value")]
  sub <- sub[order(sub$Year), ]
  peak_idx <- which.max(sub$Value)
  first_year <- min(sub$Year); peak_year <- sub$Year[peak_idx]; last_year <- max(sub$Year)
  climb <- peak_year - first_year; decline <- last_year - peak_year
  ratio <- decline / max(climb, 0.5)
  peak_val <- sub$Value[peak_idx]
  post <- sub[sub$Year >= peak_year & sub$Value > 0, ]
  decay_rate <- if(nrow(post) >= 3) coef(lm(log(Value) ~ Year, data=post))[2] else NA
  feat <- rbind(feat, data.frame(Format=f, climb, decline, ratio, log_peak=log(peak_val), decay_rate, era=peak_year))
}
cat("Formats in feature table:", nrow(feat), "\n")
fmat <- scale(feat[, c("climb","decline","ratio","log_peak","decay_rate","era")])
fmat[is.na(fmat)] <- 0
hc <- hclust(dist(fmat), method="ward.D2")
clusters1 <- cutree(hc, k=4)
cat("\n=== REDO OF MY ORIGINAL CLUSTERING (k=4, ALL 17 FORMATS) ===\n")
for (i in 1:4) cat(sprintf("Cluster %d: %s\n", i, paste(feat$Format[clusters1==i], collapse=", ")))

## ============ REDO #2: alternative approach (lifespan/years_to_peak/decline_duration, kmeans) ============
lifecycle_features <- data.frame()
for (f in formats) {
  sub <- df[df$Format==f, c("Year","Value")]
  sub <- sub[order(sub$Year),]
  first_year <- min(sub$Year); last_year <- max(sub$Year)
  peak_year <- sub$Year[which.max(sub$Value)]
  lifecycle_features <- rbind(lifecycle_features, data.frame(
    Format=f, first_year, peak_year, last_year,
    recorded_years=nrow(sub), peak_value=max(sub$Value),
    lifespan=last_year-first_year, years_to_peak=peak_year-first_year,
    decline_duration=last_year-peak_year
  ))
}
cat("\nFormats in lifecycle_features:", nrow(lifecycle_features), "\n")
print(lifecycle_features, row.names=FALSE)

cluster_data <- lifecycle_features[, c("lifespan","recorded_years","years_to_peak","decline_duration")]
cluster_scaled <- scale(cluster_data)
set.seed(123)
km <- kmeans(cluster_scaled, centers=3, nstart=25)
lifecycle_features$cluster <- km$cluster

cat("\n=== ALTERNATIVE (kmeans, k=3, ALL 17 FORMATS) ===\n")
lc_sorted <- lifecycle_features[order(lifecycle_features$cluster, lifecycle_features$peak_year), ]
print(lc_sorted[,c("Format","cluster","peak_year","years_to_peak","decline_duration","lifespan","recorded_years")], row.names=FALSE)

for (i in 1:3) cat(sprintf("\nCluster %d: %s\n", i, paste(lifecycle_features$Format[lifecycle_features$cluster==i], collapse=", ")))

## ============ COMPARISON ============
cat("\n\n=== SIDE-BY-SIDE: first clustering (k=4) vs alternative kmeans (k=3) ===\n")
comp <- merge(data.frame(Format=feat$Format, WardCluster=clusters1),
              data.frame(Format=lifecycle_features$Format, KmeansCluster=lifecycle_features$cluster), by="Format")
print(comp[order(comp$WardCluster, comp$KmeansCluster),], row.names=FALSE)
