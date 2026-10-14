# Figures for the RIAA format-lifecycle analysis.
# NOTE: the export's "Value (Actual)" column is labelled in dollars, but it matches RIAA's
# published UNIT shipments (e.g. CD 2000 = 942.5M units). All axes here say "units".
# Run from the repo root: Rscript R/08_figures/make_figures.R
df <- read.csv("data/Sales_Volume_Chart_data.csv", stringsAsFactors = FALSE, check.names = FALSE)
names(df)[1] <- "Year"
df <- df[!is.na(df$`Value (Actual)`), c("Year", "Format", "Value (Actual)")]
names(df)[3] <- "Units"
df$Units <- pmax(df$Units, 0)
wide <- tapply(df$Units, list(df$Year, df$Format), sum)
wide_na <- wide            # keep NA = "format not reported that year" for the line panels
wide[is.na(wide)] <- 0
years <- as.integer(rownames(wide))

groups <- list(
  `8-Track`  = c("8 - Track"),
  Vinyl      = c("LP/EP", "Vinyl Single"),
  Cassettes  = c("Cassette", "Cassette Single", "Other Tapes"),
  CDs        = c("CD", "CD Single", "SACD", "DVD Audio"),
  Videos     = c("Music Video (Physical)", "Download Music Video"),
  Digital    = c("Download Single", "Download Album", "Ringtones & Ringbacks"),
  Misc       = c("Kiosk", "Other Physical")
)
stopifnot(setequal(unlist(groups), colnames(wide)))   # all 17 formats accounted for
cols <- c(`8-Track` = "#9B9B9B", Vinyl = "#2C5F8A", Cassettes = "#4CAF50", CDs = "#F2A93B",
          Videos = "#2FB5C4", Digital = "#D64550", Misc = "#9B7FC7")
G <- sapply(groups, function(f) rowSums(wide[, f, drop = FALSE]))

stack_plot <- function(M, cols, main, ylab = "Units shipped (millions)") {
  cum <- t(apply(M, 1, cumsum)); base <- cbind(0, cum[, -ncol(cum), drop = FALSE])
  plot(range(years), c(0, max(cum) * 1.04), type = "n", xlab = "", ylab = ylab, main = main, las = 1, bty = "n")
  grid(col = "grey90", lty = 1)
  for (j in seq_len(ncol(M)))
    polygon(c(years, rev(years)), c(cum[, j], rev(base[, j])), col = cols[j], border = NA)
  legend("topleft", legend = colnames(M), fill = cols, bty = "n", cex = 0.85)
}

# 1. Units by format group
png("figures/units_by_group_stacked.png", 1600, 900, res = 160)
par(mar = c(3, 5, 3, 1))
stack_plot(G, cols[colnames(G)], "U.S. recorded-music units shipped, by format group, 1973-2024")
dev.off()

# 2. Replacement for the (invalid) inflation chart: how much of the total is download singles?
tot <- rowSums(wide); ds <- wide[, "Download Single"]
png("figures/units_total_vs_ex_download_singles.png", 1600, 900, res = 160)
par(mar = c(3, 5, 3, 1))
plot(years, tot, type = "l", lwd = 3, col = "#444444", las = 1, bty = "n", ylim = c(0, max(tot) * 1.05),
     xlab = "", ylab = "Units shipped (millions)",
     main = "Total units vs. total without download singles")
grid(col = "grey90", lty = 1); lines(years, tot, lwd = 3, col = "#444444")
lines(years, tot - ds, lwd = 3, col = "#D64550")
legend("topleft", c("All 17 formats", "Without download singles"), col = c("#444444", "#D64550"), lwd = 3, bty = "n")
text(1999, (tot - ds)[years == 1999], sprintf("%.0fM (1999)", (tot - ds)[years == 1999]), pos = 3, col = "#D64550", cex = .8)
text(2008, tot[years == 2008], sprintf("%.0fM (2008)", tot[years == 2008]), pos = 3, col = "#444444", cex = .8)
dev.off()

# 3. Small multiples: one panel per group, one colour per format within the panel
png("figures/small_multiples_by_group.png", 2000, 1400, res = 170)
par(mfrow = c(4, 2), mar = c(3, 5, 2.5, 1), oma = c(0, 0, 2, 0))
for (g in names(groups)) {
  M <- wide_na[, groups[[g]], drop = FALSE]
  pal <- colorRampPalette(c(cols[g], "grey20"))(max(2, ncol(M)))[seq_len(ncol(M))]
  matplot(years, M, type = "l", lty = 1, lwd = 2.2, col = pal, las = 1, bty = "n",
          xlab = "", ylab = "Units shipped (millions)", main = g, ylim = c(0, max(M, na.rm = TRUE) * 1.05))
  grid(col = "grey90", lty = 1); matlines(years, M, lty = 1, lwd = 2.2, col = pal)
  legend(if (g %in% c("8-Track", "Vinyl")) "topright" else "topleft", legend = colnames(M), col = pal, lwd = 2.2, bty = "n", cex = 0.8)
}
plot.new(); mtext("Units shipped by format group (note: panel scales differ)", outer = TRUE, font = 2)
dev.off()
cat("figures written\n")
