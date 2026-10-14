# ======================================================================================
# ARCHIVED - DO NOT USE FOR CONCLUSIONS.
# This script CPI-adjusts the "Value (Actual)" column as if it were dollars. That column is
# actually UNITS SHIPPED (millions): it matches RIAA's published unit counts (CD 2000 = 942.5M
# units; RIAA's CD *dollar* value that year was ~$13.2B). Multiplying unit counts by a price
# index is meaningless, so the "real peak in 1973 / 95% real decline" results are invalid.
# Kept only as a record of the mistake. See README.md, "What went wrong".
# ======================================================================================
df <- read.csv("data/Sales_Volume_Chart_data.csv", stringsAsFactors = FALSE)
names(df)[1] <- "Year"
df <- df[!is.na(df$Value..Actual.) & df$Value..Actual. > 0, ]
df$Value <- df$Value..Actual.

# CPI-anchored purchasing-power index (BLS CPI-U via officialdata.org), value of $200-1973 in each year's dollars
cpi_table <- data.frame(
  Year=1973:2024,
  DollarVal=c(200.00,222.07,242.34,256.31,272.97,293.69,327.03,371.17,409.46,434.68,
              448.65,468.02,484.68,493.69,511.71,532.88,558.56,588.74,613.51,631.98,
              650.90,667.57,686.49,706.76,722.97,734.23,750.45,775.68,797.75,810.36,
              828.83,850.90,879.73,908.11,933.97,969.83,966.38,982.23,1013.24,1034.21,
              1049.36,1066.38,1067.64,1081.11,1104.14,1131.67,1151.61,1165.82,1220.59,1318.27,
              1372.53,1412.23)
)
val2024 <- cpi_table$DollarVal[cpi_table$Year==2024]
cpi_table$deflator_to_2024 <- val2024 / cpi_table$DollarVal

df <- merge(df, cpi_table[,c("Year","deflator_to_2024")], by="Year")
df$RealValue2024 <- df$Value * df$deflator_to_2024

## 1. Total market: nominal vs real, key years
tot_nom <- aggregate(Value ~ Year, df, sum)
tot_real <- aggregate(RealValue2024 ~ Year, df, sum)
tot <- merge(tot_nom, tot_real, by="Year")
tot <- tot[order(tot$Year),]
cat("=== TOTAL MARKET: NOMINAL vs REAL (2024$), SELECTED YEARS ===\n")
print(tot[tot$Year %in% c(1973,1978,1988,2000,2006,2008,2012,2016,2020,2024),], row.names=FALSE)

cat(sprintf("\nNominal peak year: %d ($%.1fM)\n", tot$Year[which.max(tot$Value)], max(tot$Value)))
cat(sprintf("Real (2024$) peak year: %d ($%.1fM)\n", tot$Year[which.max(tot$RealValue2024)], max(tot$RealValue2024)))

## 2. Format-level peak comparison: nominal vs real
formats <- c("CD","Cassette","Download Single","LP/EP","Vinyl Single","Ringtones & Ringbacks")
cat("\n=== FORMAT PEAKS: NOMINAL vs REAL (2024$) ===\n")
for (f in formats) {
  sub <- df[df$Format==f,]
  nom_peak_row <- sub[which.max(sub$Value),]
  real_peak_row <- sub[which.max(sub$RealValue2024),]
  cat(sprintf("%-25s Nominal peak: %d ($%.1fM)  |  Real-2024$ peak: %d ($%.1fM)\n",
              f, nom_peak_row$Year, nom_peak_row$Value, real_peak_row$Year, real_peak_row$RealValue2024))
}

## 3. Real-dollar version of "years to -90% decline" comparison (CD vs Cassette vs Download Single)
cat("\n=== REAL-DOLLAR PEAK-TO-COLLAPSE COMPARISON ===\n")
for (f in c("Cassette","CD","Download Single")) {
  sub <- df[df$Format==f,]; sub <- sub[order(sub$Year),]
  peak_idx <- which.max(sub$RealValue2024)
  peak_year <- sub$Year[peak_idx]; peak_val <- sub$RealValue2024[peak_idx]
  post <- sub[sub$Year >= peak_year,]
  below <- post[post$RealValue2024 <= 0.10*peak_val,]
  yrs <- if(nrow(below)>0) min(below$Year)-peak_year else NA
  cat(sprintf("%-20s real peak: %d ($%.1fM in 2024$) -> years to -90%%: %s\n", f, peak_year, peak_val, yrs))
}

## 4. CD peak (2000) in real terms - what people usually don't realize
cd_2000 <- df[df$Format=="CD" & df$Year==2000,]
cat(sprintf("\nCD's 2000 peak ($942.5M nominal) = $%.0fM in 2024 dollars\n", cd_2000$RealValue2024))
total_1973_real <- tot$RealValue2024[tot$Year==1973]
total_2008_real <- tot$RealValue2024[tot$Year==2008]
cat(sprintf("Total market 1973 ($616.2M nominal) = $%.0fM in 2024 dollars\n", total_1973_real))
cat(sprintf("Total market's real (2024$) peak year value = $%.0fM, vs 2024 actual = $%.1fM -> real decline = %.1f%%\n",
            max(tot$RealValue2024), tot$RealValue2024[tot$Year==2024],
            100*(1-tot$RealValue2024[tot$Year==2024]/max(tot$RealValue2024))))
