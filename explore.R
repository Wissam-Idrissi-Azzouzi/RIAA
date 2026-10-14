df <- read.csv("data/Sales_Volume_Chart_data.csv", stringsAsFactors = FALSE)
names(df)[1] <- "Year"
df <- df[!is.na(df$Value..Actual.), ]
df$Value <- df$Value..Actual.  # in millions

cat("Year range per format:\n")
agg <- aggregate(Year ~ Format, df, function(x) paste(min(x), max(x), sep="-"))
print(agg)

cat("\n\nTotal industry value (this dataset only) by year:\n")
tot <- aggregate(Value ~ Year, df, sum)
tot <- tot[order(tot$Year), ]
print(head(tot, 12))
cat("...\n")
print(tail(tot, 12))
