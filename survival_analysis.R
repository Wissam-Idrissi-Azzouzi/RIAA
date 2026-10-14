library(survival)
df <- read.csv("data/Sales_Volume_Chart_data.csv", stringsAsFactors = FALSE)
names(df)[1] <- "Year"
df <- df[!is.na(df$Value..Actual.) & df$Value..Actual. > 0, ]
df$Value <- df$Value..Actual.

formats <- unique(df$Format)
surv_data <- data.frame()
for (f in formats) {
  sub <- df[df$Format==f, ]
  first_year <- min(sub$Year); last_year <- max(sub$Year)
  time <- last_year - first_year
  event <- ifelse(last_year < 2024, 1, 0)   # 1=died before end of study, 0=still alive in 2024 (censored)
  # classify physical vs digital for group comparison
  digital <- f %in% c("Download Single","Download Album","Download Music Video","Ringtones & Ringbacks")
  surv_data <- rbind(surv_data, data.frame(Format=f, time=time, event=event,
                                            group=ifelse(digital,"Digital","Physical/Other")))
}
cat("=== SURVIVAL DATASET ===\n")
print(surv_data[order(surv_data$time),], row.names=FALSE)
cat("\nEvents (died before 2024):", sum(surv_data$event), " | Censored (still alive in 2024):", sum(surv_data$event==0), "\n")

## Overall Kaplan-Meier
fit <- survfit(Surv(time, event) ~ 1, data=surv_data)
cat("\n=== OVERALL KAPLAN-MEIER SURVIVAL CURVE ===\n")
print(summary(fit))

cat("\nMedian survival time (years):", summary(fit)$table["median"], "\n")

## By group: Digital vs Physical/Other
fit_grp <- survfit(Surv(time, event) ~ group, data=surv_data)
cat("\n=== KAPLAN-MEIER BY GROUP (Digital vs Physical/Other) ===\n")
print(fit_grp)

## Log-rank test comparing groups
lr <- survdiff(Surv(time, event) ~ group, data=surv_data)
cat("\n=== LOG-RANK TEST (Digital vs Physical/Other) ===\n")
print(lr)
