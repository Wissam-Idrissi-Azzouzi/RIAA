# Format lifecycles in U.S. recorded music (RIAA, 1973–2024)

An R analysis of how long recorded-music formats take to rise and fall, built on the RIAA "Sales Volume" export from Tableau Public (17 formats, 1973–2024). Repo is the cleaned-up, corrected, reproducible version of the work.

There's lot to be proud of here. Measuring how long each format took to fall to 10% of its peak gives a fair way to compare formats of very different sizes, and my results are right: 12 years for the download single, 13 for the cassette and 17 for the CD.

We filtered the correlations for small numbers that look dramatic and mean little, checking whether 2020 alone explains the CD’s uneven decline, and replaced a straight-line vinyl forecast with one that levels off, citing the limited capacity of pressing plants. The limitations section is more candid than what you'd typically see. And I pushed the limits of the tools I could use without overengineering.

The export labels its `Value (Actual)` column with dollar signs ("$942.5M"). **The numbers are unit shipments in millions, not dollars.** They reconcile exactly with RIAA's published unit counts (e.g. CD 2000 = 942.5M units; RIAA's CD *dollar* value that year was about $13.2B), and the yearly total never exceeds ~1.9B, far below the industry's dollar revenue. So everything here is about **units**, not revenue, and an inflation adjustment does not apply. I mention this because I took the dollar labels at face value and wrote a CPI inflation section on top of unit counts. Multiplying counts by a price index is meaningless, so its headline claims ("real peak in 1973", "95% real decline") are invalid. The script is kept in `R/archive_invalid_inflation/` but with a warning!

I also didn't sanity-checked totals against the size of the industry. A 2024 total of "$220M" is about 1% of reported U.S. recorded-music revenue, which should have prompted a data check before any modelling. The export contains **no streaming**, so conclusions are limited to physical and download *units*, not the business.

## Questions

1. How fast does each format fall after its peak (years to reach 10% of peak)?
2. Do formats share a lifecycle shape (bell, spike-and-fade, U-shaped revival)?
3. Do formats substitute for or complement each other (year-over-year correlations)?
4. Which formats are still declining, and which have levelled off or are growing?

## Selected results (units)

| Format | Peak | Year at −90% | Years |
|---|---|---|---|
| Vinyl Single | 1973 | 1991 | 18 |
| LP/EP | 1977 | 1990 | 13 |
| Cassette | 1988 | 2001 | 13 |
| Cassette Single | 1990 | 2000 | 10 |
| CD Single | 1997 | 2002 | 5 |
| CD | 2000 | 2017 | 17 |
| SACD | 2003 | 2008 | 5 |
| Music Video (Physical) | 2005 | 2015 | 10 |
| DVD Audio | 2005 | 2008 | 3 |
| Ringtones & Ringbacks | 2007 | 2013 | 6 |
| Download Music Video | 2008 | 2017 | 9 |
| Download Single | 2012 | 2024 | 12 |
| Kiosk | 2013 | 2022 | 9 |

Download Album (−85.7%), 8-Track, Other Tapes and Other Physical never reached −90% (censored, or too short a record).
Physical formats averaged 10.3 years (n=10), digital 9.0 (n=3), but the samples are tiny and the ranges overlap heavily, so this is "digital bought no extra durability", not "digital collapses faster".

* **Trend estimates** use log-linear fits over each format's own regime (peak→present, or trough→present for revivals), minimum 8 points. Intervals were checked three ways: OLS, Newey-West, and a circular block bootstrap, because residuals are strongly autocorrelated (Durbin-Watson). 12 of 14 testable formats keep their direction under the block bootstrap; DVD Audio is borderline and Vinyl Single is statistically indistinguishable from flat. LP/EP grows about +22%/yr (block CI ≈ +18% to +26%). See `results/block_boot_results.csv`.
* **Clustering** (all 17 formats; Ward hierarchical, k-means, with PAM and mixture-model checks in earlier runs) most consistently separates **Vinyl Single + LP/EP** from everything else.
* **Interactions**: of 136 pairs, 82 overlap in time; only a few survive a "not just small numbers" filter. Download Single and Album move opposite to CD (r ≈ −0.69 and −0.65) and together with each other (r ≈ +0.91). Correlation is not causation, and this analysis does not establish why.
* **Total units** peaked in 2008 (1.92B), but download singles were 54% of that year's units. Excluding them the peak is 1999 (1.16B). See `figures/units_total_vs_ex_download_singles.png`.
* **Lifecycle models** (Bass, Gaussian, exponential-to-floor, logistic/Gompertz for LP/EP) were backtested rolling-origin; they agree on direction but not on the level the survivors settle at.

## Reproduce

Base R 4.3+ (uses the `survival` package that ships with R). From the repo root:

```
Rscript run_all.R
```

## Limitations

* Units, not revenue: a 99¢ download counts the same as a full album, so totals mix very different purchases.
* No streaming, again, so this is not the whole market and says nothing about why formats declined.
* Small samples: 3 digital formats, 10 physical, and several formats with fewer than 8 points.
* Descriptive: the analysis shows *when* and *how fast* formats fell, not *why*, who decided, or what it did to revenue.
chart scripts) were lost and are not included; `R/03_regression_diagnostics/full_diagnostics.R` and
`all17_fulldata.R` cover the influence checks, and `R/04_clustering/cluster_redo.R` the clustering.
