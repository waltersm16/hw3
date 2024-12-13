# Plot mCPR data and estimates

## Description

Plot mCPR data and estimates

## Usage

```r
plot_cp(dat, est, iso_code, CI = 95)
```

## Arguments

* `dat`: tibble which contains mCPR observations. Columns: iso, year, cp
* `est`: tibble which contains mCPR estimates. Columns: Country or area, iso, Year, Median, U95, L95, U80, L80
* `iso_code`: country iso code
* `CI`: confidence intervals to be plotted. Options can be: 80, 95, or NA (no CI plotted)

## Value

ggplot object with mCPR observations and estimates over time

