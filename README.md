# AFL_Grand-Final_just_another_game
We pulled the official statistics for every AFL match played since 2000 to see whether the last Saturday in September really is football's hardest day.

## What's here

- **`data/afl_grand_final_data.xlsx`** - every AFL match, 2000 to 2025 (5,111
  games), plus quarter-by-quarter scores. Sourced from afl.com.au (2012 onward)
  and afltables.com (2000-2011, and all quarter scores). The first sheet
  explains the columns.
- **`code/basic_stats.R`** - reproduces every number quoted in the article,
  in the order they appear. Run `install.packages(c("readxl", "dplyr"))`,
  then `Rscript code/basic_stats.R` from the repo root.
- **`code/verify_sources.R`** - independently scrapes afltables.com for
  every final 2012-2025 and checks it against the AFL feed data in
  `data/afl_grand_final_data.xlsx`. This is the source cross-check the
  article's methods note describes. Run
  `install.packages(c("rvest", "xml2", "stringr", "dplyr", "readxl"))`,
  then `Rscript code/verify_sources.R` from the repo root. First run takes
  a few minutes (126 pages, politely rate-limited); it caches its
  scrape, so every run after that is instant.
- **`code/make_charts.R`** - regenerates the three published charts as PNGs
  in `figures/`, computed directly from `data/afl_grand_final_data.xlsx`
  (not from hardcoded numbers). Run
  `install.packages(c("readxl", "dplyr", "tidyr", "ggplot2"))`, then
  `Rscript code/make_charts.R` from the repo root.

This is a Conversation piece, not a study - a handful of counting stats used to
spot a trend, not a controlled experiment. Treat it that way.
