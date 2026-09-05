# AFL_Grand-Final_just_another_game
We pulled the official statistics for every AFL match played since 2000 to see whether the last Saturday in September really is football's hardest day.

## What's here

- **`data/afl_grand_final_data.xlsx`** - every AFL match, 2000 to 2025 (5,111
  games), plus quarter-by-quarter scores. Sourced from afl.com.au (2012 onward)
  and afltables.com (2000-2011, and all quarter scores). The first sheet
  explains the columns.
- **`code/basic_stats.py`** - reproduces every number quoted in the article, in
  the order they appear. Run `pip install pandas openpyxl statsmodels`, then
  `python code/basic_stats.py` from the repo root.

This is a Conversation piece, not a study - a handful of counting stats used to
spot a trend, not a controlled experiment. Treat it that way.
