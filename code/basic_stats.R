#!/usr/bin/env Rscript
# Reproduces every figure quoted in "Are AFL Grand Finals just another game, or
# a different kettle of fish?" (The Conversation, 2026), in the order they
# appear in the piece. Finds data/afl_grand_final_data.xlsx automatically
# whether you run this via Rscript, source() it, or paste it into a console.
#
# Needs: readxl, dplyr
#   install.packages(c("readxl", "dplyr"))
# Run (from anywhere):
#   Rscript code/basic_stats.R
#
# NOTE ON METHOD: every percentage below is the average of each match's own
# rate (mean of ratios), not one ratio computed from pooled season totals.
# The two methods give very slightly different answers (contested possession
# rate this way: 38.6% -> 40.9%; pooled, it is 38.4% -> 40.5%). Averaging each
# match's own rate is used throughout so every percentage in the piece is
# computed the same way.

suppressPackageStartupMessages({
  library(readxl)
  library(dplyr)
})

# Find the data file wherever you're running this from - the repo root
# (Rscript code/basic_stats.R), from inside code/ itself, or pasted straight
# into an R console or RStudio's Source button. No reliance on figuring out
# where this script lives, which behaves differently across all of those.
candidates <- c(
  file.path("data", "afl_grand_final_data.xlsx"),          # working dir = repo root
  file.path("..", "data", "afl_grand_final_data.xlsx"),    # working dir = code/
  "afl_grand_final_data.xlsx"                               # data copied next to script
)
found <- candidates[file.exists(candidates)]
if (length(found) == 0) {
  stop('Could not find afl_grand_final_data.xlsx. Set your working directory ',
       'to the repo root first, e.g. setwd("~/path/to/AFL_Grand-Final_just_another_game"), ',
       'or in RStudio: Session > Set Working Directory > To Source File Location, ',
       'then setwd("..").')
}
DATA <- found[1]

ORDER <- c("Home & Away", "Elimination Final", "Qualifying Final",
           "Semi Final", "Preliminary Final", "Grand Final")
FINALS <- ORDER[-1]

modern <- read_excel(DATA, sheet = "2012-2025 matches") %>%
  mutate(round_type = factor(round_type, levels = ORDER),
         # Round 14, 2015, Adelaide v Geelong shows zero for every stat. That
         # match was never played - cancelled after the death of Adelaide
         # coach Phil Walsh, with both sides awarded competition points
         # instead. 0/0 here is undefined, not zero, so it becomes NA and
         # every mean() below needs na.rm = TRUE to skip it (R does not do
         # this by default the way pandas' mean() does)
         cp_rate = if_else(totalPossessions > 0,
                            100 * contestedPossessions / totalPossessions, NA_real_),
         uncontestedMarks = marks - contestedMarks)

old <- read_excel(DATA, sheet = "2000-2011 matches") %>%
  mutate(is_final = as.integer(round_type != "Home & Away"))

quarters <- read_excel(DATA, sheet = "Quarter scores 2000-2025") %>%
  mutate(round_type = factor(round_type, levels = ORDER))

section <- function(title) {
  cat("\n", strrep("=", 78), "\n", title, "\n", strrep("=", 78), "\n", sep = "")
}

# --------------------------------------------------------------------------
section("1. What actually changes in a final (2012-2025, all matches)")
# "Teams average 347 disposals in a Grand Final against 361 in a home-and-away game"
t1 <- modern %>%
  group_by(round_type) %>%
  summarise(disposals = mean(disposals), kicks = mean(kicks),
            handballs = mean(handballs),
            # contestedPossessions and uncontestedPossessions are shown
            # alongside cp_rate so the rate can be checked by hand:
            # cp_rate = 100 * contestedPossessions / (contestedPossessions
            # + uncontestedPossessions), give or take the blank 2015 match
            contestedPossessions = mean(contestedPossessions),
            uncontestedPossessions = mean(uncontestedPossessions),
            cp_rate = mean(cp_rate, na.rm = TRUE), marks = mean(marks),
            uncontestedMarks = mean(uncontestedMarks),
            contestedMarks = mean(contestedMarks),
            disposalEfficiency = mean(disposalEfficiency, na.rm = TRUE),
            .groups = "drop")
print(as.data.frame(lapply(t1, function(x) if (is.numeric(x)) round(x, 1) else x)))
cat("\nArticle quotes Grand Final vs Home & Away specifically:\n")
print(as.data.frame(t1 %>% filter(round_type %in% c("Home & Away", "Grand Final")) %>%
        mutate(across(where(is.numeric), ~round(., 1)))))

# --------------------------------------------------------------------------
section("2. Which final is the hardest (2012-2025)")
# Both tables below are averaged the same way - across every individual
# match (56 team-rows for elimination finals: 2 games x 2 teams x 14
# seasons; 28 for the Grand Final's 1 game x 2 teams x 14 seasons), so the
# different number of games each round type plays per season is already
# accounted for correctly either way. The only difference is the unit:
# one team's average, or both teams added together for the whole game.
cat("per team, per match:\n")
t2 <- modern %>%
  group_by(round_type) %>%
  summarise(tackles = mean(tackles), contestedPossessions = mean(contestedPossessions),
            cp_rate = mean(cp_rate, na.rm = TRUE),
            score = mean(score), .groups = "drop") %>%
  mutate(across(where(is.numeric), ~round(., 1)))
print(as.data.frame(t2))

# the article quotes tackles and contested possessions as both teams
# combined for the whole game - "139 tackles and 291 contested
# possessions... against 136 and 279 in Grand Finals" is this table.
# cp_rate is NOT doubled here - it's already a percentage (contested
# possessions as a share of all possessions), so it's the same figure
# whether you're describing one team or the match as a whole; doubling a
# percentage would just be wrong (40.9% x 2 = 81.8%, which means nothing).
cat("\nboth teams combined, per match - this is what the article quotes:\n")
t2b <- modern %>%
  group_by(round_type) %>%
  summarise(tackles = 2 * mean(tackles),
            contestedPossessions = 2 * mean(contestedPossessions),
            cp_rate = mean(cp_rate, na.rm = TRUE),
            # score doubled too - both teams' points added together, i.e.
            # the total score of the match
            score = 2 * mean(score),
            .groups = "drop") %>%
  mutate(across(where(is.numeric), ~round(., 1)))
print(as.data.frame(t2b))

cat("\n'Rank each September's nine finals': Grand Final's rank on contested",
    "possessions, one season at a time (1 = most contested final that year)\n")
g <- modern %>%
  filter(is_final == 1) %>%
  group_by(season, match_id, round_type) %>%
  summarise(contestedPossessions = sum(contestedPossessions), .groups = "drop")

ranks <- g %>%
  group_by(season) %>%
  arrange(desc(contestedPossessions), .by_group = TRUE) %>%
  mutate(rank_ = row_number(), n_finals = n()) %>%  # n_finals before filtering
  filter(round_type == "Grand Final") %>%
  ungroup() %>%
  transmute(season, gf_rank = rank_, n_finals)
print(as.data.frame(ranks))

top_since_2011 <- ranks %>% filter(season > 2011, gf_rank == 1) %>% pull(season)
cat("\nSeasons where GF ranked 1st on contested possessions since 2011: ",
    if (length(top_since_2011) == 0) "none" else paste(top_since_2011, collapse = ", "),
    "\n", sep = "")

# same again on tackles - this is the one the article's "average rank of 5.1
# of nine" and "most-tackled final of its season four years running, from
# 2009 to 2012" claims actually refer to (those four years are outside this
# 2012-2025 sheet; see the "both" data below for the full 2000-2025 version)
gt <- modern %>%
  filter(is_final == 1) %>%
  group_by(season, match_id, round_type) %>%
  summarise(tackles = sum(tackles), .groups = "drop")
ranks_tackles <- gt %>%
  group_by(season) %>%
  arrange(desc(tackles), .by_group = TRUE) %>%
  mutate(rank_ = row_number()) %>%
  filter(round_type == "Grand Final") %>%
  ungroup() %>%
  transmute(season, gf_rank = rank_)
cat("\nGrand Final's rank on tackles, one season at a time (2012-2025):\n")
print(as.data.frame(ranks_tackles))
cat("\naverage rank on tackles, 2012-2025:",
    round(mean(ranks_tackles$gf_rank), 1), "of 9\n")

# --------------------------------------------------------------------------
section("3. Grand Finals did not always look like this")
# The article's method: rank the 9 (or 10, in 2010) finals of a season on a
# combined-team total, and compare the Grand Final with the MEDIAN of the rest.
# The 2010 Grand Final was drawn and replayed - both games count as finals that
# season, and the drawn game (not an average of the two) is "the Grand Final".
both <- bind_rows(
  old %>% transmute(season, round_type, is_final, team, opponent,
                     tackles, contestedPossessions, gid = match_url),
  modern %>% transmute(season, round_type, is_final, team, opponent,
                        tackles, contestedPossessions, gid = match_id)
)

combined <- both %>%
  filter(is_final == 1) %>%
  group_by(season, gid, round_type) %>%
  summarise(tackles = sum(tackles), contestedPossessions = sum(contestedPossessions),
            .groups = "drop")

for (metric in c("tackles", "contestedPossessions")) {
  rows <- list()
  for (s in sort(unique(combined$season))) {
    sub <- combined %>% filter(season == s)
    if (nrow(sub) < 9) next
    gfs <- sub %>% filter(round_type == "Grand Final") %>% arrange(gid)
    if (nrow(gfs) == 0) next
    gf_val <- gfs[[metric]][1]
    gf_gid <- gfs$gid[1]
    gf_rank <- sum(sub[[metric]] > gf_val) + 1  # 1 = highest that season
    rest <- sub %>% filter(!(gid == gf_gid & round_type == "Grand Final"))
    rest_median <- median(rest[[metric]])
    rows[[length(rows) + 1]] <- data.frame(
      season = s, grand_final = gf_val, gf_rank = gf_rank,
      other_finals_median = rest_median,
      gap_pct = round(100 * (gf_val - rest_median) / rest_median, 1))
  }
  d <- bind_rows(rows)
  # this is the full 2000-2025 table behind "most-tackled final of its
  # season four years running, from 2009 to 2012" and "has not led its
  # series for contested possessions since 2011" - rank 1 = hardest final
  # of that season
  cat("\n--- ", metric, ": Grand Final vs the median of its own season's other finals ---\n", sep = "")
  print(d)
  d$block <- sapply(d$season, function(y) {
    start <- 2000 + (y - 2000) %/% 2 * 2
    paste0(start, "-", start + 1)
  })
  cat("\ntwo-year blocks:\n")
  print(as.data.frame(d %>% group_by(block) %>%
          summarise(gap_pct = round(mean(gap_pct), 1), .groups = "drop")))
}

# Has the Grand Final itself stayed about the same while the OTHER finals
# got harder? Same two sources, per-team average, split at 2012 - where the
# AFL feed begins and where every other era comparison in this script also
# splits. old (2000-2011) has no totalPossessions column, so cp_rate is
# derived the same way it's checked elsewhere in this script: contested
# possessions as a share of contested + uncontested.
both_teams <- bind_rows(
  old %>% transmute(season, round_type, is_final, tackles, contestedPossessions,
                     cp_rate = 100 * contestedPossessions /
                       (contestedPossessions + uncontestedPossessions)),
  modern %>% transmute(season, round_type, is_final, tackles, contestedPossessions,
                        cp_rate)
) %>%
  filter(is_final == 1) %>%
  mutate(era = if_else(season <= 2011, "2000-2011", "2012-2025"),
         group = if_else(round_type == "Grand Final", "Grand Final", "Other finals"))

cat("\nGrand Final vs the other finals, before and after the AFL feed begins (2012):\n")
cat("per team, per match:\n")
print(as.data.frame(both_teams %>%
  group_by(era, group) %>%
  summarise(tackles = round(mean(tackles), 1),
            contestedPossessions = round(mean(contestedPossessions), 1),
            cp_rate = round(mean(cp_rate, na.rm = TRUE), 1),
            n = n(), .groups = "drop")))

# both teams combined - cp_rate is not doubled, same reasoning as section 2
cat("\nboth teams combined, per match:\n")
print(as.data.frame(both_teams %>%
  group_by(era, group) %>%
  summarise(tackles = round(2 * mean(tackles), 1),
            contestedPossessions = round(2 * mean(contestedPossessions), 1),
            cp_rate = round(mean(cp_rate, na.rm = TRUE), 1),
            n = n(), .groups = "drop")))

# same idea, but broken out by every individual round type rather than
# collapsed to "Grand Final" vs "Other finals" - this is the section 2
# table (tackles, contestedPossessions, cp_rate, score, both teams combined)
# repeated for each era, so the two eras can be compared round type by
# round type, not just Grand Final vs everything else
by_round_era <- bind_rows(
  old %>% transmute(season, round_type, tackles, contestedPossessions, score,
                     cp_rate = 100 * contestedPossessions /
                       (contestedPossessions + uncontestedPossessions)),
  modern %>% transmute(season, round_type, tackles, contestedPossessions, score,
                        cp_rate)
) %>%
  mutate(era = if_else(season <= 2011, "2000-2011", "2012-2025"),
         round_type = factor(round_type, levels = ORDER))

for (e in c("2000-2011", "2012-2025")) {
  cat("\n", e, ", both teams combined, per match:\n", sep = "")
  print(as.data.frame(by_round_era %>% filter(era == e) %>%
    group_by(round_type) %>%
    summarise(tackles = round(2 * mean(tackles), 1),
              contestedPossessions = round(2 * mean(contestedPossessions), 1),
              cp_rate = round(mean(cp_rate, na.rm = TRUE), 1),
              score = round(2 * mean(score), 1), .groups = "drop")))
}

# Splitting into two eras hides any shape within them. Cut the same 26
# seasons into three roughly-equal thirds instead (~8-9 seasons each) to see
# whether the Grand Final built up to a peak around 2009-2013 and has since
# fallen back, rather than just stepping down once in 2012. Small samples
# here - about 9 Grand Finals per third - so treat this as a shape check,
# not a precise estimate.
thirds <- both_teams %>%
  mutate(third = case_when(season <= 2008 ~ "2000-2008",
                            season <= 2017 ~ "2009-2017",
                            TRUE ~ "2018-2025"))
cat("\nseasons per third:\n")
print(as.data.frame(thirds %>% distinct(season, third) %>% count(third)))

cat("\nGrand Final vs the other finals, in thirds, both teams combined, per match:\n")
print(as.data.frame(thirds %>%
  group_by(third, group) %>%
  summarise(tackles = round(2 * mean(tackles), 1),
            contestedPossessions = round(2 * mean(contestedPossessions), 1),
            cp_rate = round(mean(cp_rate, na.rm = TRUE), 1),
            n = n(), .groups = "drop")))

# One more cut, in five-year windows rather than thirds, to see whether the
# "peak" is really a brief spike around 2009-2013 or a longer plateau. Only
# 5-6 Grand Finals per window now, so descriptive only - too little data
# for the regression model to say anything with confidence at this
# resolution.
fifths <- both_teams %>%
  mutate(fifth = case_when(season <= 2005 ~ "2000-2005",
                            season <= 2010 ~ "2006-2010",
                            season <= 2015 ~ "2011-2015",
                            season <= 2020 ~ "2016-2020",
                            TRUE ~ "2021-2025"))
cat("\nseasons per fifth:\n")
print(as.data.frame(fifths %>% distinct(season, fifth) %>% count(fifth)))

cat("\nGrand Final vs the other finals, in five-year windows, both teams combined, per match:\n")
print(as.data.frame(fifths %>%
  group_by(fifth, group) %>%
  summarise(tackles = round(2 * mean(tackles), 1),
            contestedPossessions = round(2 * mean(contestedPossessions), 1),
            cp_rate = round(mean(cp_rate, na.rm = TRUE), 1),
            n = n(), .groups = "drop")))

# broken out by individual round type rather than pooled "other finals" -
# this is what "the qualifying final alone still the toughest final" checks
# against for the most recent window
cat("\n2021-2025, both teams combined, per match, by round type:\n")
print(as.data.frame(fifths %>% filter(fifth == "2021-2025") %>%
  group_by(round_type) %>%
  summarise(tackles = round(2 * mean(tackles), 1),
            contestedPossessions = round(2 * mean(contestedPossessions), 1),
            cp_rate = round(mean(cp_rate, na.rm = TRUE), 1),
            n = n(), .groups = "drop")))

# --------------------------------------------------------------------------
section("4. Are they close games? (all 5,111 games, 2000-2025)")
# one row per game (quarters carries a row per team; a game's two rows are
# mirror images of the same margin, so this avoids any double-counting)
q <- quarters %>%
  distinct(game_code, .keep_all = TRUE) %>%
  mutate(gap1 = abs(lead_after_q1), gap2 = abs(lead_after_q2),
         gap3 = abs(lead_after_q3), gap4 = abs(lead_after_q4))

cat("average margin at each break, EVERY round type (one row per game):\n")
by_round <- q %>%
  group_by(round_type) %>%
  summarise(`Quarter time` = round(mean(gap1), 1), `Half time` = round(mean(gap2), 1),
            `Three-qtr time` = round(mean(gap3), 1), `Full time` = round(mean(gap4), 1),
            .groups = "drop")
print(as.data.frame(by_round))

gf <- q %>% filter(round_type == "Grand Final")
cat(sprintf("\nGrand Final median full-time margin: %.1f points (mean %.1f, n=%d)\n",
            median(gf$gap4), mean(gf$gap4), nrow(gf)))

q <- q %>%
  mutate(group = case_when(round_type == "Grand Final" ~ "Grand Final",
                            round_type == "Home & Away" ~ "Home & Away",
                            TRUE ~ "Other finals"),
         growth = gap4 - gap3)

# pooled by group at every break - this is what "26.9 points, tighter even
# than an average home-and-away match (27.7) and about the same as the
# other finals rounds (27.1)" checks against; the by-round-type table above
# only breaks out the five individual finals rounds, not this pooled view
cat("\nmargin at every break, pooled by group:\n")
print(as.data.frame(q %>% group_by(group) %>%
        summarise(`Quarter time` = round(mean(gap1), 1),
                  `Half time` = round(mean(gap2), 1),
                  `Three-qtr time` = round(mean(gap3), 1),
                  `Full time` = round(mean(gap4), 1), .groups = "drop")))

cat("\naverage margin GROWTH in the final quarter, by group:\n")
print(as.data.frame(q %>% group_by(group) %>%
        summarise(growth = round(mean(growth), 1), .groups = "drop")))

# "A third of them, 33.3%, have been decided by 50 points or more, compared
# with 23.1% of other finals and 24.8% of home-and-away games"
cat("\n% of games decided by 50 points or more, by group:\n")
print(as.data.frame(q %>% group_by(group) %>%
        summarise(n = n(), pct_50_plus = round(100 * mean(gap4 >= 50), 1),
                  .groups = "drop")))

# "the team ahead at three-quarter time goes on to win 92.6% of Grand
# Finals, compared with 84.6% of other finals and 86.7% of home-and-away
# games" - uses the SIGNED lead (not the absolute gap above) so the team in
# front at three-quarter time can be compared with the team in front at full
# time; a score tied at three-quarter time (lead3 == 0, very rare) counts as
# "held", since there's no lead yet to lose
cat("\nhow often the team ahead at three-quarter time goes on to win, by group:\n")
print(as.data.frame(q %>%
        mutate(held_lead = sign(lead_after_q3) == sign(lead_after_q4) | lead_after_q3 == 0) %>%
        group_by(group) %>%
        summarise(n = n(), pct_held_lead = round(100 * mean(held_lead), 1),
                  .groups = "drop")))
