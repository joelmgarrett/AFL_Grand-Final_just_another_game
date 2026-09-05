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
# computed the same way. This is the same convention basic_stats.py uses; the
# two scripts should print identical numbers.

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
t2 <- modern %>%
  group_by(round_type) %>%
  summarise(tackles = mean(tackles), contestedPossessions = mean(contestedPossessions),
            score = mean(score), .groups = "drop") %>%
  mutate(across(where(is.numeric), ~round(., 1)))
print(as.data.frame(t2))

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
    rest <- sub %>% filter(!(gid == gf_gid & round_type == "Grand Final"))
    rest_median <- median(rest[[metric]])
    rows[[length(rows) + 1]] <- data.frame(
      season = s, grand_final = gf_val, other_finals_median = rest_median,
      gap_pct = round(100 * (gf_val - rest_median) / rest_median, 1))
  }
  d <- bind_rows(rows)
  cat("\n--- ", metric, ": Grand Final vs the median of its own season's other finals ---\n", sep = "")
  print(d %>% filter(season %in% c(2011, 2015)))
  d$block <- sapply(d$season, function(y) {
    start <- 2000 + (y - 2000) %/% 2 * 2
    paste0(start, "-", start + 1)
  })
  cat("\ntwo-year blocks:\n")
  print(as.data.frame(d %>% group_by(block) %>%
          summarise(gap_pct = round(mean(gap_pct), 1), .groups = "drop")))
}

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
            pct_within_2_goals = round(100 * mean(gap4 <= 12), 1), .groups = "drop")
print(as.data.frame(by_round))

gf <- q %>% filter(round_type == "Grand Final")
cat(sprintf("\nGrand Final median full-time margin: %.1f points (mean %.1f, n=%d)\n",
            median(gf$gap4), mean(gf$gap4), nrow(gf)))

q <- q %>%
  mutate(group = case_when(round_type == "Grand Final" ~ "Grand Final",
                            round_type == "Home & Away" ~ "Home & Away",
                            TRUE ~ "Other finals"),
         growth = gap4 - gap3)
cat("\naverage margin GROWTH in the final quarter, by group:\n")
print(as.data.frame(q %>% group_by(group) %>%
        summarise(growth = round(mean(growth), 1), .groups = "drop")))

# --------------------------------------------------------------------------
section("5. Even the umpiring looks different")
old <- old %>% mutate(frees_per_100 = 100 * freesFor / disposals)
modern <- modern %>% mutate(frees_per_100 = 100 * freesFor / disposals)

o_gf <- old %>% filter(round_type == "Grand Final") %>% pull(frees_per_100)
o_ha <- old %>% filter(round_type == "Home & Away") %>% pull(frees_per_100)
cat(sprintf("2000-2011: Grand Final %.2f vs Home & Away %.2f frees per 100 disposals\n",
            mean(o_gf), mean(o_ha)))

m_gf <- modern %>% filter(round_type == "Grand Final") %>% pull(frees_per_100)
m_of <- modern %>% filter(is_final == 1, round_type != "Grand Final") %>% pull(frees_per_100)
cat(sprintf("2012-2025: Grand Final %.2f vs other finals %.2f frees per 100 disposals\n",
            mean(m_gf), mean(m_of)))

# --------------------------------------------------------------------------
section("6. Methods note: season fixed-effects model behind the round-type claims")
dd <- modern %>%
  filter(!is.na(tackles)) %>%
  mutate(round_type = relevel(factor(round_type, levels = ORDER), ref = "Home & Away"),
         season = factor(season))

fit <- lm(tackles ~ round_type + season, data = dd)

# cluster-robust (by match_id) standard errors, matching the Python script's
# statsmodels cov_type="cluster" - implemented directly, no extra package
cluster_se <- function(fit, cluster) {
  X <- model.matrix(fit)
  u <- residuals(fit)
  clusters <- unique(cluster)
  meat <- matrix(0, ncol(X), ncol(X))
  for (cl in clusters) {
    idx <- which(cluster == cl)
    Xg <- X[idx, , drop = FALSE]
    ug <- u[idx]
    score <- t(Xg) %*% ug
    meat <- meat + score %*% t(score)
  }
  bread <- solve(t(X) %*% X)
  n <- nrow(X); k <- ncol(X); g <- length(clusters)
  adj <- (g / (g - 1)) * ((n - 1) / (n - k))
  vcov <- adj * bread %*% meat %*% bread
  sqrt(diag(vcov))
}

se <- cluster_se(fit, dd$match_id)
b <- coef(fit)
cat("Tackles by round type vs Home & Away, adjusted for season,",
    "SEs clustered by match:\n\n")
for (rt in FINALS) {
  k <- paste0("round_type", rt)
  if (k %in% names(b)) {
    est <- b[[k]]; s <- se[[k]]
    lo <- est - 1.96 * s; hi <- est + 1.96 * s
    p <- 2 * pnorm(-abs(est / s))
    cat(sprintf("  %-20s %+.2f tackles  (95%% CI %+.2f to %+.2f, p=%.4f)\n",
                rt, est, lo, hi, p))
  }
}
