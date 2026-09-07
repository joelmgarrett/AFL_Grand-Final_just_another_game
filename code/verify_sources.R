#!/usr/bin/env Rscript
# Independently checks the AFL feed data in data/afl_grand_final_data.xlsx
# against afltables.com, for every final played 2012-2025. This is the
# concordance check the article's methods note describes: "For every
# final played between 2012 and 2025 we compared AFL Tables with the AFL
# feed, game by game: 240 team-observations across 11 statistics."
#
# This script scrapes afltables.com itself (126 finals, cached after the
# first run so a re-run costs nothing) - it does not reuse any number from
# the data file's own "2012-2025 matches" sheet except as the thing being
# checked against.
#
# Needs: rvest, xml2, stringr, dplyr, readxl
#   install.packages(c("rvest", "xml2", "stringr", "dplyr", "readxl"))
# Run (from anywhere):
#   Rscript code/verify_sources.R
# First run takes a few minutes (126 pages, politely rate-limited); every
# run after that is instant, reading from ./cache_sources instead.

suppressPackageStartupMessages({
  library(rvest); library(xml2); library(stringr); library(dplyr); library(readxl)
})

candidates <- c(
  file.path("data", "afl_grand_final_data.xlsx"),
  file.path("..", "data", "afl_grand_final_data.xlsx"),
  "afl_grand_final_data.xlsx"
)
found <- candidates[file.exists(candidates)]
if (length(found) == 0) {
  stop('Could not find afl_grand_final_data.xlsx. Set your working directory ',
       'to the repo root first, e.g. setwd("~/path/to/AFL_Grand-Final_just_another_game"), ',
       'or in RStudio: Session > Set Working Directory > To Source File Location, ',
       'then setwd("..").')
}
DATA <- found[1]

# --------------------------------------------------------------------------
# 1. Scrape afltables.com independently for every final, 2012-2025
# --------------------------------------------------------------------------
FINALS <- c("qualifying final" = "Qualifying Final",
            "elimination final" = "Elimination Final",
            "semi final" = "Semi Final",
            "preliminary final" = "Preliminary Final",
            "grand final" = "Grand Final")
HDR_PAT <- '<td[^>]*align="center"[^>]*><b>\\s*([^<]*?)\\s*</b></td>'
LINK_PAT <- 'href="\\.\\./stats/games/(\\d{4})/([^".]+)\\.html"'
CACHE <- file.path(dirname(DATA), "..", "cache_sources")
if (!dir.exists(dirname(DATA))) CACHE <- "cache_sources"  # data file next to script
dir.create(CACHE, showWarnings = FALSE, recursive = TRUE)

fetch <- function(url, name) {
  p <- file.path(CACHE, name)
  if (file.exists(p) && file.size(p) > 500) {
    return(paste(readLines(p, warn = FALSE, encoding = "UTF-8"), collapse = "\n"))
  }
  raw <- as.character(read_html(url))
  writeLines(raw, p, useBytes = TRUE)
  Sys.sleep(0.4)  # a free public site with no API - be a reasonable citizen
  raw
}

# rvest re-serialises the (somewhat malformed) source HTML into well-formed
# HTML - attributes get double-quoted and tags get properly closed - so the
# patterns below match rvest's normalised output, not afltables' raw markup
season_finals <- function(y) {
  raw <- fetch(sprintf("https://afltables.com/afl/seas/%d.html", y),
               sprintf("seas%d.html", y))
  locs <- str_locate_all(raw, HDR_PAT)[[1]]
  labs <- str_match_all(raw, HDR_PAT)[[1]][, 2]
  keep <- tolower(labs) %in% names(FINALS)
  locs <- locs[keep, , drop = FALSE]
  labs <- labs[keep]
  out <- list()
  for (i in seq_along(labs)) {
    chunk_start <- locs[i, "end"]
    chunk_end <- if (i < length(labs)) locs[i + 1, "start"] - 1 else nchar(raw)
    chunk <- substr(raw, chunk_start, chunk_end)
    codes <- unique(str_match_all(chunk, LINK_PAT)[[1]][, 3])
    for (code in codes) {
      out[[length(out) + 1]] <- data.frame(
        season = y, round_type = unname(FINALS[tolower(labs[i])]), code = code)
    }
  }
  if (length(out) == 0) {
    return(data.frame(season = integer(), round_type = character(), code = character()))
  }
  do.call(rbind, out)
}

STAT_COLS <- c(TK = "tackles", CP = "contestedPossessions", UP = "uncontestedPossessions",
               IF = "inside50s", CL = "totalClearances", CG = "clangers",
               FF = "freesFor", HO = "hitouts", MI = "marksInside50",
               GL = "goals", BH = "behinds")

match_stats <- function(season, code) {
  url <- sprintf("https://afltables.com/afl/stats/games/%d/%s.html", season, code)
  raw <- fetch(url, sprintf("g%d_%s.html", season, code))
  tabs <- tryCatch(html_table(read_html(raw), fill = TRUE), error = function(e) list())
  rows <- list()
  for (t in tabs) {
    if (ncol(t) < 20) next
    hdr <- as.character(t[1, ])
    if (!("TK" %in% hdr) || !("CP" %in% hdr)) next
    team <- str_split(names(t)[1], " Match Statistics")[[1]][1]
    names(t) <- hdr
    t <- t[-1, ]
    tot <- t[t$Player == "Totals", ]
    if (nrow(tot) == 0) next
    r <- list(team = team, season = season, code = code)
    for (col in names(STAT_COLS)) {
      r[[STAT_COLS[[col]]]] <- suppressWarnings(as.numeric(tot[[col]][1]))
    }
    rows[[length(rows) + 1]] <- as.data.frame(r, stringsAsFactors = FALSE)
  }
  if (length(rows) == 0) return(NULL)
  do.call(rbind, rows)
}

cat("Fetching finals fixtures, 2012-2025 (afltables.com)...\n")
fixtures <- do.call(rbind, lapply(2012:2025, season_finals))
cat(sprintf("%d finals found (expect 126)\n", nrow(fixtures)))

cat("Fetching match stats (cached after first run)...\n")
all_rows <- list()
for (i in seq_len(nrow(fixtures))) {
  r <- match_stats(fixtures$season[i], fixtures$code[i])
  if (!is.null(r)) all_rows[[length(all_rows) + 1]] <- r
  if (i %% 20 == 0) cat(sprintf("  %d/%d\n", i, nrow(fixtures)))
}
aflt <- do.call(rbind, all_rows)
cat(sprintf("afltables team-rows scraped: %d (expect 252)\n", nrow(aflt)))

# --------------------------------------------------------------------------
# 2. Compare against the AFL feed data already in the data file
# --------------------------------------------------------------------------
NAME_MAP <- c("Sydney" = "Sydney Swans", "Adelaide" = "Adelaide Crows",
              "Geelong" = "Geelong Cats", "West Coast" = "West Coast Eagles",
              "Greater Western Sydney" = "GWS GIANTS",
              "Gold Coast" = "Gold Coast SUNS")
canon <- function(t) ifelse(t %in% names(NAME_MAP), NAME_MAP[t], t)
aflt$team_n <- canon(aflt$team)

modern <- read_excel(DATA, sheet = "2012-2025 matches") %>% filter(is_final == 1)
modern_pairs <- modern %>%
  select(season, team, opponent, tackles, contestedPossessions, uncontestedPossessions,
         inside50s, totalClearances, clangers, freesFor, hitouts, marksInside50,
         goals, behinds)

# each season+code group is exactly the two teams that played each other
aflt_pairs <- aflt %>%
  group_by(season, code) %>%
  mutate(opponent = team_n[c(2, 1)][row_number()]) %>%
  ungroup()

m <- inner_join(aflt_pairs, modern_pairs,
                by = c("season", "team_n" = "team", "opponent" = "opponent"),
                suffix = c("_aflt", "_afl"), relationship = "many-to-many")
# a team meeting the same opponent twice in one finals series is essentially
# impossible under the final-eight format, but drop any such ambiguous pairs
# rather than risk mismatching them
dupes <- m %>% count(season, team_n, opponent) %>% filter(n > 1)
if (nrow(dupes) > 0) m <- anti_join(m, dupes, by = c("season", "team_n", "opponent"))
cat(sprintf("\nmatched %d of %d finals team-rows (%d unmatched)\n",
            nrow(m), nrow(modern_pairs), nrow(modern_pairs) - nrow(m)))

STATS <- names(STAT_COLS)
LABELS <- setNames(c("Tackles", "Contested possessions", "Uncontested possessions",
                      "Inside 50s", "Clearances", "Clangers", "Frees for", "Hitouts",
                      "Marks inside 50", "Goals", "Behinds"), unname(STAT_COLS))
results <- lapply(unname(STAT_COLS), function(s) {
  a <- m[[paste0(s, "_aflt")]]
  b <- m[[paste0(s, "_afl")]]
  d <- abs(a - b)
  data.frame(stat = LABELS[[s]], n = length(d),
             identical_pct = round(100 * mean(d == 0, na.rm = TRUE), 1),
             within_1_pct = round(100 * mean(d <= 1, na.rm = TRUE), 1),
             max_diff = max(d, na.rm = TRUE),
             mean_abs_diff = round(mean(d, na.rm = TRUE), 3))
})
results <- do.call(rbind, results)
cat("\n=== source concordance: afltables vs the AFL feed, every final 2012-2025 ===\n")
print(results, row.names = FALSE)

n_values <- nrow(m) * length(STATS)
n_diff <- sum(sapply(unname(STAT_COLS), function(s) {
  sum(abs(m[[paste0(s, "_aflt")]] - m[[paste0(s, "_afl")]]) > 0, na.rm = TRUE)
}))
max_diff_overall <- max(sapply(unname(STAT_COLS), function(s) {
  max(abs(m[[paste0(s, "_aflt")]] - m[[paste0(s, "_afl")]]), na.rm = TRUE)
}))
cat(sprintf("\nTotal: %d values checked across %d team-observations and %d statistics\n",
            n_values, nrow(m), length(STATS)))
cat(sprintf("%d values differed, largest difference %d\n", n_diff, max_diff_overall))
cat("\nThis is what the article's methods note describes: '240 team-observations\n")
cat("across 11 statistics... 2,640 values checked, 48 differed, none by more than 2'.\n")
