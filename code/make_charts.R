#!/usr/bin/env Rscript
# Reproduces the three charts published with the article, computed straight
# from afl_grand_final_data.xlsx (not from hardcoded numbers), and saves them
# to figures/. No chart title/heading is baked into the images - that's added
# by whoever lays the piece out; direct labels on each chart carry the values.
#
# Needs: readxl, dplyr, tidyr, ggplot2
#   install.packages(c("readxl", "dplyr", "tidyr", "ggplot2"))
# Run (from anywhere):
#   Rscript code/make_charts.R

suppressPackageStartupMessages({
  library(readxl)
  library(dplyr)
  library(tidyr)
  library(ggplot2)
})

candidates <- c(
  file.path("data", "afl_grand_final_data.xlsx"),
  file.path("..", "data", "afl_grand_final_data.xlsx"),
  "afl_grand_final_data.xlsx"
)
found <- candidates[file.exists(candidates)]
if (length(found) == 0) {
  stop('Could not find afl_grand_final_data.xlsx. Set your working directory ',
       'to the repo root first, e.g. setwd("~/path/to/AFL_Grand-Final_just_another_game").')
}
DATA <- found[1]

out_candidates <- c("figures", file.path("..", "figures"))
OUT <- if (basename(getwd()) == "code") file.path("..", "figures") else "figures"
dir.create(OUT, showWarnings = FALSE)

ORDER <- c("Home & Away", "Elimination Final", "Qualifying Final",
           "Semi Final", "Preliminary Final", "Grand Final")

modern <- read_excel(DATA, sheet = "2012-2025 matches") %>%
  mutate(round_type = factor(round_type, levels = ORDER))
old <- read_excel(DATA, sheet = "2000-2011 matches") %>%
  mutate(is_final = as.integer(round_type != "Home & Away"))
quarters <- read_excel(DATA, sheet = "Quarter scores 2000-2025") %>%
  mutate(round_type = factor(round_type, levels = ORDER))

BLUE   <- "#2a78d6"
ORANGE <- "#eb6834"
GREY   <- "#9aa3ab"
LANE   <- "#e3e7ea"
INK    <- "#11161b"
INK2   <- "#54606b"
RULE   <- "#dbe0e4"

base_theme <- theme_minimal(base_size = 15) +
  theme(
    text = element_text(colour = INK),
    axis.text = element_text(colour = INK2, size = 12),
    axis.title = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(colour = RULE, linewidth = 0.4),
    legend.position = "none",
    plot.margin = margin(12, 18, 8, 8)
  )

# --------------------------------------------------------------------------
# Figure 1: where the Grand Final ranks among that season's nine finals
# --------------------------------------------------------------------------
rank_metric <- function(metric_col) {
  g <- modern %>%
    filter(is_final == 1) %>%
    group_by(season, match_id, round_type) %>%
    summarise(value = sum(.data[[metric_col]]), .groups = "drop")
  g %>%
    group_by(season) %>%
    arrange(desc(value), .by_group = TRUE) %>%
    mutate(rank_ = row_number()) %>%
    filter(round_type == "Grand Final") %>%
    ungroup() %>%
    transmute(season, rank_)
}

d_rank <- bind_rows(
  rank_metric("tackles") %>% mutate(metric = "Tackles"),
  rank_metric("contestedPossessions") %>% mutate(metric = "Contested possessions")
) %>%
  mutate(metric = factor(metric, levels = c("Tackles", "Contested possessions")))

RANK_LABELS <- c("1st\n(hardest)", "2nd", "3rd", "4th", "5th",
                  "6th", "7th", "8th", "9th\n(easiest)")

fig1 <- ggplot(d_rank, aes(x = season, y = rank_)) +
  geom_hline(yintercept = 5, colour = RULE, linewidth = 0.6, linetype = "22") +
  geom_segment(aes(x = season, xend = season, y = 1, yend = 9),
               colour = LANE, linewidth = 3.6, lineend = "round") +
  geom_point(aes(colour = metric), size = 8.6) +
  geom_text(aes(label = rank_), colour = "white", fontface = "bold", size = 3.3) +
  facet_grid(metric ~ ., switch = "y") +
  scale_colour_manual(values = c("Tackles" = ORANGE, "Contested possessions" = BLUE)) +
  scale_x_continuous(breaks = seq(2012, 2025, 1)) +
  scale_y_reverse(breaks = 1:9, limits = c(9.7, 0.3), labels = RANK_LABELS,
                   expand = expansion(mult = 0.04)) +
  base_theme +
  theme(
    axis.text.y = element_text(size = 10.5, lineheight = 0.85),
    panel.grid = element_blank(),
    panel.spacing.y = unit(18, "pt"),
    strip.text = element_text(colour = INK, face = "bold", size = 13),
    strip.background = element_blank(),
    strip.placement = "outside"
  )
ggsave(file.path(OUT, "figure1_gf_rank.png"), fig1, width = 11, height = 7.2, dpi = 300, bg = "white")

# --------------------------------------------------------------------------
# Figure 2: Grand Final tackle counts by era, compared to other finals
# --------------------------------------------------------------------------
both_teams <- bind_rows(
  old %>% transmute(season, round_type, is_final, tackles),
  modern %>% transmute(season, round_type, is_final, tackles)
) %>%
  filter(is_final == 1) %>%
  mutate(group = if_else(round_type == "Grand Final", "Grand Final", "Other finals"),
         group = factor(group, levels = c("Grand Final", "Other finals")),
         fifth = case_when(season <= 2005 ~ "2000–05",
                            season <= 2010 ~ "2006–10",
                            season <= 2015 ~ "2011–15",
                            season <= 2020 ~ "2016–20",
                            TRUE ~ "2021–25"),
         fifth = factor(fifth, levels = c("2000–05","2006–10","2011–15",
                                           "2016–20","2021–25")))

d_era <- both_teams %>%
  group_by(fifth, group) %>%
  summarise(combined_tackles = 2 * mean(tackles), .groups = "drop")

d_era_end <- d_era %>% group_by(group) %>% filter(fifth == levels(fifth)[nlevels(fifth)]) %>% ungroup()

fig2 <- ggplot(d_era, aes(fifth, combined_tackles, colour = group, group = group)) +
  geom_line(linewidth = 1.3) +
  geom_point(size = 3.2) +
  geom_text(data = d_era_end, aes(label = group), hjust = 0, nudge_x = 0.12,
            size = 4.8, fontface = "bold", show.legend = FALSE) +
  scale_colour_manual(values = c("Grand Final" = ORANGE, "Other finals" = BLUE)) +
  scale_x_discrete(expand = expansion(add = c(0.6, 1.7))) +
  scale_y_continuous(limits = c(80, 160), breaks = seq(90, 150, 20)) +
  base_theme
ggsave(file.path(OUT, "figure2_era_windows.png"), fig2, width = 8.5, height = 5, dpi = 300, bg = "white")

# --------------------------------------------------------------------------
# Figure 3: average margin at each break, 2000-2025 (5,111 games)
# --------------------------------------------------------------------------
q <- quarters %>%
  distinct(game_code, .keep_all = TRUE) %>%
  mutate(gap1 = abs(lead_after_q1), gap2 = abs(lead_after_q2),
         gap3 = abs(lead_after_q3), gap4 = abs(lead_after_q4),
         group = case_when(round_type == "Grand Final" ~ "Grand Final",
                            round_type == "Home & Away" ~ "Home & Away",
                            TRUE ~ "Other finals"),
         group = factor(group, levels = c("Grand Final", "Other finals", "Home & Away")))

d_margin <- q %>%
  group_by(group) %>%
  summarise(`Quarter time` = mean(gap1), `Half time` = mean(gap2),
            `Three-qtr time` = mean(gap3), `Full time` = mean(gap4), .groups = "drop") %>%
  pivot_longer(-group, names_to = "game_stage", values_to = "value") %>%
  mutate(game_stage = factor(game_stage, levels = c("Quarter time", "Half time",
                                                      "Three-qtr time", "Full time")))

d_margin_end <- d_margin %>% group_by(group) %>%
  filter(game_stage == levels(game_stage)[nlevels(game_stage)]) %>% ungroup()

fig3 <- ggplot(d_margin, aes(game_stage, value, colour = group, group = group)) +
  geom_line(linewidth = 1.3) +
  geom_point(size = 3.2) +
  geom_text(data = d_margin_end, aes(label = group), hjust = 0, nudge_x = 0.12,
            size = 4.8, fontface = "bold", show.legend = FALSE) +
  scale_colour_manual(values = c("Grand Final" = ORANGE, "Other finals" = BLUE, "Home & Away" = GREY)) +
  scale_x_discrete(expand = expansion(add = c(0.6, 1.7))) +
  scale_y_continuous(limits = c(0, 40), breaks = seq(0, 40, 10)) +
  base_theme
ggsave(file.path(OUT, "figure3_quarter_margin.png"), fig3, width = 8.5, height = 5, dpi = 300, bg = "white")

cat("wrote figure1_gf_rank.png, figure2_era_windows.png, figure3_quarter_margin.png to ", OUT, "/\n", sep = "")
