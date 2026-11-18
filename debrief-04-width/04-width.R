# =============================================================================
# Debrief 04 task: "Which side do they attack down?"
# Kestrel Bay FC 2-0 Redfern Athletic (practice match, The Portal)
#
# One script, all outputs. Run it top to bottom; every chart is saved to
# outputs/ as a PNG and the key tables are printed to the console.
#
# Definition: every completed pass or carry by Kestrel Bay that ends in the
# final third (end_x >= 80), split into channels by end_y. Open play only:
# possessions that start from a set piece are excluded.
# Coordinates: 120 x 80 pitch, every team attacks towards x = 120,
# and y = 0 is that team's LEFT touchline (see the data dictionary).
#
# Reading the code: `x |> f()` means "take x, then apply f". So
#   events |> filter(...) |> count(...)
# reads "take events, then keep some rows, then count them". It is the same as
#   count(filter(events, ...), ...)
# Main dplyr verbs used: filter (keep rows), mutate (add/change columns),
# count (count rows by group), group_by + summarise (one row per group),
# distinct (drop duplicates), left_join (add columns from another table).
# =============================================================================

# install.packages(c("readr", "dplyr", "tidyr", "ggplot2", "scales", "ggsoccer"))
library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)
library(ggsoccer)

# ---- Settings ---------------------------------------------------------------
data_dir         <- "data/raw/portal"   # folder with the Portal CSVs (not in this repo)
out_dir          <- "outputs"
team_of_interest <- "Kestrel Bay FC"
winger_name      <- "Niall Sorrell"   # filled in from Part 2's output
feeder_name      <- "Arlo Okonedo"    # filled in from Part 2's output

highlight <- "#c0392b"
muted     <- "grey70"
dir.create(out_dir, showWarnings = FALSE)

base_theme <- theme_minimal(base_size = 13) +
  theme(plot.title = element_text(face = "bold"),
        plot.title.position = "plot")

open_play_note <- "Open play only: possessions from set pieces excluded."

save_plot <- function(file, width = 7, height = 4.5, caption = open_play_note) {
  ggsave(file.path(out_dir, file), plot = last_plot() + labs(caption = caption),
         width = width, height = height, dpi = 200, bg = "white")
}

# ---- Load -------------------------------------------------------------------
events <- read_csv(file.path(data_dir, "practice_match_events.csv"),
                   show_col_types = FALSE)

# Final-third entries: completed passes and all carries ending at x >= 80.
# Channels use half-open intervals cut exactly at 80/3 and 160/3; the task's
# bounds (26.6 / 26.7, 53.3 / 53.4) would leave gaps for continuous values.
entries <- events |>
  filter(team == team_of_interest,
         play_pattern == "Regular Play",
         (type == "Pass" & outcome %in% "Complete") | type == "Carry",
         end_x >= 80) |>
  mutate(channel = cut(end_y,
                       breaks = c(0, 80/3, 160/3, 80),
                       labels = c("Left", "Centre", "Right"),
                       include.lowest = TRUE, right = FALSE))

# =============================================================================
# PART 1: Which side do they get into the final third on?
# =============================================================================
by_channel <- entries |>
  count(channel, name = "n") |>
  mutate(share = n / sum(n),
         busiest = n == max(n))

print(by_channel)

ggplot(by_channel, aes(x = channel, y = n, fill = busiest)) +
  geom_col(width = 0.6) +
  geom_text(aes(label = sprintf("%d (%.0f%%)", n, 100 * share)), vjust = -0.4, size = 4) +
  scale_fill_manual(values = c(`TRUE` = highlight, `FALSE` = muted), guide = "none") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.12))) +
  labs(title = paste0(team_of_interest, ": where they get into the final third"),
       subtitle = "Completed passes and carries ending in the final third, by channel",
       x = NULL, y = "Entries") +
  base_theme +
  theme(panel.grid.major.x = element_blank())
save_plot("01_channels.png")

# Sensitivity: does the answer depend on what we count?
sensitivity <- bind_rows(
  entries |> count(channel, name = "n") |> mutate(count_of = "All entries"),
  entries |> filter(type == "Pass")  |> count(channel, name = "n") |> mutate(count_of = "Passes only"),
  entries |> filter(type == "Carry") |> count(channel, name = "n") |> mutate(count_of = "Carries only"),
  entries |> distinct(possession, channel) |> count(channel, name = "n") |> mutate(count_of = "One per possession")
)
print(sensitivity |> pivot_wider(names_from = channel, values_from = n))

sensitivity |>
  group_by(count_of) |>
  mutate(share = n / sum(n), top = n == max(n)) |>
  ungroup() |>
  mutate(count_of = factor(count_of, levels = c("All entries", "Passes only", "Carries only", "One per possession"))) |>
  ggplot(aes(x = channel, y = share, fill = top)) +
  geom_col(width = 0.6) +
  geom_text(aes(label = n), vjust = -0.4, size = 3.6) +
  facet_wrap(~ count_of, nrow = 1) +
  scale_fill_manual(values = c(`TRUE` = highlight, `FALSE` = muted), guide = "none") +
  scale_y_continuous(labels = percent, expand = expansion(mult = c(0, 0.15))) +
  labs(title = "The answer depends on what you count",
       subtitle = "Share of final-third entries by channel. Labels: counts. Red: top channel.",
       x = NULL, y = NULL) +
  base_theme +
  theme(panel.grid.major.x = element_blank())
save_plot("02_sensitivity.png", width = 10, height = 4.5)

# =============================================================================
# PART 2: Who gives them the width on the busiest side?
# =============================================================================
busiest_channel <- by_channel$channel[by_channel$busiest][1]

by_player <- entries |>
  filter(channel == busiest_channel) |>
  count(player, position, type, name = "n") |>
  group_by(player, position) |>
  mutate(total = sum(n)) |>
  ungroup() |>
  arrange(desc(total))

print(by_player, n = 30)

by_player |>
  mutate(label = paste0(player, " (", position, ")")) |>
  ggplot(aes(x = n, y = reorder(label, total), fill = type)) +
  geom_col(width = 0.65) +
  scale_fill_manual(values = c(Carry = highlight, Pass = "grey55")) +
  labs(title = paste0("On the ", tolower(as.character(busiest_channel)), ", the winger carries it in"),
       subtitle = paste0("Final-third entries in the ", tolower(as.character(busiest_channel)),
                         " channel, by player and action"),
       x = "Entries", y = NULL, fill = NULL) +
  base_theme +
  theme(legend.position = "top", panel.grid.major.y = element_blank())
save_plot("03_who_gives_width.png", width = 8, height = 5)

# =============================================================================
# ENRICHMENT 1: Most used vs most productive
# Share of possessions entering through each channel that reached the box or
# ended in a shot, with 95% Jeffreys intervals (Beta(k + 0.5, n - k + 0.5)).
# =============================================================================
team_events <- events |> filter(team == team_of_interest)

shot_poss <- team_events |>
  filter(type == "Shot") |>
  distinct(possession)

# Box on a 120 x 80 pitch: x >= 102, 18 <= y <= 62
box_poss <- team_events |>
  filter((type == "Pass" & outcome %in% "Complete") | type == "Carry",
         end_x >= 102, between(end_y, 18, 62)) |>
  distinct(possession)

quality <- entries |>
  distinct(possession, channel) |>
  mutate(reached_box = possession %in% box_poss$possession,
         had_shot    = possession %in% shot_poss$possession) |>
  group_by(channel) |>
  summarise(possessions = n(),
            box  = sum(reached_box),
            shot = sum(had_shot),
            .groups = "drop") |>
  pivot_longer(c(box, shot), names_to = "outcome", values_to = "k") |>
  mutate(rate    = k / possessions,
         lower   = qbeta(0.025, k + 0.5, possessions - k + 0.5),
         upper   = qbeta(0.975, k + 0.5, possessions - k + 0.5),
         outcome = recode(outcome, box = "Reached the box", shot = "Ended in a shot"))

print(quality)

ggplot(quality, aes(x = channel, y = rate, colour = outcome)) +
  geom_pointrange(aes(ymin = lower, ymax = upper),
                  position = position_dodge(width = 0.4), size = 0.5) +
  geom_text(data = distinct(quality, channel, possessions),
            aes(x = channel, y = 0, label = paste0("n = ", possessions)),
            inherit.aes = FALSE, vjust = 1.6, size = 3.5, colour = "grey40") +
  scale_y_continuous(labels = percent, limits = c(-0.05, 1)) +
  scale_colour_manual(values = c("Reached the box" = highlight, "Ended in a shot" = "grey30")) +
  labs(title = "Is the most used side the most productive?",
       subtitle = "Possessions entering the final third through each channel: share that went on to...",
       x = NULL, y = NULL, colour = NULL) +
  base_theme +
  theme(legend.position = "top", panel.grid.major.x = element_blank())
save_plot("04_quality.png",
          caption = paste("Points: observed share. Bars: 95% Jeffreys interval. One match, so intervals are wide.",
                          open_play_note, sep = "\n"))

# =============================================================================
# ENRICHMENT 2: Who feeds the winger?
# Passes by the feeder that end in the final third, by receiver:
# all channels vs the busiest channel only.
# =============================================================================
feeder_passes <- entries |>
  filter(player == feeder_name, type == "Pass")

feeder_targets <- bind_rows(
  feeder_passes |> count(recipient, name = "n") |> mutate(scope = "All channels"),
  feeder_passes |> filter(channel == busiest_channel) |> count(recipient, name = "n") |>
    mutate(scope = paste(busiest_channel, "channel only"))
)

print(feeder_targets)

ggplot(feeder_targets,
       aes(x = n, y = reorder(recipient, n), fill = recipient == winger_name)) +
  geom_col(width = 0.6) +
  geom_text(aes(label = n), hjust = -0.3, size = 3.8) +
  facet_wrap(~ scope) +
  scale_fill_manual(values = c(`TRUE` = highlight, `FALSE` = muted), guide = "none") +
  scale_x_continuous(expand = expansion(mult = c(0, 0.15))) +
  labs(title = paste0(feeder_name, " looks for ", winger_name),
       subtitle = paste0("Completed passes by ", feeder_name, " into the final third, by receiver"),
       x = "Passes", y = NULL) +
  base_theme +
  theme(panel.grid.major.y = element_blank())
save_plot("05_feeder.png", width = 8, height = 4)

# =============================================================================
# ENRICHMENT 3: Does the pattern hold across the match? (half and score)
# =============================================================================
# Score state before each event: running goal count up to the event, minus the
# event itself, so a goal is counted as happening "before" its own score change.
score_state <- events |>
  arrange(index) |>
  mutate(is_goal        = type == "Shot" & outcome %in% "Goal",
         goal_for       = as.integer(is_goal & team == team_of_interest),
         goal_against   = as.integer(is_goal & team != team_of_interest),
         for_before     = cumsum(goal_for) - goal_for,
         against_before = cumsum(goal_against) - goal_against,
         state = case_when(for_before > against_before ~ "Leading",
                           for_before < against_before ~ "Trailing",
                           TRUE                        ~ "Level")) |>
  select(id, state)

entries_ctx <- entries |>
  left_join(score_state, by = "id") |>
  mutate(half = if_else(period == 1, "1st half", "2nd half"))

goals <- events |>
  filter(type == "Shot", outcome %in% "Goal") |>
  select(team, period, minute, player)
print(goals)

plot_context <- function(df, title, file) {
  df |>
    group_by(context) |>
    mutate(share = n / sum(n), total = sum(n)) |>
    ungroup() |>
    ggplot(aes(x = channel, y = share, fill = channel == busiest_channel)) +
    geom_col(width = 0.6) +
    geom_text(aes(label = n), vjust = -0.4, size = 3.8) +
    facet_wrap(~ paste0(context, " (n = ", total, ")")) +
    scale_fill_manual(values = c(`TRUE` = highlight, `FALSE` = muted), guide = "none") +
    scale_y_continuous(labels = percent, expand = expansion(mult = c(0, 0.15))) +
    labs(title = title,
         subtitle = "Share of final-third entries by channel. Labels: counts.",
         x = NULL, y = NULL) +
    base_theme +
    theme(panel.grid.major.x = element_blank())
  save_plot(file, width = 8, height = 4.5)
}

by_half  <- entries_ctx |> count(context = half,  channel, name = "n")
by_state <- entries_ctx |> count(context = state, channel, name = "n")
print(by_half)
print(by_state)

plot_context(by_half,  "Same pattern in both halves?", "06_by_half.png")
plot_context(by_state, "Same pattern whatever the score?", "07_by_score.png")

# =============================================================================
# PITCH MAP: where the entries end, winger vs everyone else
# y = 0 is the attacking team's left touchline, which is at the top when the
# team attacks to the right, hence scale_y_reverse().
# =============================================================================
entries |>
  mutate(who = if_else(player == winger_name, winger_name, "Everyone else")) |>
  ggplot(aes(x = end_x, y = end_y, colour = who, shape = type)) +
  annotate_pitch(dimensions = pitch_statsbomb, colour = "grey60", fill = "white") +
  geom_point(size = 2.2, alpha = 0.8) +
  scale_y_reverse() +
  coord_cartesian(xlim = c(60, 121)) +
  scale_colour_manual(values = setNames(c(highlight, muted), c(winger_name, "Everyone else"))) +
  labs(title = paste0(team_of_interest, ": final-third entries"),
       subtitle = "End point of each completed pass or carry. Attacking left to right.",
       colour = NULL, shape = NULL) +
  theme_pitch() +
  theme(legend.position = "bottom", plot.title = element_text(face = "bold"))
save_plot("08_pitch_map.png", width = 7, height = 6)

message("Done. Charts saved in: ", normalizePath(out_dir))
