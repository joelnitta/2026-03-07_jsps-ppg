#' Create Combined Plot of PPG Issues Over Time
#'
#' Generates a combined visualization showing the progress of PPG
#' (Pteridophyte Phylogeny Group) proposals over time. The function
#' creates two plots: a cumulative count of submitted proposals and a
#' stacked bar chart showing voting results (passed, not passed, or TBD).
#' The plots are combined side-by-side with subfigure labels using the
#' patchwork package.
#'
#' @param ppg_issues A data frame containing GitHub issue data for PPG
#'   proposals. Expected to have columns: `title` (containing status
#'   designations like "[PASSED]" or "[NOT PASSED]") and `created_at`
#'   (timestamp of issue creation).
#'
#' @return A patchwork object combining two ggplot2 plots:
#'   \describe{
#'     \item{Plot A}{Line plot showing cumulative number of proposals
#'       submitted over time}
#'     \item{Plot B}{Stacked bar chart showing the count of proposals
#'       by voting result (passed, not passed, TBD) for each time
#'       period}
#'   }
make_issues_plot <- function(ppg_issues) {
  require(patchwork)

  # set color palette
  okabe_ito_cols <- c(
    orange = "#E69F00",
    skyblue = "#56B4E9",
    bluishgreen = "#009E73",
    yellow = "#F0E442",
    blue = "#0072B2",
    vermillion = "#D55E00",
    reddishpurple = "#CC79A7"
  )

  # Format issues (proposals)
  issues <-
    ppg_issues |>
    mutate(passed = str_detect(title, "\\[PASSED\\]")) |>
    mutate(not_passed = str_detect(title, "\\[NOT PASSED\\]")) |>
    mutate(voting = !str_detect(title, "PASSED")) |>
    mutate(created_at = lubridate::as_date(created_at)) |>
    mutate(
      month = lubridate::month(created_at) |>
        str_pad(side = "left", pad = "0", width = 2)
    ) |>
    mutate(year = lubridate::year(created_at)) |>
    mutate(year_month = paste(year, month, sep = "-"))

  issues_summary <-
    issues %>%
    group_by(year_month) %>%
    summarize(
      passed = sum(passed),
      not_passed = sum(not_passed),
      under_vote = sum(voting)
    )

  # Make plot of cumulative count of submitted proposals
  cum_issue_plot <-
    issues %>%
    group_by(year_month) %>%
    mutate(year_month = lubridate::ym(year_month)) %>%
    summarize(
      n = n(),
      .groups = "drop"
    ) %>%
    mutate(n_total = cumsum(n)) %>%
    mutate(dummy = "a") %>%
    ggplot(aes(x = year_month, y = n_total, group = dummy)) +
    geom_line() +
    geom_point(size = 1) +
    labs(
      y = "Number of proposals"
    ) +
    scale_x_date(
      date_labels = "%Y-%m",
      date_breaks = "4 months"
    ) +
    theme_bw(base_size = 12) +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      axis.title.x = element_blank(),
      panel.grid.minor = element_blank()
    )

  vote_res_plot <-
    issues_summary %>%
    pivot_longer(names_to = "type", values_to = "count", -year_month) %>%
    mutate(year_month = lubridate::ym(year_month)) %>%
    ggplot(aes(x = year_month, y = count, fill = type)) +
    geom_col(position = "stack") +
    labs(
      fill = "Result"
    ) +
    scale_fill_manual(
      values = c(
        "passed" = okabe_ito_cols[["blue"]],
        "not_passed" = okabe_ito_cols[["vermillion"]]
      ),
      breaks = c("passed", "not_passed"),
      labels = c("Passed", "Not passed")
    ) +
    scale_x_date(
      date_labels = "%Y-%m",
      date_breaks = "4 months"
    ) +
    scale_y_continuous(
      breaks = seq(0, 10, by = 2),
      expand = expansion(mult = c(0, 0.05))
    ) +
    theme_bw(base_size = 12) +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      # Move legend just inside top-right corner of plot area
      legend.position = c(0.99, 0.99),
      legend.justification = c("right", "top"),
      panel.grid.minor = element_blank()
    )

  combined_plot <- cum_issue_plot +
    vote_res_plot +
    plot_annotation(tag_levels = 'A')

  combined_plot
}

make_curators_plot <- function(curators) {
  # Count number of genera that have x curators per genus
  curator_count <-
    curators |>
    select(genus, contains("curator")) |>
    pivot_longer(
      names_to = "number",
      values_to = "name",
      -genus
    ) |>
    select(-number) |>
    unique() |>
    group_by(genus) |>
    summarize(
      n = n_distinct(name, na.rm = TRUE)
    ) |>
    count(n, name = "num_genera") |>
    rename(n_curators = n) |>
    mutate(status = ifelse(n_curators == 0, "No curator", "Has curator"))

  # Set color palette
  okabe_ito_cols <- c(
    orange = "#E69F00",
    skyblue = "#56B4E9",
    bluishgreen = "#009E73",
    yellow = "#F0E442",
    blue = "#0072B2",
    vermillion = "#D55E00",
    reddishpurple = "#CC79A7"
  )

  # Make plot, highlight bar with zero curators
  ggplot(curator_count, aes(x = n_curators, y = num_genera, fill = status)) +
    geom_col(width = 0.7) +
    scale_fill_manual(
      values = c(
        "No curator" = okabe_ito_cols[["vermillion"]],
        "Has curator" = okabe_ito_cols[["blue"]]
      )
    ) +
    scale_x_continuous(
      breaks = 0:5,
      expand = expansion(mult = c(0.05, 0.05))
    ) +
    scale_y_continuous(
      expand = expansion(mult = c(0, 0.05))
    ) +
    labs(
      x = "Number of curators per genus",
      y = "Number of genera"
    ) +
    theme_bw(base_size = 24) +
    theme(
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank(),
      legend.position = "none"
    )
}
