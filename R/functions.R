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

  # Calculate date range for consistent x-axis across both plots
  # Add buffer to both ends to ensure all data points are visible
  date_range <- issues %>%
    mutate(year_month = lubridate::ym(year_month)) %>%
    summarize(
      min_date_actual = min(year_month),
      min_date = min(year_month) - lubridate::days(15),
      max_date_actual = max(year_month),
      max_date = max(year_month) + lubridate::days(15)
    )

  # Create breaks that include start and end months
  date_breaks_seq <- seq(
    from = date_range$min_date_actual,
    to = date_range$max_date_actual,
    by = "4 months"
  )
  # Ensure the last date is included
  if (max(date_breaks_seq) < date_range$max_date_actual) {
    date_breaks_seq <- c(date_breaks_seq, date_range$max_date_actual)
  }

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
      breaks = date_breaks_seq,
      limits = c(date_range$min_date, date_range$max_date)
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
      breaks = date_breaks_seq,
      limits = c(date_range$min_date, date_range$max_date)
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

make_author_map <- function(ppg2_authors) {
  # Get world map data
  world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")

  # Manual mapping for countries with different names
  country_mapping <- c(
    "USA" = "United States",
    "England UK" = "United Kingdom",
    "Scotland UK" = "United Kingdom",
    "Russian Federation" = "Russia",
    "Czech Republic" = "Czechia",
    "Republic of Korea" = "South Korea",
    "Brunei Darussalam" = "Brunei"
  )

  # Count members by country with mapping applied
  country_counts <- ppg2_authors |>
    mutate(
      country_mapped = ifelse(
        country %in% names(country_mapping),
        country_mapping[country],
        country
      )
    ) |>
    count(country_mapped, name = "n_members")

  # Get country centroids for points
  world_points <- world |>
    sf::st_centroid() |>
    sf::st_coordinates() |>
    as.data.frame() |>
    bind_cols(world |> sf::st_drop_geometry() |> select(name)) |>
    rename(lon = X, lat = Y, country_mapped = name) |>
    mutate(
      country_mapped = case_when(
        country_mapped == "United States of America" ~ "United States",
        country_mapped == "United Kingdom" ~ "United Kingdom",
        TRUE ~ country_mapped
      )
    )

  # Join counts to centroids
  plot_data <- world_points |>
    left_join(country_counts, by = "country_mapped") |>
    filter(!is.na(n_members))

  # Manual coordinate overrides for better point placement
  coord_overrides <- tribble(
    ~country_mapped , ~lon_new , ~lat_new ,
    "United States" ,      -98 ,       39 , # Continental US center
    "Russia"        ,      100 ,       60 , # Western Russia
    "France"        ,        2 ,       47 # Metropolitan France
  )

  # Apply overrides
  plot_data <- plot_data |>
    left_join(coord_overrides, by = "country_mapped") |>
    mutate(
      lon = if_else(!is.na(lon_new), lon_new, lon),
      lat = if_else(!is.na(lat_new), lat_new, lat)
    ) |>
    select(country_mapped, lon, lat, n_members)

  # Create the map
  ggplot() +
    geom_sf(data = world, fill = "gray95", color = "gray80", size = 0.3) +
    geom_point(
      data = plot_data,
      aes(x = lon, y = lat, size = n_members, color = n_members)
    ) +
    scale_size_continuous(
      range = c(2, 12),
      name = "Members"
    ) +
    scale_color_scico(
      palette = "roma",
      name = "Members",
      direction = -1
    ) +
    guides(
      size = guide_legend(),
      color = guide_legend()
    ) +
    theme_void(base_size = 18) +
    theme(
      legend.position = c(0.1, 0.3),
      legend.background = element_rect(fill = "white", color = "gray80")
    )
}
