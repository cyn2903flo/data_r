# Function to form a one sentence summary from a year
# of annual data
summarize_park <- function(one_year){
  comma <- scales::label_comma()
  one_year %>% 
    glue::glue_data(
      "En { year }, { park_name } se tuvieron { comma(recreation_visits) } visitas."
    )
}

# Takes annual data and produces a plot
plot_annual <- function(annual_data, highlight_year = 2019){
  annual_data %>% 
    ggplot(aes(year, recreation_visits)) +
    geom_point(data = ~ filter(., year == highlight_year)) +
    geom_line() +
    scale_y_continuous(labels = scales::label_comma()) +
    labs(x = "", y = "Visitas")
}

# Takes monthly data and produces a plot
plot_monthly <- function(monthly_data, highlight_year = 2019,
                         display_average = TRUE){
  p <- monthly_data %>% 
    ggplot(aes(month, recreation_visits_proportion)) +
    geom_line(aes(group = year), alpha = 0.1) +
    geom_line(data = ~ filter(.x, year == highlight_year)) +
    scale_x_continuous(breaks = 1:12, labels = month.abb) +
    scale_y_continuous(labels = scales::label_percent()) +
    labs(x = "", y = "Visitas anuales")
  if(display_average) {
    p <- p + stat_summary(fun = mean, 
      geom = "line", color = "#325D88", size = 3.5) 
  }
  p
}
