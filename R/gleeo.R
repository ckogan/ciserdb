parse_gleo <- function(data) {
  data %>%
    select(Project, Task, Details, Start, Decimal.Duration) %>%
    mutate(
      dt = as.Date(Start)
    ) %>%
    group_by(Project, Task, dt) %>%
    summarise(
      Details = paste0(Details, collapse = ";"),
      hrs = sum(Decimal.Duration))
}
