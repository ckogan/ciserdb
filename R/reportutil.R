college_hours_report <- function(con, min_hours = 10) {

  tbl(con, "EMPLOYEE_TIME") %>%
    select(PROJECT_NO, DATE, HOURS) %>%
    left_join(tbl(con, "Project") %>% select(PROJECT_NO, USER_NO)) %>%
    left_join(tbl(con, "User") %>% select(USER_NO, DEPARTMENT_NO)) %>%
    left_join(tbl(con, "CollegeDepartment") %>% select(COLLEGE, DEPARTMENT_NO)) %>%
    mutate(joiner = "1") %>%
    inner_join(tbl(con, "FISCAL_YEAR") %>% mutate(joiner = 1)) %>%
    filter(DATE >= STARTDATE, DATE <= ENDDATE) %>%
    select(YEAR, COLLEGE, HOURS) %>%
    group_by(YEAR, COLLEGE) %>%
    summarise(HOURS = sum(HOURS)) %>% collect() %>%
    ungroup() %>%
    filter(HOURS > min_hours) %>%
    mutate(
      COLLEGE = substr(COLLEGE, 1, 23)
    )
}

department_hours_report <- function(con) {
  tbl(con, "EMPLOYEE_TIME") %>%
    select(PROJECT_NO, DATE, HOURS) %>%
    left_join(tbl(con, "Project") %>% select(PROJECT_NO, USER_NO)) %>%
    left_join(tbl(con, "User") %>% select(USER_NO, DEPARTMENT_NO)) %>%
    left_join(tbl(con, "CollegeDepartment") %>% select(COLLEGE, DEPARTMENT, DEPARTMENT_NO)) %>%
    select(COLLEGE, DEPARTMENT, HOURS) %>%
    group_by(COLLEGE, DEPARTMENT) %>%
    summarise(HOURS = sum(HOURS)) %>% collect() %>%
    ungroup() %>%
    filter(HOURS > 5) %>%
    mutate(
      COLLEGE = substr(COLLEGE, 1, 23)
    ) %>% arrange(HOURS) %>% View()

}

college_hours_graph <- function(con) {
  hours <- college_hours_report(con)
  hours %>% filter(!is.na(HOURS)) %>% ggplot(aes(COLLEGE, HOURS)) +
    geom_bar(stat = "identity") + facet_wrap(~YEAR) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}



most_recent_date_for_hours <- function(con) {
  tbl(con, "EMPLOYEE_TIME") %>% select(DATE) %>% collect %>% .$DATE %>% max
}

earning_potential <- function(con) {
  tbl(con, "EMPLOYEE_TIME") %>% select(EMPLOYEE_NO, DATE, HOURS, ASSISTANCE_TYPE) %>%
    inner_join(tbl(con, "EMPLOYEE_RATE")) %>%
    filter(DATE >= STARTDATE, DATE <= ENDDATE) %>%
    select(-STARTDATE, -ENDDATE) %>%
    mutate(AMT = RATE * HOURS) %>%
    mutate(joiner = "1") %>%
    inner_join(tbl(con, "FISCAL_YEAR") %>% mutate(joiner = 1)) %>%
    filter(DATE >= STARTDATE, DATE <= ENDDATE) %>%
    group_by(YEAR, ASSISTANCE_TYPE) %>%
    summarise(AMT = sum(AMT)) %>% collect()
}

project_hours <- function(con) {
  tbl(con, "User") %>%
    inner_join(tbl(con, "Project") %>% select(PROJECT_NO, USER_NO), by = c("USER_NO")) %>%
    inner_join(tbl(con, "EMPLOYEE_TIME"), by = c("PROJECT_NO")) %>%
    mutate(joiner = 1) %>%
    left_join(tbl(con, "FISCAL_YEAR") %>% mutate(joiner = 1) %>% filter(YEAR == "2018")) %>%
    filter(DATE >= STARTDATE, DATE <= ENDDATE) %>%
    inner_join(tbl(con, "CollegeDepartment", by = c("DEPARTMENT_NO"))) %>%
    filter(COLLEGE == "College of Agricultural, Human, and Natural R") %>%
    group_by(NAME, ASSISTANCE_TYPE) %>%
    summarise(HOURS = sum(HOURS)) %>%
    collect() %>%
    spread(ASSISTANCE_TYPE, HOURS)
}

user_start_dates <- function(con) {
  tbl(con, "User") %>%
    inner_join(tbl(con, "Project") %>% select(PROJECT_NO, USER_NO), by = c("USER_NO")) %>%
    inner_join(tbl(con, "EMPLOYEE_TIME"), by = c("PROJECT_NO")) %>%
    group_by(NAME) %>%
    summarise(start_dt = min(DATE)) %>%
    collect()
}

duplicate_projects <- function(con) {
  tbl(con, "User") %>%
    inner_join(tbl(con, "Project") %>% select(PROJECT_NO, USER_NO, Title), by = c("USER_NO")) %>%
    group_by(NAME, Title) %>%
    summarise(cnt = n()) %>%
    filter(cnt > 1) %>%
    collect()
}

#' @examples
#' weekly_hours_report(con, '2018-07-01', '2019-02-12', write = T, spaced = T)
weekly_hours_report <- function(con, dt_start, dt_end, write = F, filterfun = NULL, spaced = F) {
  cur_dt <- as.character(Sys.Date())

  init_mtg <- tbl(con, "EMPLOYEE_TIME") %>%
    inner_join(tbl(con, "Project") %>% select(PROJECT_NO, USER_NO, Title), by = c("PROJECT_NO")) %>%
    collect() %>%
    group_by(PROJECT_NO) %>%
    mutate(
      INITIAL_MEETING = if_else(DATE == min(DATE), "Yes", "No")
    ) %>%
    ungroup() %>%
    select(EMPLOYEETIME_NO, INITIAL_MEETING)

  res <- tbl(con, "EMPLOYEE_TIME") %>% filter(DATE >= dt_start, DATE <= dt_end) %>%
    inner_join(tbl(con, "Employee") %>% select(EMPLOYEE_NO, EMPLOYEE_NAME = NAME), by = "EMPLOYEE_NO") %>%
    # filter(EMPLOYEE_NAME == "Clark Kogan") %>%
    # mutate(EMPLOYEE_NAME = "Kogan") %>%
    inner_join(tbl(con, "Project") %>% select(PROJECT_NO, USER_NO, Title = TRACKINGTOOLTITLE), by = c("PROJECT_NO")) %>%
    inner_join(tbl(con, "User") %>% select(USER_NO, NAME, STATUS, DEPARTMENT_NO), by = "USER_NO") %>%
    inner_join(tbl(con, "CollegeDepartment"), by = "DEPARTMENT_NO") %>%
    collect() %>%
    mutate_at(
      vars(ASSISTANCE_TYPE, STATUS), funs(toTitleCase(tolower(.)))
    ) %>%
      inner_join(init_mtg, by = "EMPLOYEETIME_NO") %>%
    left_join(tbl(con, "MEETING") %>% select(USER_NO, DATE, MEETING_TYPE) %>% collect(), by = c("USER_NO", "DATE")) %>%
    mutate(
      Videoconference = "No",
      Videoconference = ifelse(!is.na(MEETING_TYPE) & MEETING_TYPE == "Video Conference", "Yes", Videoconference)
    ) %>%
    mutate(
      EMPLOYEE_NAME = if_else(EMPLOYEE_NAME == "CLARK KOGAN", "Kogan", EMPLOYEE_NAME),
      EMPLOYEE_NAME = if_else(EMPLOYEE_NAME == "JAN DASGUPTA", "Dasgupta", EMPLOYEE_NAME),
      EMPLOYEE_NAME = if_else(EMPLOYEE_NAME == "DEBASMITA DAS", "Das", EMPLOYEE_NAME),
      EMPLOYEE_NAME = if_else(EMPLOYEE_NAME == "JILLIAN MORRISON", "Morrison", EMPLOYEE_NAME)
    ) %>%
    arrange(DATE)
  if(!is.null(filterfun))
    res <- filterfun(res)
  if(spaced) {
    res <- res %>%
      mutate(
        blank1 = "",
        blank2 = ""
      ) %>% select(Date = DATE, Client = NAME, Title, College = COLLEGE_ABBREV, Department = DEPARTMENT, blank1, `Primary Role` = STATUS, `CISER Resource` = EMPLOYEE_NAME, blank2, `Assistance Type` = ASSISTANCE_TYPE, `Number of Hours` = HOURS, Videoconference, `Initial Meeting` = INITIAL_MEETING, Description = DESCRIPTION)
  }
  else{
    res <- res %>% select(Date = DATE, Client = NAME, Title, College = COLLEGE_ABBREV, Department = DEPARTMENT, `Primary Role` = STATUS, `CISER Resource` = EMPLOYEE_NAME, `Assistance Type` = ASSISTANCE_TYPE, `Number of Hours` = HOURS, Videoconference, `Initial Meeting` = INITIAL_MEETING, Description = DESCRIPTION)
  }
  if(write) {
    file_str <- paste0('./datafiles/weekly_hours_', dt_start, '_to_', dt_end, ".csv")
    write.csv(res, file = file_str, row.names = F)
  }

  res
}

#' @export
user_hours <- function(con, user_name) {
  tbl(con, "User") %>% filter(NAME == user_name) %>%
    inner_join(tbl(con, "Project")) %>%
    inner_join(tbl(con, "EMPLOYEE_TIME")) %>% collect()
}

recent_users <- function(con, dt_start) {
  data <- tbl(con, "User") %>%
    inner_join(tbl(con, "Project")) %>%
    inner_join(tbl(con, "EMPLOYEE_TIME")) %>%
    filter(DATE > dt_start) %>%
    collect()
  unique(data %>% select(NAME, Title, Email, STARTDATE))
}

#' @export
all_by_user <- function(con, user) {
  tbl(con, "User") %>% filter(NAME == user) %>%
    inner_join(tbl(con, "Project")) %>%
    inner_join(tbl(con, "EMPLOYEE_TIME")) %>% collect()

}

unbilled_consulting <- function(con) {
  remove <- tbl(con, "INVOICE_LINE")  %>% collect() %>% .$EMPLOYEETIME_NO
  tbl(con, "EMPLOYEE_TIME_V") %>%
    filter(DATE > '2018-06-31') %>%
    collect() %>%
    filter(ASSISTANCE_TYPE == "CONSULTING", !(EMPLOYEETIME_NO %in% remove)) %>%
    group_by(NAME, USER_NO) %>%
    summarise(HOURS = sum(HOURS))
}
