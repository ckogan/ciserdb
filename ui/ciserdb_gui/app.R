#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


con <- ciserdb()


# Define UI for application that draws a histogram
ui <- fluidPage(
  useShinyalert(),

  # App title ----
  titlePanel("Enter CISER Time Tracking Data"),

  # Sidebar layout with input and output definitions ----
  sidebarLayout(

    # Sidebar panel for inputs ----
    sidebarPanel(


      # br() element to introduce extra vertical spacing ----
      br()

    ),

    # Main panel for displaying outputs ----
    mainPanel(

      # Output: Tabset w/ plot, summary, and table ----
      tabsetPanel(type = "tabs",
                  tabPanel("Time",
                           dateInput("time_date", "Date", value = Sys.Date()),
                           selectInput("time_user", "USER", tbl(con, "User") %>% select(NAME) %>% collect()),
                           uiOutput("time_proj_control"),
                           selectInput("time_employee", "EMPLOYEE", tbl(con, "Employee") %>% select(NAME) %>% collect()),
                           selectInput("time_assistance_type", "ASSISTANCE_TYPE", c("ENGAGED LEARNING", "COLLABORATION", "CONSULTING")),
                           numericInput("time_hours", "HOURS", 1, min = 0, max = 30),
                           numericInput("time_minutes", "MINUTES", 0, min = 0, max = 60),
                           selectInput("time_description", "DESCRIPTION", tbl(con, "EMPLOYEE_TIME_DESCRIPTION") %>% collect() %>% select(DESCRIPTION)),
                           checkboxInput("time_meeting", "INCLUDED MEETING?", value = FALSE, width = NULL),
                           conditionalPanel(
                             condition = "input.time_meeting == true",
                             selectInput("time_meeting_type", "Meeting Type",
                                         c('Face to Face', 'No-Show', 'Phone', 'Video Conference'))
                           ),
                           actionButton("time_submit", "Submit")
                           ),
                  tabPanel("User",
                           textInput("user_name", "USER"),
                           textInput("user_email", "EMAIL"),
                           selectInput("user_status", "STATUS", tbl(con, "User") %>% select(STATUS) %>% collect() %>% unique()),
                           selectInput("user_college", "COLLEGE", tbl(con, "CollegeDepartment") %>% select(COLLEGE) %>% collect() %>% unique()),
                           uiOutput("user_department_control"),
                           actionButton("user_submit", "Submit")
                  ),

                  tabPanel("Project",
                           selectInput("proj_user", "USER", tbl(con, "User") %>% select(NAME) %>% collect() %>% .$NAME),
                           dateInput("proj_startdate", "Project Start Date", value = Sys.Date()),
                           textInput("proj_title", "TITLE"),
                           selectInput("proj_lead", "LEAD", tbl(con, "Employee") %>% select(NAME) %>% collect() %>% .$NAME),
                           actionButton("proj_submit", "Submit")
                  ),
                  tabPanel("Publication",
                           selectInput("pub_user", "USER", tbl(con, "User") %>% select(NAME) %>% collect()),
                           uiOutput("pub_proj_control"),
                           textInput("pub_title", "TITLE"),
                           textInput("pub_doi", "DOI"),
                           textInput("pub_journal", "JOURNAL"),
                           selectInput("pub_status", "STATUS", c("ANTICIPATED", "SUBMITTED", "PUBLISHED")),
                           actionButton("pub_submit", "Submit")
                  ),
                  tabPanel("Grants",
                           selectInput("grt_user", "USER", tbl(con, "User") %>% select(NAME) %>% collect()),
                           uiOutput("grt_proj_control"),
                           checkboxInput("grt_has_pi", "PI KNOWN?", value = FALSE, width = NULL),
                           conditionalPanel(
                             condition = "input.grt_has_pi == true",
                             selectInput("grt_pi", "PI", tbl(con, "User") %>% select(NAME) %>% collect())
                           ),
                           textInput("grt_title", "TITLE"),
                           textInput("grt_number", "WSU_GRANT_NUMBER"),
                           selectInput("grt_status", "STATUS", c("UNKNOWN","PLANNED", "SUBMITTED", "ACCEPTED")),
                           actionButton("grt_submit", "Submit")
                  )
      )

    )
  )
)

# Define server logic for random distribution app ----
server <- function(input, output, session) {
  department_list <- reactive({
    tbl(con, "CollegeDepartment") %>% filter(COLLEGE == input$user_college) %>% select(DEPARTMENT) %>% collect() %>% .$DEPARTMENT
  })
  time_project_list <- reactive({
    tbl(con, "Project") %>% inner_join(tbl(con, "User")) %>% filter(NAME == input$time_user) %>% select(Title) %>% collect() %>% .$Title
  })

  pub_project_list <- reactive({
    tbl(con, "Project") %>% inner_join(tbl(con, "User")) %>% filter(NAME == input$pub_user) %>% select(Title) %>% collect() %>% .$Title
  })

  grt_project_list <- reactive({
    tbl(con, "Project") %>% inner_join(tbl(con, "User")) %>% filter(NAME == input$grt_user) %>% select(Title) %>% collect() %>% .$Title
  })

  # Reactive expression to generate the requested distribution ----
  # This is called whenever the inputs change. The output functions
  # defined below then use the value computed from this expression


  # Generate a plot of the data ----
  # Also uses the inputs to build the plot label. Note that the
  # dependencies on the inputs and the data reactive expression are
  # both tracked, and all expressions are called in the sequence
  # implied by the dependency graph.

  output$user_department_control <- renderUI({
    tagList(
      selectInput("user_department", "DEPARTMENT", department_list())
    )
  })

  output$time_proj_control <- renderUI({
    tagList(
      selectInput("time_proj", "PROJECT", time_project_list())
    )
  })

  output$pub_proj_control <- renderUI({
    tagList(
      selectInput("pub_proj", "PROJECT", pub_project_list())
    )
  })

  output$grt_proj_control <- renderUI({
    tagList(
      selectInput("grt_proj", "PROJECT", grt_project_list())
    )
  })



  observeEvent(input$time_submit, {
    # Get the employee number
    employee_no <- tbl(con, "Employee") %>% filter(NAME == input$time_employee) %>% select(EMPLOYEE_NO) %>% collect() %>% .$EMPLOYEE_NO
    project_no <- tbl(con, "Project") %>% inner_join(tbl(con, "User")) %>% filter(Title == input$time_proj, NAME == input$time_user) %>% select(PROJECT_NO) %>% collect() %>% .$PROJECT_NO
    project_no <- project_no[1]
    data <- data.frame(EMPLOYEE_NO = employee_no, DATE = input$time_date, HOURS = input$time_hours + input$time_minutes/60, PROJECT_NO = project_no, ASSISTANCE_TYPE = input$time_assistance_type, DESCRIPTION = input$time_description)
    res_employee <- dbWriteTable(con, "EMPLOYEE_TIME", data, append = T, row.names = F)

    if(input$time_meeting) {
      user_no <- tbl(con, "User") %>% filter(NAME == input$time_user) %>% collect() %>% select(USER_NO) %>% .$USER_NO
      user_no <- user_no[1]
      data <- data.frame(USER_NO = user_no, DATE = input$time_date, MEETING_TYPE = input$time_meeting_type)
      res_meeting <- dbWriteTable(con, "MEETING", data, append = T, row.names = F)
    }

    if((res_employee == TRUE && !input$time_meeting) || (res_employee == TRUE && res_meeting == TRUE)) {
      shinyalert("Results written.", type = "success")
    } else {
      shinyalert("Oops! Some results were not written.", type = "error")
    }
  })

  observeEvent(input$proj_submit, {
    user_no <- tbl(con, "User") %>% filter(NAME == input$proj_user) %>% select(USER_NO) %>% collect() %>% .$USER_NO
    project_no <- tbl(con, "Project") %>% inner_join(tbl(con, "User")) %>% filter(Title == input$time_proj, NAME == input$time_user) %>% select(PROJECT_NO) %>% collect() %>% .$PROJECT_NO
    lead_no <- tbl(con, "Employee") %>% filter(NAME == input$proj_lead) %>% collect %>% .$EMPLOYEE_NO
    data <- data.frame(USER_NO = user_no, STARTDATE = input$proj_startdate, ENDDATE = as.Date(NA), Title = input$proj_title, LEAD_EMPLOYEE_NO = lead_no, TRACKINGTOOLTITLE = input$proj_title)

    res <- dbWriteTable(con, "Project", data, append = T, row.names = F)

    if(res == TRUE) {
      shinyalert("Results written.", type = "success")
    } else {
      shinyalert("Oops! Results not written.", type = "error")
    }
  })

  observeEvent(input$user_submit, {
    # Get the employee number
    dept_no <- tbl(con, "CollegeDepartment") %>% filter(COLLEGE == input$user_college, DEPARTMENT == input$user_department) %>% collect() %>% select(DEPARTMENT_NO)
    data <- data.frame(NAME = input$user_name, Email = input$user_email, STATUS = input$user_status, DEPARTMENT_NO = dept_no)

    res <- dbWriteTable(con, "User", data, append = T, row.names = F)

    if(res == TRUE) {
      shinyalert("Results written.", type = "success")
    } else {
      shinyalert("Oops! Results not written.", type = "error")
    }
    updateSelectInput(session, "time_user", choices = tbl(con, "User") %>% select(NAME) %>% collect())
    updateSelectInput(session, "proj_user", choices = tbl(con, "User") %>% select(NAME) %>% collect())
  })


  observeEvent(input$pub_submit, {
    blank2na <- function(x) ifelse(is.na(x), as.character(NA), x)
    user_no <- tbl(con, "User") %>% filter(NAME == input$pub_user) %>% select(USER_NO) %>% collect() %>% .$USER_NO
    project_no <- tbl(con, "Project") %>% inner_join(tbl(con, "User")) %>% filter(Title == input$pub_proj, NAME == input$pub_user) %>% select(PROJECT_NO) %>% collect() %>% .$PROJECT_NO

    data <- data.frame(PROJECT_NO = project_no, TITLE = input$pub_title, DOI = blank2na(input$pub_doi), JOURNAL = blank2na(input$pub_journal), STATUS = input$pub_status)

    res <- dbWriteTable(con, "PUBLICATION", data, append = T, row.names = F)

    if(res == TRUE) {
      shinyalert("Results written.", type = "success")
    } else {
      shinyalert("Oops! Results not written.", type = "error")
    }
  })

  observeEvent(input$grt_submit, {
    blank2na <- function(x) ifelse(is.na(x), as.character(NA), x)
    user_no <- tbl(con, "User") %>% filter(NAME == input$grt_user) %>% select(USER_NO) %>% collect() %>% .$USER_NO
    if(input$grt_has_pi)
      pi_user_no <- tbl(con, "User") %>% filter(NAME == input$grt_user) %>% select(USER_NO) %>% collect() %>% .$USER_NO
    else
      pi_user_no <- as.numeric(NA)
    project_no <- tbl(con, "Project") %>% inner_join(tbl(con, "User")) %>% filter(Title == input$grt_proj, NAME == input$grt_user) %>% select(PROJECT_NO) %>% collect() %>% .$PROJECT_NO

    data <- data.frame(PROJECT_NO = project_no, TITLE = input$grt_title, PI_USER_NO = pi_user_no, WSU_GRANT_NUMBER = input$grt_number, STATUS = input$grt_status, stringsAsFactors = F)

    res <- dbWriteTable(con, "GRANTS", data, append = T, row.names = F)

    if(res == TRUE) {
      shinyalert("Results written.", type = "success")
    } else {
      shinyalert("Oops! Results not written.", type = "error")
    }
  })

}

# Run the application
shinyApp(ui = ui, server = server)

