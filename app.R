library(shiny)
library(dplyr)
library(readr)
library(shinydashboard)
library(wordcloud2)
library(DT)
library(ggplot2)
# Read in data #####################
if('trump.csv' %in% dir()){
  trump <- read_csv('trump.csv')
} else {
  trump <- read_csv('https://raw.githubusercontent.com/databrew/intro-to-data-science/main/data/trumptweets.csv')
  write_csv(trump, 'trump.csv')
}
ui <- dashboardPage(
  dashboardHeader(title = "Trump data"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard"),
      menuItem("Raw data", tabName = "rawdata")
    )
  ),
  dashboardBody(
    tabItems(
      tabItem("dashboard",
              fluidPage(
                sliderInput('date_range',
                            'Pick some dates, loser!',
                            min = min(trump$date),
                            max = max(trump$date),
                            value = c(range(trump$date))),
                fluidRow(
                  column(3,
                         textInput(inputId = 'user_text',
                                   label = 'Type something HEERRRRRE')),
                  column(9,
                         plotOutput('my_plot'))
                )
              )
      ),
      tabItem("rawdata",
              fluidPage(
                fluidRow(
                  fluidRow(
                    dataTableOutput('my_table'))
                )
              )
      )
    )
  )
)
# Define server
server <- function(input, output) {
  # end of data read-in  ##############
  # Create a reactive dataframe
  dfr <- reactive({
    user_text <- tolower(input$user_text)
    dr <- input$date_range
    out <- trump %>%
      filter(grepl(user_text, tolower(content))) %>%
      filter(date >= min(dr),
             date <= max(dr))
    out
  })
  # Create a plot
  output$my_plot <- renderPlot({
    # Capture DFR
    pd <- dfr()
    # Get by month
    pd <- pd %>%
      mutate(year_month = floor_date(date, 'month')) %>%
      group_by(year_month) %>%
      tally
    # Make plot
    ggplot(data = pd,
           aes(x = year_month,
               y = n)) +
      geom_point() +
      geom_line() +
      geom_smooth()
  })
  output$my_table <- renderDataTable({
    dfr() %>%
      select(date, content)
  })
}
# Run the application
shinyApp(ui = ui, server = server)