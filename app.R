library(dplyr)
library(ggplot2)
library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(purrr)

source("generate_pop_data.R")
# 'reset' seed
set.seed(NULL)

make_ci_table <- function(rep, popn, sample_size, ci_level) {
  # samples from population and calculates a conf int
  
  s_wpm <- sample(popn, sample_size, replace = FALSE)
  
  x <- mean(s_wpm)
  s <- sd(s_wpm)
  n <- length(s_wpm)
  
  width <- ci_level + (1-ci_level)/2
  p <- c(0, ci_level) + width
  
  # originally I used qnorm but too many of my cis were hitting the mean
  error <- qt(width ,df=n-1)*s/sqrt(n)
  ci <- c(x - error, x + error)
  
  data.frame(
    rep = rep,
    x = x,
    ci_low = ci[1],
    ci_high = ci[2]
  )
}

reps <- 1:100

ui <- dashboardPage(
  dashboardHeader(
    title = tagList(
      span(class = "logo-lg", "Stats Apps")
    )
  ),
  dashboardSidebar(
    collapsed = TRUE,
    sidebarMenu(
      id = "sidebar_menu",
      menuItem(
        "Overview", tabName = "stats_overview",
        icon = icon("chart-simple", verify_fa = FALSE)),
      menuItem(
        "Measures of Centrality",
        tabName = "measures_center"
      ),
      menuItem(
        "Measures of Spread",
        tabName = "measures_spread"
      ),
      menuItem(
        "Normalisation"
      ),
      menuItem(
        "Probability"
      ),
      menuItem(
        "Probability Distributions"
      ),
      menuItem(
        "Central Limit Theorem"
      ),
      menuItem(
        "Standard Error"
      ),
      menuItem(
        "Confidence Intervals", tabName = "conf_ints",
        icon = icon("percent")
      ),
      menuItem(
        "Hypothesis Testing"
      ),
      menuItem(
        "Analysis of Variance"
      )
    )
  ),
  dashboardBody(
    withMathJax(),
    tags$head(
      tags$link(rel="stylesheet", type="text/css", href="styles.css")
    ),
    tags$style(
      "@import url(https://use.fontawesome.com/releases/v6.2.0/css/all.css);"
    ),
    tabItems(
      tabItem(
        tabName = "stats_overview",
        titlePanel("Statistics"),
        htmltools::includeMarkdown("./statistics.md")
      ),
      tabItem(
        tabName = "conf_ints",
        titlePanel("Confidence Intervals", windowTitle = "Confidence Intervals"),
        sidebarLayout(
          fluid = TRUE,
          sidebarPanel(
            sliderInput(
              "ci_width",
              "Confidence Level Value",
              min = 0, 
              max = 1, 
              value = 0.95, 
              step = 0.01),
            numericInput(
              "s_size",
              "Sample Size",
              min = 2,
              max = 1000,
              value = 10),
            actionButton("update", "Regenarate samples")
          ),
          mainPanel(
            fluidRow(
              column(
                12,
                htmltools::includeMarkdown("ci_description.md"),
                uiOutput("ci_description")
              )
            )
          )
        ),
        plotOutput("ci_plot")
      )
    )
  )
)

server <- function(input, output) {
  
  # create data frame of means and conf ints depending on users':
  # 1. sample size
  # 2. conf int width
  ci_df <- eventReactive(
    input$update,
    ignoreNULL = FALSE,
    map_dfr(
      # take multiple samples
      reps, .f = ~make_ci_table(.x, pop_wpm, input$s_size, input$ci_width)
    ) |>
      mutate(
        true_mean = mean(pop_wpm),
        hit = if_else(
          {mean(pop_wpm) < (ci_low) | mean(pop_wpm) > (ci_high)},
          FALSE, TRUE)
      )
  )
  
  # Reduce the opacity of the grid lines: Default is 255
  col_grid <- rgb(235, 235, 235, 100, maxColorValue = 255)
  
  output$ci_plot <- renderPlot({
    ggplot(ci_df(), aes(x = rep, y = x, colour = hit)) +
      geom_hline(
        yintercept = mean(pop_wpm),
        colour = "steelblue", linetype = "dashed") +
      geom_point(alpha = 0.75) +
      geom_errorbar(aes(ymin = ci_low, ymax = ci_high), alpha = 0.75) +
      scale_colour_manual(values = c(`FALSE` = "red",`TRUE` = "black")) +
      # fix y scale to be independent of user input
      scale_y_continuous(breaks = seq(20, 70, 10)) +
      coord_cartesian(ylim = c(20, 70)) +
      theme_bw(base_size = 16) +
      theme(
        legend.position = "none",
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.grid = element_line(colour = col_grid),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank()
      )
  })
  
  n_hits <- reactive(
    ci_df() |> summarise(sum(hit == TRUE))
  )
  
  val_colour <- eventReactive(
    input$update,
    ignoreNULL = FALSE,
    {
      if_else(
        n_hits() >= (input$ci_width * 100),
        "rgba(0, 255, 0, 0.3)",
        "rgba(255, 0, 0, 0.3)")
    }
  )
  
  output$ci_description <- renderUI({
    fluidRow(
      column(
        3,
        tags$p(
          style = sprintf("background: %s; padding: 1rem; border: 2px solid black;", val_colour()),
          paste0("Number of Hits: ", n_hits())
        )
      ),
      column(
        3,
        tags$p(
          style = "padding: 1rem;",
          paste0("Expected Hits: ", input$ci_width * 100)
        )
      )
    )
  })
  
}



shinyApp(ui, server)