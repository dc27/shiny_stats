ui <- dashboardPage(
  dashboardHeader(
    title = tagList(
      span(class = "logo-lg", "Probability and Statistics")
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
        tabName = "measures_centre"
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
        tabName = "measures_centre",
        titlePanel("Measures of Centre"),
        sidebarLayout(
          fluid = TRUE,
          sidebarPanel(
            orderInput('source', 'Die Outcomes', items = 1:6,
                       as_source = TRUE, connect = 'dest'),
            orderInput('dest', 'Dest', items = NULL, placeholder = 'Drag numbers here...'),
          ),
          mainPanel(
            plotOutput("die_dot")
          )
        )
        
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
              value = 30),
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