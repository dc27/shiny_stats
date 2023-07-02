server <- function(input, output) {
  
  output$die_dot <- renderPlot({
    validate(
      need(input$dest, label = "Numbers")
    )
    
    data.frame(
      choices = (as.numeric(input$dest))
    ) |>
      ggplot(aes(x = choices)) +
      geom_dotplot(binwidth = 1, method = "histdot", dotsize = 0.1, )
  })
  
  
  # create data frame of means and conf ints depending on users':
  # 1. sample size
  # 2. conf int width
  ci_df <- eventReactive(
    input$update,
    ignoreNULL = FALSE,
    map_dfr(
      # take multiple samples
      1:100, .f = ~make_ci_table(.x, pop_wpm, input$s_size, input$ci_width)
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