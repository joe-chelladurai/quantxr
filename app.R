library(shiny)
library(tidyverse)
library(psych)


skewed_df <- function(n = 100, shape1 = 2.3, shape2 = 6.8, seed = 1234) {
  skewed <- function(n, skew = 'right', shape1, shape2, seed) {
    set.seed(seed)
    if(skew == 'right') {
      skewed <- rbeta(n, shape1, shape2)
    } else if (skew == 'left') {
      skewed <- rbeta(n, shape2, shape1)
    }
    skewed_scaled <- 1 + 4 * skewed
    return(round(skewed_scaled))
  }
  
  set.seed(seed)
  seeds <- seed:(seed + 9)
  
  df <- data.frame(matrix(ncol = length(seeds), nrow = n))
  
  for (i in 1:length(seeds)) {
    skew_direction <- ifelse(i %% 2 == 0, 'right', 'left')
    if(i == 4 || i == 10) {
      shape1_adj <- shape1 * 2  # Increase shape1 to make values larger
      shape2_adj <- shape2 * 2  # Increase shape2 to make values larger
    } else {
      shape1_adj <- shape1
      shape2_adj <- shape2
    }
    df[,i] <- skewed(n, skew_direction, shape1_adj, shape2_adj, seeds[i])
    names(df)[i] <- paste('q', i, sep = '_')
  }
  return(df)
}


ui <- fluidPage(
  titlePanel("Skewed Distribution Explorer"),
  sidebarLayout(
    sidebarPanel(width = 2,
                 numericInput("n", "Number of observations:", 100),
                 sliderInput("shape1", "Shape 1:", min = 0.1, max = 10, value = 1, step = 0.1),
                 sliderInput("shape2", "Shape 2:", min = 0.1, max = 10, value = 3, step = 0.1),
                 numericInput("seed", "Global Seed:", 1),
                 actionButton("add_button", "Snapshot"),
                 verbatimTextOutput("mean"),
                 plotOutput("sus_plot")
    ),
    mainPanel(width = 10,
              fluidRow(tableOutput("summary")),
              fluidRow(tableOutput("log_table")),
            #  fluidRow(verbatimTextOutput("factorAnalysis")),
              fluidRow(plotOutput("densityPlot")),
              fluidRow(verbatimTextOutput("code"))
    )
  )
)

server <- function(input, output) {
  
  data <- reactive({
    skewed_df(n = input$n, shape1 = input$shape1, shape2 = input$shape2, seed = input$seed)
  })
  
  
  output$densityPlot <- renderPlot({
    df <- data() %>%
      gather()
    
    ggplot(df, aes(x = value)) +
      geom_density() +
      facet_wrap( ~ key)
  })
  
  factorAnalysis <- reactive({
    fa(r = cor(data()), nfactors = 2)
  })
  
  output$factorAnalysis <- renderPrint({
    print(factorAnalysis())
  })
  
  
  rv <- reactiveValues(log_df = data.frame())
  
  # Respond to button click
  observeEvent(input$add_button, {
    # Add the input values as a new row in the data frame
    new_row <- tibble(n = input$n, shape1 = input$shape1, shape2 = input$shape2, seed = input$seed, mean = calculated_mean())
    rv$log_df <- bind_rows(rv$log_df, new_row)
  })
  
  # Display the log_df data frame
  output$log_table <- renderTable({
    rv$log_df
  })
  
  
  output$sus_plot <- renderPlot({
   
      mean_sus <- data() %>%
        mutate(total_odd = rowSums(select(., ends_with(c("1", "3", "5", "7", "9"))), na.rm = TRUE),
               total_even = rowSums(select(., ends_with(c("0", "2", "4", "6", "8"))), na.rm = TRUE)) %>%
        mutate(total_odd = total_odd - 5,
               total_even = 25 - total_even,
               total = total_odd + total_even,
               total = total * 2.5)
      
      ggplot(data = mean_sus, aes(x = total)) + geom_histogram() + scale_x_continuous(limits = c(0, 100))
    
    })
 
  
 calculated_mean <- reactive({
    mean_sus <- data() %>%
      mutate(total_odd = rowSums(select(., ends_with(c("1", "3", "5", "7", "9"))), na.rm = TRUE),
             total_even = rowSums(select(., ends_with(c("0", "2", "4", "6", "8"))), na.rm = TRUE)) %>%
      mutate(total_odd = total_odd - 5,
             total_even = 25 - total_even,
             total = total_odd + total_even,
             total = total * 2.5) %>%
      summarise(mean = mean(total))
    
    mean_sus
  })
  
  output$mean <- renderPrint({
   calculated_mean()
  })
  output$summary <- renderTable({

    
    stats <- psych::describe(data())
    
   stats |> as_tibble()
  })
  
  output$code <- renderText({
    set.seed(input$seed)
    seeds <- (input$seed):((input$seed) + 9)
    
    code <- paste(
      "# This code will generate a similar data set, but results may not be identical due to random seed generation.\n",
      "library(tidyverse)\n\n",
      
      "# Define the skewed function\n",
      "skewed <- function(n, skew = 'right', shape1, shape2, seed) {\n",
      "  set.seed(seed)\n",
      "  if(skew == 'right') {\n",
      "    skewed <- rbeta(n, shape1, shape2)\n",
      "  } else if (skew == 'left') {\n",
      "    skewed <- rbeta(n, shape2, shape1)\n",
      "  }\n",
      "  skewed_scaled <- 1 + 4 * skewed\n",
      "  return(round(skewed_scaled))\n",
      "}\n\n",
      
      "seeds <- c(", paste(seeds, collapse = ", "), ")\n",
      "n <- ", input$n, "\n",
      "shape1 <- ", input$shape1, "\n",
      "shape2 <- ", input$shape2, "\n",
      
      "df <- data.frame(matrix(ncol = length(seeds), nrow = n))\n",
      "for (i in 1:length(seeds)) {\n",
      "  skew_direction <- ifelse(i %% 2 == 0, 'right', 'left')\n",
      "  df[,i] <- skewed(n, skew_direction, shape1, shape2, seeds[i])\n",
      "  names(df)[i] <- paste('q', i, sep = '_')\n",
      "}\n",
      sep = ""
    )
    
    return(code)
  })
}

shinyApp(ui = ui, server = server)
