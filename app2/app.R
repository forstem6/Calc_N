#
# This Shiny app calculates required sample sizes for surveys.
# The user can specify the parameters for the calculation and can also specify 
# a self-defined sample size.
#
# The app then shows a table for predefined class sizes and the needed N and % 
# based on the selected parameters. On the right the tabular data is show graphically.
#

# Required packages
library(shiny)
library(shinyjs)  # To make the percentage of 1,2,3 dependent on the percentage of 4,5
library(tidyverse)
library(ggrepel)  # To display the labels in the graphs nicely.

# Define UI
ui <- fluidPage(
  
  # First we need to "activate" shinyjs.
  useShinyjs(),
  
  # Application title
  titlePanel("Berechnung benötigter Stichprobenumfänge"),
  
  fluidRow(
    
    # Sidebar with the parameters
    column(2,
           wellPanel(
             # Percentage of responses 4 and 5
             numericInput("res45",
                          "Percentage of Responses '4' and '5':",
                          min = 0,
                          max = 100,
                          value = 50),
             
             # Percentage of responses 1, 2 and 3 (locked as it is dependent on the previous input)
             numericInput("res123",
                          "Percentage of Responses '1', '2', and '3':",
                          NULL),
             
             # Sample error
             numericInput("samperr",
                          "Sampling Error (in %):",
                          min = 0,
                          max = 100,
                          value = 10,
                          step = 5),
             
             # Confidence Interval 
             numericInput("CI",
                          "Confidence Interval (in %)",
                          min = 0,
                          max = 100,
                          value = 95,
                          step = 5),
             
             # Optional custom N
             numericInput("cust_N",
                          "Custom N (shown in plots):",
                          min = 1,
                          max = 1000,
                          value = 100)
           )
    ),
    
    # Tabular results output
    column(3,
           tableOutput("n_table")
    ),
    
    # Plots
    column(7,
           plotOutput("perc_plot"),
           plotOutput("n_plot")
    )
  )
)

# Function for calculating the needed sample sizes
n.needed.tab <- function(popsize, prop, samp.err, alpha) {
  
  # Calculate the z-score of the alpha, based on the confidence interval
  z.alpha <- qnorm(p=alpha/2, lower.tail=FALSE)
  
  # Calculate the needed n with the formula.
  n_needed <- (popsize * prop[1] * prop[2]) / ((popsize-1) * (samp.err/z.alpha)^2 + prop[1] * prop[2])
  
  # Create a df with the tabular data, class size, needed N, and percentage of needed N per class.
  table_n <- data.frame(Class_Size = popsize, 
                        Needed_N = ceiling(n_needed), 
                        Needed_Rate = ceiling((ceiling(n_needed)/popsize)*100))
  
  return(table_n)
  
}

# Predefined ("hardcoded") class sizes (ofc this could be made a parameter, but it might take a while to list all the values in a field,)
Popsizes <- c(5, 10, 15, 20, 25, 30, 40, 50, 60, 70, 80, 100, 120, 150, 180, 200, 220)


# Define server logic required to draw a histogram
server <- function(input, output) {
  # This automatically adapts the percentage of 1,2, and 3 depending on the input in Percentage 4,5
  observeEvent(input$res45,{
    updateNumericInput(inputId = "res123", value = (100-input$res45))
    disable("res123")
  })
  
  # Transform the user input for proportion and CI to the values needed for the function
  prop <-  reactive(c(input$res45/100, 1-(input$res45/100)))
  alpha <- reactive(1-(input$CI/100))
  
  popsiz <- reactive({
    if (input$cust_N %in% Popsizes) {
      popsiz <- Popsizes
    } else {
      popsiz <- append(Popsizes, input$cust_N)
      popsiz <- sort(popsiz)
    }
    popsiz
    print(popsiz)
  })
  
  print(popsiz)
  
  # Calculate the table with the needed ns here.
  n_calc <- reactive({
    n_s <- n.needed.tab(popsize = popsiz(), 
                        prop = prop(), 
                        samp.err = input$samperr/100, 
                        alpha = alpha()
    )
    return(n_s)
  })
  
  # Render the table, with displaying the percentages with "%", striped table, values aligned centrally,     no decimal places;  
  output$n_table <- renderTable({
    tab <- n_calc()
    tab$Needed_Rate <- paste0(tab$Needed_Rate, "%") 
    tab
  },
  striped = TRUE,
  digits = 0,
  align = "c"
  )
  
  # Create plot with N needed by class size, labels are percentages; Custom N is shown as a vertical line;
  plot_N <- reactive({
    res_plot <- n_calc() |>
      ggplot(aes(x = Class_Size, y = Needed_N, label = paste0(Needed_Rate, "%"))) +
      geom_line() +
      geom_point() +
      geom_vline(xintercept = input$cust_N, color = "antiquewhite3") +
      geom_label_repel(position = position_nudge_repel(x = 1, y = -2)) +
      scale_x_continuous(breaks = popsiz()) +
      scale_y_continuous(breaks = as.data.frame(n_calc())$Needed_N) +
      labs(x= "Anzahl der Studierenden in der LV",
           y = "benötigte Studierende bei gewählten Parametern")
    return(res_plot)
  })
  
  # Create plot with rate of N by class size, labels are needed N; Custom N is shown as a vertical line;
  plot_perc <- reactive({
    ggplot(n_calc(), aes(x = Class_Size, y = Needed_Rate, label = Needed_N)) + 
      geom_line() + 
      geom_point() +
      geom_vline(xintercept = input$cust_N, color = "antiquewhite3") +
      geom_label_repel(position = position_nudge_repel(x = 1, y = -2)) + 
      scale_x_continuous(breaks = popsiz()) +
      scale_y_continuous(breaks = as.data.frame(n_calc())$Needed_Rate) +
      labs(x = "Anzahl der Studierenden in der LV",
           y = "Prozent der Studierenden die mitmachen müssen (bei gewählten Parametern)")
  })
  
  # Render the plots
  output$n_plot <- 
    renderPlot({
      plot_N()
      
    })
  output$perc_plot <- 
    renderPlot({
      plot_perc()
      
    })
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)
