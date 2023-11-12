library(shiny)
library(ggplot2)
library(readr)


# Read CSV from GitHub or local path
beer_data <- read.csv("beerapp/merged_file.csv")

# Sort the state data alphabetically
sorted_states <- c("All States", sort(unique(beer_data$State)))

ui <- fluidPage(
  titlePanel("Beer Data Analysis"),
  sidebarLayout(
    sidebarPanel(
      # Remove fileInput and actionButton
      radioButtons("plot_type", "Select Plot Type", choices = c("Histogram", "Boxplot", "Scatterplot")),
      br(),
      selectInput("state_filter", "Filter by State", choices = sorted_states),
      br(),
      checkboxInput("add_regression", "Add Linear Regression Line"),
    ),
    mainPanel(
      plotOutput("data_plot")
    )
  )
)

server <- function(input, output) {
  output$data_plot <- renderPlot({
    plot_data <- beer_data
    
    # Filter data based on the selected state
    if (input$state_filter != "All States") {
      plot_data <- plot_data[plot_data$State == input$state_filter, ]
    }
    
    p <- ggplot()  # Initialize ggplot object
    
    if (input$plot_type == "Histogram") {
      p <- p + geom_histogram(data = plot_data, aes(x = IBU), binwidth = 5, fill = "blue", color = "black") +
        labs(title = "IBU Distribution", x = "IBU", y = "Frequency")
    } else if (input$plot_type == "Boxplot") {
      p <- p + geom_boxplot(data = plot_data, aes(x = State, y = IBU), fill = "blue", color = "black") +
        labs(title = "IBU Boxplot by State", x = "State", y = "IBU")
    } else if (input$plot_type == "Scatterplot") {
      p <- p + geom_point(data = plot_data, aes(x = ABV, y = IBU), color = "blue") +
        labs(title = "Scatterplot of ABV vs. IBU", x = "ABV", y = "IBU")
    }
    
    if (input$add_regression) {
      p <- p + geom_smooth(data = plot_data,aes(x = ABV, y = IBU), method = "lm", color = "red")
    }
    
    print(p)  # Print the ggplot object
  })
}

shinyApp(ui = ui, server = server)
