library(shiny)
library(ggplot2)
library(randomForest)
library(shinythemes)
library(bslib)
library(shinyjs)
library(DT)

# Load your data and model from "project.R"
source("project.R")

# Define the UI with the "Flatly" theme
ui <- fluidPage(
  theme = bslib::bs_theme(version = 5, bootswatch = "flatly"),
  useShinyjs(),
  tags$head(
    tags$style(
      HTML(
        "
    body {        
          background-image: url('data:image/jpeg;base64,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');
          background-size: cover;
          background-repeat: no-repeat;
          background-position: center;
        }
        .navbar-default {
          background-color: #2c3e50;
          border-color: #2c3e50;
        }
        .navbar-default .navbar-brand {
          color: #ecf0f1;
        }
        .navbar-default .navbar-nav>li>a {
          color: #ecf0f1;
        }
        .navbar-default .navbar-nav>li>a:hover,
        .navbar-default .navbar-nav>li>a:focus {
          color: #ecf0f1;
          background-color: #34495e;
        }
        .navbar-default .navbar-toggle {
          border-color: #34495e;
        }
        .navbar-default .navbar-toggle:hover,
        .navbar-default .navbar-toggle:focus {
          background-color: #34495e;
        }
        .well {
          background-color: #34495e;
          border: 2px solid #2c3e50;
          border-radius: 10px;
          padding: 15px;
          color: #ecf0f1;
          font-family: 'Segoe UI', Tahoma, Geneva, Verdana, sans-serif;
        }
        .result-box {
          background-color: #ecf0f1;
          border: 2px solid #bdc3c7;
          border-radius: 10px;
          padding: 15px;
          margin-top: 20px;
        }
        .prediction-box {
          background-color: #ecf0f1;
          border: 2px solid #2980b9;
          border-radius: 10px;
          padding: 15px;
          color: #ecf0f1;
          font-size: 30px !important;
          font-weight: bold;
        }
        .prediction-box .shiny-text-output {
          font-size: 30px !important;
          font-weight: bold;
          color: #3498db;
        }
        .dataset-panel {
          background-color: #ecf0f1;
          border: 2px solid #bdc3c7;
          border-radius: 10px;
          padding: 15px;
          margin-top: 20px;
        }
        "
      )
    )
  ),
  navbarPage(
    title = "Random Forest Model with Shiny",
    tabPanel(
      "Plot",
      sidebarLayout(
        sidebarPanel(
          selectInput("plot_type", "Plot Type:", choices = c("Scatter Plot", "Bar Chart")),
          selectInput("x_axis", "X-Axis:", ""),
          selectInput("y_axis", "Y-Axis:", ""),
          actionButton("generate_plot", "Generate Plot", class = "btn-primary")
        ),
        mainPanel(
          div(
            class = "result-box",
            h2("Plot Results:"),
            plotOutput("generated_plot")
          )
        )
      )
    ),
    tabPanel(
      "Prediction Model",
      sidebarLayout(
        sidebarPanel(
          wellPanel(
            numericInput("age", HTML("Age: "), min = 18, max = 78, value = 40),
            numericInput(
              "category",
              HTML("Category (1=Blouse, 2=Bottom, 3=Kurta, 4=Saree, 5=Set, 6=Top, 7=Western, 8=Ethnic Dress):"),
              min = 1,
              max = 8,
              value = 1
            ),
            numericInput("size", HTML("Size (1=Free, 2=S, 3=M, 4=XL, 5=XXL, 6=L, 7=XS):"), min = 1, max = 7, value = 1),
            numericInput("gender", HTML("Gender (1=Male, 2=Female):"), min = 1, max = 2, value = 1),
            numericInput(
              "channel",
              HTML("Channel (1=Myntra, 2=Ajio, 3=Amazon, 4=Meesho, 5=Flipkart, 6=Nalli, 7=Others):"),
              min = 1,
              max = 7,
              value = 1
            )
          ),
          actionButton("predict_button", "Predict", class = "btn-primary")
        ),
        mainPanel(
          div(
            class = "result-box",
            h2("Random Forest Model Results:"),
          ),
          br(),
          br(),
          div(
            class = "prediction-box",
            verbatimTextOutput("prediction_result")
          )
        )
      )
    ),
    tabPanel(
      "Dataset",
      fluidRow(
        div(
          DTOutput("data_table"),
          class = "dataset-panel",
          style = "width: 100%;"
        )
      )
    )
  )
)

# Define the server logic (no changes)
server <- function(input, output, session) {
  observe({
    if (input$plot_type == "Scatter Plot" || input$plot_type == "Bar Chart") {
      updateSelectInput(session, "x_axis", choices = colnames(data))
      updateSelectInput(session, "y_axis", choices = colnames(data))
    }
  })
  
  observeEvent(input$generate_plot, {
    if (input$plot_type == "Scatter Plot") {
      output$generated_plot <- renderPlot({
        ggplot(data, aes_string(x = input$x_axis, y = input$y_axis)) +
          geom_point() +
          labs(title = paste("Scatter Plot of", input$x_axis, "vs.", input$y_axis))
      })
    } else if (input$plot_type == "Bar Chart") {
      output$generated_plot <- renderPlot({
        ggplot(data, aes_string(x = input$x_axis, y = input$y_axis)) +
          geom_bar(stat = "identity", fill = "orange") +
          labs(title = paste("Bar Chart of", input$x_axis, "vs.", input$y_axis))
      })
    }
  })
  
  # Render dataset table
  output$data_table <- renderDT({
    datatable(data)
  })
  observeEvent(input$predict_button, {
    isolate({
      if (!is.null(input$age) && !is.null(input$category) && !is.null(input$size) && !is.null(input$gender) && !is.null(input$channel)) {
        new_data <- data.frame(Age = input$age, Category = input$category, Size = input$size, Gender = input$gender, Channel = input$channel)
        
        predictions <- predict(rf_model, new_data)
        prediction <- predictions[1]
        output$prediction_result <- renderText({
          paste("Predicted Amount:", round(as.numeric(prediction), 2))
        })
        shinyjs::runjs("$('.shiny-text-output').addClass('prediction-box');")
      } else {
        output$prediction_result <- renderText({
          "Please enter Age, Category, Size, Gender, and Channel to predict the Amount."
        })
      }
    })
  })
}

shinyApp(ui, server)