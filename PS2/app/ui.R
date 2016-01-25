library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Loan Data"),

  fluidRow(
    # Approved column
    column(2,
      sliderInput("mean.solvency.approved",
                  "Mean Solvency Approved",
                  min = 1,
                  max = 200,
                  value = 100),
      sliderInput("mean.piratio.approved",
                  "Mean PI Ratio Approved",
                  min = 1,
                  max = 15,
                  value = 5),
      sliderInput("sd.solvency.approved",
                  "Std Dev Solvency Approved",
                  min = 1,
                  max = 20,
                  value = 2),
      sliderInput("sd.piratio.approved",
                  "Std Dev PI Ratio Approved",
                  min = 1,
                  max = 20,
                  value = 2),
      sliderInput("rho.approved",
                  "Correlation Approvals",
                  min = -1,
                  max = 1,
                  value = -0.3,
                  step = 0.1)
    ),

    column(2,
           sliderInput("mean.solvency.denied",
                       "Mean Solvency Denials",
                       min = 1,
                       max = 200,
                       value = 100),
           sliderInput("mean.piratio.denied",
                       "Mean PI Ratio Denials",
                       min = 1,
                       max = 15,
                       value = 5),
           sliderInput("sd.solvency.denied",
                       "Std Dev Solvency Denials",
                       min = 1,
                       max = 20,
                       value = 2),
           sliderInput("sd.piratio.denied",
                       "Std Dev PI Ratio Denials",
                       min = 1,
                       max = 20,
                       value = 2),
           sliderInput("rho.denied",
                       "Correlation Denials",
                       min = -1,
                       max = 1,
                       value = 0.8,
                       step = 0.1)
    ),

    # Plot in the rest of the space
    column(8,
      plotOutput("distPlot"),
      dataTableOutput("confMatrix")
    )
  )
))

