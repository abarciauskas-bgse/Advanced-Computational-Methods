library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Loan Data"),

  fluidRow(
    column(4,
      sliderInput("mean.solvency.approved",
                  "Mean Solvency Approved",
                  min = 1,
                  max = 200,
                  value = 1),
      sliderInput("mean.solvency.denied",
                  "Mean Solvency Denied",
                  min = 1,
                  max = 200,
                  value = 5),
      sliderInput("mean.piratio.approved",
                  "Mean PI Ratio Approved",
                  min = 1,
                  max = 15,
                  value = 1),
      sliderInput("mean.piratio.denied",
                  "Mean PI Ratio Denied",
                  min = 1,
                  max = 15,
                  value = 5),
      sliderInput("sd.solvency.approved",
                  "Std Dev Solvency Approved",
                  min = 1,
                  max = 20,
                  value = 2),
      sliderInput("sd.solvency.denied",
                  "Std Dev Solvency Denied",
                  min = 1,
                  max = 20,
                  value = 2),
      sliderInput("sd.piratio.approved",
                  "Std Dev PI Ratio Approved",
                  min = 1,
                  max = 20,
                  value = 2),
      sliderInput("sd.piratio.denied",
                  "Std Dev PI Ratio Denied",
                  min = 1,
                  max = 20,
                  value = 2),
      numericInput("rho.approved", label = h3("Correlation Approvals"), value = -0.2),
      numericInput("rho.denied", label = h3("Correlation Denials"), value = 0.5)
    ),

    column(8,
      plotOutput("distPlot")
    )
  )
))

