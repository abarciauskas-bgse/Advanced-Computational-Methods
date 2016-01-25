library(shiny)
# loading in required packages
if (!require("mvtnorm")) install.packages("mvtnorm"); library(mvtnorm)
if (!require("ggplot2")) install.packages("ggplot2"); library(ggplot2)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
  # Expression that generates a histogram. The expression is
  # wrapped in a call to renderPlot to indicate that:
  #
  #  1) It is "reactive" and therefore should re-execute automatically
  #     when inputs change
  #  2) Its output type is a plot
  
  output$distPlot <- renderPlot({
    # create small wrapper functions
    sigmaXY <- function(rho, sdX, sdY) {
      covTerm <- rho * sdX * sdY
      VCmatrix <- matrix(c(sdX^2, covTerm, covTerm, sdY^2), 
                         2, 2, byrow = TRUE)
      return(VCmatrix)
    }
    
    genBVN <- function(n = 1, seed = NA, muXY=c(0,1), sigmaXY=diag(2)) {
      if(!is.na(seed)) set.seed(seed)
      rdraws <- rmvnorm(n, mean = muXY, sigma = sigmaXY)
      return(rdraws)
    }
    
    # creating a function for all of this
    loanData <- function(noApproved, noDenied,
                         muApproved, muDenied,
                         sdApproved, sdDenied,
                         rhoApproved, rhoDenied, seed=1111) {
      sigmaApproved <- sigmaXY(rho=rhoApproved, sdX=sdApproved[1], sdY=sdApproved[2])
      sigmaDenied <- sigmaXY(rho=rhoDenied, sdX=sdDenied[1], sdY=sdDenied[2])
      approved <- genBVN(noApproved, muApproved, sigmaApproved, seed = seed)
      denied <- genBVN(noDenied, muDenied, sigmaDenied, seed = seed+1)
      loanDf <- as.data.frame(rbind(approved,denied))
      deny <- c(rep("Approved", noApproved), rep("Denied", noDenied))
      target = c(rep(0, noApproved), rep(1, noDenied))
      loanDf <- data.frame(loanDf, deny, target)
      colnames(loanDf) <- c("PIratio", "solvency", "deny", "target")
      return(loanDf)
    }
    
    # generating some data
    loanDf <- loanData(noApproved=50, noDenied=50,
                       muApproved=c(input$mean.solvency.approved, input$mean.piratio.approved),
                       muDenied=c(input$mean.solvency.denied, input$mean.piratio.denied),
                       sdApproved=c(input$sd.solvency.approved, input$sd.piratio.approved),
                       sdDenied=c(input$sd.solvency.denied, input$sd.piratio.denied),
                       rhoApproved=input$rho.approved, rhoDenied=input$rho.denied)

    datafit <- lm(target ~ solvency + PIratio + 1, data=loanDf)

    # grabbing the coefficients
    weights <- coef(datafit)[c("solvency", "PIratio")]
    bias <- coef(datafit)[1]
    
    sum(predict(datafit)>0.5)
    
    # Computing the boundary: since it is a 2-dimensional example the boundary 
    # is a line. 
    intercept <- (-bias + 0.5)/weights["PIratio"]
    slope <- -(weights["solvency"]/weights["PIratio"])
    
    # when plotting, a more general solution is to use geom_line()
    x <- seq(min(loanDf["PIratio"]), max(loanDf["PIratio"]), 
             length.out = nrow(loanDf))
    y <- -(weights["PIratio"]/weights["solvency"])*x + 
      (0.5-bias)/weights["solvency"]
    
    # careful, colnames have to match!
    boundaryDf <- data.frame(PIratio=x, solvency=y, 
                             deny=rep("Boundary", length(x)))
    
    # now plotting again, but with geom_line(), and we create a plot function, 
    plotDiscFnc <- function() {
      ggplot(data = loanDf, 
             aes(x = solvency, y = PIratio, colour=deny)) + 
        geom_point() +
        xlab("solvency") +
        ylab("PI ratio") +
        theme_bw() + 
        geom_line(data=boundaryDf) + 
        scale_color_manual("", 
                           values = c("Boundary" = "grey", 
                                      "Approved" = "blue", "Denied" = "red"))
    }
    plotDiscFnc()
  })
})
