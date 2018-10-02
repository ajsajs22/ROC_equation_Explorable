
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)


shinyUI(fluidPage(

# Application title
titlePanel("Visulising the Binormal (Parametric) ROC Curve Model"),

# Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      tags$head(includeCSS('www/style.css')),
      
   
      sliderInput("alpha",
                  "Alpha ('intercept')",
                  min = -1,
                  max = 6,
                  value = 1, step=0.2,
                  
                  animate = TRUE, 
                  animationOptions(interval = 100, playButton = TRUE, 
                                   pauseButton = TRUE)
                  
                  ),
      
      sliderInput("beta",
                  "Beta ('slope')",
                  min = -1,
                  max = 5,
                  value = 1, step=0.2,
                  
                  animate = TRUE, 
                  animationOptions(interval = 100, playButton = TRUE, 
                                   pauseButton = TRUE)),
      
      "Important! The below sliders are not interactive (i.e. you cant use them), but they move when alpha and beta are
       moved above.",
      
      br(), br(),
      
      
      sliderInput("mean_healthy",
                  "Mean test value for healthy:",
                  min = 0,
                  max = 7,
                  value = 5, step=0.5),
      
      sliderInput("sd_healthy",
                  "Standard deviation of test values for healthy:",
                  min = 0.1,
                  max = 5,
                  value = 1, step=0.2),
  
      
      sliderInput("mean_diseased",
                  "Mean test value for diseased: fixed at 6",
                  min = 6,
                  max = 6,
                  value = 6, step=0.000002),


      sliderInput("sd_diseased",
                  "Standard deviation of test values for diseased: Fixed at 1",
                  min = 1,
                  max = 1,
                  value = 1, step=0.000002)

          ),


 


  mainPanel(
                 fluidRow(
                  column(10, offset=1,
                         
                         "(Note, this 'sketch' is a little buggy, if you find the page constantly updating without your input, please refresh.)",
                         br(),br(),
                   
                        "The purpose of this small piece is to allow the user to understand 
                        the standard parametisation of a binormal ROC curve with 'intercept' (alpha) and 'slope' (beta) parameters." ,
                        br(),br(),
                        
                        "The parameterisation is given by:", br(), 
                        
                        withMathJax(),
                        
                        
                        helpText("$$ROC(threshold.t) = \\Phi(alpha + beta\\Phi^{-1}(threshold.t))$$"),
                        
                        
                        "Where", br(),
                        helpText("$$alpha = (test.result.in.diseased - test.result.in.healthy) / s.d.(test.result.in.diseased)$$"),
                        helpText("$$beta =  s.d.(test.result.in.healthy) / s.d.(test.result.in.diseased)$$"),
                       # br(),
                        
                        "While the underlying distributions for a given ROC curve of this type are not unique,
                        the particular distributions drawn are derived by fixing the mean and 
                        standard deviation in the diseased group at 6 and 1 respectively."
                        ))
                 ,
                        
                        fluidRow (
                         column(6,
                                 plotOutput("ROCplot2")
                          ) 
                         ,
                          column(6, 
                                 plotOutput("overlay_threshold")
                         )
                        )
                        
                 , hr(),
                 
                 fluidRow(
                   "The below are the expressions for sensitivity and specificity from which the curve is derived
                     (re-arrange both expressions in terms of threshold and put them equal to each other). ",
                   
                   br(),
                   
                   helpText("$$test.result.in.diseased \\sim Normal(mean.diseased, sd.diseased^2)$$"),
                   helpText("$$test.result.in.healthy \\sim Normal(mean.healthy, sd.healthy^2)$$"),
                   "For each threshold",
                   br(),
                   
                   helpText("$$sensitivity(threshold.t) = P(test.result.in.diseased > threshold.t)$$"),
                   helpText("$$= \\Phi \\left(\\frac{mean.diseased - threshold.t}{sd.diseased} \\right)$$"),
                   
                   helpText("$$1 - specificity(threshold.t) = P(test.result.in.healthy > threshold.t)$$"),
                   helpText("$$= \\Phi \\left(\\frac{mean.healthy - threshold.t}{sd.healthy}\\right)$$")
                   
                 )
                 )
                 
                 )
        
       
)

)

