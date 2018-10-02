
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)

# options(shiny.error = browser)

 # The smalller the resolution the higher the number of plotting points but 
 # the slower the app will be to update 

THRESHOLD <- 20
  resolution <- 0.05

 # Defining the colour scheme used in the app here to make it easy to change/update

## Colour schemes for 3 transparent colours seems easy, but four needs more research!
  
  col_healthy <- rgb(0,100,0,127, maxColorValue = 255)
  col_diseased <- rgb(139,0,0,127, maxColorValue = 255)
  
  FN_col <- rgb(166, 97, 26, 127, maxColorValue=255) 
  TP_col <- rgb(223, 194, 125, 127, maxColorValue=255)
  
  TN_col <- rgb(166, 217, 106, 127, maxColorValue=255)
  FP_col <- rgb(26, 150, 65, 127, maxColorValue=255)
 
   trans <- rgb(255,255,255,255, maxColorValue=255)
  col_bar<-c(trans,trans,trans,trans)
 


  shinyServer(function(input, output, session) {
  
    
    
    ####### Surpressing warnings
    
    
   # storeWarn<- getOption("warn")
   # options(warn = -1) 
    
    
 
    
    
  ########################################################
  #### MODULARISING REACTIVITY TO IMPROVE PERFORMANCE ####
  ########################################################
  
  ## Not sure this is helping speed up as no task done below, 
  ## but principle is OK
  
  
    ALPHA1 <- reactive({input$alpha})
    BETA1  <- reactive({input$beta})
    

   
 
  ##############################################################
  ##### PLOT OF TEST DISTRIBUTIONS FOR HEALTHY AND DISEASED ####
  ##############################################################
   
   
   ###### Below is the 2-way reactivity so if you move the distribution of the diseased
   ####### alpha and beta update and vice versa - but problems with constantly iterating 
   ### between values when moved. Putting in isolate clauses fixes most but not quite all
   #### of it.

   
  isolate(
  observe({
    if(input$mean_healthy){
      updateSliderInput(session, "mean_healthy", 
                        value = input$mean_healthy)
      updateSliderInput(session, "alpha", 
                        value = round(((6 - input$mean_healthy)), 
                                      digits =2))
      cat(file=stderr(), "when mean health updated to", input$mean_healthy, " ")
      cat(file=stderr(), "alpha updated to", input$alpha, " ")
      cat(file=stderr(), "")
    } 
  }
  ))


isolate(
  observe({
    if(input$sd_healthy){
      updateSliderInput(session, "sd_healthy", 
                        value = input$sd_healthy)
      updateSliderInput(session, "beta", 
                        value = signif((input$sd_healthy), digits =2))
      cat(file=stderr(), "when sd healthy updated to", input$sd_healthy, "")
      cat(file=stderr(), "beta updated to", input$beta,"")
      cat(file=stderr(), "")
    } 
  }
  )  )

  isolate(
 observe({
  if(input$alpha){
    updateSliderInput(session, "alpha", 
                      value = input$alpha)
    updateSliderInput(session, "mean_healthy", 
                      value = 6  - input$alpha) 
    cat(file=stderr(), "when alpha updated to", input$alpha, "")
    cat(file=stderr(), "mean healthy updated to", input$mean_healthy, "")
    cat(file=stderr(), "")
  } 
}
))

 isolate(
  observe({
    if(input$beta){
      updateSliderInput(session, "beta", 
                        value = input$beta)
      updateSliderInput(session, "sd_healthy", 
                        value = signif(( input$beta ) , digits = 2))
      cat(file=stderr(), "when beta updated to", input$beta, "")
      cat(file=stderr(), "sd healthy updated to", input$sd_healthy, "")
      cat(file=stderr(), "")
    } 
  }
  ))


   
  output$overlay_threshold <- renderPlot({
       

    
  # calculating the range for plot to fit both distributions fully on
    
    xmin1<- c(input$mean_healthy-(5*input$sd_healthy), input$mean_diseased-(5*input$sd_diseased))
    xmin2 <-min(xmin1,-6)
    
    xmax1<- c(input$mean_healthy+(5*input$sd_healthy), input$mean_diseased+(5*input$sd_diseased))
    xmax2 <-max(xmax1,10)
    
   
    
  ## HEALTHY PATIENT DISTRIBUTION
    seq_healthy<-seq(xmin2,xmax2,resolution)
    densities_healthy<-dnorm(seq_healthy, input$mean_healthy,input$sd_healthy)
    
  #Calculating PDF plot values split at threshold for healthy
   
    

    
 ## DISEASED PATIENT DISTRIBUTION
    
    seq_diseased<-seq(xmin2,xmax2,resolution)
    densities_diseased<-dnorm(seq_diseased, input$mean_diseased,input$sd_diseased)
    
 
    
    ## Working out the max height of the 2 densities to set plot limit
    
 seq_c <- c(densities_healthy, densities_diseased)
   ymax <- max(seq_c)
    
    ## Plotting PDF outlines for the diseased and healthy populations   
    
    plot(seq_healthy, densities_healthy, col=col_healthy,xlab="", ylab="", 
         type="l",lwd=4, cex=2, main="Distribution of test values in diseased and healthy", 
        cex.axis=.8, cex.main=0.9, ylim = c(0, (ymax+ymax/5)), xlim=c(-8, 16))
    
   title(ylab="Density", mgp=c(2,1,0),cex.lab=1)
  title(xlab="Test Response", mgp=c(2,1,0),cex.lab=1)
    
    lines(seq_diseased, densities_diseased, col=col_diseased, 
         type="l",lwd=4)
   
    
   col_legend <- c(col_healthy, col_diseased)
    leg_txt <- c("Healthy", "Diseased")
   y_place <- ymax + (ymax/4.5)
   x_place <- -5
    
    legend(x=x_place, y=y_place, leg_txt, lty=c(1,1), lwd=c(4,4), col=col_legend, cex=0.75, box.col=NA)
    
    
  })
  
    
    
    
    #############################################################
    ### ROC PLOT 2 Plotting binomial threshold plot ###
    #############################################################
    
    output$ROCplot2 <- renderPlot({
      
      widthr  <- session$clientData$output_ROCplot2_width
      cex.w <- widthr/375
      
          
          ## HEALTHY PATIENT CUMULATIVE DISTRIBUTION
          
       # Hard wiring range of thresholds ROC calculated for is 0 to 60
         seq_ROC2<-seq(0,1, 0.01)
          
          
          roc.calc1 <-  (ALPHA1() + BETA1() * qnorm(seq_ROC2))
          roc.calc2 <- pnorm(roc.calc1)
          
       cdf_healthy<-pnorm(seq_ROC2, input$mean_healthy,input$sd_healthy)
          cdf_diseased<-pnorm(seq_ROC2, input$mean_diseased,input$sd_diseased)
          

          plot((1-seq_ROC2), (roc.calc2), col="black", ylab="", xlab="", 
               type="l",lwd=4, cex=1.3*cex.w, main="Receiver Operating Characteristic (ROC) Plot",
               cex.main=.9*cex.w, cex.axis=.8*cex.w,  xlim=c(1,0), ylim=c(0,1), las=1, at=c(0,.2,.4,.6,.8,1), labels=c("0","20","40","60","80","100" ))
          
          title(ylab="True Positive Fraction: Sensitivity (%)", mgp=c(2,1,0),cex.lab=1*cex.w)
          title(xlab="True Negative Fraction: Specificity (%)", mgp=c(2,1,0),cex.lab=1*cex.w)
          
          abline(a=1,b=-1, col="black", lty=2)
          
          
          sequence_n <-THRESHOLD * (1/resolution)
          
          plotminx <-max(0.1, (cdf_healthy[sequence_n+1]-.07) )
          plotminy <-max(0.1, (1-cdf_diseased[sequence_n+1]-.07) )
        
          
          text(plotminx, plotminy , label=max(roc.calc2))
          
          #### Adding estimates of tp, fp, fn, tn fractions to to ROC plot to link plots  
         
  
    },  width = function() {
    min(600, session$clientData$output_ROCplot2_width * 0.95,session$clientData$output_ROCplot2_height * 0.95 )
   },  height = function() {
     min(630,session$clientData$output_ROCplot2_width, session$clientData$output_ROCplot2_height )
      
   
      
    }) 
    
    
  
})
