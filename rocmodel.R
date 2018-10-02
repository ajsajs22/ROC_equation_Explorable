

ALPHA1 <- 0.3
BETA1 <-1 
seq_ROC2<-seq(0,1,0.02)

roc.calc1 <-  (ALPHA1 + BETA1 * qnorm(seq_ROC2))
roc.calc2 <- pnorm(roc.calc1)

print(seq_ROC2)
print(roc.calc1)
print(roc.calc2)

cdf_healthy<-pnorm(seq_ROC2, input$mean_healthy,input$sd_healthy)
cdf_diseased<-pnorm(seq_ROC2, input$mean_diseased,input$sd_diseased)



plot((seq_ROC2), (roc.calc2), col="black", xlab="", 
     type="l",lwd=4)

