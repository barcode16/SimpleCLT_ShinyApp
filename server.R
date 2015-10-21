# Derived from https://github.com/tgouhier/climit

library(shiny)
library(ggplot2)
library(dplyr)


shinyServer(function(input, output) {
  
# Plot 1 - Sampling Distribution Using Theory
  
  output$binom.dist = renderPlot({
    
  
  binomial.histogram <- function(n, p = 0.5, col = "white") { 
    bin.min <- 0 
    bin.max <- n 
    bin.range <- bin.min : bin.max 
    my.binomial <- NULL 
    my.binomial$breaks <- (bin.min - 0.5) : (bin.max + 0.5) 
    my.binomial$counts <- dbinom(bin.range, bin.max, prob) 
    attr(my.binomial, "class") <- "histogram" 
    col.border <- col 
    col.border["white" == col] <- "black" 
    plot(my.binomial, border = col.border, col = col, main = 
           paste("Histogram of Binomial (n = ", n, ", p = ", prob, ")", sep = ""), 
         xlab = "k", ylab = "P[X = k]") 
    return(my.binomial) 
  } # end function binomial.histogram 
  
  
  # Example of calling "binomial.histogram" function. 
  n <- input$n
  prob <- input$p/100 
  my.colors <- c("paleturquoise", "sky blue") 
  my.binomial <- binomial.histogram(n, p, my.colors)
  
  
  }) 

  

   # plot 2 - Sampling Distribution By Simulation
  
  output$sampling.dist = renderPlot({
    
    n = input$n
    p = input$p/100
    k = 400
    
    set.seed(4444)
    
    dist <- rbinom(k, n, prob = p)
    dist <- data.frame(dist)
    dist$dist <- dist$dist/n*100
    dist <- arrange(dist, dist)
    dist95 <- dist[((k*0.025)+1):(k*0.975), ]
    dist_out <- (filter(dist, !(dist %in% dist95)))
    dist_out <- unlist(dist_out$dist)
    mindist <- floor(min(dist$dist))
    maxdist <- floor(max(dist$dist))
    mybreaks <- as.integer(maxdist-mindist)
    myxlim <- c(mindist-1, maxdist+1)
    
    dist$highlight <- ifelse(dist$dist %in% dist_out, "highlight", "normal")
    mycolours <- c("highlight" = "red", "normal" = "grey50")
    
    mind <- min(dist95)
    maxd <- max(dist95)
    m <- mean(dist$dist)
    

g1<- ggplot(dist, aes(x = dist)) + 
  geom_histogram(binwidth = mybreaks/14, aes(fill=highlight)) +
  xlim(myxlim)+
  theme_bw() 
 print(g1)
   

  })
  
})