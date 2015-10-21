library(shiny)
library(ggplot2)
library(dplyr)
library(ggthemes)
library(grid)

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
  
  
  # call "binomial.histogram" function. 
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
    myxlim <- c(mindist-2, maxdist+2)
    myylim <- c(0, max(table(dist$dist))*(mybreaks/14)*1.3)
    
    dist$highlight <- ifelse(dist$dist %in% dist_out, "highlight", "normal")
    mycolours <- c("highlight" = "red", "normal" = "grey50")
    
    mind <- min(dist95)
    maxd <- max(dist95)
    m <- mean(dist$dist)
    

g1<- ggplot(dist, aes(x = dist)) + 
  geom_histogram(binwidth = mybreaks/14, aes(fill=highlight), colour = "black") +
  scale_fill_manual(values = c("red", "grey50"))+
  xlim(myxlim)+
#  ylim(myylim)+
  theme_fivethirtyeight()+
#  annotate("segment", x = dist$dist[(k*0.025)+1], xend = dist$dist[(k*0.975)],
#           y = Inf, yend = Inf,
#           arrow=arrow(ends="both", angle = 90, length = unit(.2, "cm")), vjust=10)+
# annotate("text", x = (dist$dist[(k*0.025)+1]+dist$dist[(k*0.975)+1])/2, y = Inf,
#           label = paste("95% of Samples Are In The Range ", 
#                         round(dist$dist[(k*0.025)+1],1), "% : ", round(dist$dist[(k*0.975)], 1), "% (Mean = ", round(mean(dist$dist),1), "%)", sep = ""), vjust =2) + 
  annotate("segment", x = mean(dist$dist), xend =  mean(dist$dist), y = 0, 
           yend = max(table(dist$dist))*(mybreaks/14)*1.05, colour = "dodgerblue4", size = 2)+
  guides(fill=FALSE)+
  ggtitle(paste("95% of Samples Are In The Range ", 
                round(dist$dist[(k*0.025)+1],1), "% : ", round(dist$dist[(k*0.975)], 1), "% (Mean = ", round(mean(dist$dist),1), "%)", sep = ""))+
  theme(plot.title=element_text(hjust=0.5))
  print(g1)
   

  })
  
})