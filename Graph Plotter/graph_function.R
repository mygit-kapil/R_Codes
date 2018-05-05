graph <- function(file)
{
  graph_type <- readline(prompt="Enter the type of graph (line/bar/pie/ggplot):  ")
  
  if(graph_type == "line")
  {
    xline <- readline(prompt="Enter the x co-ordinate: ")
    x <- nrow(file)
    x <- as.integer(x)
    DF <- data.frame(A=numeric(x), B=numeric(x))
    DF$A <- file[[xline]]
    yline <- readline(prompt="Enter the y co-ordinate: ")
    DF$B <- file[[yline]]
    DF <- DF[order(DF$A),]
    mean_input <- readline(prompt="Do you want the mean value to be shown in graph? (y/n): ")
    if(mean_input == "y")
    {
      plot(DF$A, DF$B, col= "blue", xlab = xline, ylab = yline, type = 'l')
      textxy(DF$A, DF$B, DF$A)
      abline(v=mean(DF$A), col= "red")
      abline(v=median(DF$A), col="green")
      
    }
    
    else
    {
      plot(DF$A, DF$B, col= "blue", xlab = xline, ylab = yline, type = 'l')
      
    }
    
    
  }
  
  else if(graph_type == "bar")
  {
    xbar <- readline(prompt="Enter the x co-ordinate: ")
    ybar <- readline(prompt="Enter the y co-ordinate: ")
    row_cnt <- nrow(file)
    row_cnt <- as.integer(row_cnt)
    row_cnt <- c(0, row_cnt)
    col_lar <- max(file[[ybar]], na.rm = TRUE)
    col_lar <- as.integer(col_lar)
    col_lar <- c(0, col_lar)
    counts <- file[[ybar]]
    M <- file[[xbar]]
    label <- readline(prompt="Do you wish to change the labels and add a title? :(y/n)")
    if(label=="y")
    {
      x_lab <- readline(prompt="Enter the x-axis label :")
      y_lab <- readline(prompt="Enter the y-axis label :")
      title <- readline(prompt="Enter the title of the graph :")
      barplot(counts, names.arg=M, main=title, xlab=x_lab, ylab=y_lab, col="blue", xlim = row_cnt, ylim = col_lar)
    }
    else
    {
      barplot(counts, names.arg=M, xlab=xbar, ylab=ybar, col="blue", xlim = row_cnt, ylim = col_lar)
    }  
  }
  
  else if(graph_type == "pie")
  { 
    library(plotrix)
    xpie <- readline(prompt="Enter the quantity column: ")
    ypie <- readline(prompt="Enter the label column: ")
    table <- file[[xpie]]
    leg <- file[[ypie]]
    print(leg)
    piepercent<- round(100*table/sum(table), 1)
    pie(table, labels =piepercent, col = rainbow(length(table)))
    legend("topright", legend= leg, cex = 0.8,
           fill = rainbow(length(table)))
  }  
  
  else if(graph_type == "ggplot")
  {
    source("ggplot graph.R")
    ggplot_graphs(file)
  }
  
  else
  {
    print("Invalid Input")
    graph(file)
  }
  
}
