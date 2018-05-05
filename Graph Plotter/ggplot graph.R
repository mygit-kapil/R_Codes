ggplot_graphs <- function(file)
{
  library(ggplot2)
  library(quantreg)
  graph_type <- readline(prompt="Enter the type of ggplot graph (point/line/histogram):  ")
  if(graph_type == "point")
  {
    xline <- readline(prompt="Enter the x co-ordinate: ")
    x <- nrow(file)
    x <- as.integer(x)
    DF <- data.frame(A=numeric(x), B=numeric(x))
    DF$A <- file[[xline]]
    yline <- readline(prompt="Enter the y co-ordinate: ")
    DF$B <- file[[yline]]
    DF <- DF[order(DF$A),]
    print(DF)
    c <- ggplot(DF, aes(x=A, y=B)) + geom_point() + labs(x=xline, y=yline)
    print(c)
    #print("graph printed")
  }
  
  else if(graph_type == "line")
  {
    library(ggplot2)
    library(quantreg)
    xline <- readline(prompt="Enter the x co-ordinate: ")
    x <- nrow(file)
    x <- as.integer(x)
    DF <- data.frame(A=numeric(x), B=numeric(x))
    DF$A <- file[[xline]]
    yline <- readline(prompt="Enter the y co-ordinate: ")
    DF$B <- file[[yline]]
    DF <- DF[order(DF$A),]
    print(DF)
    c <- ggplot(DF, aes(x=A, y=B)) + geom_line() + labs(x=xline, y=yline)
    print(c)
  }
  
  else if (graph_type == "histogram")
  {
    xline <- readline(prompt="Enter the x co-ordinate: ")
    x <- nrow(file)
    x <- as.integer(x)
    DF <- data.frame(A=numeric(x))
    DF$A <- file[[xline]]
    c <- ggplot(DF, aes(x=A)) + geom_histogram() + labs(x=xline) + stat_bin(binwidth = 0.01)
    print(c)
  }
  
  else
  {
    print("Invalid Input")
    ggplot_graphs(file)
  }
  
}