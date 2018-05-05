graphical <- function()
{
  library("xlsx", lib.loc="~/R/win-library/3.3")
  library("calibrate")
  library("XLConnect")
  #library("gdata")
  library("ggplot2")
  library("tools")
  file_name <- file.choose()
  extension <- file_ext(file_name)
  print(extension)
  if(extension == "xlsx")
  {
    wb <- loadWorkbook(file_name)
    #print(wb)
    sheets <- getSheets(wb)
    #print(sheets)
    n <- length(sheets)
    #print(z)
    #name_sheet <- names(sheets)
    #print(names(sheets))
    #n <- length(name_sheet)
    #print(n)
    for(n in 1:n)
    {
      print(sheets[n])
    }
    sheet_name <- readline(prompt="Enter the sheet name: ")
    file <- read.xlsx(file_name, sheetName = sheet_name)
    cols <- colnames(file)
    x <- ncol(file)
    for(x in 1:x)
    {
      print(cols[x])
    }
  }
  else if(extension == "xls")
  {
    library(gdata) 
    sheets <- sheetNames(file_name)
    #print(names)
    n <- length(sheets)
    #print(n)
    for(n in 1:n)
    {
      print(sheets[n])
    }
    sheet_name <- readline(prompt="Enter the sheet name: ")
    file <- read.xlsx(file_name, sheetName = sheet_name)
    cols <- colnames(file)
    x <- ncol(file)
    for(x in 1:x)
    {
      print(cols[x])
    }
  }
  else if(extension == "csv")
  {
    separator <- readline(prompt="Enter the type of separator: ")
    header <- readline(prompt="Header included? (y/n) : ")
    if(header == "y")
      file <- read.csv(file= file_name, header=TRUE, sep=separator)
    else
      file <- read.csv(file= file_name, header=FALSE, sep=separator)
    cols <- colnames(file)
    x <- ncol(file)
    for(x in 1:x)
    {
      print(cols[x])
    }
  }
  else
  {
    print("ERROR!!!")
  }
  source("graph_function.R")
  graph(file)
}