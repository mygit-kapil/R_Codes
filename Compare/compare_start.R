print("Please choose the source folder")
source_folder <- choose.dir(default = "", caption = "Select R source folder")
setwd(source_folder)
source("compare.R")
compare()