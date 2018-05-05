compare <- function()
{
  ##Library needed in the function. Install if not already available.##
  library(plyr)
  repeat
  {
    ##Choose files to be compared##
    cat("\nSelect the 1st csv file")
    file1 <- file.choose()
    cat("\nFile 1 is :", file1)
    
    cat("\nSelect the 2nd csv file")
    file2 <- file.choose()
    cat("\nFile 2 is :", file2)
    
    ##Files are read to a data frame##
    DF1 <- read.csv(file1, header = T, stringsAsFactors=F, strip.white=T)
    DF2 <- read.csv(file2, header = T, stringsAsFactors=F, strip.white=T)
    
    ##Unique values from the data frames are fetched##
    DF1_uniq <- unique(DF1)
    DF2_uniq <- unique(DF2)
    
    if(nrow(DF1) != nrow(DF1_uniq))
    {
      dup_res <- readline(prompt="\nFile 1 has duplicates in it. Do you want to remove the duplicates? (y/n) : ")
      if(tolower(dup_res) == 'y')
      {
        DF1 <- DF1_uniq
      }
    }
    
    if(nrow(DF2) != nrow(DF2_uniq))
    {
      dup_res <- readline(prompt="\nFile 2 has duplicates in it. Do you want to remove the duplicates? (y/n) : ")
      if(tolower(dup_res) == 'y')
      {
        DF2 <- DF2_uniq
      }
    }
    
    ##NA's in dataframes are replaced with 'NA' string##
    DF1[is.na(DF1)] <- 'NA'
    DF2[is.na(DF2)] <- 'NA'
    
    cols1 <- colnames(DF1)
    cols2 <- colnames(DF2)
  
    
    if(nrow(DF1) != nrow(DF2))
    {
      count_resp <- readline(prompt="\nThe number of rows do not match in both the files. Do you want to continue? (y/n) : ")
      
      if(tolower(count_resp) == "n")
      {
        break
      }
    }
    
    
    repeat
    {
      x <- ncol(DF1)
      cat("\nThe columns of File 1 are :")
      for(x in 1:x)
      {
        cat('\n',cols1[x])
      }
      column_name_1 <- readline(prompt="\nEnter the column name of file 1: ")
      unique_col1 <- readline(prompt="\nEnter the unique column of file 1: ")
      
      y <- ncol(DF2)
      cat("\nThe columns of File 2 are :")
      for(y in 1:y)
      {
        cat('\n',cols2[y])
      }
      column_name_2 <- readline(prompt="\nEnter the column name of file 2: ")
      unique_col2 <- readline(prompt="\nEnter the unique column of file 2: ")
      
      DF1 <- DF1[order(DF1[[unique_col1]], decreasing = FALSE),]
      DF2 <- DF2[order(DF2[[unique_col2]], decreasing = FALSE),]
      
      ##Data of the input columns compared##
      out <- ifelse(DF1[[column_name_1]] == DF2[[column_name_2]], 1, 0)
      
      val <- which(out==0)
      DF1_col <- data.frame(DF1[[unique_col1]], DF1[[column_name_1]], stringsAsFactors=FALSE)
      names(DF1_col) <- c(unique_col1, column_name_1)
      DF2_col <- data.frame(DF2[[unique_col2]], DF2[[column_name_2]], stringsAsFactors=FALSE)
      names(DF2_col) <- c(unique_col2, column_name_2)
      
      if(!length(val) == 0 && length(val) <= 5)
      {
        cat("\nThere is discrepancy for 5 or less rows in the data compared.")
        
        i <- length(val)
        for(i in 1:i)
        {
          cat("\nRow Num :", val[i],"\n")
          cat("File 1 value : ", DF1_col[val[i],2], "\n")
          cat("File 2 value : ", DF2_col[val[i],2], "\n\n")
        }
      }
      else if(length(val) > 5)
      {
        cat("\nData discrepancy for more than 5 rows. Writing the data to a file")
        output <- data.frame(matrix(ncol = 4, nrow = 0))
        
        i <- length(val)
        for(i in 1:i)
        {
          newRow <- data.frame(DF1_col[val[i],], DF2_col[val[i],])
          output <- rbind(output, newRow)
        }
        
        ##Output file generated for data discrepancies for more than 5 rows##
        cols <- c(paste("File1",unique_col1, sep="."),paste("File1",column_name_1, sep="."),paste("File2",unique_col2, sep="."),paste("File2",column_name_2, sep="."))
        colnames(output) <- cols
        location <- choose.dir(default = "", caption = "Select the location of the file to be saved")
        fileout <- paste(location, "\\", column_name_1, "_", column_name_2, ".csv", sep="")
        write.csv(output, file = fileout, row.names = FALSE, quote = FALSE)
        
      }
      else
      {
        cat("\nNo discrepancy in the data between the columns")
      }
      
      loop_resp1 <- readline(prompt="\nDo you want to compare more data from the same files? (y/n): ")
      
      if(tolower(loop_resp1) == "n")
      {
        break
      }
      
    }
    
    loop_resp2 <- readline(prompt="\nDo you want to compare data from different files? (y/n): ")
    
    if(tolower(loop_resp2) == "n")
    {
      break
    }
    
  }
  
}
