readCabrilloDirectory <- function(directoryPath, includeHeader = FALSE) {
  library(tibble)
  library(dplyr)
  
  # Function to read a single Cabrillo file
  readCabrillo <- function(filePath) {
    qsos <- list()
    
    lines <- readLines(filePath)
    
    for (line in lines) {
      if (grepl("^QSO:", line)) {
        parts <- strsplit(line, " +")[[1]]
        qsos[[length(qsos) + 1]] <- tibble(
          Frequency = parts[2],
          Mode = parts[3],
          Date = parts[4],
          Time = parts[5],
          CallSent = parts[6],
          RSTSent = parts[7],
          ExchangeSent = parts[8],
          CallReceived = parts[9],
          RSTReceived = parts[10],
          ExchangeReceived = parts[11]
        )
      }
    }
    
    qsosDF <- bind_rows(qsos)
    return(qsosDF)
  }
  
  # List all Cabrillo files in the directory
  cabrilloFiles <- list.files(directoryPath, pattern = "\\.log$", full.names = TRUE)
  
  # Initialize an empty list to store dataframes from each file
  dfs <- list()
  
  # Read each Cabrillo file and store the resulting dataframe
  for (filePath in cabrilloFiles) {
    dfs <- append(dfs, list(readCabrillo(filePath)))
  }
  
  # Combine all dataframes into one
  completeDF <- bind_rows(dfs)
  
  return(completeDF)
}

completeDF <- readCabrilloDirectory("Data/")

