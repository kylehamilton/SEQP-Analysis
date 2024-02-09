readCabrillo <- function(filePath, includeHeader = FALSE) {
  library(tibble)
  
  # Initialize variables
  qsos <- list()
  headerInfo <- list()
  
  # Read the file
  lines <- readLines(filePath)
  
  # Process each line
  for (line in lines) {
    if (grepl("^QSO:", line)) {
      parts <- strsplit(line, " +")[[1]]  # Use " +" to split by one or more spaces
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
    } else if (includeHeader == TRUE) {
      # Process header lines only if includeHeader is TRUE
      headerParts <- strsplit(line, ":", fixed = TRUE)
      if (length(headerParts[[1]]) == 2) {
        key <- strtrim(headerParts[[1]][1], nchar(headerParts[[1]][1]))
        value <- strtrim(headerParts[[1]][2], nchar(headerParts[[1]][2]))
        headerInfo[[key]] <- value
      }
    }
  }
  
  # Combine all QSOs into a single dataframe
  qsosDF <- bind_rows(qsos)
  
  # Return based on includeHeader flag
  if (includeHeader) {
    return(list(Header = headerInfo, QSOs = qsosDF))
  } else {
    return(qsosDF)
  }
}


df_test3 <- readCabrillo("k4bai.log", includeHeader = FALSE) 
