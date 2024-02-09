library(readr) # Read Rectangular Text Data CRAN v2.1.4
library(lubridate) # Make Dealing with Dates a Little Easier CRAN v1.9.3
library(dplyr) # A Grammar of Data Manipulation CRAN v1.1.4

PSKR_20231014T180000_e <- read_delim("PSK/PSKR_20231014T180000_e.tsv", 
                                     delim = "\t", escape_double = FALSE, 
                                     trim_ws = TRUE)

PSKR_20231014T180000_o <- read_delim("PSK/PSKR_20231014T180000_o.tsv", 
                                     delim = "\t", escape_double = FALSE, 
                                     trim_ws = TRUE)

PSKR_20231015T000000_o <- read_delim("PSK/PSKR_20231015T000000_o.tsv", 
                                     delim = "\t", escape_double = FALSE, 
                                     trim_ws = TRUE)

PSKR_20231015T000009_e<- read_delim("PSK/PSKR_20231015T000009_e.tsv", 
                                     delim = "\t", escape_double = FALSE, 
                                     trim_ws = TRUE)

df_psk <- rbind(PSKR_20231014T180000_e, PSKR_20231014T180000_o, PSKR_20231015T000000_o, PSKR_20231015T000009_e)

rm(PSKR_20231014T180000_e);rm(PSKR_20231014T180000_o);rm(PSKR_20231015T000000_o);rm(PSKR_20231015T000009_e)
gc()


df_psk$dateTimeUTC <- as.POSIXct(df_psk$flowStartSeconds, origin = "1970-01-01", tz = "UTC")


df_psk <- df_psk %>%
  filter(senderCallsign %in% Callsign_List)

# test <- df_psk[1:25,]
# write.csv(test, file = "first 25 rows.csv")

# Round down dateTimeUTC to the nearest 10 minutes
df_psk$TimeChunk <- floor_date(df_psk$dateTimeUTC, "10 minutes")

# Filter only the rows with senderCallsign in your values list
df_filtered <- df_psk %>% 
  filter(senderCallsign %in% Callsign_List)

# Count occurrences within each 10-minute chunk for the filtered dataframe
count_per_chunk <- df_filtered %>%
  group_by(TimeChunk, senderCallsign) %>%
  summarise(Count = n(), .groups = 'drop')

# View the result
print(count_per_chunk)
