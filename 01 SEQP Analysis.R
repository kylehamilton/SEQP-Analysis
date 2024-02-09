library(tibble) # Simple Data Frames CRAN v3.2.1
library(dplyr) # A Grammar of Data Manipulation CRAN v1.1.4
library(lubridate) # Make Dealing with Dates a Little Easier CRAN v1.9.3
library(Cabrillo) # Read Cabrillo formated data into R github.com/kylehamilton/Cabrillo v1.0


# Load Data ---------------------------------------------------------------

df_seqp <- readCabrilloDirectory("Data/")

# Clean Up Grid Squares ---------------------------------------------------

# Some logs have six character long grid squares
# This only keeps the first four characters.
df_seqp$Grid_Sent <- substr(df_seqp$ExchangeSent, 1, 4)
df_seqp$Grid_Received <- substr(df_seqp$ExchangeReceived, 1, 4)


# Clean Up Bands ----------------------------------------------------------

# Create the new 'Band' variable
df_seqp$Band <- substr(df_seqp$Frequency, 1, 2)


# Clean Up Modes ----------------------------------------------------------

df_seqp <- df_seqp %>% 
  mutate(
    Mode = if_else(Mode == "RY", "DG", Mode)
  )

# Points ------------------------------------------------------------------

df_seqp <- df_seqp %>% 
  mutate(
    # If contact mode is PH award 1 point, any other mode award 2 points.
    contact_points = if_else(Mode == "PH", 1, 2, missing = NA)
  )


# Dates and Times ---------------------------------------------------------

# Combine string data for the Date and Time, and convert to POSIXct
df_seqp$Date_Time <- as.POSIXct(paste(df_seqp$Date, substr(df_seqp$Time, 1, 2), substr(df_seqp$Time, 3, 4)),
                                format = "%Y-%m-%d %H %M", tz = "GMT")


  

# Analysis ----------------------------------------------------------------

# unique_contacts <- df_seqp %>%
#   group_by(CallSent) %>%
#   summarise(UniqueContacts = length(unique(CallReceived)))
# 
# print(unique_contacts)
  
# Dup Contacts with 10 minute rule

# Group, sort, and then identify unique contacts
unique_contacts <- df_seqp %>%
  arrange(CallSent, CallReceived, Band, Mode, Date_Time) %>% # Sort data
  group_by(CallSent, CallReceived, Band, Mode) %>%
  mutate(diff = as.numeric(difftime(Date_Time, lag(Date_Time, default = first(Date_Time)), units = "mins"))) %>%
  filter(is.na(diff) | diff > 10) %>% # Apply 10-minute rule
  ungroup() %>%
  distinct(CallSent, CallReceived, Band, Mode, .keep_all = TRUE) # Keep unique contacts

# Count unique contacts for each CallSent
contact_counts <- unique_contacts %>%
  group_by(CallSent) %>%
  summarise(UniqueContacts = n_distinct(CallReceived))

# Print the count of unique contacts
print(contact_counts)


# Calculate multiplier
multiplier <- df_seqp %>%
  group_by(CallSent, Grid_Received, Band) %>%
  summarise(count = n_distinct(Grid_Received)) %>%
  ungroup() %>%
  group_by(CallSent) %>%
  summarise(Multiplier = sum(count))

df_output <- multiplier

# Calculate the total number of unique contacts for each CallSent across all CallReceived
total_unique_contacts <- df_seqp %>%
  distinct(CallSent, CallReceived) %>%
  group_by(CallSent) %>%
  summarise(TotalUniqueContacts = n())

# View the result
print(total_unique_contacts)


# Calculate the total number of unique contacts for each CallSent,
# broken down by Mode and Band
total_unique_contacts_by_mode_band <- df_seqp %>%
  distinct(CallSent, CallReceived, Mode, Band) %>%
  group_by(CallSent, Mode, Band) %>%
  summarise(TotalUniqueContacts = n(), .groups = 'drop')

# View the result
print(total_unique_contacts_by_mode_band)

# Calculate the total number of unique contacts for each CallSent,
# broken down by Mode
total_unique_contacts_by_mode<- df_seqp %>%
  distinct(CallSent, CallReceived, Mode) %>%
  group_by(CallSent, Mode) %>%
  summarise(TotalUniqueContacts = n(), .groups = 'drop')

total_unique_contacts_by_mode <- total_unique_contacts_by_mode %>% 
  mutate(
    points_ph = if_else(Mode == "PH", TotalUniqueContacts * 1, NA),
    points_dg = if_else(Mode == "DG", TotalUniqueContacts * 2, NA),
    points_cw = if_else(Mode == "CW", TotalUniqueContacts * 2, NA),
  )

# total_unique_contacts_by_mode <- total_unique_contacts_by_mode %>% 
#   mutate(
#     points_subtotal = points_ph + points_dg + points_cw
#   )

# View the result
print(total_unique_contacts_by_mode)



# Combining rows based on CallSent and overwriting NA values
combined_df <- total_unique_contacts_by_mode %>%
  group_by(CallSent) %>%
  summarise(
    Mode = first(Mode), # Assuming Mode should be taken from the first occurrence
    TotalUniqueContacts = sum(TotalUniqueContacts, na.rm = TRUE), # Sum if there are multiple non-NA values
    points_ph = max(points_ph, na.rm = TRUE), # Replace NA with actual values, use max to get non-NA
    points_dg = max(points_dg, na.rm = TRUE),
    points_cw = max(points_cw, na.rm = TRUE),
    .groups = 'drop' # Drop grouping structure after summarisation
  )

combined_df <- combined_df %>% 
  select(-Mode)

# View the combined dataframe
print(combined_df)


combined_df <- combined_df %>%
  mutate(across(everything(), ~ ifelse(. == -Inf, 0, .)))



combined_df <- combined_df %>% 
  mutate(
    points_subtotal = points_ph + points_dg + points_cw
  )

# Calculate multiplier
multiplier <- df_seqp %>%
  group_by(CallSent, Grid_Received, Band) %>%
  summarise(count = n_distinct(Grid_Received)) %>%
  ungroup() %>%
  group_by(CallSent) %>%
  summarise(Multiplier = sum(count))

combined_df <- inner_join(combined_df, multiplier)

combined_df <- combined_df %>% 
  mutate(
    points_contact_total = points_subtotal * Multiplier
  )

Callsign_List <- combined_df$CallSent
