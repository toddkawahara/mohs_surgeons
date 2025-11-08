library(readxl)
library(dplyr)

# Merge all years of Mohs Billing together into one file
df_2014 <- read.csv('Medicare_Physician_Other_Practitioners_by_Provider_and_Service_2014.csv') %>%
  mutate(Billing_Year = 2014) %>% 
  rename(NPI = Rndrng_NPI) %>%
  relocate(Billing_Year, .before = 1)
df_2015 <- read.csv('Medicare_Physician_Other_Practitioners_by_Provider_and_Service_2015.csv') %>%
  mutate(Billing_Year = 2015) %>% 
  rename(NPI = Rndrng_NPI) %>%
  relocate(Billing_Year, .before = 1)
df_2016 <- read.csv('Medicare_Physician_Other_Practitioners_by_Provider_and_Service_2016.csv') %>%
  mutate(Billing_Year = 2016) %>% 
  rename(NPI = Rndrng_NPI) %>%
  relocate(Billing_Year, .before = 1)
df_2017 <- read.csv('Medicare_Physician_Other_Practitioners_by_Provider_and_Service_2017.csv') %>%
  mutate(Billing_Year = 2017) %>% 
  rename(NPI = Rndrng_NPI) %>%
  relocate(Billing_Year, .before = 1)
df_2018 <- read.csv('Medicare_Physician_Other_Practitioners_by_Provider_and_Service_2018.csv') %>%
  mutate(Billing_Year = 2018) %>% 
  rename(NPI = Rndrng_NPI) %>%
  relocate(Billing_Year, .before = 1)
df_2019 <- read.csv('Medicare_Physician_Other_Practitioners_by_Provider_and_Service_2019.csv') %>%
  mutate(Billing_Year = 2019) %>% 
  rename(NPI = Rndrng_NPI) %>%
  relocate(Billing_Year, .before = 1)
df_2020 <- read.csv('Medicare_Physician_Other_Practitioners_by_Provider_and_Service_2020.csv') %>%
  mutate(Billing_Year = 2020) %>% 
  rename(NPI = Rndrng_NPI) %>%
  relocate(Billing_Year, .before = 1)
df_2021 <- read.csv('Medicare_Physician_Other_Practitioners_by_Provider_and_Service_2021.csv') %>%
  mutate(Billing_Year = 2021) %>% 
  rename(NPI = Rndrng_NPI) %>%
  relocate(Billing_Year, .before = 1)
df_2022 <- read.csv('Medicare_Physician_Other_Practitioners_by_Provider_and_Service_2022.csv') %>%
  mutate(Billing_Year = 2022) %>% 
  rename(NPI = Rndrng_NPI) %>%
  relocate(Billing_Year, .before = 1)
df_2023 <- read.csv('Medicare_Physician_Other_Practitioners_by_Provider_and_Service_2023.csv') %>%
  mutate(Billing_Year = 2023) %>% 
  rename(NPI = Rndrng_NPI) %>%
  relocate(Billing_Year, .before = 1)

codes_df <- rbind(df_2014,df_2015,df_2016,df_2017,df_2018,df_2019,df_2020,df_2021,df_2022,df_2023)

# Read in Data
surgeon_df <- read_excel("mohs.surgeon.xlsx", sheet = "Final")

# Filter to desired codes
codes_df <- codes_df %>%
  filter(HCPCS_Cd %in% c(17311, 17312, 17313, 17314))

# Filter to grads before 2023
surgeon_df <- surgeon_df %>%
  mutate(NPI = na_if(NPI, "N/A")) %>%
  mutate(NPI = as.numeric(NPI)) %>%
  filter(`Graduation Year` < 2023)

# Merge the data by NPI
merged_df <- inner_join(surgeon_df, codes_df, by = "NPI") %>%
  filter(Billing_Year == `Graduation Year` + 1)

# Get total surgeon grads in a year
yearly_surgeons <- surgeon_df %>%
  group_by(`Graduation Year`) %>%
  summarize(grads = n())

# Get total surgeon grads after merge
merged_surgeons <- merged_df %>%
  distinct(`Graduation Year`, NPI) %>%
  group_by(`Graduation Year`) %>%
  summarize(surgeons = n())

# See surgeons lost per year because no NPI in the codes_df
surgeons_dropped <- merge(yearly_surgeons, merged_surgeons, by = 'Graduation Year') %>%
  mutate(surgeons_lost = grads - surgeons)

# See what surgeons got dropped in the merge because no NPI in the codes_df
surgeon_df_filter <- surgeon_df %>% 
  filter(surgeon_df$NPI %in% setdiff(surgeon_df$NPI, merged_df$NPI))

length(setdiff(surgeon_df$NPI, merged_df$NPI))

# Save merged as new excel file
write.csv(merged_df, "Final_Mohs_Billing.csv")

# Save surgeons dropped in merge because no NPI in the codes_df
write.csv(surgeon_df_filter, 'dropped_surgeons.csv', row.names = FALSE)
write.csv(surgeons_dropped, 'dropped_surgeons_yearly.csv', row.names = FALSE)

