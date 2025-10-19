library(tidyr)
library(dplyr)
library(ggplot2)
library(gt)
library(usmap)
library(writexl)

#Loading in data
df_billing <- read.csv('Final_Mohs_Billing.csv', row.names = 1) %>%
  # filter to first year out of practice
  filter(Billing_Year == Graduation.Year + 1) %>%
  # group areas together
  mutate(
    Rndrng_Prvdr_RUCA_Desc = case_when(
      Rndrng_Prvdr_RUCA_Desc == "Metropolitan area core: primary flow within an urbanized area of 50,000 and greater" ~ 'Metropolitan',
      Rndrng_Prvdr_RUCA_Desc == "Micropolitan area core: primary flow within an urban cluster of 10,000 to 49,999" ~ 'Micropolitan', 
      Rndrng_Prvdr_RUCA_Desc == "Secondary flow 30% to <50% to a larger urbanized area of 50,000 and greater" ~ 'Metropolitan', 
      Rndrng_Prvdr_RUCA_Desc == "Metropolitan area high commuting: primary flow 30% or more to a urbanized area of 50,000 and greater" ~ 'Metropolitan', 
      Rndrng_Prvdr_RUCA_Desc == "Micropolitan high commuting: primary flow 30% or more to a urban cluster of 10,000 to 49,999" ~ 'Micropolitan', 
      Rndrng_Prvdr_RUCA_Desc == "Rural areas: primary flow to a tract outside a urbanized area of 50,000 and greater or UC" ~ 'Rural', 
      Rndrng_Prvdr_RUCA_Desc == "Secondary flow 30% to <50% to a urbanized area of 50,000 and greater" ~ 'Micropolitan', 
      Rndrng_Prvdr_RUCA_Desc == "Unknown" ~ 'Unknown'
    )
  )
# total services
df_services <- df_billing %>%
  group_by(Billing_Year) %>%
  summarize(services = sum(Tot_Srvcs))

# proportion of services that are metro
df_metro_services <- df_billing %>%
  filter(Rndrng_Prvdr_RUCA_Desc == 'Metropolitan') %>%
  group_by(Billing_Year) %>%
  summarize(services = sum(Tot_Srvcs))

df_services_merged <- merge(df_metro_services, df_services, by = 'Billing_Year') %>%
  mutate(ratio = services.x/services.y)

# proportion of services that are outpatient
df_outpatient_services <- df_billing %>%
  filter(Place_Of_Srvc == 'O') %>%
  group_by(Billing_Year) %>%
  summarize(services = sum(Tot_Srvcs))

df_services_merged1 <- merge(df_outpatient_services, df_services, by = 'Billing_Year') %>%
  mutate(ratio = services.x/services.y)