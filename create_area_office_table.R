library(tidyr)
library(dplyr)
library(ggplot2)
library(gt)
library(writexl)

# Loading in data
df_office <- read.csv('df_office.csv', row.names = 1)

df_demographics <- read.csv('df_demographic.csv', row.names = 1) %>%
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
  ) %>%
  # re edit surgeons pct
  select(-surgeons_pct) %>%
  # get total surgeons in each year
  group_by(Billing_Year) %>%
  mutate(total_surgeons_year = sum(surgeons)) %>%
  ungroup() %>%
  # get surgeons in each year area
  group_by(Billing_Year, Rndrng_Prvdr_RUCA_Desc) %>%
  mutate(surgeons = sum(surgeons)) %>%
  unique() %>%
  mutate(surgeons_pct = surgeons/total_surgeons_year) %>%
  ungroup()

# pivot office data
df_office_table <- df_office %>%
  select(c(Billing_Year, Place_Of_Srvc, surgeons_pct)) %>%
  mutate(surgeons_pct = round(surgeons_pct, 4)) %>%
  pivot_wider(names_from = Billing_Year, values_from = surgeons_pct)

# pivot service area data
df_demographic_table <- df_demographics %>% 
  select(Billing_Year, Rndrng_Prvdr_RUCA_Desc, surgeons_pct) %>%
  mutate(surgeons_pct = round(surgeons_pct, 4)) %>%
  complete(Billing_Year, Rndrng_Prvdr_RUCA_Desc, fill = list(surgeons_pct = 0)) %>%
  pivot_wider(names_from = Billing_Year, values_from = surgeons_pct)

# rename cols to concat dfs
df1 <- df_office_table %>% rename(group = Place_Of_Srvc)
df2 <- df_demographic_table %>% rename(group = Rndrng_Prvdr_RUCA_Desc)

# designate sections for table
df1$section <- "Inpatient/Outpatient"
df2$section <- "Area"

# convert to decimals to percentages
df_combined <- bind_rows(df1, df2) %>%
  mutate(`2014` = scales::percent(`2014`, accuracy = 0.01), 
         `2015` = scales::percent(`2015`, accuracy = 0.01), 
         `2016` = scales::percent(`2016`, accuracy = 0.01), 
         `2017` = scales::percent(`2017`, accuracy = 0.01), 
         `2018` = scales::percent(`2018`, accuracy = 0.01), 
         `2019` = scales::percent(`2019`, accuracy = 0.01), 
         `2020` = scales::percent(`2020`, accuracy = 0.01), 
         `2021` = scales::percent(`2021`, accuracy = 0.01), 
         `2022` = scales::percent(`2022`, accuracy = 0.01), 
         `2023` = scales::percent(`2023`, accuracy = 0.01)) %>%
  # rename office codes
  mutate(group = recode(
    group, 
    'F' = 'Inpatient', 
    'O' = 'Outpatient'
  ))

# create table
df_combined %>%
  gt(rowname_col = "group", groupname_col = "section") %>%
  tab_header(
    title = "Surgeon Demographic Table"
  )

# write excel file
write_xlsx(df_combined, 'df_demographic_table.xlsx')

