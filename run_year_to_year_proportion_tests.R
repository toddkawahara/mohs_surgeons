library(tidyr)
library(dplyr)

# ---- loading in data ----
df_office <- read.csv('df_office.csv', row.names = 1)

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

df_2014 <- df_billing %>% filter(Billing_Year == 2014)
df_2015 <- df_billing %>% filter(Billing_Year == 2015)
df_2016 <- df_billing %>% filter(Billing_Year == 2016)
df_2017 <- df_billing %>% filter(Billing_Year == 2017)
df_2018 <- df_billing %>% filter(Billing_Year == 2018)
df_2019 <- df_billing %>% filter(Billing_Year == 2019)
df_2020 <- df_billing %>% filter(Billing_Year == 2020)
df_2021 <- df_billing %>% filter(Billing_Year == 2021)
df_2022 <- df_billing %>% filter(Billing_Year == 2022)
df_2023 <- df_billing %>% filter(Billing_Year == 2023)


# ----metro services ----
# getting services in metro area
df_2014_metro <- df_2014 %>% group_by(NPI) %>% summarize(metro_services = sum(Tot_Srvcs[Rndrng_Prvdr_RUCA_Desc == 'Metropolitan']), services = sum(Tot_Srvcs)) %>% ungroup()
df_2015_metro <- df_2015 %>% group_by(NPI) %>% summarize(metro_services = sum(Tot_Srvcs[Rndrng_Prvdr_RUCA_Desc == 'Metropolitan']), services = sum(Tot_Srvcs)) %>% ungroup()
df_2016_metro <- df_2016 %>% group_by(NPI) %>% summarize(metro_services = sum(Tot_Srvcs[Rndrng_Prvdr_RUCA_Desc == 'Metropolitan']), services = sum(Tot_Srvcs)) %>% ungroup()
df_2017_metro <- df_2017 %>% group_by(NPI) %>% summarize(metro_services = sum(Tot_Srvcs[Rndrng_Prvdr_RUCA_Desc == 'Metropolitan']), services = sum(Tot_Srvcs)) %>% ungroup()
df_2018_metro <- df_2018 %>% group_by(NPI) %>% summarize(metro_services = sum(Tot_Srvcs[Rndrng_Prvdr_RUCA_Desc == 'Metropolitan']), services = sum(Tot_Srvcs)) %>% ungroup()
df_2019_metro <- df_2019 %>% group_by(NPI) %>% summarize(metro_services = sum(Tot_Srvcs[Rndrng_Prvdr_RUCA_Desc == 'Metropolitan']), services = sum(Tot_Srvcs)) %>% ungroup()
df_2020_metro <- df_2020 %>% group_by(NPI) %>% summarize(metro_services = sum(Tot_Srvcs[Rndrng_Prvdr_RUCA_Desc == 'Metropolitan']), services = sum(Tot_Srvcs)) %>% ungroup()
df_2021_metro <- df_2021 %>% group_by(NPI) %>% summarize(metro_services = sum(Tot_Srvcs[Rndrng_Prvdr_RUCA_Desc == 'Metropolitan']), services = sum(Tot_Srvcs)) %>% ungroup()
df_2022_metro <- df_2022 %>% group_by(NPI) %>% summarize(metro_services = sum(Tot_Srvcs[Rndrng_Prvdr_RUCA_Desc == 'Metropolitan']), services = sum(Tot_Srvcs)) %>% ungroup()
df_2023_metro <- df_2023 %>% group_by(NPI) %>% summarize(metro_services = sum(Tot_Srvcs[Rndrng_Prvdr_RUCA_Desc == 'Metropolitan']), services = sum(Tot_Srvcs)) %>% ungroup()

# two sample proportion tests
# 2014 to 2015, p-value = 0.1143
prop.test(
  x = c(sum(df_2014_metro$metro_services), sum(df_2015_metro$metro_services)),
  n = c(sum(df_2014_metro$services), sum(df_2015_metro$services))
)
# 2015 to 2016, p-value = 2.2e-16
prop.test(
  x = c(sum(df_2015_metro$metro_services), sum(df_2016_metro$metro_services)),
  n = c(sum(df_2015_metro$services), sum(df_2016_metro$services))
)
# 2016 to 2017, p-value = 2.2e-16
prop.test(
  x = c(sum(df_2016_metro$metro_services), sum(df_2017_metro$metro_services)),
  n = c(sum(df_2016_metro$services), sum(df_2017_metro$services))
)
# 2017 to 2018, p-value = 2.2e-16
prop.test(
  x = c(sum(df_2017_metro$metro_services), sum(df_2018_metro$metro_services)),
  n = c(sum(df_2017_metro$services), sum(df_2018_metro$services))
)
# 2018 to 2019, p-value = 2.2e-16
prop.test(
  x = c(sum(df_2018_metro$metro_services), sum(df_2019_metro$metro_services)),
  n = c(sum(df_2018_metro$services), sum(df_2019_metro$services))
)
# 2019 to 2020, p-value = 2.2e-16
prop.test(
  x = c(sum(df_2019_metro$metro_services), sum(df_2020_metro$metro_services)),
  n = c(sum(df_2019_metro$services), sum(df_2020_metro$services))
)
# 2020 to 2021, p-value = 2.2e-16
prop.test(
  x = c(sum(df_2020_metro$metro_services), sum(df_2021_metro$metro_services)),
  n = c(sum(df_2020_metro$services), sum(df_2021_metro$services))
)
# 2021 to 2022, p-value = 2.2e-16
prop.test(
  x = c(sum(df_2021_metro$metro_services), sum(df_2022_metro$metro_services)),
  n = c(sum(df_2021_metro$services), sum(df_2022_metro$services))
)
# 2022 to 2023, p-value = 2.2e-16
prop.test(
  x = c(sum(df_2022_metro$metro_services), sum(df_2023_metro$metro_services)),
  n = c(sum(df_2022_metro$services), sum(df_2023_metro$services))
)

# ---- micro services ----
# getting services in micro area
df_2014_micro <- df_2014 %>% group_by(NPI) %>% summarize(micro_services = sum(Tot_Srvcs[Rndrng_Prvdr_RUCA_Desc == 'Micropolitan']), services = sum(Tot_Srvcs)) %>% ungroup()
df_2015_micro <- df_2015 %>% group_by(NPI) %>% summarize(micro_services = sum(Tot_Srvcs[Rndrng_Prvdr_RUCA_Desc == 'Micropolitan']), services = sum(Tot_Srvcs)) %>% ungroup()
df_2016_micro <- df_2016 %>% group_by(NPI) %>% summarize(micro_services = sum(Tot_Srvcs[Rndrng_Prvdr_RUCA_Desc == 'Micropolitan']), services = sum(Tot_Srvcs)) %>% ungroup()
df_2017_micro <- df_2017 %>% group_by(NPI) %>% summarize(micro_services = sum(Tot_Srvcs[Rndrng_Prvdr_RUCA_Desc == 'Micropolitan']), services = sum(Tot_Srvcs)) %>% ungroup()
df_2018_micro <- df_2018 %>% group_by(NPI) %>% summarize(micro_services = sum(Tot_Srvcs[Rndrng_Prvdr_RUCA_Desc == 'Micropolitan']), services = sum(Tot_Srvcs)) %>% ungroup()
df_2019_micro <- df_2019 %>% group_by(NPI) %>% summarize(micro_services = sum(Tot_Srvcs[Rndrng_Prvdr_RUCA_Desc == 'Micropolitan']), services = sum(Tot_Srvcs)) %>% ungroup()
df_2020_micro <- df_2020 %>% group_by(NPI) %>% summarize(micro_services = sum(Tot_Srvcs[Rndrng_Prvdr_RUCA_Desc == 'Micropolitan']), services = sum(Tot_Srvcs)) %>% ungroup()
df_2021_micro <- df_2021 %>% group_by(NPI) %>% summarize(micro_services = sum(Tot_Srvcs[Rndrng_Prvdr_RUCA_Desc == 'Micropolitan']), services = sum(Tot_Srvcs)) %>% ungroup()
df_2022_micro <- df_2022 %>% group_by(NPI) %>% summarize(micro_services = sum(Tot_Srvcs[Rndrng_Prvdr_RUCA_Desc == 'Micropolitan']), services = sum(Tot_Srvcs)) %>% ungroup()
df_2023_micro <- df_2023 %>% group_by(NPI) %>% summarize(micro_services = sum(Tot_Srvcs[Rndrng_Prvdr_RUCA_Desc == 'Micropolitan']), services = sum(Tot_Srvcs)) %>% ungroup()

# two sample proportion tests
# 2014 to 2015, p-value = 0.1143
prop.test(
  x = c(sum(df_2014_micro$micro_services), sum(df_2015_micro$micro_services)),
  n = c(sum(df_2014_micro$services), sum(df_2015_micro$services))
)
# 2015 to 2016, p-value = 2.2e-16
prop.test(
  x = c(sum(df_2015_micro$micro_services), sum(df_2016_micro$micro_services)),
  n = c(sum(df_2015_micro$services), sum(df_2016_micro$services))
)
# 2016 to 2017, p-value = 2.2e-16
prop.test(
  x = c(sum(df_2016_micro$micro_services), sum(df_2017_micro$micro_services)),
  n = c(sum(df_2016_micro$services), sum(df_2017_micro$services))
)
# 2017 to 2018, p-value = 2.2e-16
prop.test(
  x = c(sum(df_2017_micro$micro_services), sum(df_2018_micro$micro_services)),
  n = c(sum(df_2017_micro$services), sum(df_2018_micro$services))
)
# 2018 to 2019, p-value = 2.2e-16
prop.test(
  x = c(sum(df_2018_micro$micro_services), sum(df_2019_micro$micro_services)),
  n = c(sum(df_2018_micro$services), sum(df_2019_micro$services))
)
# 2019 to 2020, p-value = 2.2e-16
prop.test(
  x = c(sum(df_2019_micro$micro_services), sum(df_2020_micro$micro_services)),
  n = c(sum(df_2019_micro$services), sum(df_2020_micro$services))
)
# 2020 to 2021, p-value = 2.2e-16
prop.test(
  x = c(sum(df_2020_micro$micro_services), sum(df_2021_micro$micro_services)),
  n = c(sum(df_2020_micro$services), sum(df_2021_micro$services))
)
# 2021 to 2022, p-value = 2.2e-16
prop.test(
  x = c(sum(df_2021_micro$micro_services), sum(df_2022_micro$micro_services)),
  n = c(sum(df_2021_micro$services), sum(df_2022_micro$services))
)
# 2022 to 2023, p-value = 2.2e-16
prop.test(
  x = c(sum(df_2022_micro$micro_services), sum(df_2023_micro$micro_services)),
  n = c(sum(df_2022_micro$services), sum(df_2023_micro$services))
)

# ---- metro surgeons ----
# two sample proportion tests
# 2014 to 2015, p-value = 0.8744
prop.test(
  x = c(df_demographics %>% filter((Billing_Year == 2014) & (Rndrng_Prvdr_RUCA_Desc == 'Metropolitan')) %>% pull(surgeons), 
        df_demographics %>% filter((Billing_Year == 2015) & (Rndrng_Prvdr_RUCA_Desc == 'Metropolitan')) %>% pull(surgeons)),
  n = c(df_demographics %>% filter((Billing_Year == 2014) & (Rndrng_Prvdr_RUCA_Desc == 'Metropolitan')) %>% pull(total_surgeons_year), 
        df_demographics %>% filter((Billing_Year == 2015) & (Rndrng_Prvdr_RUCA_Desc == 'Metropolitan')) %>% pull(total_surgeons_year))
)
# 2015 to 2016, p-value = 0.86
prop.test(
  x = c(df_demographics %>% filter((Billing_Year == 2015) & (Rndrng_Prvdr_RUCA_Desc == 'Metropolitan')) %>% pull(surgeons), 
        df_demographics %>% filter((Billing_Year == 2016) & (Rndrng_Prvdr_RUCA_Desc == 'Metropolitan')) %>% pull(surgeons)),
  n = c(df_demographics %>% filter((Billing_Year == 2015) & (Rndrng_Prvdr_RUCA_Desc == 'Metropolitan')) %>% pull(total_surgeons_year), 
        df_demographics %>% filter((Billing_Year == 2016) & (Rndrng_Prvdr_RUCA_Desc == 'Metropolitan')) %>% pull(total_surgeons_year))
)
# 2016 to 2017, p-value = 1
prop.test(
  x = c(df_demographics %>% filter((Billing_Year == 2016) & (Rndrng_Prvdr_RUCA_Desc == 'Metropolitan')) %>% pull(surgeons), 
        df_demographics %>% filter((Billing_Year == 2017) & (Rndrng_Prvdr_RUCA_Desc == 'Metropolitan')) %>% pull(surgeons)),
  n = c(df_demographics %>% filter((Billing_Year == 2016) & (Rndrng_Prvdr_RUCA_Desc == 'Metropolitan')) %>% pull(total_surgeons_year), 
        df_demographics %>% filter((Billing_Year == 2017) & (Rndrng_Prvdr_RUCA_Desc == 'Metropolitan')) %>% pull(total_surgeons_year))
)
# 2017 to 2018, p-value = 1
prop.test(
  x = c(df_demographics %>% filter((Billing_Year == 2017) & (Rndrng_Prvdr_RUCA_Desc == 'Metropolitan')) %>% pull(surgeons), 
        df_demographics %>% filter((Billing_Year == 2018) & (Rndrng_Prvdr_RUCA_Desc == 'Metropolitan')) %>% pull(surgeons)),
  n = c(df_demographics %>% filter((Billing_Year == 2017) & (Rndrng_Prvdr_RUCA_Desc == 'Metropolitan')) %>% pull(total_surgeons_year), 
        df_demographics %>% filter((Billing_Year == 2018) & (Rndrng_Prvdr_RUCA_Desc == 'Metropolitan')) %>% pull(total_surgeons_year))
)
# 2018 to 2019, p-value = 1
prop.test(
  x = c(df_demographics %>% filter((Billing_Year == 2018) & (Rndrng_Prvdr_RUCA_Desc == 'Metropolitan')) %>% pull(surgeons), 
        df_demographics %>% filter((Billing_Year == 2019) & (Rndrng_Prvdr_RUCA_Desc == 'Metropolitan')) %>% pull(surgeons)),
  n = c(df_demographics %>% filter((Billing_Year == 2018) & (Rndrng_Prvdr_RUCA_Desc == 'Metropolitan')) %>% pull(total_surgeons_year), 
        df_demographics %>% filter((Billing_Year == 2019) & (Rndrng_Prvdr_RUCA_Desc == 'Metropolitan')) %>% pull(total_surgeons_year))
)
# 2019 to 2020, p-value = 1
prop.test(
  x = c(df_demographics %>% filter((Billing_Year == 2019) & (Rndrng_Prvdr_RUCA_Desc == 'Metropolitan')) %>% pull(surgeons), 
        df_demographics %>% filter((Billing_Year == 2020) & (Rndrng_Prvdr_RUCA_Desc == 'Metropolitan')) %>% pull(surgeons)),
  n = c(df_demographics %>% filter((Billing_Year == 2019) & (Rndrng_Prvdr_RUCA_Desc == 'Metropolitan')) %>% pull(total_surgeons_year), 
        df_demographics %>% filter((Billing_Year == 2020) & (Rndrng_Prvdr_RUCA_Desc == 'Metropolitan')) %>% pull(total_surgeons_year))
)
# 2020 to 2021, p-value = 0.1634
prop.test(
  x = c(df_demographics %>% filter((Billing_Year == 2020) & (Rndrng_Prvdr_RUCA_Desc == 'Metropolitan')) %>% pull(surgeons), 
        df_demographics %>% filter((Billing_Year == 2021) & (Rndrng_Prvdr_RUCA_Desc == 'Metropolitan')) %>% pull(surgeons)),
  n = c(df_demographics %>% filter((Billing_Year == 2020) & (Rndrng_Prvdr_RUCA_Desc == 'Metropolitan')) %>% pull(total_surgeons_year), 
        df_demographics %>% filter((Billing_Year == 2021) & (Rndrng_Prvdr_RUCA_Desc == 'Metropolitan')) %>% pull(total_surgeons_year))
)
# 2021 to 2022, p-value = 0.04394
prop.test(
  x = c(df_demographics %>% filter((Billing_Year == 2021) & (Rndrng_Prvdr_RUCA_Desc == 'Metropolitan')) %>% pull(surgeons), 
        df_demographics %>% filter((Billing_Year == 2022) & (Rndrng_Prvdr_RUCA_Desc == 'Metropolitan')) %>% pull(surgeons)),
  n = c(df_demographics %>% filter((Billing_Year == 2021) & (Rndrng_Prvdr_RUCA_Desc == 'Metropolitan')) %>% pull(total_surgeons_year), 
        df_demographics %>% filter((Billing_Year == 2022) & (Rndrng_Prvdr_RUCA_Desc == 'Metropolitan')) %>% pull(total_surgeons_year))
)
# 2022 to 2023, p-value = 1
prop.test(
  x = c(df_demographics %>% filter((Billing_Year == 2022) & (Rndrng_Prvdr_RUCA_Desc == 'Metropolitan')) %>% pull(surgeons), 
        df_demographics %>% filter((Billing_Year == 2023) & (Rndrng_Prvdr_RUCA_Desc == 'Metropolitan')) %>% pull(surgeons)),
  n = c(df_demographics %>% filter((Billing_Year == 2022) & (Rndrng_Prvdr_RUCA_Desc == 'Metropolitan')) %>% pull(total_surgeons_year), 
        df_demographics %>% filter((Billing_Year == 2023) & (Rndrng_Prvdr_RUCA_Desc == 'Metropolitan')) %>% pull(total_surgeons_year))
)

# ---- micro surgeons ----
# two sample propoprtion tests
# 2014 to 2015, p-value = 0.8744
prop.test(
  x = c(df_demographics %>% filter((Billing_Year == 2014) & (Rndrng_Prvdr_RUCA_Desc == 'Micropolitan')) %>% pull(surgeons), 
        df_demographics %>% filter((Billing_Year == 2015) & (Rndrng_Prvdr_RUCA_Desc == 'Micropolitan')) %>% pull(surgeons)),
  n = c(df_demographics %>% filter((Billing_Year == 2014) & (Rndrng_Prvdr_RUCA_Desc == 'Micropolitan')) %>% pull(total_surgeons_year), 
        df_demographics %>% filter((Billing_Year == 2015) & (Rndrng_Prvdr_RUCA_Desc == 'Micropolitan')) %>% pull(total_surgeons_year))
)
# 2015 to 2016, p-value = 0.86
prop.test(
  x = c(df_demographics %>% filter((Billing_Year == 2015) & (Rndrng_Prvdr_RUCA_Desc == 'Micropolitan')) %>% pull(surgeons), 
        df_demographics %>% filter((Billing_Year == 2016) & (Rndrng_Prvdr_RUCA_Desc == 'Micropolitan')) %>% pull(surgeons)),
  n = c(df_demographics %>% filter((Billing_Year == 2015) & (Rndrng_Prvdr_RUCA_Desc == 'Micropolitan')) %>% pull(total_surgeons_year), 
        df_demographics %>% filter((Billing_Year == 2016) & (Rndrng_Prvdr_RUCA_Desc == 'Micropolitan')) %>% pull(total_surgeons_year))
)
# 2016 to 2017, p-value = 1
prop.test(
  x = c(df_demographics %>% filter((Billing_Year == 2016) & (Rndrng_Prvdr_RUCA_Desc == 'Micropolitan')) %>% pull(surgeons), 
        df_demographics %>% filter((Billing_Year == 2017) & (Rndrng_Prvdr_RUCA_Desc == 'Micropolitan')) %>% pull(surgeons)),
  n = c(df_demographics %>% filter((Billing_Year == 2016) & (Rndrng_Prvdr_RUCA_Desc == 'Micropolitan')) %>% pull(total_surgeons_year), 
        df_demographics %>% filter((Billing_Year == 2017) & (Rndrng_Prvdr_RUCA_Desc == 'Micropolitan')) %>% pull(total_surgeons_year))
)
# 2017 to 2018, p-value = 0.9072
prop.test(
  x = c(df_demographics %>% filter((Billing_Year == 2017) & (Rndrng_Prvdr_RUCA_Desc == 'Micropolitan')) %>% pull(surgeons), 
        df_demographics %>% filter((Billing_Year == 2018) & (Rndrng_Prvdr_RUCA_Desc == 'Micropolitan')) %>% pull(surgeons)),
  n = c(df_demographics %>% filter((Billing_Year == 2017) & (Rndrng_Prvdr_RUCA_Desc == 'Micropolitan')) %>% pull(total_surgeons_year), 
        df_demographics %>% filter((Billing_Year == 2018) & (Rndrng_Prvdr_RUCA_Desc == 'Micropolitan')) %>% pull(total_surgeons_year))
)
# 2018 to 2019, p-value = 0.7753
prop.test(
  x = c(df_demographics %>% filter((Billing_Year == 2018) & (Rndrng_Prvdr_RUCA_Desc == 'Micropolitan')) %>% pull(surgeons), 
        df_demographics %>% filter((Billing_Year == 2019) & (Rndrng_Prvdr_RUCA_Desc == 'Micropolitan')) %>% pull(surgeons)),
  n = c(df_demographics %>% filter((Billing_Year == 2018) & (Rndrng_Prvdr_RUCA_Desc == 'Micropolitan')) %>% pull(total_surgeons_year), 
        df_demographics %>% filter((Billing_Year == 2019) & (Rndrng_Prvdr_RUCA_Desc == 'Micropolitan')) %>% pull(total_surgeons_year))
)
# 2019 to 2020, p-value = 1
prop.test(
  x = c(df_demographics %>% filter((Billing_Year == 2019) & (Rndrng_Prvdr_RUCA_Desc == 'Micropolitan')) %>% pull(surgeons), 
        df_demographics %>% filter((Billing_Year == 2020) & (Rndrng_Prvdr_RUCA_Desc == 'Micropolitan')) %>% pull(surgeons)),
  n = c(df_demographics %>% filter((Billing_Year == 2019) & (Rndrng_Prvdr_RUCA_Desc == 'Micropolitan')) %>% pull(total_surgeons_year), 
        df_demographics %>% filter((Billing_Year == 2020) & (Rndrng_Prvdr_RUCA_Desc == 'Micropolitan')) %>% pull(total_surgeons_year))
)
# 2020 to 2021, NO MICROPOLITANS IN 2021
# 2021 to 2022, NO MICROPOLITANS IN 2021
# 2022 to 2023, p-value = 1
prop.test(
  x = c(df_demographics %>% filter((Billing_Year == 2022) & (Rndrng_Prvdr_RUCA_Desc == 'Micropolitan')) %>% pull(surgeons), 
        df_demographics %>% filter((Billing_Year == 2023) & (Rndrng_Prvdr_RUCA_Desc == 'Micropolitan')) %>% pull(surgeons)),
  n = c(df_demographics %>% filter((Billing_Year == 2022) & (Rndrng_Prvdr_RUCA_Desc == 'Micropolitan')) %>% pull(total_surgeons_year), 
        df_demographics %>% filter((Billing_Year == 2023) & (Rndrng_Prvdr_RUCA_Desc == 'Micropolitan')) %>% pull(total_surgeons_year))
)

# ----outpatient services ----
# getting services in metro area
df_2014_outpatient <- df_2014 %>% group_by(NPI) %>% summarize(outpatient_services = sum(Tot_Srvcs[Place_Of_Srvc == 'O']), services = sum(Tot_Srvcs)) %>% ungroup()
df_2015_outpatient <- df_2015 %>% group_by(NPI) %>% summarize(outpatient_services = sum(Tot_Srvcs[Place_Of_Srvc == 'O']), services = sum(Tot_Srvcs)) %>% ungroup()
df_2016_outpatient <- df_2016 %>% group_by(NPI) %>% summarize(outpatient_services = sum(Tot_Srvcs[Place_Of_Srvc == 'O']), services = sum(Tot_Srvcs)) %>% ungroup()
df_2017_outpatient <- df_2017 %>% group_by(NPI) %>% summarize(outpatient_services = sum(Tot_Srvcs[Place_Of_Srvc == 'O']), services = sum(Tot_Srvcs)) %>% ungroup()
df_2018_outpatient <- df_2018 %>% group_by(NPI) %>% summarize(outpatient_services = sum(Tot_Srvcs[Place_Of_Srvc == 'O']), services = sum(Tot_Srvcs)) %>% ungroup()
df_2019_outpatient <- df_2019 %>% group_by(NPI) %>% summarize(outpatient_services = sum(Tot_Srvcs[Place_Of_Srvc == 'O']), services = sum(Tot_Srvcs)) %>% ungroup()
df_2020_outpatient <- df_2020 %>% group_by(NPI) %>% summarize(outpatient_services = sum(Tot_Srvcs[Place_Of_Srvc == 'O']), services = sum(Tot_Srvcs)) %>% ungroup()
df_2021_outpatient <- df_2021 %>% group_by(NPI) %>% summarize(outpatient_services = sum(Tot_Srvcs[Place_Of_Srvc == 'O']), services = sum(Tot_Srvcs)) %>% ungroup()
df_2022_outpatient <- df_2022 %>% group_by(NPI) %>% summarize(outpatient_services = sum(Tot_Srvcs[Place_Of_Srvc == 'O']), services = sum(Tot_Srvcs)) %>% ungroup()
df_2023_outpatient <- df_2023 %>% group_by(NPI) %>% summarize(outpatient_services = sum(Tot_Srvcs[Place_Of_Srvc == 'O']), services = sum(Tot_Srvcs)) %>% ungroup()

# two sample proportion tests
# 2014 to 2015, p-value = 2.2e-16
prop.test(
  x = c(sum(df_2014_outpatient$outpatient_services), sum(df_2015_outpatient$outpatient_services)),
  n = c(sum(df_2014_outpatient$services), sum(df_2015_outpatient$services))
)
# 2015 to 2016, p-value = 2.2e-16
prop.test(
  x = c(sum(df_2015_outpatient$outpatient_services), sum(df_2016_outpatient$outpatient_services)),
  n = c(sum(df_2015_outpatient$services), sum(df_2016_outpatient$services))
)
# 2016 to 2017, p-value = 9.583e-16
prop.test(
  x = c(sum(df_2016_outpatient$outpatient_services), sum(df_2017_outpatient$outpatient_services)),
  n = c(sum(df_2016_outpatient$services), sum(df_2017_outpatient$services))
)
# 2017 to 2018, p-value = 2.2e-16
prop.test(
  x = c(sum(df_2017_outpatient$outpatient_services), sum(df_2018_outpatient$outpatient_services)),
  n = c(sum(df_2017_outpatient$services), sum(df_2018_outpatient$services))
)
# 2018 to 2019, p-value = 2.2e-16
prop.test(
  x = c(sum(df_2018_outpatient$outpatient_services), sum(df_2019_outpatient$outpatient_services)),
  n = c(sum(df_2018_outpatient$services), sum(df_2019_outpatient$services))
)
# 2019 to 2020, p-value = 2.2e-16
prop.test(
  x = c(sum(df_2019_outpatient$outpatient_services), sum(df_2020_outpatient$outpatient_services)),
  n = c(sum(df_2019_outpatient$services), sum(df_2020_outpatient$services))
)
# 2020 to 2021, p-value = 2.2e-16
prop.test(
  x = c(sum(df_2020_outpatient$outpatient_services), sum(df_2021_outpatient$outpatient_services)),
  n = c(sum(df_2020_outpatient$services), sum(df_2021_outpatient$services))
)
# 2021 to 2022, p-value = 0.007963
prop.test(
  x = c(sum(df_2021_outpatient$outpatient_services), sum(df_2022_outpatient$outpatient_services)),
  n = c(sum(df_2021_outpatient$services), sum(df_2022_outpatient$services))
)
# 2022 to 2023, p-value = 2.2e-16
prop.test(
  x = c(sum(df_2022_outpatient$outpatient_services), sum(df_2023_outpatient$outpatient_services)),
  n = c(sum(df_2022_outpatient$services), sum(df_2023_outpatient$services))
)

# ---- outpatient surgeons ----
# two sample proportion tests
# 2014 to 2015, p-value = 0.3405
prop.test(
  x = c(df_office %>% filter((Billing_Year == 2014) & (Place_Of_Srvc == 'O')) %>% pull(surgeons), 
        df_office %>% filter((Billing_Year == 2015) & (Place_Of_Srvc == 'O')) %>% pull(surgeons)),
  n = c(df_office %>% filter((Billing_Year == 2014) & (Place_Of_Srvc == 'O')) %>% pull(total_surgeons_year), 
        df_office %>% filter((Billing_Year == 2015) & (Place_Of_Srvc == 'O')) %>% pull(total_surgeons_year))
)
# 2015 to 2016, p-value = 0.121
prop.test(
  x = c(df_office %>% filter((Billing_Year == 2015) & (Place_Of_Srvc == 'O')) %>% pull(surgeons), 
        df_office %>% filter((Billing_Year == 2016) & (Place_Of_Srvc == 'O')) %>% pull(surgeons)),
  n = c(df_office %>% filter((Billing_Year == 2015) & (Place_Of_Srvc == 'O')) %>% pull(total_surgeons_year), 
        df_office %>% filter((Billing_Year == 2016) & (Place_Of_Srvc == 'O')) %>% pull(total_surgeons_year))
)
# 2016 to 2017, p-value = 0.6204
prop.test(
  x = c(df_office %>% filter((Billing_Year == 2016) & (Place_Of_Srvc == 'O')) %>% pull(surgeons), 
        df_office %>% filter((Billing_Year == 2017) & (Place_Of_Srvc == 'O')) %>% pull(surgeons)),
  n = c(df_office %>% filter((Billing_Year == 2016) & (Place_Of_Srvc == 'O')) %>% pull(total_surgeons_year), 
        df_office %>% filter((Billing_Year == 2017) & (Place_Of_Srvc == 'O')) %>% pull(total_surgeons_year))
)
# 2017 to 2018, p-value = 0.7953
prop.test(
  x = c(df_office %>% filter((Billing_Year == 2017) & (Place_Of_Srvc == 'O')) %>% pull(surgeons), 
        df_office %>% filter((Billing_Year == 2018) & (Place_Of_Srvc == 'O')) %>% pull(surgeons)),
  n = c(df_office %>% filter((Billing_Year == 2017) & (Place_Of_Srvc == 'O')) %>% pull(total_surgeons_year), 
        df_office %>% filter((Billing_Year == 2018) & (Place_Of_Srvc == 'O')) %>% pull(total_surgeons_year))
)
# 2018 to 2019, p-value = 0.1807
prop.test(
  x = c(df_office %>% filter((Billing_Year == 2018) & (Place_Of_Srvc == 'O')) %>% pull(surgeons), 
        df_office %>% filter((Billing_Year == 2019) & (Place_Of_Srvc == 'O')) %>% pull(surgeons)),
  n = c(df_office %>% filter((Billing_Year == 2018) & (Place_Of_Srvc == 'O')) %>% pull(total_surgeons_year), 
        df_office %>% filter((Billing_Year == 2019) & (Place_Of_Srvc == 'O')) %>% pull(total_surgeons_year))
)
# 2019 to 2020, p-value = 0.1861
prop.test(
  x = c(df_office %>% filter((Billing_Year == 2019) & (Place_Of_Srvc == 'O')) %>% pull(surgeons), 
        df_office %>% filter((Billing_Year == 2020) & (Place_Of_Srvc == 'O')) %>% pull(surgeons)),
  n = c(df_office %>% filter((Billing_Year == 2019) & (Place_Of_Srvc == 'O')) %>% pull(total_surgeons_year), 
        df_office %>% filter((Billing_Year == 2020) & (Place_Of_Srvc == 'O')) %>% pull(total_surgeons_year))
)
# 2020 to 2021, p-value = 0.3504
prop.test(
  x = c(df_office %>% filter((Billing_Year == 2020) & (Place_Of_Srvc == 'O')) %>% pull(surgeons), 
        df_office %>% filter((Billing_Year == 2021) & (Place_Of_Srvc == 'O')) %>% pull(surgeons)),
  n = c(df_office %>% filter((Billing_Year == 2020) & (Place_Of_Srvc == 'O')) %>% pull(total_surgeons_year), 
        df_office %>% filter((Billing_Year == 2021) & (Place_Of_Srvc == 'O')) %>% pull(total_surgeons_year))
)
# 2021 to 2022, p-value = 0.8786
prop.test(
  x = c(df_office %>% filter((Billing_Year == 2021) & (Place_Of_Srvc == 'O')) %>% pull(surgeons), 
        df_office %>% filter((Billing_Year == 2022) & (Place_Of_Srvc == 'O')) %>% pull(surgeons)),
  n = c(df_office %>% filter((Billing_Year == 2021) & (Place_Of_Srvc == 'O')) %>% pull(total_surgeons_year), 
        df_office %>% filter((Billing_Year == 2022) & (Place_Of_Srvc == 'O')) %>% pull(total_surgeons_year))
)
# 2022 to 2023, p-value = 0.1618
prop.test(
  x = c(df_office %>% filter((Billing_Year == 2022) & (Place_Of_Srvc == 'O')) %>% pull(surgeons), 
        df_office %>% filter((Billing_Year == 2023) & (Place_Of_Srvc == 'O')) %>% pull(surgeons)),
  n = c(df_office %>% filter((Billing_Year == 2022) & (Place_Of_Srvc == 'O')) %>% pull(total_surgeons_year), 
        df_office %>% filter((Billing_Year == 2023) & (Place_Of_Srvc == 'O')) %>% pull(total_surgeons_year))
)

