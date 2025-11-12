library(tidyr)
library(dplyr)
library(ggplot2)

# ---- loading in data ----
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


# ---- metropolitan/micropolitan ----
### metro services ###
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

### micro services ###
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

### metro surgeons ###
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

### micro surgeons ###
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



# ---- outpatient/inpatient ----
### outpatient services ###
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

### outpatient surgeons ###
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

# ---- total codes per surgeon t test ----
# getting total codes for each surgeon in a year
df_2014_total_codes <- df_2014 %>% group_by(NPI) %>% summarize(services = sum(Tot_Srvcs)) %>% ungroup()
df_2015_total_codes <- df_2015 %>% group_by(NPI) %>% summarize(services = sum(Tot_Srvcs)) %>% ungroup()
df_2016_total_codes <- df_2016 %>% group_by(NPI) %>% summarize(services = sum(Tot_Srvcs)) %>% ungroup()
df_2017_total_codes <- df_2017 %>% group_by(NPI) %>% summarize(services = sum(Tot_Srvcs)) %>% ungroup()
df_2018_total_codes <- df_2018 %>% group_by(NPI) %>% summarize(services = sum(Tot_Srvcs)) %>% ungroup()
df_2019_total_codes <- df_2019 %>% group_by(NPI) %>% summarize(services = sum(Tot_Srvcs)) %>% ungroup()
df_2020_total_codes <- df_2020 %>% group_by(NPI) %>% summarize(services = sum(Tot_Srvcs)) %>% ungroup()
df_2021_total_codes <- df_2021 %>% group_by(NPI) %>% summarize(services = sum(Tot_Srvcs)) %>% ungroup()
df_2022_total_codes <- df_2022 %>% group_by(NPI) %>% summarize(services = sum(Tot_Srvcs)) %>% ungroup()
df_2023_total_codes <- df_2023 %>% group_by(NPI) %>% summarize(services = sum(Tot_Srvcs)) %>% ungroup()

# t tests
# 2014 to 2015, p-value = 0.7671
t.test(df_2014_total_codes$services, df_2015_total_codes$services)
# 2015 to 2016, p-value = 0.8674
t.test(df_2015_total_codes$services, df_2016_total_codes$services)
# 2016 to 2017, p-value = 0.1442
t.test(df_2016_total_codes$services, df_2017_total_codes$services)
# 2017 to 2018, p-value = 0.06189
t.test(df_2017_total_codes$services, df_2018_total_codes$services)
# 2018 to 2019, p-value = 0.3709
t.test(df_2018_total_codes$services, df_2019_total_codes$services)
# 2019 to 2020, p-value = 0.6457
t.test(df_2019_total_codes$services, df_2020_total_codes$services)
# 2020 to 2021, p-value = 0.3229
t.test(df_2020_total_codes$services, df_2021_total_codes$services)
# 2021 to 2022, p-value = 0.4009
t.test(df_2021_total_codes$services, df_2022_total_codes$services)
# 2022 to 2023, p-value = 0.7787
t.test(df_2022_total_codes$services, df_2023_total_codes$services)

# ---- individual codes per surgeon t test ----
# getting individual codes for each surgeon in a year
df_2014_individ_codes <- df_2014 %>% group_by(NPI, HCPCS_Cd) %>% summarize(services = sum(Tot_Srvcs)) %>% ungroup()
df_2015_individ_codes <- df_2015 %>% group_by(NPI, HCPCS_Cd) %>% summarize(services = sum(Tot_Srvcs)) %>% ungroup()
df_2016_individ_codes <- df_2016 %>% group_by(NPI, HCPCS_Cd) %>% summarize(services = sum(Tot_Srvcs)) %>% ungroup()
df_2017_individ_codes <- df_2017 %>% group_by(NPI, HCPCS_Cd) %>% summarize(services = sum(Tot_Srvcs)) %>% ungroup()
df_2018_individ_codes <- df_2018 %>% group_by(NPI, HCPCS_Cd) %>% summarize(services = sum(Tot_Srvcs)) %>% ungroup()
df_2019_individ_codes <- df_2019 %>% group_by(NPI, HCPCS_Cd) %>% summarize(services = sum(Tot_Srvcs)) %>% ungroup()
df_2020_individ_codes <- df_2020 %>% group_by(NPI, HCPCS_Cd) %>% summarize(services = sum(Tot_Srvcs)) %>% ungroup()
df_2021_individ_codes <- df_2021 %>% group_by(NPI, HCPCS_Cd) %>% summarize(services = sum(Tot_Srvcs)) %>% ungroup()
df_2022_individ_codes <- df_2022 %>% group_by(NPI, HCPCS_Cd) %>% summarize(services = sum(Tot_Srvcs)) %>% ungroup()
df_2023_individ_codes <- df_2023 %>% group_by(NPI, HCPCS_Cd) %>% summarize(services = sum(Tot_Srvcs)) %>% ungroup()

# t tests
### code 17311 ###
# 2014 to 2015, p-value = 0.9153
t.test(df_2014_individ_codes %>% filter(HCPCS_Cd == 17311) %>% select(services), df_2015_individ_codes %>% filter(HCPCS_Cd == 17311) %>% select(services))
# 2015 to 2016, p-value = 0.8156
t.test(df_2015_individ_codes %>% filter(HCPCS_Cd == 17311) %>% select(services), df_2016_individ_codes %>% filter(HCPCS_Cd == 17311) %>% select(services))
# 2016 to 2017, p-value = 0.1842
t.test(df_2016_individ_codes %>% filter(HCPCS_Cd == 17311) %>% select(services), df_2017_individ_codes %>% filter(HCPCS_Cd == 17311) %>% select(services))
# 2017 to 2018, p-value = 0.2316
t.test(df_2017_individ_codes %>% filter(HCPCS_Cd == 17311) %>% select(services), df_2018_individ_codes %>% filter(HCPCS_Cd == 17311) %>% select(services))
# 2018 to 2019, p-value = 0.5904
t.test(df_2018_individ_codes %>% filter(HCPCS_Cd == 17311) %>% select(services), df_2019_individ_codes %>% filter(HCPCS_Cd == 17311) %>% select(services))
# 2019 to 2020, p-value = 0.6716
t.test(df_2019_individ_codes %>% filter(HCPCS_Cd == 17311) %>% select(services), df_2020_individ_codes %>% filter(HCPCS_Cd == 17311) %>% select(services))
# 2020 to 2021, p-value = 0.2861
t.test(df_2020_individ_codes %>% filter(HCPCS_Cd == 17311) %>% select(services), df_2021_individ_codes %>% filter(HCPCS_Cd == 17311) %>% select(services))
# 2021 to 2022, p-value = 0.2606
t.test(df_2021_individ_codes %>% filter(HCPCS_Cd == 17311) %>% select(services), df_2022_individ_codes %>% filter(HCPCS_Cd == 17311) %>% select(services))
# 2022 to 2023, p-value = 0.9355
t.test(df_2022_individ_codes %>% filter(HCPCS_Cd == 17311) %>% select(services), df_2023_individ_codes %>% filter(HCPCS_Cd == 17311) %>% select(services))
       
### code 17312 ###
# 2014 to 2015, p-value = 0.4351
t.test(df_2014_individ_codes %>% filter(HCPCS_Cd == 17312) %>% select(services), df_2015_individ_codes %>% filter(HCPCS_Cd == 17312) %>% select(services))
# 2015 to 2016, p-value = 0.9886
t.test(df_2015_individ_codes %>% filter(HCPCS_Cd == 17312) %>% select(services), df_2016_individ_codes %>% filter(HCPCS_Cd == 17312) %>% select(services))
# 2016 to 2017, p-value = 0.3247
t.test(df_2016_individ_codes %>% filter(HCPCS_Cd == 17312) %>% select(services), df_2017_individ_codes %>% filter(HCPCS_Cd == 17312) %>% select(services))
# 2017 to 2018, p-value = 0.02277
t.test(df_2017_individ_codes %>% filter(HCPCS_Cd == 17312) %>% select(services), df_2018_individ_codes %>% filter(HCPCS_Cd == 17312) %>% select(services))
# 2018 to 2019, p-value = 0.2231
t.test(df_2018_individ_codes %>% filter(HCPCS_Cd == 17312) %>% select(services), df_2019_individ_codes %>% filter(HCPCS_Cd == 17312) %>% select(services))
# 2019 to 2020, p-value = 0.8845
t.test(df_2019_individ_codes %>% filter(HCPCS_Cd == 17312) %>% select(services), df_2020_individ_codes %>% filter(HCPCS_Cd == 17312) %>% select(services))
# 2020 to 2021, p-value = 0.09505
t.test(df_2020_individ_codes %>% filter(HCPCS_Cd == 17312) %>% select(services), df_2021_individ_codes %>% filter(HCPCS_Cd == 17312) %>% select(services))
# 2021 to 2022, p-value = 0.6462
t.test(df_2021_individ_codes %>% filter(HCPCS_Cd == 17312) %>% select(services), df_2022_individ_codes %>% filter(HCPCS_Cd == 17312) %>% select(services))
# 2022 to 2023, p-value = 0.6582
t.test(df_2022_individ_codes %>% filter(HCPCS_Cd == 17312) %>% select(services), df_2023_individ_codes %>% filter(HCPCS_Cd == 17312) %>% select(services))

### code 17313 ###
# 2014 to 2015, p-value = 0.4835
t.test(df_2014_individ_codes %>% filter(HCPCS_Cd == 17313) %>% select(services), df_2015_individ_codes %>% filter(HCPCS_Cd == 17313) %>% select(services))
# 2015 to 2016, p-value = 0.6405
t.test(df_2015_individ_codes %>% filter(HCPCS_Cd == 17313) %>% select(services), df_2016_individ_codes %>% filter(HCPCS_Cd == 17313) %>% select(services))
# 2016 to 2017, p-value = 0.0609
t.test(df_2016_individ_codes %>% filter(HCPCS_Cd == 17313) %>% select(services), df_2017_individ_codes %>% filter(HCPCS_Cd == 17313) %>% select(services))
# 2017 to 2018, p-value = 0.06884
t.test(df_2017_individ_codes %>% filter(HCPCS_Cd == 17313) %>% select(services), df_2018_individ_codes %>% filter(HCPCS_Cd == 17313) %>% select(services))
# 2018 to 2019, p-value = 0.2977
t.test(df_2018_individ_codes %>% filter(HCPCS_Cd == 17313) %>% select(services), df_2019_individ_codes %>% filter(HCPCS_Cd == 17313) %>% select(services))
# 2019 to 2020, p-value = 0.2546
t.test(df_2019_individ_codes %>% filter(HCPCS_Cd == 17313) %>% select(services), df_2020_individ_codes %>% filter(HCPCS_Cd == 17313) %>% select(services))
# 2020 to 2021, p-value = 0.342
t.test(df_2020_individ_codes %>% filter(HCPCS_Cd == 17313) %>% select(services), df_2021_individ_codes %>% filter(HCPCS_Cd == 17313) %>% select(services))
# 2021 to 2022, p-value = 0.08755
t.test(df_2021_individ_codes %>% filter(HCPCS_Cd == 17313) %>% select(services), df_2022_individ_codes %>% filter(HCPCS_Cd == 17313) %>% select(services))
# 2022 to 2023, p-value = 0.7411
t.test(df_2022_individ_codes %>% filter(HCPCS_Cd == 17313) %>% select(services), df_2023_individ_codes %>% filter(HCPCS_Cd == 17313) %>% select(services))

### code 17314 ###
# 2014 to 2015, p-value = 0.7315
t.test(df_2014_individ_codes %>% filter(HCPCS_Cd == 17314) %>% select(services), df_2015_individ_codes %>% filter(HCPCS_Cd == 17314) %>% select(services))
# 2015 to 2016, p-value = 0.196
t.test(df_2015_individ_codes %>% filter(HCPCS_Cd == 17314) %>% select(services), df_2016_individ_codes %>% filter(HCPCS_Cd == 17314) %>% select(services))
# 2016 to 2017, p-value = 0.1531
t.test(df_2016_individ_codes %>% filter(HCPCS_Cd == 17314) %>% select(services), df_2017_individ_codes %>% filter(HCPCS_Cd == 17314) %>% select(services))
# 2017 to 2018, p-value = 0.0813
t.test(df_2017_individ_codes %>% filter(HCPCS_Cd == 17314) %>% select(services), df_2018_individ_codes %>% filter(HCPCS_Cd == 17314) %>% select(services))
# 2018 to 2019, p-value = 0.4059
t.test(df_2018_individ_codes %>% filter(HCPCS_Cd == 17314) %>% select(services), df_2019_individ_codes %>% filter(HCPCS_Cd == 17314) %>% select(services))
# 2019 to 2020, p-value = 0.2887
t.test(df_2019_individ_codes %>% filter(HCPCS_Cd == 17314) %>% select(services), df_2020_individ_codes %>% filter(HCPCS_Cd == 17314) %>% select(services))
# 2020 to 2021, p-value = 0.5273
t.test(df_2020_individ_codes %>% filter(HCPCS_Cd == 17314) %>% select(services), df_2021_individ_codes %>% filter(HCPCS_Cd == 17314) %>% select(services))
# 2021 to 2022, p-value = 0.2415
t.test(df_2021_individ_codes %>% filter(HCPCS_Cd == 17314) %>% select(services), df_2022_individ_codes %>% filter(HCPCS_Cd == 17314) %>% select(services))
# 2022 to 2023, p-value = 0.198
t.test(df_2022_individ_codes %>% filter(HCPCS_Cd == 17314) %>% select(services), df_2023_individ_codes %>% filter(HCPCS_Cd == 17314) %>% select(services))

# ---- grouped codes per surgeon t tests ---- 
# ---- for codes  17311 and 17312 -----
df_2014_grouped_codes1 <- df_2014 %>% filter(HCPCS_Cd %in% c(17311, 17312)) %>% group_by(NPI) %>% summarize(services = sum(Tot_Srvcs)) %>% ungroup()
df_2015_grouped_codes1 <- df_2015 %>% filter(HCPCS_Cd %in% c(17311, 17312)) %>% group_by(NPI) %>% summarize(services = sum(Tot_Srvcs)) %>% ungroup()
df_2016_grouped_codes1 <- df_2016 %>% filter(HCPCS_Cd %in% c(17311, 17312)) %>% group_by(NPI) %>% summarize(services = sum(Tot_Srvcs)) %>% ungroup()
df_2017_grouped_codes1 <- df_2017 %>% filter(HCPCS_Cd %in% c(17311, 17312)) %>% group_by(NPI) %>% summarize(services = sum(Tot_Srvcs)) %>% ungroup()
df_2018_grouped_codes1 <- df_2018 %>% filter(HCPCS_Cd %in% c(17311, 17312)) %>% group_by(NPI) %>% summarize(services = sum(Tot_Srvcs)) %>% ungroup()
df_2019_grouped_codes1 <- df_2019 %>% filter(HCPCS_Cd %in% c(17311, 17312)) %>% group_by(NPI) %>% summarize(services = sum(Tot_Srvcs)) %>% ungroup()
df_2020_grouped_codes1 <- df_2020 %>% filter(HCPCS_Cd %in% c(17311, 17312)) %>% group_by(NPI) %>% summarize(services = sum(Tot_Srvcs)) %>% ungroup()
df_2021_grouped_codes1 <- df_2021 %>% filter(HCPCS_Cd %in% c(17311, 17312)) %>% group_by(NPI) %>% summarize(services = sum(Tot_Srvcs)) %>% ungroup()
df_2022_grouped_codes1 <- df_2022 %>% filter(HCPCS_Cd %in% c(17311, 17312)) %>% group_by(NPI) %>% summarize(services = sum(Tot_Srvcs)) %>% ungroup()
df_2023_grouped_codes1 <- df_2023 %>% filter(HCPCS_Cd %in% c(17311, 17312)) %>% group_by(NPI) %>% summarize(services = sum(Tot_Srvcs)) %>% ungroup()

# t tests
### codes 17311 and 17312 ###
# 2014 to 2015, p-value = 0.7249
t.test(df_2014_grouped_codes1$services, df_2015_grouped_codes1$services)
# 2015 to 2016, p-value = 0.8579
t.test(df_2015_grouped_codes1$services, df_2016_grouped_codes1$services)
# 2016 to 2017, p-value = 0.2028
t.test(df_2016_grouped_codes1$services, df_2017_grouped_codes1$services)
# 2017 to 2018, p-value = 0.08322
t.test(df_2017_grouped_codes1$services, df_2018_grouped_codes1$services)
# 2018 to 2019, p-value = 0.4194
t.test(df_2018_grouped_codes1$services, df_2019_grouped_codes1$services)
# 2019 to 2020, p-value = 0.7145
t.test(df_2019_grouped_codes1$services, df_2020_grouped_codes1$services)
# 2020 to 2021, p-value = 0.1973
t.test(df_2020_grouped_codes1$services, df_2021_grouped_codes1$services)
# 2021 to 2022, p-value = 0.4094
t.test(df_2021_grouped_codes1$services, df_2022_grouped_codes1$services)
# 2022 to 2023, p-value = 0.7932
t.test(df_2022_grouped_codes1$services, df_2023_grouped_codes1$services)

# for codes  17313 and 17314
df_2014_grouped_codes2 <- df_2014 %>% filter(HCPCS_Cd %in% c(17313, 17314)) %>% group_by(NPI) %>% summarize(services = sum(Tot_Srvcs)) %>% ungroup()
df_2015_grouped_codes2 <- df_2015 %>% filter(HCPCS_Cd %in% c(17313, 17314)) %>% group_by(NPI) %>% summarize(services = sum(Tot_Srvcs)) %>% ungroup()
df_2016_grouped_codes2 <- df_2016 %>% filter(HCPCS_Cd %in% c(17313, 17314)) %>% group_by(NPI) %>% summarize(services = sum(Tot_Srvcs)) %>% ungroup()
df_2017_grouped_codes2 <- df_2017 %>% filter(HCPCS_Cd %in% c(17313, 17314)) %>% group_by(NPI) %>% summarize(services = sum(Tot_Srvcs)) %>% ungroup()
df_2018_grouped_codes2 <- df_2018 %>% filter(HCPCS_Cd %in% c(17313, 17314)) %>% group_by(NPI) %>% summarize(services = sum(Tot_Srvcs)) %>% ungroup()
df_2019_grouped_codes2 <- df_2019 %>% filter(HCPCS_Cd %in% c(17313, 17314)) %>% group_by(NPI) %>% summarize(services = sum(Tot_Srvcs)) %>% ungroup()
df_2020_grouped_codes2 <- df_2020 %>% filter(HCPCS_Cd %in% c(17313, 17314)) %>% group_by(NPI) %>% summarize(services = sum(Tot_Srvcs)) %>% ungroup()
df_2021_grouped_codes2 <- df_2021 %>% filter(HCPCS_Cd %in% c(17313, 17314)) %>% group_by(NPI) %>% summarize(services = sum(Tot_Srvcs)) %>% ungroup()
df_2022_grouped_codes2 <- df_2022 %>% filter(HCPCS_Cd %in% c(17313, 17314)) %>% group_by(NPI) %>% summarize(services = sum(Tot_Srvcs)) %>% ungroup()
df_2023_grouped_codes2 <- df_2023 %>% filter(HCPCS_Cd %in% c(17313, 17314)) %>% group_by(NPI) %>% summarize(services = sum(Tot_Srvcs)) %>% ungroup()

### codes 17313 and 17314 ###
# 2014 to 2015, p-value = 0.7356
t.test(df_2014_grouped_codes2$services, df_2015_grouped_codes2$services)
# 2015 to 2016, p-value = 0.7236
t.test(df_2015_grouped_codes2$services, df_2016_grouped_codes2$services)
# 2016 to 2017, p-value = 0.09282
t.test(df_2016_grouped_codes2$services, df_2017_grouped_codes2$services)
# 2017 to 2018, p-value = 0.03305
t.test(df_2017_grouped_codes2$services, df_2018_grouped_codes2$services)
# 2018 to 2019, p-value = 0.2001
t.test(df_2018_grouped_codes2$services, df_2019_grouped_codes2$services)
# 2019 to 2020, p-value = 0.2438
t.test(df_2019_grouped_codes2$services, df_2020_grouped_codes2$services)
# 2020 to 2021, p-value = 0.4194
t.test(df_2020_grouped_codes2$services, df_2021_grouped_codes2$services)
# 2021 to 2022, p-value = 0.1825
t.test(df_2021_grouped_codes2$services, df_2022_grouped_codes2$services)
# 2022 to 2023, p-value = 0.9226
t.test(df_2022_grouped_codes2$services, df_2023_grouped_codes2$services)

# ---- metropolitan codes per surgeon ----

# getting total codes for each surgeon in a year
df_2014_total_codes_area <- df_2014 %>% filter(Rndrng_Prvdr_RUCA_Desc == 'Metropolitan') %>% group_by(NPI) %>% summarize(services = sum(Tot_Srvcs)) %>% ungroup()
df_2015_total_codes_area <- df_2015 %>% filter(Rndrng_Prvdr_RUCA_Desc == 'Metropolitan') %>% group_by(NPI) %>% summarize(services = sum(Tot_Srvcs)) %>% ungroup()
df_2016_total_codes_area <- df_2016 %>% filter(Rndrng_Prvdr_RUCA_Desc == 'Metropolitan') %>% group_by(NPI) %>% summarize(services = sum(Tot_Srvcs)) %>% ungroup()
df_2017_total_codes_area <- df_2017 %>% filter(Rndrng_Prvdr_RUCA_Desc == 'Metropolitan') %>% group_by(NPI) %>% summarize(services = sum(Tot_Srvcs)) %>% ungroup()
df_2018_total_codes_area <- df_2018 %>% filter(Rndrng_Prvdr_RUCA_Desc == 'Metropolitan') %>% group_by(NPI) %>% summarize(services = sum(Tot_Srvcs)) %>% ungroup()
df_2019_total_codes_area <- df_2019 %>% filter(Rndrng_Prvdr_RUCA_Desc == 'Metropolitan') %>% group_by(NPI) %>% summarize(services = sum(Tot_Srvcs)) %>% ungroup()
df_2020_total_codes_area <- df_2020 %>% filter(Rndrng_Prvdr_RUCA_Desc == 'Metropolitan') %>% group_by(NPI) %>% summarize(services = sum(Tot_Srvcs)) %>% ungroup()
df_2021_total_codes_area <- df_2021 %>% filter(Rndrng_Prvdr_RUCA_Desc == 'Metropolitan') %>% group_by(NPI) %>% summarize(services = sum(Tot_Srvcs)) %>% ungroup()
df_2022_total_codes_area <- df_2022 %>% filter(Rndrng_Prvdr_RUCA_Desc == 'Metropolitan') %>% group_by(NPI) %>% summarize(services = sum(Tot_Srvcs)) %>% ungroup()
df_2023_total_codes_area <- df_2023 %>% filter(Rndrng_Prvdr_RUCA_Desc == 'Metropolitan') %>% group_by(NPI) %>% summarize(services = sum(Tot_Srvcs)) %>% ungroup()

# t tests
# 2014 to 2015, p-value = 0.876
t.test(df_2014_total_codes_area$services, df_2015_total_codes_area$services)
# 2015 to 2016, p-value = 0.8406
t.test(df_2015_total_codes_area$services, df_2016_total_codes_area$services)
# 2016 to 2017, p-value = 0.2316
t.test(df_2016_total_codes_area$services, df_2017_total_codes_area$services)
# 2017 to 2018, p-value = 0.1015
t.test(df_2017_total_codes_area$services, df_2018_total_codes_area$services)
# 2018 to 2019, p-value = 0.4689
t.test(df_2018_total_codes_area$services, df_2019_total_codes_area$services)
# 2019 to 2020, p-value = 0.776
t.test(df_2019_total_codes_area$services, df_2020_total_codes_area$services)
# 2020 to 2021, p-value = 0.3704
t.test(df_2020_total_codes_area$services, df_2021_total_codes_area$services)
# 2021 to 2022, p-value = 0.3398
t.test(df_2021_total_codes_area$services, df_2022_total_codes_area$services)
# 2022 to 2023, p-value = 0.9
t.test(df_2022_total_codes_area$services, df_2023_total_codes_area$services)

# ---- micropolitan codes per surgeon ----

# getting total codes for each surgeon in a year
df_2014_total_codes_area_micro <- df_2014 %>% filter(Rndrng_Prvdr_RUCA_Desc == 'Micropolitan') %>% group_by(NPI) %>% summarize(services = sum(Tot_Srvcs)) %>% ungroup()
df_2015_total_codes_area_micro <- df_2015 %>% filter(Rndrng_Prvdr_RUCA_Desc == 'Micropolitan') %>% group_by(NPI) %>% summarize(services = sum(Tot_Srvcs)) %>% ungroup()
df_2016_total_codes_area_micro <- df_2016 %>% filter(Rndrng_Prvdr_RUCA_Desc == 'Micropolitan') %>% group_by(NPI) %>% summarize(services = sum(Tot_Srvcs)) %>% ungroup()
df_2017_total_codes_area_micro <- df_2017 %>% filter(Rndrng_Prvdr_RUCA_Desc == 'Micropolitan') %>% group_by(NPI) %>% summarize(services = sum(Tot_Srvcs)) %>% ungroup()
df_2018_total_codes_area_micro <- df_2018 %>% filter(Rndrng_Prvdr_RUCA_Desc == 'Micropolitan') %>% group_by(NPI) %>% summarize(services = sum(Tot_Srvcs)) %>% ungroup()
df_2019_total_codes_area_micro <- df_2019 %>% filter(Rndrng_Prvdr_RUCA_Desc == 'Micropolitan') %>% group_by(NPI) %>% summarize(services = sum(Tot_Srvcs)) %>% ungroup()
df_2020_total_codes_area_micro <- df_2020 %>% filter(Rndrng_Prvdr_RUCA_Desc == 'Micropolitan') %>% group_by(NPI) %>% summarize(services = sum(Tot_Srvcs)) %>% ungroup()
df_2022_total_codes_area_micro <- df_2022 %>% filter(Rndrng_Prvdr_RUCA_Desc == 'Micropolitan') %>% group_by(NPI) %>% summarize(services = sum(Tot_Srvcs)) %>% ungroup()
df_2023_total_codes_area_micro <- df_2023 %>% filter(Rndrng_Prvdr_RUCA_Desc == 'Micropolitan') %>% group_by(NPI) %>% summarize(services = sum(Tot_Srvcs)) %>% ungroup()

# t tests
# 2014 to 2015, p-value = 0.119
t.test(df_2014_total_codes_area_micro$services, df_2015_total_codes_area_micro$services)
# 2015 to 2016, p-value = 0.8354
t.test(df_2015_total_codes_area_micro$services, df_2016_total_codes_area_micro$services)
# 2016 to 2017, p-value = 0.3348
t.test(df_2016_total_codes_area_micro$services, df_2017_total_codes_area_micro$services)
# 2017 to 2018, p-value = 0.2567
t.test(df_2017_total_codes_area_micro$services, df_2018_total_codes_area_micro$services)
# 2018 to 2019, p-value = 0.3396
t.test(df_2018_total_codes_area_micro$services, df_2019_total_codes_area_micro$services)
# 2019 to 2020, p-value = 0.5205
t.test(df_2019_total_codes_area_micro$services, df_2020_total_codes_area_micro$services)
# 2022 to 2023, p-value = 0.05174
t.test(df_2022_total_codes_area_micro$services, df_2023_total_codes_area_micro$services)

# ---- outpatient codes per surgeon ----

# getting total codes for each surgeon in a year
df_2014_total_codes_office <- df_2014 %>% filter(Place_Of_Srvc == 'O') %>% group_by(NPI) %>% summarize(services = sum(Tot_Srvcs)) %>% ungroup()
df_2015_total_codes_office <- df_2015 %>% filter(Place_Of_Srvc == 'O') %>% group_by(NPI) %>% summarize(services = sum(Tot_Srvcs)) %>% ungroup()
df_2016_total_codes_office <- df_2016 %>% filter(Place_Of_Srvc == 'O') %>% group_by(NPI) %>% summarize(services = sum(Tot_Srvcs)) %>% ungroup()
df_2017_total_codes_office <- df_2017 %>% filter(Place_Of_Srvc == 'O') %>% group_by(NPI) %>% summarize(services = sum(Tot_Srvcs)) %>% ungroup()
df_2018_total_codes_office <- df_2018 %>% filter(Place_Of_Srvc == 'O') %>% group_by(NPI) %>% summarize(services = sum(Tot_Srvcs)) %>% ungroup()
df_2019_total_codes_office <- df_2019 %>% filter(Place_Of_Srvc == 'O') %>% group_by(NPI) %>% summarize(services = sum(Tot_Srvcs)) %>% ungroup()
df_2020_total_codes_office <- df_2020 %>% filter(Place_Of_Srvc == 'O') %>% group_by(NPI) %>% summarize(services = sum(Tot_Srvcs)) %>% ungroup()
df_2021_total_codes_office <- df_2021 %>% filter(Place_Of_Srvc == 'O') %>% group_by(NPI) %>% summarize(services = sum(Tot_Srvcs)) %>% ungroup()
df_2022_total_codes_office <- df_2022 %>% filter(Place_Of_Srvc == 'O') %>% group_by(NPI) %>% summarize(services = sum(Tot_Srvcs)) %>% ungroup()
df_2023_total_codes_office <- df_2023 %>% filter(Place_Of_Srvc == 'O') %>% group_by(NPI) %>% summarize(services = sum(Tot_Srvcs)) %>% ungroup()

# t tests
# 2014 to 2015, p-value = 0.7134
t.test(df_2014_total_codes_office$services, df_2015_total_codes_office$services)
# 2015 to 2016, p-value = 0.7141
t.test(df_2015_total_codes_office$services, df_2016_total_codes_office$services)
# 2016 to 2017, p-value = 0.08815
t.test(df_2016_total_codes_office$services, df_2017_total_codes_office$services)
# 2017 to 2018, p-value = 0.06017
t.test(df_2017_total_codes_office$services, df_2018_total_codes_office$services)
# 2018 to 2019, p-value = 0.3188
t.test(df_2018_total_codes_office$services, df_2019_total_codes_office$services)
# 2019 to 2020, p-value = 0.5788
t.test(df_2019_total_codes_office$services, df_2020_total_codes_office$services)
# 2020 to 2021, p-value = 0.5437
t.test(df_2020_total_codes_office$services, df_2021_total_codes_office$services)
# 2021 to 2022, p-value = 0.7019
t.test(df_2021_total_codes_office$services, df_2022_total_codes_office$services)
# 2022 to 2023, p-value = 0.73
t.test(df_2022_total_codes_office$services, df_2023_total_codes_office$services)

# ---- inpatient codes per surgeon ----

# getting total codes for each surgeon in a year
df_2014_total_codes_office_f <- df_2014 %>% filter(Place_Of_Srvc == 'F') %>% group_by(NPI) %>% summarize(services = sum(Tot_Srvcs)) %>% ungroup()
df_2015_total_codes_office_f <- df_2015 %>% filter(Place_Of_Srvc == 'F') %>% group_by(NPI) %>% summarize(services = sum(Tot_Srvcs)) %>% ungroup()
df_2016_total_codes_office_f <- df_2016 %>% filter(Place_Of_Srvc == 'F') %>% group_by(NPI) %>% summarize(services = sum(Tot_Srvcs)) %>% ungroup()
df_2017_total_codes_office_f <- df_2017 %>% filter(Place_Of_Srvc == 'F') %>% group_by(NPI) %>% summarize(services = sum(Tot_Srvcs)) %>% ungroup()
df_2018_total_codes_office_f <- df_2018 %>% filter(Place_Of_Srvc == 'F') %>% group_by(NPI) %>% summarize(services = sum(Tot_Srvcs)) %>% ungroup()
df_2019_total_codes_office_f <- df_2019 %>% filter(Place_Of_Srvc == 'F') %>% group_by(NPI) %>% summarize(services = sum(Tot_Srvcs)) %>% ungroup()
df_2020_total_codes_office_f <- df_2020 %>% filter(Place_Of_Srvc == 'F') %>% group_by(NPI) %>% summarize(services = sum(Tot_Srvcs)) %>% ungroup()
df_2021_total_codes_office_f <- df_2021 %>% filter(Place_Of_Srvc == 'F') %>% group_by(NPI) %>% summarize(services = sum(Tot_Srvcs)) %>% ungroup()
df_2022_total_codes_office_f <- df_2022 %>% filter(Place_Of_Srvc == 'F') %>% group_by(NPI) %>% summarize(services = sum(Tot_Srvcs)) %>% ungroup()
df_2023_total_codes_office_f <- df_2023 %>% filter(Place_Of_Srvc == 'F') %>% group_by(NPI) %>% summarize(services = sum(Tot_Srvcs)) %>% ungroup()

# t tests
# 2014 to 2015, p-value = 0.9862
t.test(df_2014_total_codes_office_f$services, df_2015_total_codes_office_f$services)
# 2015 to 2016, p-value = 0.9282
t.test(df_2015_total_codes_office_f$services, df_2016_total_codes_office_f$services)
# 2016 to 2017, p-value = 0.7528
t.test(df_2016_total_codes_office_f$services, df_2017_total_codes_office_f$services)
# 2017 to 2018, p-value = 0.3378
t.test(df_2017_total_codes_office_f$services, df_2018_total_codes_office_f$services)
# 2018 to 2019, p-value = 0.1817
t.test(df_2018_total_codes_office_f$services, df_2019_total_codes_office_f$services)
# 2019 to 2020, p-value = 0.4267
t.test(df_2019_total_codes_office_f$services, df_2020_total_codes_office_f$services)
# 2020 to 2021, p-value = 0.2686
t.test(df_2020_total_codes_office_f$services, df_2021_total_codes_office_f$services)
# 2021 to 2022, p-value = 0.5433
t.test(df_2021_total_codes_office_f$services, df_2022_total_codes_office_f$services)
# 2022 to 2023, p-value = 0.5972
t.test(df_2022_total_codes_office_f$services, df_2023_total_codes_office_f$services)

# ---- yearly inpatient vs outpatient ----
df_npi_total <- df_billing %>% group_by(Billing_Year, NPI, Place_Of_Srvc) %>% summarize(total_services = sum(Tot_Srvcs)) %>% ungroup()

# filtering to each year
df_yearly_practice_2014 <- df_npi_total %>% filter(Billing_Year == 2014)
df_yearly_practice_2015 <- df_npi_total %>% filter(Billing_Year == 2015)
df_yearly_practice_2016 <- df_npi_total %>% filter(Billing_Year == 2016)
df_yearly_practice_2017 <- df_npi_total %>% filter(Billing_Year == 2017)
df_yearly_practice_2018 <- df_npi_total %>% filter(Billing_Year == 2018)
df_yearly_practice_2019 <- df_npi_total %>% filter(Billing_Year == 2019)
df_yearly_practice_2020 <- df_npi_total %>% filter(Billing_Year == 2020)
df_yearly_practice_2021 <- df_npi_total %>% filter(Billing_Year == 2021)
df_yearly_practice_2022 <- df_npi_total %>% filter(Billing_Year == 2022)
df_yearly_practice_2023 <- df_npi_total %>% filter(Billing_Year == 2023)

# t tests
# all, p-value = 3.181e-10
t.test(df_npi_total %>% filter(Place_Of_Srvc == 'O') %>% select(total_services), df_npi_total %>% filter(Place_Of_Srvc == 'F') %>% select(total_services))
# 2014, p-value = 0.1584
t.test(df_yearly_practice_2014 %>% filter(Place_Of_Srvc == 'O') %>% select(total_services), df_yearly_practice_2014 %>% filter(Place_Of_Srvc == 'F') %>% select(total_services))
# 2015, p-value = 0.06424
t.test(df_yearly_practice_2015 %>% filter(Place_Of_Srvc == 'O') %>% select(total_services), df_yearly_practice_2015 %>% filter(Place_Of_Srvc == 'F') %>% select(total_services))
# 2016, p-value = 0.3308
t.test(df_yearly_practice_2016 %>% filter(Place_Of_Srvc == 'O') %>% select(total_services), df_yearly_practice_2016 %>% filter(Place_Of_Srvc == 'F') %>% select(total_services))
# 2017, p-value = 0.01189
t.test(df_yearly_practice_2017 %>% filter(Place_Of_Srvc == 'O') %>% select(total_services), df_yearly_practice_2017 %>% filter(Place_Of_Srvc == 'F') %>% select(total_services))
# 2018, p-value = 0.06496
t.test(df_yearly_practice_2018 %>% filter(Place_Of_Srvc == 'O') %>% select(total_services), df_yearly_practice_2018 %>% filter(Place_Of_Srvc == 'F') %>% select(total_services))
# 2019, p-value = 0.0008706
t.test(df_yearly_practice_2019 %>% filter(Place_Of_Srvc == 'O') %>% select(total_services), df_yearly_practice_2019 %>% filter(Place_Of_Srvc == 'F') %>% select(total_services))
# 2020, p-value = 0.01393
t.test(df_yearly_practice_2020 %>% filter(Place_Of_Srvc == 'O') %>% select(total_services), df_yearly_practice_2020 %>% filter(Place_Of_Srvc == 'F') %>% select(total_services))
# 2021, p-value = 0.002163
t.test(df_yearly_practice_2021 %>% filter(Place_Of_Srvc == 'O') %>% select(total_services), df_yearly_practice_2021 %>% filter(Place_Of_Srvc == 'F') %>% select(total_services))
# 2022, p-value = 0.632
t.test(df_yearly_practice_2022 %>% filter(Place_Of_Srvc == 'O') %>% select(total_services), df_yearly_practice_2022 %>% filter(Place_Of_Srvc == 'F') %>% select(total_services))
# 2023, p-value = 0.0008682
t.test(df_yearly_practice_2023 %>% filter(Place_Of_Srvc == 'O') %>% select(total_services), df_yearly_practice_2023 %>% filter(Place_Of_Srvc == 'F') %>% select(total_services))

df_yearly_practice <- df_npi_total %>% group_by(Billing_Year, Place_Of_Srvc) %>% summarize(avg_services = mean(total_services)) %>% ungroup()
ggplot(df_yearly_practice, 
       aes(x = Billing_Year, y = avg_services, color = Place_Of_Srvc)) + 
  geom_line() + 
  geom_point() + 
  theme_classic() + 
  labs(title = 'Yearly Average Billings per Surgeon (Inpatient/Outpatient)', 
       x = 'Year', 
       y = 'Codes per Surgeon') + 
  scale_color_brewer(palette = "Set1")

# ---- regression ----
df_billing_services <- df_billing %>%
  group_by(Billing_Year) %>%
  mutate(total_services = sum(Tot_Srvcs)) %>%
  ungroup() %>%
  group_by(Billing_Year, HCPCS_Cd) %>%
  mutate(code_services = sum(Tot_Srvcs)) %>%
  ungroup() %>%
  group_by(Billing_Year) %>%
  mutate(total_surgeons = n_distinct(NPI)) %>%
  ungroup() %>%
  distinct(Billing_Year, HCPCS_Cd, code_services, total_services, total_surgeons) %>%
  mutate(code_avg_services = code_services/total_surgeons, 
         total_avg_services = total_services/total_surgeons)

df_billing_services_17311 <- df_billing_services %>% filter(HCPCS_Cd == 17311)
df_billing_services_17312 <- df_billing_services %>% filter(HCPCS_Cd == 17312)
df_billing_services_17313 <- df_billing_services %>% filter(HCPCS_Cd == 17313)
df_billing_services_17314 <- df_billing_services %>% filter(HCPCS_Cd == 17314)
df_billing_services_all <- df_billing_services %>% distinct(Billing_Year, total_services, total_surgeons, total_avg_services)

model_17311 <- lm(code_avg_services ~ Billing_Year, data = df_billing_services_17311)
model_17312 <- lm(code_avg_services ~ Billing_Year, data = df_billing_services_17312)
model_17313 <- lm(code_avg_services ~ Billing_Year, data = df_billing_services_17313)
model_17314 <- lm(code_avg_services ~ Billing_Year, data = df_billing_services_17314)
model_all <- lm(total_avg_services ~ Billing_Year, data = df_billing_services_all)

summary(model_17311)
summary(model_17312)
summary(model_17313)
summary(model_17314)
summary(model_all)

# plot
p_values = c(0.951, 0.011, 0.007, 0.835)

label_positions <- df_billing_services %>%
  filter(Billing_Year == 2023) %>%
  mutate(p_values = p_values) %>%
  select(HCPCS_Cd, Billing_Year, code_avg_services, p_values)

ggplot(df_billing_services, 
       aes(x = Billing_Year, 
           y = code_avg_services, 
           color = factor(HCPCS_Cd)
           )
       ) +
  geom_line(linewidth = 0.8) + 
  geom_smooth(method = 'lm', 
              se = FALSE, 
              linetype = 'longdash', 
              linewidth = 0.825
              ) +
  geom_point() +
  geom_text(data = label_positions, 
            aes(label = paste0('trend p-value: ', p_values)), 
            hjust = 1, vjust = -2, size = 3.2
            ) +
  theme_classic() +
  labs(
    title = "Average Code Services per Year",
    y = "Services",
    x = "Year",
    color = "Code"
  )

# total
label_positions_total <- df_billing_services %>%
  slice(n()) %>%
  select(Billing_Year, total_avg_services) %>%
  mutate(p_value = 0.391)

# trend plots
ggplot(df_billing_services %>% distinct(Billing_Year, total_avg_services), 
       aes(x = Billing_Year, 
           y = total_avg_services
          )
      ) + 
  geom_line(linewidth = 0.9) + 
  geom_smooth(method = 'lm', 
              color = 'black',
              se = FALSE, 
              linetype = 'longdash', 
              linewidth = 0.825
  ) +
  geom_point() +
  geom_text(data = label_positions_total, 
            aes(label = paste0('trend p-value: ', p_value)), 
            hjust = 1, vjust = 10, size = 3.2
  ) +
  theme_classic() +
  labs(
    title = "Average Total Services per Year",
    y = "Services",
    x = "Year",
    color = "Code"
  )
