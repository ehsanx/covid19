setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

################################################
# See the following link for the original codes:
# https://joachim-gassen.github.io/2020/03/tidying-the-new-johns-hopkins-covid-19-datasests/
################################################
  
library(tidyverse)
library(lubridate)
library(countrycode)

clean_jhd_to_long <- function(df) {
  df_str <- deparse(substitute(df))
  var_str <- substr(df_str, 1, str_length(df_str) - 4)
  
  df %>% 
    select(-`Province/State`, -Lat, -Long) %>%
    rename(country = `Country/Region`) %>%
    mutate(iso3c = countrycode(country,
                               origin = "country.name",
                               destination = "iso3c")) %>%
    select(-country) %>%
    filter(!is.na(iso3c)) %>%
    group_by(iso3c) %>%
    summarise_at(vars(-group_cols()), sum) %>% 
    pivot_longer(
      -iso3c, 
      names_to = "date_str", 
      values_to = var_str
    ) %>%
    ungroup() %>%
    mutate(date = mdy(date_str)) %>%
    select(iso3c, date, !! sym(var_str)) 
}

confirmed_raw <- read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv", col_types = cols())
deaths_raw <- read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv", col_types = cols())

# Recovered data I pull from the old depreciated dataset. This might generate issues going forward
recovered_raw <- read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_recovered_global.csv", col_types = cols())

jh_covid19_data <- clean_jhd_to_long(confirmed_raw) %>%
  full_join(clean_jhd_to_long(deaths_raw), by = c("iso3c", "date")) %>%
  full_join(clean_jhd_to_long(recovered_raw), by = c("iso3c", "date"))

jhd_countries <- tibble(
  country = unique(confirmed_raw$`Country/Region`),
  iso3c = countrycode(country,
                      origin = "country.name",
                      destination = "iso3c")
) %>% filter(!is.na(iso3c))

old_jhd_countries <- tibble(
  country = unique(recovered_raw$`Country/Region`),
  iso3c = countrycode(country,
                      origin = "country.name",
                      destination = "iso3c")
) %>% filter(!is.na(iso3c),
             ! iso3c %in% jhd_countries$iso3c)

jhd_countries <- rbind(jhd_countries, old_jhd_countries)

jh_covid19_data %>%
  left_join(jhd_countries, by = "iso3c") %>%
  select(country, iso3c, date, confirmed, deaths, recovered) -> jh_covid19_data

save(jh_covid19_data, file="jhcovid19.RData")
head(jh_covid19_data)

lastx <- function(x) {last(na.omit(x))}
d=100000000
jh_covid19_data %>%
  group_by(iso3c) %>%
  arrange(iso3c, date) %>%
  summarise(
    confirmed = lastx(confirmed), 
    deaths = lastx(deaths), 
    recovered = lastx(recovered)
  ) %>% left_join(jh_covid19_data %>% select(iso3c) %>% distinct()) -> jh2
jh2 <- as.data.frame(jh2)
head(jh2)

save(jh2, file="jhsummary.RData")

# Merging some World Bank data

library(tidyverse)
library(wbstats)

pull_worldbank_data <- function(vars) {
  new_cache <- wbcache()
  all_vars <- as.character(unique(new_cache$indicators$indicatorID))
  data_wide <- wb(indicator = vars, mrv = 10, return_wide = TRUE)
  new_cache$indicators[new_cache$indicators[,"indicatorID"] %in% vars, ] %>%
    rename(var_name = indicatorID) %>%
    mutate(var_def = paste(indicator, "\nNote:",
                           indicatorDesc, "\nSource:", sourceOrg)) %>%
    select(var_name, var_def) -> wb_data_def
  new_cache$countries %>%
    select(iso3c, iso2c, country, region, income) -> ctries
  left_join(data_wide, ctries, by = "iso3c") %>%
    rename(year = date,
           iso2c = iso2c.y,
           country = country.y) %>%
    select(iso3c, iso2c, country, region, income, everything()) %>%
    select(-iso2c.x, -country.x) %>%
    filter(!is.na(NY.GDP.PCAP.KD),
           region != "Aggregates") -> wb_data
  wb_data$year <- as.numeric(wb_data$year)
  wb_data_def<- left_join(data.frame(var_name = names(wb_data),
                                     stringsAsFactors = FALSE),
                          wb_data_def, by = "var_name")
  wb_data_def$var_def[1:6] <- c(
    "Three letter ISO country code as used by World Bank",
    "Two letter ISO country code as used by World Bank",
    "Country name as used by World Bank",
    "World Bank regional country classification",
    "World Bank income group classification",
    "Calendar year of observation"
  )
  wb_data_def$type = c("cs_id", rep("factor",  4), "ts_id",
                       rep("numeric", ncol(wb_data) - 6))
  return(list(wb_data, wb_data_def))
}

vars <- c("SP.POP.TOTL", "AG.LND.TOTL.K2", "EN.POP.DNST", "EN.URB.LCTY", "SP.DYN.LE00.IN", "NY.GDP.PCAP.KD",
          "SH.IMM.IBCG", "SH.IMM.IBCG.Q1.ZS", "SH.IMM.IBCG.Q2.ZS", "SH.IMM.IBCG.Q3.ZS", "SH.IMM.IBCG.Q4.ZS", "SH.IMM.IBCG.Q5.ZS",
          "5.13.01.01.who", "5.51.01.04.immun",
          #"HF.IMM.FULL", #"HF.IMM.FULL.Q1", "HF.IMM.FULL.Q2", "HF.IMM.FULL.Q3", "HF.IMM.FULL.Q4", "HF.IMM.FULL.Q5",
          #"HF.IMM.MEAS", #"HF.IMM.MEAS.Q1", "HF.IMM.MEAS.Q2", "HF.IMM.MEAS.Q3", "HF.IMM.MEAS.Q4", "HF.IMM.MEAS.Q5",
          "SH.IMM.CHLD.ZS", "SH.IMM.HEPB", "SH.IMM.HIB3", "SH.IMM.IDPT", "SH.IMM.MEAS", "SH.IMM.POL3")
wb_list <- pull_worldbank_data(vars)
wb_data <- wb_list[[1]]

names(wb_data) <- c("iso3c", "iso2c", "country", "region", "income", "year", "immun.who", 
  "immun", "land.Area", "pop.density", "pop.large.city", "GDP.capita", 
  "IMM.CHLD.ZS", "IMM.HEPB", "IMM.HIB3", 
  "IMM.IBCG", "IMM.IBCG.Q1.ZS", "IMM.IBCG.Q2.ZS", "IMM.IBCG.Q3.ZS", "IMM.IBCG.Q4.ZS", "IMM.IBCG.Q5.ZS", 
  "IMM.IDPT", "IMM.MEAS", "IMM.POL3", 
  "life.exp", "total.pop")

head(wb_data)

wb_data %>%
  group_by(iso3c) %>%
  arrange(iso3c, year) %>%
  summarise(
    population = last(na.omit(total.pop)),
    immun  = sum(na.omit(immun)),
    immun.who  = sum(na.omit(immun.who)),
    land_area_skm = last(na.omit(land.Area)),
    pop_density = last(na.omit(pop.density)),
    pop_largest_city = last(na.omit(pop.large.city)),
    gdp_capita = last(na.omit(GDP.capita)),
    life_expectancy = last(na.omit(life.exp)),
    IMM.HEPB  = sum(na.omit(IMM.HEPB)),
    IMM.HIB3  = sum(na.omit(IMM.HIB3)),
    IMM.IBCG  = sum(na.omit(IMM.IBCG)),
    IMM.IDPT  = sum(na.omit(IMM.IDPT)),
    IMM.MEAS  = sum(na.omit(IMM.MEAS)),
    IMM.POL3  = sum(na.omit(IMM.POL3))
  ) %>% left_join(wb_data %>% select(iso3c, region, income, country) %>% distinct()) -> wb_cs


head(as.data.frame(wb_cs))
jh2 %>% 
  group_by(deaths) %>%
      full_join(wb_cs, by = "iso3c") -> wb.jh
head(wb.jh)
save(wb.jh, file="wbank-jh.RData")
################################################


require(tidyverse)
bcg <- read.csv("BCGatlas.csv")
head(wb.jh)
wb.jh %>%
  group_by(iso3c) %>%
  full_join(bcg %>% select(iso3c,BCG.year,BCG.status) %>% distinct()) -> wb.jhx
head(wb.jhx)
require(countrycode)
wb.jhx$iso2c <- countrycode(wb.jhx$iso3c, origin = 'iso3c', destination = 'iso2c')
wb.jh <- as.data.frame(wb.jhx)
wb.jh$BCG <- wb.jh$IMM.IBCG > 0
ex2 <- function(x="confirmed"){
  wb.jhx <- wb.jh[c("iso2c","iso3c","country","population", "pop_density", "income",
                    "confirmed","deaths", "BCG.status", 
                    "BCG.year", "BCG")]
  wb.jhx %>%  
    arrange(-confirmed) %>% 
    filter(!is.na(deaths)) %>% 
    filter(!is.na(country)) ->res
  return(res)
}
dta <- ex2(x="confirmed")
dta$iso2c <- as.character(dta$iso2c)
dta$iso2c[dta$iso3c=="NAM"] <- "NA"
save(dta, file="jhwbxx.RData")

head(dta)
table(dta$income)


dta$'Confirmed cases' <- dta$confirmed
dta$'Reported deaths' <- dta$deaths
dta$'Confirmed by Population' <- dta$confirmed/dta$population
dta$'Deaths by Population' <- dta$deaths/dta$population

dta$'Confirmed by population density' <- dta$confirmed/dta$pop_density
dta$'Confirmed by population density'[is.na(dta$'Confirmed by population density')] <- 0
dta$'Deaths by population density' <- dta$deaths/dta$pop_density
dta$'Deaths by population density'[is.na(dta$'Deaths by population density')] <- 0

dta$'BCG world Atlas' <- dta$BCG.status
dta$'BCG world Bank' <- dta$BCG

head(dta)
saveRDS(dta, file="covid.Rds")
