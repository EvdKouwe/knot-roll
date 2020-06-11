
# install.packages("tidyverse")
# install.packages("Rmisc")
# install.packages("reshape")
# install.packages("gridExtra")
# install.packages("cowplot")
# install.packages("grid")
# install.packages("rmarkdown")
# install.packages("gdata")
# install.packages("rio")
# install.packages("janitor")



library(tidyverse)
library(Rmisc)
library(reshape)
library(gridExtra)
library(grid)
library(cowplot)
library(rmarkdown)
library(gdata)
library(rio)



# daily covid19 statistics per day per country
import("https://www.ecdc.europa.eu/sites/default/files/documents/COVID-19-geographic-disbtribution-worldwide.xlsx") -> covid19

# file with population number per country. to be trimmed
import("r_pop_per_country_over_time.xls") %>%
  slice(4:268) %>%
  select(country=1, country_code=2, pop=63) %>%
  drop_na() %>%
  mutate(pop = as.numeric(pop)) -> world_pop

# reset for a clean start
keep(covid19, world_pop, sure = TRUE)



text <- 10
last_date <- covid19$dateRep[1]
topnumber <- 10

# cases
covid19 %>%
  filter(dateRep %in% (last_date)) %>%
  filter(countriesAndTerritories %in% unique(countriesAndTerritories)) %>%
  top_n(topnumber, cases) %>%
  select(countriesAndTerritories) -> top10cases
list_countries_top10cases <- top10cases[['countriesAndTerritories']]

covid19 %>%
  select(dateRep, cases, deaths, countriesAndTerritories) %>%
  mutate(country = as.factor(countriesAndTerritories)) %>%
  select(dateRep, country, cases, deaths) %>%
  filter(country %in% list_countries_top10cases) %>%
  ggplot(mapping = aes(x = dateRep, y = cases, fill = country, color=country)) +
  geom_line() +
  geom_point() +
  theme_cowplot(text) + ylab("cases per day") + xlab("time")  -> cases


# deaths
covid19 %>%
  filter(dateRep %in% (last_date)) %>%
  filter(countriesAndTerritories %in% unique(countriesAndTerritories)) %>%
  top_n(topnumber, deaths) %>%
  select(countriesAndTerritories) -> top10deaths
list_countries_top10cases <- top10deaths[['countriesAndTerritories']]

covid19 %>%
  select(dateRep, cases, deaths, countriesAndTerritories) %>%
  mutate(country = as.factor(countriesAndTerritories)) %>%
  select(dateRep, country, cases, deaths) %>%
  filter(country %in% list_countries_top10cases) %>%
  ggplot(mapping = aes(x = dateRep, y = deaths, fill = country, color=country)) +
  geom_line() +
  geom_point() +
  theme_cowplot(text) + ylab("deaths per day") + xlab("time") -> deaths

summary <- plot_grid(cases, deaths, nrow = 1)
cases
deaths