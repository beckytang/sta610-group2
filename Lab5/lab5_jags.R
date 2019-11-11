library(R2jags)
library(tidyverse)
load("~/Documents/GitHub/sta610-group2/Data/data_full.Rda")
# model
mod <- function(){
  for (i in 1:n_county){
    y[i] ~ dnorm(mu[i], phi[i])
    mu[i] <- 
  }
  
  # priors
  phi[1:n_county] ~ dgamma(s0/2, s0*nu0/2)
  s0 ~ dgamma(c, d)
  nu0 ~ dexp(lambda)
  
}

# county-pop
y <- unique(data_full$TotalCountyPopulation)
n_county <- length(y)

race_counts <- list()
race_codes <- unique(data_full$race_code)
for (i in 1:length(race_codes)){
  race_counts[[i]] <-  data_full %>%
    group_by(county_desc, race_code) %>%
    mutate(count = sum(Freq)) %>%
    select(county_desc, race_code, count) %>%
    unique() %>%
    filter(race_code == race_codes[i]) %>%
    ungroup() %>%
    select(count)
}
ethnic_counts <- list()
ethnic_codes <- unique(data_full$ethnic_code)
for (i in 1:length(ethnic_codes)){
  ethnic_counts[[i]] <-  data_full %>%
    group_by(county_desc, ethnic_code) %>%
    mutate(count = sum(Freq)) %>%
    select(county_desc, ethnic_code, count) %>%
    unique() %>%
    filter(ethnic_code == ethnic_codes[i]) %>%
    ungroup() %>%
    select(count)
}
age_counts <- list()
age_codes <- unique(data_full$age)
for (i in 1:length(age_codes)){
  age_counts[[i]] <-  data_full %>%
    group_by(county_desc, age) %>%
    mutate(count = sum(Freq)) %>%
    select(county_desc, age, count) %>%
    unique() %>%
    filter(age == age_codes[i]) %>%
    ungroup() %>%
    select(count)
}
sex_counts <- list()
sex_codes <- unique(data_full$sex_code)
for (i in 1:length(sex_codes)){
  sex_counts[[i]] <-  data_full %>%
    group_by(county_desc, sex_code) %>%
    mutate(count = sum(Freq)) %>%
    select(county_desc, sex_code, count) %>%
    unique() %>%
    filter(sex_code == sex_codes[i]) %>%
    ungroup() %>%
    select(count)
}
