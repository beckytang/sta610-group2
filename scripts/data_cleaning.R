require(tidyverse)

census <- read_delim("Data/Census2010_long.txt",delim = " ")
voters <- read_delim("Data/voter_stats_20161108.txt",delim = " ")

voters_county <- voters %>% 
  mutate(age = case_when(age == "Age 18 - 25" ~ "18-25",
                         age == "Age 26 - 40" ~ "26-40",
                         age == "Age 41 - 65" ~ "41-65",
                         age == "Age Over 66" ~ "66+")) %>% 
  group_by(county_desc,race_code,ethnic_code,sex_code,age) %>% 
  summarise(total_voters = sum(total_voters))

census_clean <- census %>% 
  mutate(sex_code = case_when(Gender == "Male" ~ "M",
                              Gender == "Female" ~ "F"),
         race_code = case_when(Race == "WhiteAlone" ~ "W",
                               Race == "AsianAlone" ~ "A",
                               Race == "BlackAlone" ~ "B",
                               Race == "TwoOrMoreRaces" ~ "M",
                               Race == "NativeHawaiianOrOtherPacificIslanderAlone" ~ "O", #active choice
                               Race == "SomeOtherRaceAlone" ~ "O",
                               Race == "AmericanIndianOrAlaskaNativeAlone" ~ "I"),
         ethnic_code = case_when(Hispanic == "Hispanic" ~ "HL",
                                 Hispanic == "NotHispanic" ~ "NL"),
         county_desc = Geography,
         age = Age) %>% 
  select(county_desc,sex_code,race_code,ethnic_code,age,Freq,TotalCountyPopulation)

data_full <- left_join(census_clean,voters_county) %>% 
  mutate(total_voters = ifelse(is.na(total_voters),0,total_voters),
         total_nonvoters = Freq - total_voters)

