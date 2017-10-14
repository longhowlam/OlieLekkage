#### Oil Leak percentage for different brands of cars

library(readr)
library(dplyr)
library(ggplot2)

## Import open car data from RDW 
defects = read_csv("Open_Data_RDW__Geconstateerde_Gebreken.csv")
defects_descr = read_csv("Open_Data_RDW__Gebreken.csv")

#cars_basisdata = read_csv(
#  "Personenauto_basisdata.csv",
#  col_types = cols(`Datum eerste toelating` = col_date(format = "%d/%m/%Y"))
#)

### split in three because then it fits on GIT
cars_basisdata1 = readRDS("cars_basisdata1.RDs")
cars_basisdata2 = readRDS("cars_basisdata2.RDs")
cars_basisdata3 = readRDS("cars_basisdata3.RDs")

cars_basisdata = bind_rows(cars_basisdata3, cars_basisdata2, cars_basisdata1)


## determin age of car
cars_basisdata = cars_basisdata %>%
  mutate(
   age = as.integer((Sys.Date() - `Datum eerste toelating`)/365)
  )

## group so that we have the number of cars per brand/make and age
## leave out small numbers and old ages

NCarsPerTypeandAge = cars_basisdata %>%
  group_by(
    Merk,
    Handelsbenaming,
    age
  ) %>%
  summarise(N = n()) %>%
  filter(
    N > 600, age < 14
  ) %>%
  arrange(
    Merk,
    Handelsbenaming
  )
  

## Look at oil leaks of last year and match with basic car data
DefectsPerAuto = defects %>%
  filter(
    `Meld datum door keuringsinstantie` > 20160816,
    `Gebrek identificatie` == "RA2" # is oil leak
  ) %>%
  left_join(
    cars_basisdata
  ) %>%
  group_by(
      Merk,
      Handelsbenaming,
      age
  ) %>%
  summarise( nleaks = n_distinct(Kenteken) ) %>%  #kenteken is the Dutch licence number of a car
  inner_join(
    NCarsPerTypeandAge
  ) %>%
  mutate(
    leakpercentage = nleaks/N
  )

## plot leakpercentage, remove some brands

DefectsPerAuto %>% 
  filter(
    !(Merk %in% c("LEXUS","LANCIA", "LAND ROVER", "SMART", "SUBARU", "SAAB", "DAEWOO"))
  ) %>%
  ggplot( aes(age, leakpercentage )) + 
  geom_point() +
  geom_smooth(se=FALSE) + 
  facet_wrap(~Merk)
