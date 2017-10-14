#### Oil Leak percentage for different brands of cars

library(readr)
library(dplyr)
library(ggplot2)

## Import open car data from RDW 
defects_descr = read_csv("Open_Data_RDW__Gebreken.csv")

#cars_basisdata = read_csv(
#  "Personenauto_basisdata.csv",
#  col_types = cols(`Datum eerste toelating` = col_date(format = "%d/%m/%Y"))
#)

### split in three because then it fits on GIT
download.file("http://5.100.228.219/data/cars_basisdata1.RDs", "cars1.RDs", mode="wb")
download.file("http://5.100.228.219/data/cars_basisdata2.RDs", "cars2.RDs", mode="wb")
download.file("http://5.100.228.219/data/cars_basisdata3.RDs", "cars3.RDs", mode="wb")

cars_basisdata1 = readRDS("cars1.RDs")
cars_basisdata2 = readRDS("cars2.RDs")
cars_basisdata3 = readRDS("cars3.RDs")

cars_basisdata = bind_rows(cars_basisdata3, cars_basisdata2, cars_basisdata1)

download.file("http://5.100.228.219/data/defects1.RDs", "defects1.RDs", mode="wb")
download.file("http://5.100.228.219/data/defects2.RDs", "defects2.RDs", mode="wb")
download.file("http://5.100.228.219/data/defects3.RDs", "defects3.RDs", mode="wb")

defects1 = readRDS("defects1.RDs")
defects2 = readRDS("defects2.RDs")
defects3 = readRDS("defects3.RDs")
defects = bind_rows(defects1, defects2, defects3)

rm(cars_basisdata1, cars_basisdata2, cars_basisdata3)
rm(defects1, defects2, defects3)

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
