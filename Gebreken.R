#### Olie Lek percentage voor verschillende auto's

library(dplyr)
library(ggplot2)

## Importeer gedownloade RDW data
Gebreken = read_csv("Open_Data_RDW__Geconstateerde_Gebreken.csv")
GebrekenOmschr = read_csv("Open_Data_RDW__Gebreken.csv")
Personenauto_basisdata = read_csv(
  "Personenauto_basisdata.csv",
  col_types = cols(`Datum eerste toelating` = col_date(format = "%d/%m/%Y"))
)

## bereken ouderdom van de auto
Personenauto_basisdata = Personenauto_basisdata %>%
  mutate(
   ouderdom = as.integer((Sys.Date() - `Datum eerste toelating`)/365)
  )

## groupeer zodat we aantal autos per merk/type en ouderdom hebben
## laat kleine aantallen en te oude jaren weg

AantalAutosPerTypeOuderdom = Personenauto_basisdata %>%
  group_by(
    Merk,
    Handelsbenaming,
    ouderdom
  ) %>%
  summarise(N = n()) %>%
  filter(
    N > 600, ouderdom < 14
  ) %>%
  arrange(
    Merk,
    Handelsbenaming
  )
  

## Kijk naar geconstateerde olie lekkages van afgelopen jaar en match met auto basis data
GebrekenPerAuto = Gebreken %>%
  filter(
    `Meld datum door keuringsinstantie` > 20160816,
    `Gebrek identificatie` == "RA2" # is overmatige olie lekkage
  ) %>%
  left_join(
    Personenauto_basisdata
  ) %>%
  group_by(
      Merk,
      Handelsbenaming,
      ouderdom
  ) %>%
  summarise( nlekken = n_distinct(Kenteken) ) %>%
  inner_join(
    AantalAutosPerTypeOuderdom
  ) %>%
  mutate(
    lekpercentage = nlekken/N
  )

## plot lekpercentage
## haal een paar merken weg
GebrekenPerAuto %>% 
  filter(
    !(Merk %in% c("LEXUS","LANCIA", "LAND ROVER", "SMART", "SUBARU", "SAAB", "DAEWOO"))
  ) %>%
  ggplot( aes(ouderdom, lekpercentage )) + 
  geom_point() +
  geom_smooth(se=FALSE) + 
  facet_wrap(~Merk)
