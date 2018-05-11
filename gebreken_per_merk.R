library(readr)
library(dplyr)
library(stringr)
library(lubridate)
library(ggplot2)
library(plotly)

#### Import CSV sources #################################################################
 # cars <- read_csv(
 #   "cars.csv", 
 #   col_types = cols(
 #    `Datum eerste toelating` = col_date(format = "%d/%m/%Y"),
 #     `API Gekentekende_voertuigen_assen` = col_skip(), 
 #     `API Gekentekende_voertuigen_brandstof` = col_skip(), 
 #     `API Gekentekende_voertuigen_carrosserie` = col_skip(), 
 #     `API Gekentekende_voertuigen_carrosserie_specifiek` = col_skip(), 
 #     `API Gekentekende_voertuigen_voertuigklasse` = col_skip(), 
 #     `Aantal rolstoelplaatsen` = col_skip(), 
 #     `Aantal staanplaatsen` = col_skip(), 
 #     `Aantal wielen` = col_skip(), `Europese voertuigcategorie toevoeging` = col_skip(), 
 #     `Maximum ondersteunende snelheid` = col_skip(), 
 #     `Plaats chassisnummer` = col_skip(), 
 #     `Taxi indicator` = col_skip(), `Type gasinstallatie` = col_skip(), 
 #     Typegoedkeuringsnummer = col_skip(), 
 #     `Vermogen (brom/snorfiets)` = col_skip(), 
 #     `Vervaldatum tachograaf` = col_skip(), 
 #     `Volgnummer wijziging EU typegoedkeuring` = col_skip(),
 #     `Afwijkende maximum snelheid`  = col_skip(),
 #     `Afstand hart koppeling tot achterzijde voertuig`  = col_skip(),
 #     `Afstand voorzijde voertuig tot hart koppeling`  = col_skip()
 #   )
 # )
# GebrekenPerAuto <- read_csv("gebreken.csv")
# 
# 
# saveRDS(cars, "cars.RDs")

# saveRDS(
#   cars %>%
#     filter(Voertuigsoort == "Personenauto") %>% 
#     select(Kenteken, Merk, Handelsbenaming, `Datum eerste toelating`),
#   "cars_small.RDs")

# saveRDS(GebrekenPerAuto, "GebrekenPerAuto.RDs")


#### import RDS #########################################################################
cars <- readRDS("~/RProjects/OlieLekkage/cars.RDs")
cars_small = readRDS("cars_small.RDs")
GebrekenPerAuto <- readRDS("~/RProjects/OlieLekkage/GebrekenPerAuto.RDs")
GebrekenCode = read_csv("Open_Data_RDW__Gebreken.csv")

##### aggregate by date #################################################################
GebrekenPerAuto2 =  GebrekenPerAuto %>% 
  left_join(cars %>% select(Kenteken, Voertuigsoort,Merk, Handelsbenaming, `Datum eerste toelating`))
  
saveRDS(GebrekenPerAuto2, "GebrekenPerAuto2.RDs")

GebrekenPerAuto2 = readRDS("GebrekenPerAuto2.RDs")

top16merken = GebrekenPerAuto2 %>%
  group_by(Merk) %>%  
  summarise(n=n()) %>% 
  arrange(desc(n)) %>% 
  slice(1:16)

pertijd = GebrekenPerAuto2 %>% 
  group_by(`Meld datum door keuringsinstantie`) %>%
  summarise(n=n())

pertijd  = pertijd %>%
  filter(`Meld datum door keuringsinstantie` >= 20150501) %>% 
  filter(n > 3) %>% 
  mutate(datum = ymd(`Meld datum door keuringsinstantie`)) 

ggplot(pertijd, aes(datum,n)) + 
  geom_line() +
  scale_x_date(date_breaks = "3 month")  

plot_ly(pertijd, x=~datum, y= ~n) %>% add_lines()

####### per tijd en merk ####################################


pertijdMerk = GebrekenPerAuto2 %>% 
  group_by(Merk,`Meld datum door keuringsinstantie`) %>%
  summarise(n=n()) %>% 
  inner_join(top16merken %>% select(Merk))


pertijdMerk  = pertijdMerk %>%
  filter(`Meld datum door keuringsinstantie` >= 20150501) %>% 
  filter(n > 3) %>% 
  mutate(datum = ymd(`Meld datum door keuringsinstantie`)) 


ggplot(pertijdMerk, aes(datum,n)) + 
  geom_line() +
  scale_x_date(date_breaks = "3 month")  +
  facet_wrap(~Merk, ncol=4)

TMP = GebrekenPerAuto2 %>% group_by(Voertuigsoort) %>%  summarise(n=n())

###########  join personenautos met gebreken data in 2017 ########################

Gebrekenin2017 = GebrekenPerAuto %>% 
  filter(
    `Meld datum door keuringsinstantie` >= 20170101,
    `Meld datum door keuringsinstantie` < 20180101
    ) 

cars_small2  = cars_small %>% 
  left_join(
    Gebrekenin2017
  ) 

cars_small2 = cars_small2 %>% 
  mutate(
    ouderdom = ceiling((lubridate::ymd("20180101") - `Datum eerste toelating`)/365),
    APK_Gebrek = ifelse(is.na(`Meld datum door keuringsinstantie`),0,1)
  )

cars3 = cars_small2 %>% group_by(Kenteken, Merk, Handelsbenaming, ouderdom ) %>%  summarise(APK_Gebrek = max(APK_Gebrek))

cars4 = cars3 %>% group_by(Merk, Handelsbenaming, ouderdom) %>%  summarise(n=n(),perc_gebreken = mean(APK_Gebrek))

top25 = cars4 %>% group_by(Merk, Handelsbenaming) %>%  summarise(som = sum(n)) %>% arrange(desc(som))
top25 = top25[1:25,]

cars5 = cars4 %>% inner_join(top25) %>%  filter(ouderdom < 26, ouderdom > 4)

cars5 = cars3  %>%  inner_join(top25) %>% filter(ouderdom < 26, ouderdom > 4)

%>% 
ggplot( aes(ouderdom, APK_Gebrek )) + geom_smooth(aes(col=Merk), se=F)

plotly::ggplotly(p)
