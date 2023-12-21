

suppressPackageStartupMessages({
  library(shiny)
  library(shinyjs)
  library(shinyBS)
  library(shinythemes)
  library(shinyWidgets)
  library(shinydashboard)
  library(data.table)
  library(tidyverse)
  library(plotly)
  library(readxl)
  library(sf)
  library(treemap)
  library(d3treeR)
  library(leaflet)
  library(countrycode)
  library(DT)
  library(npsf)
  library(frontier)
  library(maps)
})

setwd("C:/Users/aadama/OneDrive - Université Clermont Auvergne/UEMOA/App")


GRD <- read_excel("data/GRD.xls")

GRD[ GRD$iso == "KSV", "iso"] <- "XKX"
GRD[ GRD$iso == "WBG", "iso"] <- "PSE"

GRD <- GRD %>%
  rename(Country_Code = iso) %>%
  select(-c(`group(iso)`, `=1 if General Govt data`, country))

TaxVarGRD <- names(GRD)[c(12:(ncol(GRD)-4))]

VarTax <- c("Revenue excluding grants and social contributions",
            "Taxes excluding social contributions", "Non-resource tax excluding social contributions",
            "Resource taxes",  "Direct taxes excluding social contributions and resource revenue",
            "Non-resource component of taxes on income, profits, and capital gains", "Individuals",
            "Non-resource component of Corporations and other enterprises", "Taxes on payroll and workforce",
            "Taxes on property", "Non-Resource Component of Indirect Tax", "Taxes on goods and services,                Total",
            "VAT", "Taxes on goods and services, of which Excises",
            "Taxes on international trade and transactions Of which Import",
            "Taxes on international trade and transactions Of which Export",
            "Consolidated Non-Tax Revenue", "Grants", "Social contributions",
            "Total Resource Revenue"
)

# VarTax <- c("Revenue including social contributions")

tbloc <- fread("data/TradingBloc.csv", stringsAsFactors = FALSE, na.strings = c("" ,"n/a", "NA")) 
tbloc <- tbloc %>%
  mutate(Country_Code = countrycode(sourcevar = Country, origin= "country.name", destination = "iso3c")) %>%
  select(-Country)


oecdafrs <- fread("data/RS_AFR_24112023.csv", stringsAsFactors = FALSE, na.strings = c("", "n\a", "NA", "..", "n/a", "--"))

oecdafrm <- fread("data/RS_AFR_24112023_00_15.csv", stringsAsFactors = FALSE, na.strings = c("", "n\a", "NA", "..", "n/a", "--")) 

oecdafre <- fread("data/RS_AFR_24112023_90_99.csv", stringsAsFactors = FALSE, na.strings = c("", "n\a", "NA", "..", "n/a", "--"))

oecdafrs <- oecdafrs %>%
  filter(Year>2015) 


oecdafr <- oecdafrs %>%
  bind_rows(oecdafrm) %>%
  bind_rows(oecdafre)

oecdafr <- oecdafr %>%
  select(-c(`Unit Code`, Unit, `PowerCode Code`, PowerCode, `Reference Period Code`, `Reference Period`, `Flag Codes`, Flags, YEA, VAR, TAX, GOV)) %>%
  rename(Country_Code = COU,  year = Year) %>%
  filter(!(Country_Code %in% "AFRIC")) 

macroVaroecd <- oecdafr %>%
  filter(`Level of government` %in% "Total" & `Revenue category` %in% "Total tax and non-tax revenue" &
           (Indicator %in% c("GDP for tax reporting years at market prices, national currency", 
                             "GDP for tax reporting years at market prices, USD", 
                             "Exchange rate between the national currency and USD")
           )
  ) %>%
  select(Country_Code, Country, year, Indicator, Value)

macroVaroecd <- pivot_wider(macroVaroecd, id_cols = c(Country_Code, Country, year, ), names_from = c(Indicator), values_from = Value)

oecdafr <- oecdafr %>%
  filter(!(Indicator %in% c("GDP for tax reporting years at market prices, national currency", 
                            "GDP for tax reporting years at market prices, USD", 
                            "Exchange rate between the national currency and USD"                                  
                            
  )))

oecdafr <- pivot_wider(oecdafr, id_cols = c(Country_Code, Country, year), 
                       names_from = c(`Revenue category`, Indicator, `Level of government`),
                       names_sep = " ", values_from = Value)

oecdafr <- oecdafr %>%
  select(-Country)

oecdafrFinal <- macroVaroecd %>%
  left_join(oecdafr, by = c("Country_Code", "year")) %>%
  select(-Country)

remove(macroVaroecd, oecdafr, oecdafrs, oecdafrm, oecdafre)

# oecdgrd %>% 
#   select(Country_Code, year, `Total non-tax revenue % of GDP`, `1000 Taxes on income, profits and capital gains Revenue as % of GDP Federal or Central government`,
#          `Customs and import duties % of GDP`, everything()) %>%
# View()

# GRD %>%
#   select(-c(1:2, 4:6, 8:11)) %>%
#   select(Country_Code, year, `Revenue including social contributions` , `Taxes on international trade and transactions, Total`,
#          `Direct taxes excluding social contributions and resource revenue`, everything()) %>% 
#   filter(Country_Code %in% "GNB") %>%
#   View()

# View(GRD[, -c(1:2, 4:6, 8:11)])

oecdgrd <- GRD %>%
  full_join(oecdafrFinal, by = c("Country_Code", "year"))

oecdgrd <- oecdgrd %>% 
  mutate(
    `Total tax and non-tax revenue as % of GDP` = case_when(
      !is.na(`Total tax and non-tax revenue Revenue as % of GDP Federal or Central government`) ~ `Total tax and non-tax revenue Revenue as % of GDP Federal or Central government`, 
      is.na(`Total tax and non-tax revenue Revenue as % of GDP Federal or Central government`) ~ `Revenue including social contributions`
    ),
    `Total tax revenue as % GDP` = case_when(
      !is.na(`Total tax revenue Revenue as % of GDP Federal or Central government`) ~ `Total tax revenue Revenue as % of GDP Federal or Central government`,
      is.na(`Total tax revenue Revenue as % of GDP Federal or Central government`)  ~ `Taxes including social contributions`
    ),
    `Non-resource tax excluding social contributions as % of GDP` = `Non-resource tax excluding social contributions`, 
    `Direct taxes including social contributions and resource revenue as % of GDP` = `Direct taxes including social contributions and resource revenue`,
    `Taxes on income, profits and capital gains as % of GDP` = case_when(
      !is.na(`1000 Taxes on income, profits and capital gains Revenue as % of GDP Federal or Central government`) ~ `1000 Taxes on income, profits and capital gains Revenue as % of GDP Federal or Central government`, 
      is.na(`1000 Taxes on income, profits and capital gains Revenue as % of GDP Federal or Central government`) ~ `Taxes on income, profits, and capital gains`
    ),
    `Taxes on income, profits and capital gains of individuals % as of GDP` = case_when(
      !is.na(`1100 Taxes on income, profits and capital gains of individuals Revenue as % of GDP Federal or Central government`) ~ `1100 Taxes on income, profits and capital gains of individuals Revenue as % of GDP Federal or Central government`, 
      is.na(`1100 Taxes on income, profits and capital gains of individuals Revenue as % of GDP Federal or Central government`) ~ Individuals
    ),
    `Taxes on income, profits and capital gains of corporates as % GDP` = case_when(
      !is.na(`1200 Taxes on income, profits and capital gains of corporates Revenue as % of GDP Federal or Central government`) ~ `1200 Taxes on income, profits and capital gains of corporates Revenue as % of GDP Federal or Central government`,
      is.na(`1200 Taxes on income, profits and capital gains of corporates Revenue as % of GDP Federal or Central government`)  ~ `Corporations and other enterprises`
    ),
    `On profits of corporates as % GDP` = `1210 On profits of corporates Revenue as % of GDP Federal or Central government`,
    `On capital gains of corporates as % of GDP` = `1220 On capital gains of corporates Revenue as % of GDP Federal or Central government`,
    `Indirect as % of GDP` = Indirect ,
    `Taxes on goods and services as % of GDP` = case_when(
      !is.na(`5000 Taxes on goods and services Revenue as % of GDP Federal or Central government`) ~ `5000 Taxes on goods and services Revenue as % of GDP Federal or Central government`,
      is.na(`5000 Taxes on goods and services Revenue as % of GDP Federal or Central government`)  ~ `Taxes on goods and services,                Total`
    ),
    `Value added taxes as % of GDP` = case_when(
      !is.na(`5111 Value added taxes Revenue as % of GDP Federal or Central government`) ~ `5111 Value added taxes Revenue as % of GDP Federal or Central government`,
      is.na(`5111 Value added taxes Revenue as % of GDP Federal or Central government`) ~ VAT
    ),
    `Sales tax % of GDP` = case_when(
      !is.na(`5112 Sales tax Revenue as % of GDP Federal or Central government`) ~ `5112 Sales tax Revenue as % of GDP Federal or Central government`,
      is.na(`5112 Sales tax Revenue as % of GDP Federal or Central government`) ~ `Taxes on goods and services, of which Taxes on Sales`
    ),
    `Excises % of GDP` = case_when(
      !is.na(`5121 Excises Revenue as % of GDP Federal or Central government`) ~ `5121 Excises Revenue as % of GDP Federal or Central government`,
      is.na(`5121 Excises Revenue as % of GDP Federal or Central government`) ~ `Taxes on goods and services, of which Excises`
    ),
    `Taxes on international trade and transactions % of GDP` = `Taxes on international trade and transactions, Total`,
    `Customs and import duties % of GDP` = case_when(
      !is.na(`5123 Customs and import duties Revenue as % of GDP Federal or Central government`) ~ `5123 Customs and import duties Revenue as % of GDP Federal or Central government`, 
      is.na(`5123 Customs and import duties Revenue as % of GDP Federal or Central government`) ~ `Taxes on international trade and transactions Of which Import`
    ),
    `Taxes on exports % of GDP` = case_when(
      !is.na(`5124 Taxes on exports Revenue as % of GDP Federal or Central government`) ~ `5124 Taxes on exports Revenue as % of GDP Federal or Central government`,
      is.na(`5124 Taxes on exports Revenue as % of GDP Federal or Central government`) ~ `Taxes on international trade and transactions Of which Export`
    ),
    `Taxes on payroll and workforce % of GDP` = case_when(
      !is.na(`3000 Taxes on payroll and workforce Revenue as % of GDP Federal or Central government`) ~ `3000 Taxes on payroll and workforce Revenue as % of GDP Federal or Central government`,
      is.na(`3000 Taxes on payroll and workforce Revenue as % of GDP Federal or Central government`)  ~ `Taxes on payroll and workforce`
    ),
    `Taxes on property % of GDP` = case_when(
      !is.na(`4000 Taxes on property Revenue as % of GDP Federal or Central government`) ~ `4000 Taxes on property Revenue as % of GDP Federal or Central government`,
      is.na(`4000 Taxes on property Revenue as % of GDP Federal or Central government`)  ~ `Taxes on property`
    ),
    `Total non-tax revenue % of GDP` = case_when(
      !is.na(`Total non-tax revenue Revenue as % of GDP Federal or Central government`) ~ `Total non-tax revenue Revenue as % of GDP Federal or Central government`,
      is.na(`Total non-tax revenue Revenue as % of GDP Federal or Central government`)  ~ `Consolidated Non-Tax Revenue`
    ),
    `Non-tax revenue: Rents and royalties % of GDP` = case_when(
      !is.na(`Non-tax revenue: Rents and royalties Revenue as % of GDP Federal or Central government`) ~ `Non-tax revenue: Rents and royalties Revenue as % of GDP Federal or Central government`,
      is.na(`Non-tax revenue: Rents and royalties Revenue as % of GDP Federal or Central government`)  ~ `Total Resource Revenue`
    )
    
  ) 



# oecdgrd %>% 
#   select(Country_Code, year, 
#          `Total tax and non-tax revenue as % of GDP`, `Total tax revenue as % GDP`, 
#          `Non-resource tax excluding social contributions as % of GDP`, 
#          `Direct taxes including social contributions and resource revenue as % of GDP`, 
#          `Taxes on income, profits and capital gains as % of GDP`, 
#          `Taxes on income, profits and capital gains of individuals % as of GDP`, 
#          `Taxes on income, profits and capital gains of corporates as % GDP`, 
#          `On profits of corporates as % GDP`, 
#          `On capital gains of corporates as % of GDP`, `Indirect as % of GDP`, 
#          `Taxes on goods and services as % of GDP`, 
#          `Value added taxes as % of GDP`, `Sales tax % of GDP`, `Excises % of GDP`, 
#          `Taxes on international trade and transactions % of GDP`, 
#          `Customs and import duties % of GDP`, `Taxes on exports % of GDP`, 
#          `Taxes on payroll and workforce % of GDP`, `Taxes on property % of GDP`, 
#          `Total non-tax revenue % of GDP`, `Non-tax revenue: Rents and royalties % of GDP`,
#          `Total tax and non-tax revenue Revenue as % of GDP Federal or Central government`,
#          `Total tax revenue Revenue as % of GDP Federal or Central government`,
#          `Non-resource tax excluding social contributions`,
#          `Direct taxes including social contributions and resource revenue`,
#          `1000 Taxes on income, profits and capital gains Revenue as % of GDP Federal or Central government`,
#          `1100 Taxes on income, profits and capital gains of individuals Revenue as % of GDP Federal or Central government`,
#          `1200 Taxes on income, profits and capital gains of corporates Revenue as % of GDP Federal or Central government`,
#          `1210 On profits of corporates Revenue as % of GDP Federal or Central government`,
#          `1220 On capital gains of corporates Revenue as % of GDP Federal or Central government`,
#          `Indirect`,
#          `5000 Taxes on goods and services Revenue as % of GDP Federal or Central government`,
#          `5111 Value added taxes Revenue as % of GDP Federal or Central government`,
#          `5112 Sales tax Revenue as % of GDP Federal or Central government`,
#          `5121 Excises Revenue as % of GDP Federal or Central government`,
#          `Taxes on international trade and transactions, Total`,
#          `5123 Customs and import duties Revenue as % of GDP Federal or Central government` ,
#          `5124 Taxes on exports Revenue as % of GDP Federal or Central government`,
#          `3000 Taxes on payroll and workforce Revenue as % of GDP Federal or Central government`,
#          `4000 Taxes on property Revenue as % of GDP Federal or Central government`,
#          `Total non-tax revenue Revenue as % of GDP Federal or Central government`,
#          `Non-tax revenue: Rents and royalties Revenue as % of GDP Federal or Central government`
#   ) %>%
#   View()


oecdafr <- fread("data/RS_AFR_24112023.csv", stringsAsFactors = FALSE, na.strings = c("", "n\a", "NA", "..", "n/a", "--"))

oecdafr <- oecdafr %>%
  select(-c(`Unit Code`, Unit, `PowerCode Code`, PowerCode, `Reference Period Code`, `Reference Period`, `Flag Codes`, Flags, YEA, VAR, TAX, GOV)) %>%
  rename(Country_Code = COU)


oecdTaxVar <- c("Total tax and non-tax revenue", "Total tax revenue",
                "1000 Taxes on income, profits and capital gains",
                "2000 Social security contributions (SSC)",
                "3000 Taxes on payroll and workforce",
                "4000 Taxes on property",
                "5000 Taxes on goods and services",
                "5111 Value added taxes",
                "5121 Excises", "5123 Customs and import duties",
                "5124 Taxes on exports",
                "Total non-tax revenue",
                "Total non-tax revenue excluding grants",
                "Non-tax revenue: Rents and royalties",
                "Total tax revenues not including social security contributions")

# oecdTaxVar <- c("Total tax and non-tax revenue")

bceaoMacro <- fread("data/bceaoMacro.csv", header = FALSE, fill = TRUE,
                    stringsAsFactors = FALSE, na.strings = c("" ,"n/a", "NA", "-"))

listP <- c("COTE D'IVOIRE","BENIN","BURKINA FASO","MALI","NIGER","SENEGAL","GUINEE BISSAU","TOGO")


bceaoMacro <- bceaoMacro %>%
  filter_all(any_vars(!is.na(.))) %>%
  select(
    where(
      ~!all(is.na(.x))
    )
  ) %>%
  mutate(PAYS = rep(listP, each = 21)) %>%
  select(PAYS, everything()) %>%
  filter(!is.na(V2))

namesCol <- as.character(bceaoMacro[1, ])[-1]

names(bceaoMacro)[-1] <- namesCol

bceaoMacro <- bceaoMacro %>%
  filter(!(CODE %in% "CODE"))


bceaoMacro <- pivot_longer(bceaoMacro, cols = `1980`:`2023`, names_to = "ANNEE", 
                           values_to = "value", values_transform = list(value = as.numeric))


bceaoTofe <- fread("data/bceaoTofe.csv", header = FALSE, fill = TRUE,
                   stringsAsFactors = FALSE, na.strings = c("" ,"n/a", "NA", "-")) 

bceaoTofe <- bceaoTofe %>%
  filter_all(any_vars(!is.na(.))) %>%
  select(
    where(
      ~!all(is.na(.x))
    )
  ) %>%
  mutate(PAYS = rep(listP, each = 72)) %>%
  select(PAYS, everything()) %>%
  filter(!is.na(V2))

namesCol <- as.character(bceaoTofe[1, ])[-1]

names(bceaoTofe)[-1] <- namesCol

bceaoTofe <- bceaoTofe %>%
  filter(!(CODE %in% "CODE"))

bceaoTofe <- pivot_longer(bceaoTofe, cols = `1980`:`2023`, names_to = "ANNEE", 
                          values_to = "value", values_transform = list(value = as.numeric))

bceao <- bceaoMacro %>%
  bind_rows(bceaoTofe)


remove(bceaoMacro, bceaoTofe)

bceao <- pivot_wider(bceao, id_cols = c(PAYS, ANNEE), 
                     names_from = c(LIBELLE, `UNITE DE MESURE`, MAGNITUDE, `TYPE SERIE`),
                     names_sep = " ", values_from = value)

# bceao <- bceao %>%
#   select(PAYS, ANNEE, all_of(bceaoVar))

bceao <- bceao %>%
  select(PAYS, ANNEE, everything())

bceao <- bceao %>%
  mutate(
    `Recettes fiscales % PIB` = 100*(`Recettes fiscales FCFA Milliards Flux`/`PRODUIT INTERIEUR BRUT (PIB) NOMINAL F. CFA Milliards Flux`), 
    `valeur ajoutee du secteur primaire % PIB` = 100*(`valeur ajoutee du secteur primaire F. CFA Milliards Flux`/`PRODUIT INTERIEUR BRUT (PIB) NOMINAL F. CFA Milliards Flux`)
  )

bceao <- bceao %>%
  mutate(
    `Droits et taxes a l'importation % PIB` = 100*(`Droits et taxes a l'importation F. CFA Milliards Flux`/`PRODUIT INTERIEUR BRUT (PIB) NOMINAL F. CFA Milliards Flux`), 
    `Recettes totales et dons % PIB` = 100*(`Recettes totales et dons FCFA Milliards Flux`/`PRODUIT INTERIEUR BRUT (PIB) NOMINAL F. CFA Milliards Flux`),
    `Recettes totales hors dons % PIB` = 100*(`Recettes totales hors dons FCFA Milliards Flux`/`PRODUIT INTERIEUR BRUT (PIB) NOMINAL F. CFA Milliards Flux`),
    `Impots sur le commerce exterieur % PIB` = 100*(`Impots sur le commerce exterieur FCFA Milliards Flux`/ `PRODUIT INTERIEUR BRUT (PIB) NOMINAL F. CFA Milliards Flux`),
    `Impots directs % PIB` = 100*(`Impots directs FCFA Milliards Flux`/`PRODUIT INTERIEUR BRUT (PIB) NOMINAL F. CFA Milliards Flux`),
    `Recettes non fiscales % PIB` = 100*(`Recettes non fiscales FCFA Milliards Flux`/`PRODUIT INTERIEUR BRUT (PIB) NOMINAL F. CFA Milliards Flux`), 
    Country_Code = countrycode(sourcevar = PAYS, origin = "country.name.fr", destination = "iso3c"), 
    ANNEE = as.numeric(ANNEE)
  ) 

# From comparing values from IMF Article IV it appears that the values reported as direct tax are the sum of direct and indirect taxes. 

bceao <- bceao %>%
  select(
    Country_Code, year = ANNEE, `Total revenue including grants` = `Recettes totales et dons % PIB`, 
    `Total revenue excluding grants` = `Recettes totales hors dons % PIB`, `Tax revenue` = `Recettes fiscales % PIB`,
    `Tax on international trade` = `Impots sur le commerce exterieur % PIB`,
    `Direct and indirect taxes` = `Impots directs % PIB`, 
    `Nontax revenue` = `Recettes non fiscales % PIB`, 
    `GDP at current prices (Billions of local Currency)` = `PRODUIT INTERIEUR BRUT (PIB) NOMINAL F. CFA Milliards Flux`
  ) %>%
  filter(year < 2023)

oecdgrd <- oecdgrd %>%
  full_join(bceao, by = c("Country_Code", "year"))

# oecdgrd %>%
#   select(c(Country_Code, year, `Total tax and non-tax revenue as % of GDP`, `Total tax revenue as % GDP`, 
#            `Taxes on international trade and transactions % of GDP`, `Total non-tax revenue % of GDP`)) %>%
#   View()

oecdgrd <- oecdgrd %>% 
  mutate(
    `Total tax and non-tax revenue as % of GDP` = case_when(
      !is.na(`Total tax and non-tax revenue as % of GDP`) ~ `Total tax and non-tax revenue as % of GDP`, 
      is.na(`Total tax and non-tax revenue as % of GDP`) ~ `Total revenue including grants`
    ),
    `Total tax revenue as % GDP` = case_when(
      !is.na(`Total tax revenue as % GDP`) ~ `Total tax revenue as % GDP`,
      is.na(`Total tax revenue as % GDP`)  ~ `Tax revenue`
    ),
    `Taxes on international trade and transactions % of GDP` =  case_when(
      !is.na(`Taxes on international trade and transactions % of GDP`) ~ `Taxes on international trade and transactions % of GDP`, 
      is.na(`Taxes on international trade and transactions % of GDP`)  ~ `Tax on international trade`
    ),
    `Total non-tax revenue % of GDP` = case_when(
      !is.na(`Total non-tax revenue % of GDP`) ~ `Total non-tax revenue % of GDP`,
      is.na(`Total non-tax revenue % of GDP`)  ~ `Nontax revenue`
    )
    
  ) 

oecdgrdSelectedVar <- oecdgrd %>% 
  select(Country_Code, year, 
         `Total tax and non-tax revenue as % of GDP`, `Total tax revenue as % GDP`, 
         `Non-resource tax excluding social contributions as % of GDP`, 
         `Direct taxes including social contributions and resource revenue as % of GDP`, 
         `Taxes on income, profits and capital gains as % of GDP`, 
         `Taxes on income, profits and capital gains of individuals % as of GDP`, 
         `Taxes on income, profits and capital gains of corporates as % GDP`, 
         `On profits of corporates as % GDP`, 
         `On capital gains of corporates as % of GDP`, `Indirect as % of GDP`, 
         `Taxes on goods and services as % of GDP`, 
         `Value added taxes as % of GDP`, `Sales tax % of GDP`, `Excises % of GDP`, 
         `Taxes on international trade and transactions % of GDP`, 
         `Customs and import duties % of GDP`, `Taxes on exports % of GDP`, 
         `Taxes on payroll and workforce % of GDP`, `Taxes on property % of GDP`, 
         `Total non-tax revenue % of GDP`, `Non-tax revenue: Rents and royalties % of GDP`
  )

TaxVarF <- names(oecdgrdSelectedVar)[-c(1:2)]
TaxVarFInit <- names(oecdgrdSelectedVar)[-c(1:2, 10:11, 15, 21)]


CLASS <- read_excel("data/CLASS.xlsx") %>%
  filter(!is.na(Region)) %>%
  rename(Country_Code = Code, Country_Name = Economy)

groupsWB <- read_excel("data/CLASS.xlsx", sheet = "Groups") %>%
  select(-...5) %>%
  mutate(iso = CountryCode)

groupsWB <- pivot_wider(groupsWB, names_from = GroupName, values_from = CountryCode)

groupsWB <- groupsWB %>% 
  mutate(across(everything(), 
                ~ case_when(iso %in% . ~ 1, 
                            !(iso %in% .) ~ 0),
                .names = "Group var: {.col}")
  ) %>%
  select(iso, starts_with("Group var")) %>%
  select(-c(`Group var: GroupCode`, `Group var: CountryName`, `Group var: iso`)) %>%
  distinct()

Identifier <- groupsWB %>%
  rename(Country_Code = iso) %>%
  right_join(CLASS, by = "Country_Code") %>% 
  left_join(tbloc, by = "Country_Code") %>%
  select(Country_Code, Country_Name, Region, `Income group`, `Lending category`, `Trading Bloc`, everything()) %>%
  mutate(
    
    `Africa Zones` = case_when(
      `Group var: Africa Eastern and Southern` %in% 1 ~ "Group var: Africa Eastern and Southern", 
      `Group var: Africa Western and Central`  %in% 1 ~ "Group var: Africa Western and Central"
    ), 
    `Demographic dividend` = case_when(
      `Group var: Early-demographic dividend` %in% 1 ~ "Early-demographic dividend", 
      `Group var: Pre-demographic dividend` %in% 1 ~ "Pre-demographic dividend", 
      `Group var: Late-demographic dividend` %in% 1 ~ "Late-demographic dividend", 
      `Group var: Early-demographic dividend` %in% 1 ~ "Early-demographic dividend", 
      `Group var: Post-demographic dividend`  %in% 1 ~ "Post-demographic dividend"
    ),
    `Small states` = case_when(
      `Group var: Caribbean small states` %in% 1 ~ "Caribbean small states",
      `Group var: Pacific island small states` %in% 1 ~ "Pacific island small states", 
      `Group var: Other small states`  %in% 1 ~ "Other small states"
    )
  ) %>% 
  select(Country_Code, Country_Name, Region, `Income group`, `Lending category`, `Trading Bloc`,
         `Africa Zones`, `Demographic dividend`, `Small states`, 
         `Group var: Arab World`, `Group var: Central Europe and the Baltics`, `Group var: Euro area`,
         `Group var: European Union`, `Group var: Fragile and conflict affected situations`, 
         `Group var: Heavily indebted poor countries (HIPC)`, `Group var: Least developed countries: UN classification`, 
         `Group var: OECD members`
  )

Identifier[Identifier$Country_Name %in% "Liberia", "Trading Bloc"] <- "ECO"

CountriesWorldList <- fread("data/Identifier.csv", 
                            stringsAsFactors = FALSE, na.strings = c("" ,"n/a", "NA")) 

# uniformisation des variables ID 
names(CountriesWorldList)[names(CountriesWorldList) == "HISO3"] <- "Country_Code"
names(CountriesWorldList)[names(CountriesWorldList) == "HCountryName"] <- "Country_Name"

CountriesWorldList <- CountriesWorldList[!is.na(Country_Code), c("Country_Code", "Country_Name"), with = FALSE]

# dat %>% 
#   mutate(
#     across(
#       .cols = everything(),
#       .fns = rank,
#       .names = "{.col}_rank"
#     )
#   )

world_map <- map_data("world")
world_map$Country_Code <- countrycode::countrycode(sourcevar = world_map$region, origin = "country.name", destination = "iso3c")
world_map[world_map$region %in% "Virgin Islands", "Country_Code"] <- "VIR"
world_map[world_map$region %in% "Kosovo", "Country_Code"] <- "XKX"
world_map[world_map$region %in% "Barbuda", "Country_Code"] <- "ATG"
world_map[world_map$region %in% "Micronesia", "Country_Code"] <- "FSM"
world_map[world_map$region %in% "Saint Martin", "Country_Code"] <- "MAF"
world_map[world_map$region %in% "Ascension Island", "Country_Code"] <- "SHN"
world_map[world_map$region %in% "Grenadines", "Country_Code"] <- "VCT"

world_map <- world_map %>%
  filter(!is.na(Country_Code))

data_map <- read_sf(dsn = "data/world-administrative-boundaries/world-administrative-boundaries.shp")

dataALL <- Identifier %>%
  right_join(oecdgrd, by = "Country_Code")

dataApp <- Identifier %>%
  # select(Country_Code, Country_Name, Region, `Income group`) %>%
  right_join(oecdgrdSelectedVar, by = "Country_Code")

remove(CLASS, groupsWB, tbloc)

remove(GRD, bceao, oecdafrFinal, oecdgrd)

dfweo <- fread("data/WEOOct2023all.xls", stringsAsFactors = FALSE, na.strings = c("", "n\a", "NA", "..", "n/a", "--"))

# Remove rows without data in column ISO
dfweo <- dfweo[!is.na(ISO), ]

# Get columns information from the file to identify where identifiers stop and where data start
eidcol <- which(colnames(dfweo)=="1980")-1
smcol <- which(colnames(dfweo)=="1980")
emcol <- ncol(dfweo)-1
ecoldata <- ncol(dfweo)
col2cvt <- smcol:emcol
dfweo[,col2cvt] <- lapply(dfweo[,..col2cvt],function(x){as.numeric(gsub(",", "", x))})

# Reshape and organize

dfweoPanel <- melt.data.table(dfweo, id = c(1:eidcol, ecoldata), 
                              measure.vars = c(smcol:emcol), 
                              variable.name = "Year")
names(dfweoPanel)[1] <- "WEOCountryCode"
names(dfweoPanel)[3] <- "Variable_code"

dfweoPanel[, Variables := paste(`Subject Descriptor`, Units, Scale, sep = " - ")]

dfweoPanel <- dcast.data.table(data = dfweoPanel,
                               ISO + Country + Year + WEOCountryCode ~ Variables,
                               value.var = "value")

names(dfweoPanel) <- gsub(' - NA', "", names(dfweoPanel))

dfweoPanel <- dfweoPanel %>%
  rename(iso3 = ISO, year = Year)

dfweoPanel[dfweoPanel$iso3 == "UVK", "iso3"] <- "XKX"
dfweoPanel[dfweoPanel$iso3 == "WBG", "iso3"] <- "PSE"
dfweoPanel[dfweoPanel$iso3 == "CIV", "Country"] <- "Cote d'Ivoire"
dfweoPanel[dfweoPanel$iso3 == "STP", "Country"] <- "Sao Tome and Principe"

dfweoPanel$year <- as.numeric(as.character(dfweoPanel$year))

dfweoPanel <- setDT(dfweoPanel)

cols.num <- names(dfweoPanel)[!names(dfweoPanel) %in% c("iso3", "Country", "WEOCountryCode", "year")]

dfweoPanel <- dfweoPanel[, (cols.num) := lapply(.SD, as.numeric), .SDcols = cols.num]

dfweoPanel <-  dfweoPanel %>%
  filter(year < 2023) %>%
  select(Country_Code = iso3, year, everything()) %>%
  select(-WEOCountryCode, Country)


remove(dfweo)

dataApp <- dataApp %>%
  left_join(dfweoPanel, by = c("Country_Code", "year"))

dataALL <- dataALL %>%
  left_join(dfweoPanel, by = c("Country_Code", "year"))

hdi <- fread("data/human-development-index.csv", stringsAsFactors = FALSE, na.strings = c("", "n\a", "NA", "..", "n/a", "--"))

hdi <- hdi %>%
  filter(!is.na(Code)) %>%
  rename(year = Year, Country_Code = Code) %>%
  select(-Entity)

dataApp <- dataApp %>%
  left_join(hdi,  by = c("Country_Code", "year"))


dataALL <- dataALL %>%
  left_join(hdi, by = c("Country_Code", "year"))


wdi <- fread("data/wdi/Data.csv", stringsAsFactors = FALSE, na.strings = c("", "n\a", "NA", "..", "n/a", "--"))

wdi <- wdi[!is.na(`Country Code`), ] %>%
  rename(Country_Code = `Country Code`, year = Time) %>%
  select(-`Time Code`, `Country Name`)

dataApp <- dataApp %>%
  left_join(wdi,  by = c("Country_Code", "year")) %>%
  filter(year<2023) 

dataApp <- dataApp %>%
  rename(Year = year)

dataALL <- dataALL %>%
  left_join(wdi, by = c("Country_Code", "year")) %>%
  filter(year<2023) %>%
  rename(Year = year)

GRPVAR <- c("Region", "Income group", "Lending category", "Trading Bloc", "Africa Zones", 
            "Demographic dividend", "Small states", "Group var: Arab World", 
            "Group var: Central Europe and the Baltics", "Group var: Euro area", 
            "Group var: European Union", "Group var: OECD members", 
            "Group var: Fragile and conflict affected situations", 
            "Group var: Heavily indebted poor countries (HIPC)", "Group var: Least developed countries: UN classification")


cSSA <- c("Angola", "Botswana", "Burundi", "Comoros", "Congo, Dem. Rep.", 
          "Eritrea", "Eswatini", "Ethiopia", "Kenya", "Lesotho",
          "Madagascar", "Malawi", "Mauritius", "Mozambique", "Namibia", 
          "Rwanda", "São Tomé and Príncipe", "Seychelles", "Somalia", "South Africa", 
          "South Sudan", "Sudan", "Tanzania", "Uganda", "Zambia", 
          "Zimbabwe", "Benin", "Burkina Faso", "Cabo Verde", "Cameroon",
          "Central African Republic", "Chad", "Congo, Rep.", "Côte d’Ivoire", "Equatorial Guinea",
          "Gabon", "Gambia, The", "Ghana", "Guinea", "Guinea-Bissau",
          "Liberia", "Mali", "Mauritania", "Niger", "Nigeria",
          "Senegal", "Sierra Leone", "Togo")

cUEMOA <- Identifier[Identifier$Country_Name %in% c("Benin", "Burkina Faso", "Côte d’Ivoire", "Guinea-Bissau", "Mali", "Niger", "Senegal", "Togo"), "Country_Code"]

cUEMOA <- as.vector(cUEMOA$Country_Code)


Regdata <- dataApp %>%
  select(
    Country_Code, Country_Name, Year, Region, `Total tax revenue as % GDP`, 
    `GDP per capita (constant 2015 US$) [NY.GDP.PCAP.KD]`, `GDP per capita, PPP (current international $) [NY.GDP.PCAP.PP.CD]`,
    `Gross domestic product per capita, current prices - Purchasing power parity; international dollars - Units`,
    `Agriculture, forestry, and fishing, value added (% of GDP) [NV.AGR.TOTL.ZS]`, `Trade (% of GDP) [NE.TRD.GNFS.ZS]`, 
    `Total natural resources rents (% of GDP) [NY.GDP.TOTL.RT.ZS]`, 
      `Net official development assistance and official aid received (current US$) [DT.ODA.ALLD.CD]`,
      `Net official development assistance and official aid received (constant 2020 US$) [DT.ODA.ALLD.KD]`
  ) %>%
  mutate(trend = Year - min(Year, na.rm = TRUE) +1) %>%
  filter(Region %in% "Sub-Saharan Africa" )

Regdata$Id <- c(1:uniqueN(levels(as.factor(Regdata$Country_Code))))[as.factor(Regdata$Country_Code)]



formu <-  log(`Total tax revenue as % GDP`) ~ log(`GDP per capita (constant 2015 US$) [NY.GDP.PCAP.KD]`) + 
  log(`Agriculture, forestry, and fishing, value added (% of GDP) [NV.AGR.TOTL.ZS]`) + 
  log(`Trade (% of GDP) [NE.TRD.GNFS.ZS]`) + log(`Total natural resources rents (% of GDP) [NY.GDP.TOTL.RT.ZS]`) + trend
  

model <- npsf::sf(log(`Total tax revenue as % GDP`) ~ log(`GDP per capita (constant 2015 US$) [NY.GDP.PCAP.KD]`) + 
                    log(`Agriculture, forestry, and fishing, value added (% of GDP) [NV.AGR.TOTL.ZS]`),
                  data = Regdata, it = c("Id", "Year"),
                  prod = TRUE, model = "BC1992",
                  eff.time.invariant = FALSE, mean.u.0i.zero = TRUE)












