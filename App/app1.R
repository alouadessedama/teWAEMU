# 
# list.of.packages <- c("shiny", "shinyjs", "shinyBS", "shinythemes", "DT", "plotly", "tidyverse", "highcharter", 
#                       "data.table", "stringr", "fmsb", "treemap", "remotes", "maps", "wbstats", "shinydashboard", 
#                       "sf", "leaflet", "shinyWidgets", "countrycode")
# new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
# if(length(new.packages)) install.packages(new.packages)

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


# installation de d3treeR
# install.packages("remotes")
# remotes::install_github("d3treeR/d3treeR")

# library(d3treeR)

# setwd("C:/Users/aadama/OneDrive - Université Clermont Auvergne/UEMOA/App")

# function 

add_summary_rows <- function(.data, ...) {
  group_modify(.data, function(x, y) bind_rows(x, summarise(x, ...)))
}


# ----------- Data 

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
#   select(Country_Code, year, `Total non-tax revenue % of GDP`, `1000 Taxes on income, profits and capital gains Revenue as % of GDP Total`,
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
      !is.na(`Total tax and non-tax revenue Revenue as % of GDP Total`) ~ `Total tax and non-tax revenue Revenue as % of GDP Total`, 
      is.na(`Total tax and non-tax revenue Revenue as % of GDP Total`) ~ `Revenue including social contributions`
    ),
    `Total tax revenue as % GDP` = case_when(
      !is.na(`Total tax revenue Revenue as % of GDP Total`) ~ `Total tax revenue Revenue as % of GDP Total`,
      is.na(`Total tax revenue Revenue as % of GDP Total`)  ~ `Taxes including social contributions`
    ),
    `Non-resource tax excluding social contributions as % of GDP` = `Non-resource tax excluding social contributions`, 
    `Direct taxes including social contributions and resource revenue as % of GDP` = `Direct taxes including social contributions and resource revenue`,
    `Taxes on income, profits and capital gains as % of GDP` = case_when(
      !is.na(`1000 Taxes on income, profits and capital gains Revenue as % of GDP Total`) ~ `1000 Taxes on income, profits and capital gains Revenue as % of GDP Total`, 
      is.na(`1000 Taxes on income, profits and capital gains Revenue as % of GDP Total`) ~ `Taxes on income, profits, and capital gains`
    ),
    `Taxes on income, profits and capital gains of individuals % as of GDP` = case_when(
      !is.na(`1100 Taxes on income, profits and capital gains of individuals Revenue as % of GDP Total`) ~ `1100 Taxes on income, profits and capital gains of individuals Revenue as % of GDP Total`, 
      is.na(`1100 Taxes on income, profits and capital gains of individuals Revenue as % of GDP Total`) ~ Individuals
    ),
    `Taxes on income, profits and capital gains of corporates as % GDP` = case_when(
      !is.na(`1200 Taxes on income, profits and capital gains of corporates Revenue as % of GDP Total`) ~ `1200 Taxes on income, profits and capital gains of corporates Revenue as % of GDP Total`,
      is.na(`1200 Taxes on income, profits and capital gains of corporates Revenue as % of GDP Total`)  ~ `Corporations and other enterprises`
    ),
    `On profits of corporates as % GDP` = `1210 On profits of corporates Revenue as % of GDP Total`,
    `On capital gains of corporates as % of GDP` = `1220 On capital gains of corporates Revenue as % of GDP Total`,
    `Indirect as % of GDP` = Indirect ,
    `Taxes on goods and services as % of GDP` = case_when(
      !is.na(`5000 Taxes on goods and services Revenue as % of GDP Total`) ~ `5000 Taxes on goods and services Revenue as % of GDP Total`,
      is.na(`5000 Taxes on goods and services Revenue as % of GDP Total`)  ~ `Taxes on goods and services,                Total`
    ),
    `Value added taxes as % of GDP` = case_when(
      !is.na(`5111 Value added taxes Revenue as % of GDP Total`) ~ `5111 Value added taxes Revenue as % of GDP Total`,
      is.na(`5111 Value added taxes Revenue as % of GDP Total`) ~ VAT
    ),
    `Sales tax % of GDP` = case_when(
      !is.na(`5112 Sales tax Revenue as % of GDP Total`) ~ `5112 Sales tax Revenue as % of GDP Total`,
      is.na(`5112 Sales tax Revenue as % of GDP Total`) ~ `Taxes on goods and services, of which Taxes on Sales`
    ),
    `Excises % of GDP` = case_when(
      !is.na(`5121 Excises Revenue as % of GDP Total`) ~ `5121 Excises Revenue as % of GDP Total`,
      is.na(`5121 Excises Revenue as % of GDP Total`) ~ `Taxes on goods and services, of which Excises`
    ),
    `Taxes on international trade and transactions % of GDP` = `Taxes on international trade and transactions, Total`,
    `Customs and import duties % of GDP` = case_when(
      !is.na(`5123 Customs and import duties Revenue as % of GDP Total`) ~ `5123 Customs and import duties Revenue as % of GDP Total`, 
      is.na(`5123 Customs and import duties Revenue as % of GDP Total`) ~ `Taxes on international trade and transactions Of which Import`
    ),
    `Taxes on exports % of GDP` = case_when(
      !is.na(`5124 Taxes on exports Revenue as % of GDP Total`) ~ `5124 Taxes on exports Revenue as % of GDP Total`,
      is.na(`5124 Taxes on exports Revenue as % of GDP Total`) ~ `Taxes on international trade and transactions Of which Export`
    ),
    `Taxes on payroll and workforce % of GDP` = case_when(
      !is.na(`3000 Taxes on payroll and workforce Revenue as % of GDP Total`) ~ `3000 Taxes on payroll and workforce Revenue as % of GDP Total`,
      is.na(`3000 Taxes on payroll and workforce Revenue as % of GDP Total`)  ~ `Taxes on payroll and workforce`
    ),
    `Taxes on property % of GDP` = case_when(
      !is.na(`4000 Taxes on property Revenue as % of GDP Total`) ~ `4000 Taxes on property Revenue as % of GDP Total`,
      is.na(`4000 Taxes on property Revenue as % of GDP Total`)  ~ `Taxes on property`
    ),
    `Total non-tax revenue % of GDP` = case_when(
      !is.na(`Total non-tax revenue Revenue as % of GDP Total`) ~ `Total non-tax revenue Revenue as % of GDP Total`,
      is.na(`Total non-tax revenue Revenue as % of GDP Total`)  ~ `Consolidated Non-Tax Revenue`
    ),
    `Non-tax revenue: Rents and royalties % of GDP` = case_when(
      !is.na(`Non-tax revenue: Rents and royalties Revenue as % of GDP Total`) ~ `Non-tax revenue: Rents and royalties Revenue as % of GDP Total`,
      is.na(`Non-tax revenue: Rents and royalties Revenue as % of GDP Total`)  ~ `Total Resource Revenue`
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
#          `Total tax and non-tax revenue Revenue as % of GDP Total`,
#          `Total tax revenue Revenue as % of GDP Total`,
#          `Non-resource tax excluding social contributions`,
#          `Direct taxes including social contributions and resource revenue`,
#          `1000 Taxes on income, profits and capital gains Revenue as % of GDP Total`,
#          `1100 Taxes on income, profits and capital gains of individuals Revenue as % of GDP Total`,
#          `1200 Taxes on income, profits and capital gains of corporates Revenue as % of GDP Total`,
#          `1210 On profits of corporates Revenue as % of GDP Total`,
#          `1220 On capital gains of corporates Revenue as % of GDP Total`,
#          `Indirect`,
#          `5000 Taxes on goods and services Revenue as % of GDP Total`,
#          `5111 Value added taxes Revenue as % of GDP Total`,
#          `5112 Sales tax Revenue as % of GDP Total`,
#          `5121 Excises Revenue as % of GDP Total`,
#          `Taxes on international trade and transactions, Total`,
#          `5123 Customs and import duties Revenue as % of GDP Total` ,
#          `5124 Taxes on exports Revenue as % of GDP Total`,
#          `3000 Taxes on payroll and workforce Revenue as % of GDP Total`,
#          `4000 Taxes on property Revenue as % of GDP Total`,
#          `Total non-tax revenue Revenue as % of GDP Total`,
#          `Non-tax revenue: Rents and royalties Revenue as % of GDP Total`
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
    # `Taxes on international trade and transactions % of GDP` =  case_when(
    #   !is.na(`Taxes on international trade and transactions % of GDP`) ~ `Taxes on international trade and transactions % of GDP`, 
    #   is.na(`Taxes on international trade and transactions % of GDP`)  ~ `Tax on international trade`
    # ),
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
  select(-c(`Time Code`, `Country Name`))

wdiAdd <- fread("data/wdi/othercontrols.csv", stringsAsFactors = FALSE, na.strings = c("", "n\a", "NA", "..", "n/a", "--"))

wdiAdd <- wdiAdd[!is.na(`Country Code`), ] %>%
  rename(Country_Code = `Country Code`, year = Time) %>%
  select(-c(`Time Code`, `Country Name`, `Personal remittances, received (% of GDP) [BX.TRF.PWKR.DT.GD.ZS]`))

wdi <- wdi %>%
  left_join(wdiAdd, by = c("Country_Code", "year"))

wdi <- wdi %>%
  mutate(
    `Imports of goods and services (% of GDP) [NE.IMP.GNFS.ZS]` = case_when(
      !is.na(`Imports of goods and services (% of GDP) [NE.IMP.GNFS.ZS].y`) ~ `Imports of goods and services (% of GDP) [NE.IMP.GNFS.ZS].y`, 
      is.na(`Imports of goods and services (% of GDP) [NE.IMP.GNFS.ZS].y`) ~ `Imports of goods and services (% of GDP) [NE.IMP.GNFS.ZS].x`
    ), 
    `Exports of goods and services (% of GDP) [NE.EXP.GNFS.ZS]` = case_when(
      !is.na(`Exports of goods and services (% of GDP) [NE.EXP.GNFS.ZS].y`) ~ `Exports of goods and services (% of GDP) [NE.EXP.GNFS.ZS].y`,
      is.na(`Exports of goods and services (% of GDP) [NE.EXP.GNFS.ZS].y`) ~ `Exports of goods and services (% of GDP) [NE.EXP.GNFS.ZS].x`
    ), 
    `Net official development assistance and official aid received (% of GDP)` = 100*(`Net official development assistance and official aid received (current US$) [DT.ODA.ALLD.CD]` /`GDP (current US$) [NY.GDP.MKTP.CD]`),
    `Net official development assistance received (% of GDP)` = 100*(`Net official development assistance received (current US$) [DT.ODA.ODAT.CD]`/`GDP (current US$) [NY.GDP.MKTP.CD]`)
  ) %>%
  select(-c(`Imports of goods and services (% of GDP) [NE.IMP.GNFS.ZS].y`, `Imports of goods and services (% of GDP) [NE.IMP.GNFS.ZS].x`,
            `Exports of goods and services (% of GDP) [NE.EXP.GNFS.ZS].y`, `Exports of goods and services (% of GDP) [NE.EXP.GNFS.ZS].x`))


# wdi %>%
#   select(`Country Name`, year,
#     `Imports of goods and services (% of GDP) [NE.IMP.GNFS.ZS].x`, `Imports of goods and services (% of GDP) [NE.IMP.GNFS.ZS].y`, 
#          `Exports of goods and services (% of GDP) [NE.EXP.GNFS.ZS].x`, `Exports of goods and services (% of GDP) [NE.EXP.GNFS.ZS].y`, 
#     `Personal remittances, received (% of GDP) [BX.TRF.PWKR.DT.GD.ZS].x` , `Personal remittances, received (% of GDP) [BX.TRF.PWKR.DT.GD.ZS].y`   ) %>%
#   summary()

dataApp <- dataApp %>%
  left_join(wdi,  by = c("Country_Code", "year")) %>%
  filter(year<2023) %>%
  select(-Country) %>%
  rename(Year = year, Old_Country_Name = Country_Name) %>%
  left_join(CountriesWorldList, by = "Country_Code") %>%
  mutate(Country_Name = ifelse(!is.na(Old_Country_Name), Old_Country_Name, Country_Name)) %>%
  select(-Old_Country_Name) %>%
  filter(!is.na(Country_Code)) %>%
  select(Country_Code, Country_Name, Year, everything())
  


dataALL <- dataALL %>%
  left_join(wdi, by = c("Country_Code", "year")) %>%
  filter(year<2023) %>%
  select(-Country) %>%
  rename(Year = year, Old_Country_Name = Country_Name) %>%
  left_join(CountriesWorldList, by = "Country_Code") %>%
  mutate(Country_Name = ifelse(!is.na(Old_Country_Name), Old_Country_Name, Country_Name)) %>%
  select(-Old_Country_Name) %>%
  filter(!is.na(Country_Code)) %>%
  select(Country_Code, Country_Name, Year, everything())

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



# Define UI for app  ----
ui <- tagList(useShinyjs(),
              #themeSelector(),
              tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "style.css")),
              navbarPage("Tax Effort Analysis", theme = shinytheme("flatly"),
                
                tabPanel(title = tags$div("Description", class = "tabname"),
                         tags$br(),
                         fluidRow(column(1),
                                  column(6, align="justify",
                                         fluidRow(tags$div("Description of the Dashboard", class ="Title1T"),
                                                  align = "center"),
                                         tags$br(),
                                         tags$br(),
                                         
                                         fluidRow(
                                           column(12, 
                                                  tags$div(
                                                  )
                                           )
                                         )
                                  ),
                                  column(4,
                                         fluidRow(
                                           column(1),
                                           column(11,
                                                  tags$div("About the Application", class ="Title1T"),
                                                  HTML("
                                          <h4 style ='font-family:garamond;color:#0000b3;'> Team: </h4>
                                          <ul>
                                          <li><strong>Alou Adess&eacute; Dama</strong>, FERDI</li>
                                          <li><strong>Anouck Daubree</strong>, FERDI</li>
                                          <li><strong>Gr&eacutegoire Rota-Graziosi</strong>, UCA-CERDI-CNRS</li>

                                          </ul>
                                          </p>
                                          <h4 style ='font-family:garamond;color:#0000b3;'>Developper:</h4>
                                          <p>
                                          <ul>
                                          <li><strong>Alou Adess&eacute; Dama</strong>, Ph.D in Economics from UCA-CERDI-CNRS.</li>
                                          </ul>
                                          </p>"),
                                                  HTML("<h4 style ='font-family:garamond;color:#0000b3;'>Affiliations:</h4>"),
                                                  tags$br(),
                                                  
                                                  fluidRow(align ="center",
                                                    column(1),
                                                    column(5, tags$img(src ="Logo_Ferdi.jpg", height = 90)),
                                                    column(4, tags$img(src ="logo_UCA.jpg", height = 100)),
                                                    
                                                  ),
                                                  tags$br(),
                                                  tags$br(),
                                                  fluidRow(align ="center",
                                                    column(1),
                                                    column(4, tags$img(src ="LOGO_CNRS_2019_RVB.PNG", height = 130)),
                                                    column(5, tags$img(src ="logo_CERDI_UCA-01.jpg", height = 140))
                                                  ),
                                                  tags$br(),
                                                  tags$br(),
                                                  tags$div(
                                                    tags$p("Report bugs to ...")
                                                  )
                                           )
                                         )
                                  ))
                ),
                
                tabPanel(title = tags$div("Tables for the Report", class = "tabname"),
                         br(),
                         sidebarLayout(
                           sidebarPanel = sidebarPanel(
                           ),
                           mainPanel = mainPanel(
                             
                          
                             fluidRow(
                               tags$div("Tax and non-tax revenue", class = "TitleText"), 
                               align = "center"
                               ),
                             fluidRow(column(1), 
                                      column(10, "The following table reports tax and non-tax revenue data for WAEMU and individual countries. 
                                             The data combine OECD Revenue statistics in Africa 2023 and the latest version of UNIWIDER Government Revenue dataset")),
                             fluidRow(
                               column(3, uiOutput("preambule_uiF")), 
                               column(8, uiOutput("addallvar_uiF")),
                               dataTableOutput("preambuleTabF")
                             ),
                             br(), 
                             br(), 
                             
                             
                             fluidRow(column(1), 
                                      column(10, "The following table provides international comparison for selected tax variables for individual WAEMU countries.")),
                             fluidRow(
                               column(3, uiOutput("TabTwoC_ui")), 
                               column(3, uiOutput("tabTwoY_ui")),
                               dataTableOutput("TabComparison")
                             ),
                             br(), 
                             br(), 
                             fluidRow(column(1), 
                                      column(10, "For the record: the following table reports tax and non-tax revenue data for WAEMU and individual countries. 
                                             Here the data include only UNIWIDER Government Revenue dataset")),
                             fluidRow(
                               column(3, uiOutput("preambule_ui")), 
                               column(7, uiOutput("addallvar_ui")),
                               dataTableOutput("preambuleTab")
                             ),
                             br(), 
                             br(), 
                             fluidRow(column(1), 
                                      column(10, "For the record: the following table reports tax and non-tax revenue data for WAEMU and individual countries. 
                                             Here the data include only OECD Revenue statistics in Africa 2023")),
                             fluidRow(
                               column(3, uiOutput("preambule_ui1")),
                               column(7, uiOutput("addallvar_ui1")),
                               dataTableOutput("preambuleTab1")
                             ),
                             br(), 
                             br()

                         )
                         )
                ), 
                
                
                tabPanel(title = tags$div("Tables", class = "tabname"),
                         br(),
                         sidebarLayout(
                           sidebarPanel = sidebarPanel(
                           ),
                           mainPanel = mainPanel(
                             
                             
                             fluidRow(
                               tags$div("Tax and non-tax revenue", class = "TitleText"), 
                               align = "center"
                             ),
                             fluidRow(column(1), 
                                      column(10, "The following table reports tax and non-tax revenue data for WAEMU and individual countries. 
                                             The data combine OECD Revenue statistics in Africa 2023 and the latest version of UNIWIDER Government Revenue dataset")),
                             
                             fluidRow(
                               column(3, uiOutput("tableAllC_ui")), 
                               column(8, uiOutput("tableallcAddvar_ui")), 
                               dataTableOutput("tableAllCTab")
                             ),
                             br(), 
                             br()
                             
                           )
                         )
                ), 
                
                
                # ---------------------------------------------------------
                # Graphical analysis
                #----------------------------------------------------------
                tabPanel(title = tags$div("Graphical analysis", class = "tabname"),
                         br(),
                         sidebarLayout(
                           
                           # side bar panel 
                           sidebarPanel(width = 4,
                                        fluidRow(selectInput("choose_graph", label = tags$div("Select graph", class ="TitleInput"),
                                                             choices = c("Choisir" = "",
                                                                         "Scatter plot" = "Nuage de points",
                                                                         "Histogram" = "Histogramme", 
                                                                         "Bar plot" = "Barplot", 
                                                                         "Line plot" = "Evolution temporelle",
                                                                         "Treemap", 
                                                                         "Map" = "map"
                                                             ),
                                                             selected = "Nuage de points")),
                                        uiOutput("input_graphic")
                                        
                           ),
                           
                           # the main panel 
                           mainPanel(
                             uiOutput("mainGraphic")
                           )
                         )
                ),
                
                # ---------------------------------------------------------
                # Analysis
                #----------------------------------------------------------
                
                tabPanel("Estimation of Tax Effort",
                         sidebarLayout(
                           sidebarPanel(
                             fluidRow(
                               tags$span(
                                 popify(
                                   fileInput("user.data", label = tags$div("Upload data", class ="TitleInput"), 
                                             multiple = FALSE, accept = c("text/csv", ".csv", ".xls")),
                                   "Upload your data",
                                   "The  format of file should be <b>csv</b>. The first row of the data should be the name of the variables.The data should have a <b>year</b> column and at least one <b>iso3</b> or <b>country</b> column."))
                             ),
                             fluidRow(uiOutput("importErrorUI")),
                             uiOutput("InpSelecUser_ui"),
                             width = 3
                           ),
                           
                           mainPanel(
                             width = 9,
                             tabsetPanel(
                               type = "pills",
                               tabPanel(
                                 "Stat. Des.", tags$br(),
                                 fluidRow(column(12, h4("Summary of dependent and predictors variables"),
                                                 align = "center")),
                                 fluidRow(column(12, dataTableOutput("StatSelVar"))),

                                 fluidRow(
                                   column(12, h4(""))
                                 ),
                                 fluidRow(column(12, tableOutput("UsData")))
                               ),
                               
                               tabPanel("Tax effort - analysis", tags$br(),
                                        # dataTableOutput("Seetable"),
                                        fluidRow(
                                          column(width = 6,
                                              dataTableOutput("tabsum_sg")),
                                          column(width = 6,
                                              plotOutput("TeGrEv", height = "308px"))
                                        ),
                                        fluidRow(dropdownButton(
                                          label = "Graph options",
                                          uiOutput("CorVarTeSg_ui"),
                                          circle = FALSE, status = "primary",
                                          icon = icon("sliders"), width = "400px",
                                          tooltip = tooltipOptions(title = "Press to change the graph")
                                          ),
                                          column(6, plotOutput("corr_te_macvarSg")),
                                        ),
                                        fluidRow(
                                          dataTableOutput("tabTeSg")
                                        ),
                                        
                                        
                                        br()
                               ),
                               tabPanel("Tax effort - estimation results",
                                        fluidRow(verbatimTextOutput("ModSecGen"))
                               )
                             )
                           )
                         )
                )
                
))

server <- function(input, output, session) {
  
  output$UsData <- renderTable({
    req(input$user.data)
    
    df <- fread(input$user.data$datapath, stringsAsFactors = FALSE, 
                na.strings = c("", "n\a", "NA", ".."))
    head(df)
    
  })
  
  
  df_finale <- reactive({
    if(is.null(input$user.data)) {
      dfe <- dataApp
    } else {
    req(input$user.data)
    dfuser <- tryCatch(fread(input$user.data$datapath, stringsAsFactors = FALSE, 
                             na.strings = c("", "n\a", "NA", "..")))
    
    if(
      ("iso3" %in% names(dfuser)) &
      ("year" %in% names(dfuser)) &
      ("country" %in% names(dfuser))
    ) {
      
      # delete country_code variable from user data
      if(any(grepl("Country_Code", names(dfuser), ignore.case = TRUE))) {
        dfuser <- dfuser %>% select(-c(grep("Country_Code", names(dfuser), ignore.case = TRUE, value = TRUE)))
      }
      
      # delete Year variable from user data
      if(any(grepl("Year", names(dfuser), ignore.case = TRUE))) {
        dfuser <- dfuser %>% select(-c(grep("Year", names(dfuser), ignore.case = TRUE, value = TRUE)))
      }
      
      # delete country_name variable from user data
      if(any(grepl("Country_Name", names(dfuser), ignore.case = TRUE))) {
        dfuser <- dfuser %>% select(-c(grep("Country_Name", names(dfuser), ignore.case = TRUE, value = TRUE)))
      }
      
      df <- setDT(dfuser)
      
      # uniformisation des variables ID 
      names(df)[names(df) == "iso3"] <- "Country_Code"
      names(df)[names(df) == "year"] <- "Year"
      #names(df)[names(df) == "country"] <- "Country_Name"
      
      # uniformisation des types pour les trois variables 
      #df[, Country_Name := as.character(Country_Name)]
      df[, Country_Code := as.character(Country_Code)]
      df[, Year := as.numeric(Year)]
      
      dfe <- dataApp %>%
        full_join(df, by = c("Country_Code", "Year")) %>%
        mutate(Country_Name = ifelse(is.na(Country_Name), country, Country_Name)) %>%
        filter(!is.na(Country_Code))

      return(dfe)
      
      # case user have iso3 and year
      
    } else if(
      ("iso3" %in% names(dfuser)) &
      ("year" %in% names(dfuser))
    ) {
      
      # delete country_code variable from user data
      if(any(grepl("Country_Code", names(dfuser), ignore.case = TRUE))) {
        dfuser <- dfuser %>% select(-c(grep("Country_Code", names(dfuser), ignore.case = TRUE, value = TRUE)))
      }
      
      # delete Year variable from user data
      if(any(grepl("Year", names(dfuser), ignore.case = TRUE))) {
        dfuser <- dfuser %>% select(-c(grep("Year", names(dfuser), ignore.case = TRUE, value = TRUE)))
      }
      
      # delete country_name variable from user data
      if(any(grepl("Country_Name", names(dfuser), ignore.case = TRUE))) {
        dfuser <- dfuser %>% select(-c(grep("Country_Name", names(dfuser), ignore.case = TRUE, value = TRUE)))
      }
      
      df <- setDT(dfuser)
      
      # uniformisation des variables ID 
      names(df)[names(df) == "iso3"] <- "Country_Code"
      names(df)[names(df) == "year"] <- "Year"
      
      
      # uniformisation des types pour les trois variables 
      # df[, Country_Name := as.character(Country_Name)]
      df[, Country_Code := as.character(Country_Code)]
      df[, Year := as.numeric(Year)]
      
      dfe <- dataApp %>%
        full_join(dfuser, by = c("Country_Code", "Year")) %>%
        mutate(Country_Name = ifelse(is.na(Country_Name), country, Country_Name))
      
      dfe <- dfe %>%
        left_join(CountriesWorldList, by = "Country_Code") %>%
        filter(!is.na(Country_Code))
      df <- setDT(df)

      return(dfe)
      
      # case where we have country and year and no iso3
    } else if(
      ("country" %in% names(dfuser)) &
      ("year" %in% names(dfuser))
    ) {
      
      # delete country_name variable from user data
      if("Country_Name" %in% names(dfuser)) {
        dfuser <- dfuser %>% select(-c(grep("Country_Name", names(dfuser),ignore.case=TRUE,value=TRUE)))
      }
      
      # delete Year variable from user data
      if("Year" %in% names(dfuser)) {
        dfuser <- dfuser %>% select(-c(Year))
      }
      
      # delete country_Code variable from user data
      if("Country_Code" %in% names(dfuser)) {
        dfuser <- dfuser %>% select(-c(grep("Country_Code", names(dfuser),ignore.case=TRUE,value=TRUE)))
      }
      
      df <- setDT(dfuser)
      
      # uniformisation des variables ID 
      # names(df)[names(df) == "country"] <- "Country_Name"
      names(df)[names(df) == "year"] <- "Year"
      
      df$Country_Code <- countrycode(sourcevar = df$country, 
                                     origin = "country.name", destination = "iso3c")
      
      setDT(df)
      
      # uniformisation des types pour les trois variables 
      # df[, Country_Name := as.character(Country_Name)]
      df[, Country_Code := as.character(Country_Code)]
      df[, Year := as.numeric(Year)]
      
      dfe <- dataApp %>%
        full_join(df, by = c("Country_Code", "Year")) %>%
        mutate(Country_Name = ifelse(is.na(Country_Name), country, Country_Name)) %>%
        filter(!is.na(Country_Code))
      
      return(dfe)
      
    } else {
      return()
    }
    
  }
  
})

# importation error handling 
output$importErrorUI <- renderUI({
  req(input$user.data, !is.null(importError()))
  tags$div(importError(), style="color:red")
})

lapply(c("importErrorUI"),
       function(x) outputOptions(output, x, suspendWhenHidden = FALSE))

importError <- reactive({
  req(input$user.data)
  dfuser <- tryCatch(fread(input$user.data$datapath, stringsAsFactors = FALSE, 
                           na.strings = c("", "n\a", "NA", "..")))
  
  if((
    (!("iso3" %in% names(dfuser))) &
    (!("country" %in% names(dfuser)))
  ) |
  (!("year" %in% names(dfuser)))
  ) {
    
    "Error! The specified instructions have not been followed"
  } else {
    return()
    
  }
})

  num.df_finale <- reactive({
    req(df_finale())
    df <- df_finale()
    df <- Filter(is.numeric, df)
    df
  })
  
  
  
  # ------------------------------------------------------------------------------------------------------
  # Tables for report  ##################################################################
  #-------------------------------------------------------------------------------------------------------
  
  # Data used for the tables
  
  #--------------------------------------------------------------------------
  
  
  rctTableData <- reactive({
    
    if(is.null(df_finale())) {
      
      return(NULL)
      
    } else {
      
      grp_data <- Identifier[, c("Country_Code", GRPVAR), with = FALSE]                     #
      df <- setDT(df_finale())                                                                  
      
      # filtrer les variables de groupe qui sont déjà dans la table exp_data()              
      var_df <- names(df)[!(names(df) %in% GRPVAR)]                                         
      df <- df[, c(var_df), with = FALSE]                                                   
      
      df <- df %>% 
        left_join(grp_data, by = c("Country_Code")) %>%
        select(Country_Code, Country_Name, Year, everything())
      
      setDT(df)
    }
    
  })
  
  
  ### Final table 

  output$preambule_uiF <- renderUI({
    req(rctTableData())
    selectInput("preambuleF", "Select country", choices = c("WAEMU", unique(rctTableData()$Country_Name[rctTableData()$Country_Code %in% cUEMOA])),
                selected = "WAEMU", multiple = FALSE, width = 200)
  })

  output$addallvar_uiF <- renderUI({
    req(rctTableData())

    selectInput("preambuleAddvarF", "AddVariable", choices = c("Select" = "", base::setdiff(TaxVarF, TaxVarFInit)),
                multiple = TRUE, width = 700)

  })

  output$preambuleTabF <- renderDataTable({
    req(input$preambuleF)

    macrovar <- c("Gross domestic product, current prices - National currency - Billions")

    dTaxUEMOA <- rctTableData() %>%
      select(Country_Name, Country_Code, Year, all_of(macrovar), all_of(TaxVarF)) %>%
      filter(Country_Code %in% cUEMOA) %>%
      group_by(Year) %>%
      add_summary_rows(
        Country_Name = "WAEMU", Country_Code = "WAEMU", across(where(is.numeric), ~ mean(., na.rm = TRUE))
      )


    if(length(input$preambuleAddvarF) > 0) {

      datTax <- dTaxUEMOA %>%
        filter(Country_Name %in% input$preambuleF) %>%
        select(Country_Name, Country_Code, Year, all_of(macrovar), all_of(TaxVarF))

      datTax <- pivot_longer(data = datTax, cols = c(4:ncol(datTax)), names_to = "Tax Variables", values_to = "value")

      datTax <- pivot_wider(data = datTax, id_cols = c(Country_Name, Country_Code, `Tax Variables`), names_from = Year, values_from = value)

      datTax <- datTax %>%
        select(-c(Country_Name, Country_Code)) %>%
        filter(`Tax Variables` %in% c(macrovar, TaxVarFInit, input$preambuleAddvarF))

      datatable(datTax,
                extensions = c('Buttons', 'Scroller'),
                options = list(
                  dom = 'Bt',
                  buttons = list(
                    list(
                      extend = 'csv',
                      exportOptions = list(columns = ':visible'),
                      title = NULL
                    ),
                    list(
                      extend = 'excel',
                      exportOptions = list(columns = ':visible'),
                      title = NULL
                    ),
                    list(
                      extend = 'pdf',
                      exportOptions = list(columns = ':visible')
                    ),
                    list(
                      extend = 'colvis',
                      columns = c(2:ncol(datTax))
                      # text='Show/Hide Columns',
                      # collectionLayout='fixed four-column'
                    )
                  ),
                  columnDefs = list(list(visible=FALSE, targets=c(2:c(ncol(datTax)-5)))),
                  deferRender = TRUE,
                  scrollX = 400,
                  scrollY = 800,
                  scroller = TRUE),
                caption = "Notes: The table reports tax and non-tax revenue data for WAEMU countries."
                ) %>%
        formatRound(columns = 2:ncol(datTax), digits = 1)

    } else {
      datTax <- dTaxUEMOA %>%
        filter(Country_Name %in% input$preambuleF) %>%
        select(Country_Name, Country_Code, Year, all_of(macrovar), all_of(TaxVarF))

      datTax <- pivot_longer(data = datTax, cols = c(4:ncol(datTax)), names_to = "Tax Variables", values_to = "value")

      datTax <- pivot_wider(data = datTax, id_cols = c(Country_Name, Country_Code, `Tax Variables`), names_from = Year, values_from = value)

      datTax <- datTax %>%
        select(-c(Country_Name, Country_Code)) %>%
        filter(`Tax Variables` %in% c(macrovar, TaxVarFInit))

      datatable(datTax,
                extensions = c('Buttons', 'Scroller'),
                options = list(
                  dom = 'Bt',
                  buttons = list(
                    list(
                      extend = 'csv',
                      exportOptions = list(columns = ':visible'),
                      title = NULL
                    ),
                    list(
                      extend = 'excel',
                      exportOptions = list(columns = ':visible'),
                      title = NULL
                    ),
                    list(
                      extend = 'pdf',
                      exportOptions = list(columns = ':visible'),
                      title = "Table 1"
                    ),
                    list(
                      extend = 'colvis',
                      columns = c(2:ncol(datTax))
                      # text='Show/Hide Columns',
                      # collectionLayout='fixed four-column'
                    )
                  ),
                  # buttons = list('csv', 'excel', 'pdf', exportOptions = list(columns = ':visible'),
                  #                list(extend = 'colvis', columns = c(2:ncol(datTax)))
                  # ),
                  columnDefs = list(list(visible=FALSE, targets=c(2:c(ncol(datTax)-5)))),
                  deferRender = TRUE,
                  scrollX = 400,
                  scrollY = 700,
                  scroller = TRUE)) %>%
        formatRound(columns = 2:ncol(datTax), digits = 1)
    }

  })
  
  
  #### Table 2
  
  output$TabTwoC_ui <- renderUI({
    req(rctTableData())
    selectInput("TabTwoC", "Select country", choices = c(unique(rctTableData()$Country_Name[rctTableData()$Country_Code %in% cUEMOA])),
                selected = unique(rctTableData()$Country_Name[rctTableData()$Country_Code %in% cUEMOA])[2] , multiple = FALSE, width = 200)
  })
  
  output$tabTwoY_ui <- renderUI({
    req(rctTableData())
    selectInput("TabTwoY", "Select Year", choices = unique(rctTableData()$Year), selected = 2021, multiple = FALSE, width = 200)
  })
  
  
  output$TabComparison <- renderDataTable({
    req(rctTableData(), input$TabTwoC, input$TabTwoY)
    
    selected <- input$TabTwoC
    other <- base::setdiff(unique(rctTableData()$Country_Name[rctTableData()$Country_Code %in% cUEMOA]), input$TabTwoC)
    
    dTaxtbYear <- dataApp %>%
      filter(!is.na(`Trading Bloc`)) %>%
      select(Country_Name, Country_Code, Year, `Trading Bloc`, all_of(TaxVarF)) %>%
      group_by(Year, `Trading Bloc`) %>%
      add_summary_rows(
        Country_Name = "WAEMU", Country_Code = "WAEMU", across(where(is.numeric), ~ mean(., na.rm = TRUE))
      ) %>%
      mutate(
        Country_Name = case_when(Country_Name %in% "WAEMU" ~ `Trading Bloc`, 
                                 .default = Country_Name) , 
        Country_Code = case_when(Country_Code %in% "WAEMU" ~ `Trading Bloc`, 
                                 .default = Country_Code)
      ) %>%
      ungroup() %>%
      select(-`Trading Bloc`)
    
    # dTaxUEMOA <- dataApp %>%
    #   filter(`Trading Bloc` %in% "WAE") %>%
    #   select(Country_Name, Country_Code, Year, all_of(TaxVarF)) %>%
    #   group_by(Year) %>%
    #   add_summary_rows(
    #     Country_Name = "WAEMU", Country_Code = "WAEMU", across(where(is.numeric), ~ mean(., na.rm = TRUE))
    #   )
    
    dtaxSSA <- dataApp %>%
      filter(Region %in% "Sub-Saharan Africa") %>%
      select(Country_Name, Country_Code, Year, all_of(TaxVarF)) %>%
      group_by(Year) %>%
      summarise_at(TaxVarF, mean, na.rm = TRUE) %>%
      mutate(Country_Name = "SSA", Country_Code = "SSA") %>% 
      select(Country_Name, Country_Code, everything())
    
    incgroup <- unique(dataApp$`Income group`[dataApp$Country_Name %in% selected])
    
    dtaxSSAInc <- dataApp %>%
      filter(Region %in% "Sub-Saharan Africa", `Income group` %in% incgroup) %>%
      select(Country_Name, Country_Code, Year, all_of(TaxVarF)) %>%
      group_by(Year) %>%
      summarise_at(TaxVarF, mean, na.rm = TRUE) %>%
      mutate(Country_Name = paste(incgroup, " in SSA"), Country_Code = paste(incgroup, " in SSA")) %>% 
      select(Country_Name, Country_Code, everything())
    
    
    dataTabTwo <- dTaxtbYear %>%
      bind_rows(dtaxSSA) %>%
      bind_rows(dtaxSSAInc) %>%
      arrange(Year)
    
    datTax <- dataTabTwo %>%
      filter(Year == input$TabTwoY) %>%
      filter(!(Country_Name %in% other)) %>% 
      select(-Year) %>%
      select(Country_Name, Country_Code, everything())
    
    datTax <- datTax %>%
      slice((n()-2):n(), 1:(n()-3)) 
    
    datTax <- datTax %>%
      slice(n(), 1:(n()-1)) 

    datatable(datTax,
              extensions = c('Buttons', 'Scroller'),
              options = list(
                dom = 'Bt',
                buttons = list(
                  list(
                    extend = 'csv',
                    exportOptions = list(columns = ':visible'),
                    title = NULL
                  ),
                  list(
                    extend = 'excel',
                    exportOptions = list(columns = ':visible'),
                    title = NULL
                  ),
                  list(
                    extend = 'pdf',
                    exportOptions = list(columns = ':visible')
                  ),
                  list(
                    extend = 'colvis',
                    columns = c(2:ncol(datTax))
                  )
                ),
                columnDefs = list(list(visible=FALSE, targets=c(2:3, 5:6, 8:12, 15, 18:20, 23))),
                deferRender = TRUE,
                scrollX = 500,
                scrollY = 2000,
                scroller = TRUE)) %>%
      formatRound(columns = 3:ncol(datTax), digits = 1) %>%
      formatStyle(
        "Country_Name", 
        target = "row", 
        backgroundColor = styleEqual(c(selected, "CEM", "EAC", "ECO", "SAC"), c("lightblue", "gray", "gray", "gray", "gray"), default = NULL)
      )
    
    
    
    
  })

  
  # GRD
  #----------------
  
  output$preambule_ui <- renderUI({
    
    selectInput("preambule", "Select country", choices = c("WAEMU", unique(dataALL$Country_Name[dataALL$Country_Code %in% cUEMOA])),
                selected = "WAEMU", multiple = FALSE, width = 200)
  })

  output$addallvar_ui <- renderUI({
    selectInput("preambuleAddvar", "AddVariable", choices = c("Select" = "", base::setdiff(TaxVarGRD, VarTax)),
                multiple = TRUE, width = 500)

  })

  output$preambuleTab <- renderDataTable({
    req(input$preambule)

    macrovar <- c("Gross domestic product, current prices - National currency - Billions")

    dTaxUEMOA <-dataALL %>%
      select(Country_Name, Country_Code, Year, all_of(macrovar), all_of(TaxVarGRD)) %>%
      filter(Country_Code %in% cUEMOA) %>%
      group_by(Year) %>%
      add_summary_rows(
        Country_Name = "WAEMU", Country_Code = "WAEMU", across(where(is.numeric), ~ mean(., na.rm = TRUE))
      )


      if(length(input$preambuleAddvar) > 0) {

        datTax <- dTaxUEMOA %>%
          filter(Country_Name %in% input$preambule) %>%
          select(Country_Name, Country_Code, Year, all_of(macrovar), all_of(TaxVarGRD))

      datTax <- pivot_longer(data = datTax, cols = c(4:ncol(datTax)), names_to = "Tax Variables", values_to = "value")

      datTax <- pivot_wider(data = datTax, id_cols = c(Country_Name, Country_Code, `Tax Variables`), names_from = Year, values_from = value)

      datTax <- datTax %>%
        select(-c(Country_Name, Country_Code)) %>%
        filter(`Tax Variables` %in% c(macrovar, VarTax, input$preambuleAddvar))

      datatable(datTax,
                extensions = c('Buttons', 'Scroller'),
                options = list(
                  dom = 'Bt',
                  buttons = list(
                    list(
                      extend = 'csv',
                      exportOptions = list(columns = ':visible'),
                      title = NULL
                    ),
                    list(
                      extend = 'excel',
                      exportOptions = list(columns = ':visible'),
                      title = NULL
                    ),
                    list(
                      extend = 'pdf',
                      exportOptions = list(columns = ':visible')
                    ),
                    list(
                      extend = 'colvis',
                      columns = c(2:ncol(datTax))
                      # text='Show/Hide Columns',
                      # collectionLayout='fixed four-column'
                    )
                  ),
                  columnDefs = list(list(visible=FALSE, targets=c(2:c(ncol(datTax)-5)))),
                  deferRender = TRUE,
                  scrollX = 400,
                  scrollY = 400,
                  scroller = TRUE)) %>%
        formatRound(columns = 2:ncol(datTax), digits = 1)

    } else {
      datTax <- dTaxUEMOA %>%
        filter(Country_Name %in% input$preambule) %>%
        select(Country_Name, Country_Code, Year, all_of(macrovar), all_of(TaxVarGRD))

      datTax <- pivot_longer(data = datTax, cols = c(4:ncol(datTax)), names_to = "Tax Variables", values_to = "value")

      datTax <- pivot_wider(data = datTax, id_cols = c(Country_Name, Country_Code, `Tax Variables`), names_from = Year, values_from = value)

      datTax <- datTax %>%
        select(-c(Country_Name, Country_Code)) %>%
        filter(`Tax Variables` %in% c(macrovar, VarTax))

      datatable(datTax,
                extensions = c('Buttons', 'Scroller'),
                options = list(
                  dom = 'Bt',
                  buttons = list(
                    list(
                      extend = 'csv',
                      exportOptions = list(columns = ':visible'),
                      title = NULL
                      ),
                    list(
                      extend = 'excel',
                      exportOptions = list(columns = ':visible'),
                      title = NULL
                    ),
                    list(
                      extend = 'pdf',
                      exportOptions = list(columns = ':visible'),
                      title = "Table 1"
                    ),
                    list(
                      extend = 'colvis',
                      columns = c(2:ncol(datTax))
                      # text='Show/Hide Columns',
                      # collectionLayout='fixed four-column'
                      )
                  ),
                  # buttons = list('csv', 'excel', 'pdf', exportOptions = list(columns = ':visible'),
                  #                list(extend = 'colvis', columns = c(2:ncol(datTax)))
                  # ),
                  columnDefs = list(list(visible=FALSE, targets=c(2:c(ncol(datTax)-5)))),
                  deferRender = TRUE,
                  scrollX = 400,
                  scrollY = 400,
                  scroller = TRUE)) %>%
        formatRound(columns = 2:ncol(datTax), digits = 1)
    }


  })
  
  # OECD
  
  output$preambule_ui1 <- renderUI({
    selectInput("preambule1", "Select country", choices = c("WAEMU", unique(oecdafr$Country[oecdafr$Country_Code %in% cUEMOA])),
                selected = "WAEMU", multiple = FALSE, width = 200)
  })

  output$addallvar_ui1 <- renderUI({
    selectInput("preambuleAddvar1", "AddVariable", choices = c("Select" = "", base::setdiff(unique(oecdafr$`Revenue category`), oecdTaxVar)),
                multiple = TRUE, width = 500)

  })

  output$preambuleTab1 <- renderDataTable({
    req(input$preambule1)

    oecdafrSmall <- oecdafr %>%
      filter(Indicator %in% "Revenue as % of GDP")

    dTaxUEMOA <- oecdafrSmall %>%
      filter(Country_Code %in% cUEMOA) %>%
      group_by(Year, `Level of government`, `Revenue category`) %>%
      add_summary_rows(
        Country = "WAEMU", Country_Code = "WAEMU", across(where(is.numeric), ~ mean(., na.rm = TRUE))
      )

    if(length(input$preambuleAddvar1) > 0) {

      datTax <- dTaxUEMOA %>%
        filter(Country %in% input$preambule1) %>%
        select(-Indicator)


      datTax <- pivot_wider(datTax, id_cols = c(Country_Code, Country, `Revenue category`, `Level of government`), names_from = c(Year), values_from = Value)


      datTax <- datTax %>%
        select(-c(Country, Country_Code)) %>%
        filter(`Revenue category` %in% c(oecdTaxVar, input$preambuleAddvar1))

      datatable(datTax,
                extensions = c('Buttons', 'Scroller'),
                options = list(
                  dom = 'Bt',
                  buttons = list(
                    list(
                      extend = 'csv',
                      exportOptions = list(columns = ':visible'),
                      title = NULL
                    ),
                    list(
                      extend = 'excel',
                      exportOptions = list(columns = ':visible'),
                      title = NULL
                    ),
                    list(
                      extend = 'pdf',
                      exportOptions = list(columns = ':visible')
                    ),
                    list(
                      extend = 'colvis',
                      columns = c(2:ncol(datTax))
                    )
                  ),
                  columnDefs = list(list(visible=FALSE, targets=c(3:c(ncol(datTax)-5)))),
                  deferRender = TRUE,
                  scrollX = 400,
                  scrollY = 400,
                  scroller = TRUE)) %>%
        formatRound(columns = 3:ncol(datTax), digits = 1)

    } else {

      datTax <- dTaxUEMOA %>%
        filter(Country %in% input$preambule1) %>%
        select(-Indicator)


      datTax <- pivot_wider(datTax, id_cols = c(Country_Code, Country, `Revenue category`, `Level of government`), names_from = c(Year), values_from = Value)


      datTax <- datTax %>%
        select(-c(Country, Country_Code)) %>%
        filter(`Revenue category` %in% c(oecdTaxVar))

      datatable(datTax,
                extensions = c('Buttons', 'Scroller'),
                options = list(
                  dom = 'Bt',
                  buttons = list(
                    list(
                      extend = 'csv',
                      exportOptions = list(columns = ':visible'),
                      title = NULL
                    ),
                    list(
                      extend = 'excel',
                      exportOptions = list(columns = ':visible'),
                      title = NULL
                    ),
                    list(
                      extend = 'pdf',
                      exportOptions = list(columns = ':visible')
                    ),
                    list(
                      extend = 'colvis',
                      columns = c(2:ncol(datTax))
                    )
                  ),
                  columnDefs = list(list(visible=FALSE, targets=c(3:c(ncol(datTax)-5)))),
                  deferRender = TRUE,
                  scrollX = 400,
                  scrollY = 400,
                  scroller = TRUE)) %>%
        formatRound(columns = 3:ncol(datTax), digits = 1)
    }

  })
  
  
  
  
  
  
  # ------------------------------------------------------------------------------------------------------
  # Tables all countries  ##################################################################
  #-------------------------------------------------------------------------------------------------------
  
  
  
  ### Final table 
  
  output$tableAllC_ui <- renderUI({
    req(rctTableData())
    selectInput("tableallc", "Select country", choices = c(unique(rctTableData()$Country_Name)),
                selected = "Mali", multiple = FALSE, width = 200)
  })
  
  output$tableallcAddvar_ui <- renderUI({
    req(rctTableData())
    
    selectInput("tableallcAddvar", "AddVariable", choices = c("Select" = "", base::setdiff(TaxVarF, TaxVarFInit)),
                multiple = TRUE, width = 700)
    
  })
  

  output$tableAllCTab <- renderDataTable({
    req(input$tableallc)

    macrovar <- c("Gross domestic product, current prices - National currency - Billions")

    dTaxAll <- rctTableData() %>%
      select(Country_Name, Country_Code, Year, all_of(macrovar), all_of(TaxVarF))


    if(length(input$tableallcAddvar) > 0) {

      datTax <- dTaxAll %>%
        filter(Country_Name %in% input$tableallc) %>%
        select(Country_Name, Country_Code, Year, all_of(macrovar), all_of(TaxVarF))

      datTax <- pivot_longer(data = datTax, cols = c(4:ncol(datTax)), names_to = "Tax Variables", values_to = "value")

      datTax <- pivot_wider(data = datTax, id_cols = c(Country_Name, Country_Code, `Tax Variables`), names_from = Year, values_from = value)

      datTax <- datTax %>%
        select(-c(Country_Name, Country_Code)) %>%
        filter(`Tax Variables` %in% c(macrovar, TaxVarFInit, input$tableallcAddvar))

      datatable(datTax,
                extensions = c('Buttons', 'Scroller'),
                options = list(
                  dom = 'Bt',
                  buttons = list(
                    list(
                      extend = 'csv',
                      exportOptions = list(columns = ':visible'),
                      title = NULL
                    ),
                    list(
                      extend = 'excel',
                      exportOptions = list(columns = ':visible'),
                      title = NULL
                    ),
                    list(
                      extend = 'pdf',
                      exportOptions = list(columns = ':visible')
                    ),
                    list(
                      extend = 'colvis',
                      columns = c(2:ncol(datTax))
                      # text='Show/Hide Columns',
                      # collectionLayout='fixed four-column'
                    )
                  ),
                  columnDefs = list(list(visible=FALSE, targets=c(2:c(ncol(datTax)-5)))),
                  deferRender = TRUE,
                  scrollX = 400,
                  scrollY = 800,
                  scroller = TRUE),
                caption = "Notes: The table reports tax and non-tax revenue data."
      ) %>%
        formatRound(columns = 2:ncol(datTax), digits = 1)

    } else {
      
      datTax <- dTaxAll %>%
        filter(Country_Name %in% input$tableallc) %>%
        select(Country_Name, Country_Code, Year, all_of(macrovar), all_of(TaxVarF))

      datTax <- pivot_longer(data = datTax, cols = c(4:ncol(datTax)), names_to = "Tax Variables", values_to = "value")

      datTax <- pivot_wider(data = datTax, id_cols = c(Country_Name, Country_Code, `Tax Variables`), names_from = Year, values_from = value)

      datTax <- datTax %>%
        select(-c(Country_Name, Country_Code)) %>%
        filter(`Tax Variables` %in% c(macrovar, TaxVarFInit))

      datatable(datTax,
                extensions = c('Buttons', 'Scroller'),
                options = list(
                  dom = 'Bt',
                  buttons = list(
                    list(
                      extend = 'csv',
                      exportOptions = list(columns = ':visible'),
                      title = NULL
                    ),
                    list(
                      extend = 'excel',
                      exportOptions = list(columns = ':visible'),
                      title = NULL
                    ),
                    list(
                      extend = 'pdf',
                      exportOptions = list(columns = ':visible'),
                      title = "Table 1"
                    ),
                    list(
                      extend = 'colvis',
                      columns = c(2:ncol(datTax))
                      # text='Show/Hide Columns',
                      # collectionLayout='fixed four-column'
                    )
                  ),
                  # buttons = list('csv', 'excel', 'pdf', exportOptions = list(columns = ':visible'),
                  #                list(extend = 'colvis', columns = c(2:ncol(datTax)))
                  # ),
                  columnDefs = list(list(visible=FALSE, targets=c(2:c(ncol(datTax)-5)))),
                  deferRender = TRUE,
                  scrollX = 400,
                  scrollY = 700,
                  scroller = TRUE)) %>%
        formatRound(columns = 2:ncol(datTax), digits = 1)
    }

  })
  
  
  
  
  
  # ------------------------------------------------------------------------------------------------------
  # Graphical analysis ##################################################################
  #-------------------------------------------------------------------------------------------------------
  
  # Data used for the figures
  
  #--------------------------------------------------------------------------
  
  rctPlotData <- reactive({
    
    if(is.null(df_finale())) {
      
      return(NULL)
      
    } else {
      
      grp_data <- Identifier[, c("Country_Code", GRPVAR), with = FALSE]                     #
      df <- setDT(df_finale())                                                                  
      
      # filtrer les variables de groupe qui sont déjà dans la table exp_data()              
      var_df <- names(df)[!(names(df) %in% GRPVAR)]                                         
      df <- df[, c(var_df), with = FALSE]                                                   
      
      df <- df %>% 
        left_join(grp_data, by = c("Country_Code")) %>%
        select(Country_Code, Country_Name, Year, everything())
      
      setDT(df)
    }
    
  })
  
  
  # recupère mes inputs dans une reactives
  
  output$input_graphic <- renderUI({
    
    # Obligation de selection d'un graphic 
    req(input$choose_graph)
    
    df <- setDT(rctPlotData())
    
    # listes de toutes les variables numériques dans la base à exporter
    num_var <- unlist(lapply(df, is.numeric))  
    num_var <- names(df[, ..num_var])[-1]
    
    if(input$choose_graph == "Barplot"){
        tagList(
          
          fluidRow(selectizeInput("var_bar", h4("Ajouter les variables"), 
                                  choices = num_var,
                                  selected = num_var[1],
                                  multiple = TRUE, width = 400)),
          
          fluidRow(column(6, selectInput("type_grp", h5("Type de groupement"),
                                         choices = c("Choisir" = "","Pays" = "Country_Name", GRPVAR),
                                         selected = "Country_Name",
                                         multiple = FALSE, width = 200)),
                   column(6, uiOutput("add_bar_grp")))
          
        )
      } 
      
      else if(input$choose_graph == "Evolution temporelle"){
        
        
        tagList(
          fluidRow(column(6, selectizeInput("var_line", h4("Ajouter les variables"),
                                            choices = num_var,
                                            selected = num_var[1],
                                            multiple = TRUE)),
                   
                   column(6, selectizeInput("pays_line_plt", h4("Ajouter les pays"),
                                            choices = unique(rctPlotData()$Country_Name),
                                            selected = c("France","Germany"),
                                            multiple = TRUE) ))
        )
      }
      
      else if(input$choose_graph == "Histogramme"){
        
        tagList(
          selectizeInput("var_hist", h4("Ajouter les variables"),
                         choices = num_var,
                         selected = num_var[1], multiple = TRUE, width = 400,
                         options = list(maxItems = 6))
        )
      }
      
      else if(input$choose_graph == "Nuage de points") {
        
        tagList(
          
          fluidRow(column(6, selectizeInput("scatter_var1", "Variable Y",
                                            choices = c("Choisir" = "", num_var),
                                            selected = num_var[1],
                                            multiple = FALSE)),
                   
                   column(6, selectizeInput("scatter_var2", "Variable X",
                                            choices = c("Choisir" = "", num_var),
                                            selected = num_var[2],
                                            multiple = FALSE))),
          
          fluidRow(column(6, selectizeInput("scatter_grp", "La variable de groupe",
                                            choices = c("Choisir" = "",  GRPVAR),
                                            selected = GRPVAR[1],
                                            multiple = FALSE)))
        )
      } else if(input$choose_graph == "Treemap"){
        
        tagList(
          
          fluidRow(column(6, selectizeInput("treeNumVar", "Choisir une variable",
                                            choices = c("Choisir" = "", num_var),
                                            selected = num_var[1],
                                            multiple = FALSE)),
                   
                   column(6, fluidRow(selectInput("treeGrp", "Variable de groupe",
                                                  choices = c("Choisir" = "", GRPVAR),
                                                  selected = GRPVAR[1],
                                                  multiple = FALSE, width = 200)),
                          
                          fluidRow(uiOutput("uiTreeYr"))))
          
        )
      } else if(input$choose_graph == "map"){
        
        tagList(
          fluidRow(column(6, selectizeInput("var_map", h4("Sélectionner une variable"),
                                            choices = num_var,
                                            selected = num_var[1], multiple = FALSE, width = 400)), 
                   column(6, uiOutput("uiMapYr"))
                   
          )
        )
      }
      
    
  })

  
  #-----------------------------------------------------------------------
  # activation et desactivation certain button 
  
  
  # lines 
  observe({
    toggleState(id = "pays_line_plt", condition = length(input$var_line) == 1)
  })
  
  
  #-------------------------------------------------------------------------
  # output in main panel 
  
  output$mainGraphic <- renderUI({
    
    req(input$choose_graph)
    
    # histogramme 
    if(input$choose_graph == "Histogramme") uiOutput("hist_plt")
    else if(input$choose_graph == "Barplot") uiOutput("bar_plt")
    else if(input$choose_graph == "Nuage de points") uiOutput("scatter_plt")
    else if(input$choose_graph == "Evolution temporelle") uiOutput("line_plt")
    else if(input$choose_graph == "Treemap") d3tree2Output("tree_plt")
    else if(input$choose_graph == "map") tagList(
      fluidRow(switchInput(inputId = "interMap", label = "Switch to interactive map", value = FALSE, labelWidth = "150%"), align = "center"),
      fluidRow(uiOutput("map_plt"))
    )
    
    
  })
  
  # -----------------------------------------------
  # Barplot
  
  # Selon le type de group
  output$add_bar_grp <- renderUI({
    
    req(input$type_grp, rctPlotData())
    
    if(input$type_grp == "Country_Name"){
      
      selectizeInput("grp_barplt", h5("Ajouter les pays"),
                     choices = unique(rctPlotData()$`Country_Name`),
                     selected = unique(rctPlotData()$`Country_Name`)[1:4],
                     multiple = TRUE)
      
    } else {
      
      # # traitement de données 
      df <- rctPlotData()
      
      selectizeInput("grp_barplt", h5("Ajouter les groupes"),
                     choices = unique(df[, c(input$type_grp), with = FALSE]),
                     selected = unique(df[, get(input$type_grp)]),
                     multiple = TRUE)
    }
  })
  
  output$bar_plt <- renderUI({
    
    req(input$var_bar, rctPlotData())
    
    if(length(input$var_bar) == 1){
      
      tagList(
        fluidRow(plotlyOutput("bar_plt11")),
        tags$br(),
        tags$hr(),
        tags$br(),
        fluidRow(plotlyOutput("bar_plt12")),
        tags$br(),
        tags$br()
      )
    } else if(length(input$var_bar) >= 2 & input$type_grp != "Country_Name"){
      
      tagList(
        
        fluidRow(column(4,selectizeInput("hist_var_moy_yr", "Choisir l'annee :",
                                         choices = c(paste0("Moyenne ", unique(rctPlotData()$Year)),
                                                     "Moyenne sur la periode"),
                                         selected = "Moyenne 1990",
                                         multiple = FALSE))),
        fluidRow(plotlyOutput("bar_plt21")),
        tags$br()
      )
    } else if(length(input$var_bar) >= 2 & input$type_grp == "Country_Name"){
      
      tagList(
        tags$hr(),
        tags$br(),
        fluidRow(plotlyOutput("bar_plt22")),
        tags$br(),
        tags$br()
      )
    }
  })
  
  
  
  # tous les renders des barplots
  
  output$bar_plt11 <- renderPlotly({
    
    req(length(input$var_bar) == 1, input$type_grp, input$grp_barplt, rctPlotData())
    
    don <-  rctPlotData() %>%
      select(grp = input$type_grp, "Year", var =  input$var_bar) %>%
      mutate(y10 = cut(Year, breaks = 3, dig.lab = 4))
    
    don$grp <- as_factor(don$grp)
    
    don_agg <- aggregate(var ~ grp + y10,
                         data  = don,
                         mean, na.rm = TRUE)
    
    don_agg <- don_agg[don_agg$grp %in% input$grp_barplt, ]
    
    
    p <- ggplot(data = don_agg,
                aes(x = y10, y = var, fill = grp)) +
      geom_bar(stat = "identity", color = "white", width = 0.5,
               position = position_dodge()) +
      scale_fill_brewer(palette= "Spectral") + ylab("") + xlab("Moyenne par interval de 10 ans") +
      ggtitle(input$var_bar) +
      theme_minimal() + theme(legend.position = "bottom", 
                              plot.title = element_text(size = 16, color="#993333", face="bold"))
    
    ggplotly(p, tooltip = c("text"))
    
  })
  
  output$bar_plt12 <- renderPlotly({
    
    req(length(input$var_bar) == 1, input$type_grp, input$grp_barplt, rctPlotData())
    
    if(input$type_grp != 'Country_Name'){
      
      don <- rctPlotData()  %>%
        select(grp = input$type_grp, "Year", var =  input$var_bar)
      
      setDT(don) # data.table 
      
      don <- don[!is.na(var)] # supression des valeurs manquantes pour la variable choisie 
      
      don$grp <- as_factor(don$grp)
      
      # moyenne sur toute la période
      don[, "Moyenne_periode" := lapply(.SD, mean, na.rm = TRUE),
          by = grp, .SDcols = c("var")]
      
      # moyenne par année
      don[, moy_var := lapply(.SD, mean, na.rm = TRUE),
          by = c("grp", "Year"), .SDcols = c("var")]
      
      don[, "var" := NULL] # supression de la colonne var 
      
      # une observation par couple (grp, year)
      don <- don[, head(.SD, 1), by = c("grp", "Year")]
      
      # ordonne par année et selection par grp
      don <- don[order(Year), .SD[unique(c(1,.N))], by = grp]
      
      don[Year == min(Year), paste0("Moyenne_", min(don$Year)) := lapply(.SD, mean, na.rm = TRUE),
          by = grp, .SDcols = c("moy_var")]
      
      don[Year == max(Year), paste0("Moyenne_", max(don$Year)) := lapply(.SD, mean, na.rm = TRUE),
          by = grp, .SDcols = c("moy_var")]
      
      don_agg <- don[!(is.na(paste0("Moyenne_", min(don$Year))) & is.na(paste0("Moyenne_", max(don$Year))) & 
                         is.na(Moyenne_periode)),
      ][, c("grp", paste0("Moyenne_", min(don$Year)), paste0("Moyenne_", max(don$Year)),
            "Moyenne_periode"), with = FALSE]
      
      don_agg <- don_agg[, lapply(.SD, na.omit), by = .(grp, Moyenne_periode)
      ][, head(.SD, 1), by = .(grp)]
      
      don_agg <- reshape2::melt(don_agg, id = c("grp"))
      
      don_agg <- don_agg[don_agg$grp %in% input$grp_barplt, ]
      
      p <- ggplot(data = don_agg,
                  aes(x = variable, y = value, fill = grp)) +
        geom_bar(stat = "identity", color = "white", width = 0.5,
                 position = position_dodge()) +
        theme_minimal() + theme(legend.position = "bottom") +
        ylab("") + xlab("Moyenne en Min Year, Max Year, toute la période") +
        ggtitle(input$var_bar) +
        scale_fill_brewer(palette= "Spectral") +
        theme_minimal() + theme(legend.position = "bottom", 
                                plot.title = element_text(size = 16, color="#993333",  face="bold"))
      
      ggplotly(p, tooltip = c("text"))
      
    } else if(input$type_grp == 'Country_Name'){
      
      don <-  rctPlotData() %>%
        select(grp = input$type_grp, "Year", var =  input$var_bar)
      
      don$grp <- as_factor(don$grp)
      
      setDT(don)[, "Moyenne_periode" := lapply(.SD, mean, na.rm = TRUE),
                 by = grp, .SDcols = c("var")]
      
      don_agg <- don[!(is.na(Moyenne_periode)),][, .(grp, Moyenne_periode)]
      
      don_agg <- don_agg[, head(.SD, 1), by = .(grp)]
      
      don_agg <- reshape2::melt(don_agg, id = c("grp"))
      
      don_agg <- don_agg[don_agg$grp %in% input$grp_barplt, ]
      
      p <- ggplot(data = don_agg,
                  aes(x = variable, y = value, fill = grp)) +
        geom_bar(stat = "identity", color = "white", width = 0.5,
                 position = position_dodge()) +
        ylab("") + xlab("Moyenne sur toute la période") +
        ggtitle(input$var_bar) +
        scale_fill_brewer(palette= "Spectral") +
        scale_y_discrete(labels = function(x) str_wrap(x, width = 30)) +
        theme_minimal() + theme(legend.position = "bottom",
                                plot.title = element_text(size = 16, color="#993333", face="bold"))
      
      ggplotly(p, tooltip = c("text"))
    }
  })
  
  output$bar_plt21 <- renderPlotly({
    
    req(length(input$var_bar) >= 2, input$type_grp != 'Country_Name',
        input$grp_barplt, input$hist_var_moy_yr)
    
    if(input$hist_var_moy_yr != "Moyenne sur la periode"){
      
      don <-  rctPlotData() %>%
        select(grp = input$type_grp, "Year", input$var_bar) %>%
        mutate(graph_selected = paste0("Moyenne ", Year)) %>% select(-Year)
      
      don$grp <- as_factor(don$grp)
      
      don_agg <- setDT(don)[graph_selected == input$hist_var_moy_yr, lapply(.SD, mean, na.rm = TRUE),
                            by = grp, .SDcols = input$var_bar]
      
      don_agg <- reshape2::melt(don_agg, id = c("grp"))
      
      don_agg <- don_agg[don_agg$grp %in% input$grp_barplt, ]
      
      p <- ggplot(data = don_agg,
                  aes(x = variable, y = value, fill = grp)) +
        geom_bar(stat = "identity", color = "white", width = 0.5,
                 position = position_dodge()) +
        ggtitle(input$hist_var_moy_yr) + xlab("") +
        # geom_text(aes(label = round(value, 2)),
        #           position = position_dodge(0.5),
        #           vjust=1.6, color="Black", size = 2.5) +
        scale_fill_brewer(palette= "Spectral") +
        scale_x_discrete(labels = function(x) str_wrap(x, width = 30)) +
        theme_minimal() + theme(legend.position = "bottom",
                                plot.title = element_text(size = 16, color="#993333", face="bold"))
      
      ggplotly(p, tooltip = c("text"))
      
    } else if(input$hist_var_moy_yr == "Moyenne sur la periode") {
      
      don <-  rctPlotData() %>%
        select(grp = input$type_grp, input$var_bar)
      
      don$grp <- as_factor(don$grp)
      
      don_agg <- setDT(don)[, lapply(.SD, mean, na.rm = TRUE),
                            by = grp, .SDcols = input$var_bar]
      
      don_agg <- reshape2::melt(don_agg, id = c("grp"))
      
      don_agg <- don_agg[don_agg$grp %in% input$grp_barplt, ]
      
      p <- ggplot(data = don_agg,
                  aes(x = variable, y = value, fill = grp)) +
        geom_bar(stat = "identity", color = "white", width = 0.5,
                 position = position_dodge()) +
        ggtitle(input$hist_var_moy_yr) + xlab("") +
        # geom_text(aes(label = round(value, 2)),
        #           position = position_dodge(0.5),
        #           vjust=1.6, color="Black", size = 2.5) +
        scale_fill_brewer(palette= "Spectral") +
        scale_x_discrete(labels = function(x) str_wrap(x, width = 30)) +
        theme_minimal() + theme(legend.position = "bottom",
                                plot.title = element_text(size = 16, color="#993333", face="bold"))
      
      ggplotly(p, tooltip = c("text"))
    }
    
  })
  
  output$bar_plt22 <- renderPlotly({
    
    req(length(input$var_bar) >= 2, input$type_grp == 'Country_Name',
        input$grp_barplt)
    
    don <-  rctPlotData() %>%
      select(grp = input$type_grp, input$var_bar)
    
    don$grp <- as_factor(don$grp)
    
    don_agg <- setDT(don)[, lapply(.SD, mean, na.rm = TRUE),
                          by = grp, .SDcols = input$var_bar]
    
    don_agg <- reshape2::melt(don_agg, id = c("grp"))
    
    don_agg <- don_agg[don_agg$grp %in% input$grp_barplt, ]
    
    p <- ggplot(data = don_agg,
                aes(x = variable, y = value, fill = grp)) +
      geom_bar(stat = "identity", color = "white", width = 0.5,
               position = position_dodge()) +
      ggtitle("Moyenne sur toute la période") + xlab("") +
      # geom_text(aes(label = round(value, 2)),
      #           position = position_dodge(0.5),
      #           vjust=1.6, color="Black", size = 2.5) +
      scale_fill_brewer(palette= "Spectral") +
      scale_x_discrete(labels = function(x) str_wrap(x, width = 30)) +
      theme_minimal() + theme(legend.position = "bottom",
                              plot.title = element_text(size = 16, color="#993333", face="bold"))
    
    ggplotly(p, tooltip = c("text"))
    
  })
  
  
  # -----------------------------------------------
  # historgramme
  output$hist_plt <- renderUI({
    
    req(input$var_hist)
    
    if(length(input$var_hist) == 1){
      tagList(
        plotlyOutput("hist1", height = "100%")
      )
    } else if(length(input$var_hist) == 2){
      tagList(
        fluidRow(column(6, plotlyOutput("hist21")),
                 column(6, plotlyOutput("hist22")))
      )
    } else if(length(input$var_hist) == 3){
      tagList(
        fluidRow(column(4, plotlyOutput("hist31")),
                 column(4, plotlyOutput("hist32")),
                 column(4, plotlyOutput("hist33")))
      )
    } else if(length(input$var_hist) == 4){
      tagList(
        fluidRow(column(6, plotlyOutput("hist41")),
                 column(6, plotlyOutput("hist42"))),
        fluidRow(column(6, plotlyOutput("hist43")),
                 column(6, plotlyOutput("hist44")))
      )
    } else if(length(input$var_hist) == 5){
      tagList(
        fluidRow(column(4, plotlyOutput("hist51")),
                 column(4, plotlyOutput("hist52")),
                 column(4, plotlyOutput("hist53"))),
        fluidRow(column(4, plotlyOutput("hist54")),
                 column(4, plotlyOutput("hist55")))
      )
    } else if(length(input$var_hist) == 6){
      tagList(
        fluidRow(column(4, plotlyOutput("hist61")),
                 column(4, plotlyOutput("hist62")),
                 column(4, plotlyOutput("hist63"))),
        fluidRow(column(4, plotlyOutput("hist64")),
                 column(4, plotlyOutput("hist65")),
                 column(4, plotlyOutput("hist66")))
      )
    }
    
  })
  
  ## render des graphic pour tous les histogrames
  
  output$hist1 <- renderPlotly({
    req(length(input$var_hist) == 1)
    
    rctPlotData() %>%
      select(input$var_hist) %>% na.omit() %>%
      plot_ly(x = ~get(input$var_hist), type = 'histogram') %>%
      layout(
        xaxis = list(
          title = input$var_hist)
      )
    
  })
  
  output$hist21 <- renderPlotly({
    req(length(input$var_hist) == 2)
    
    rctPlotData() %>%
      select(input$var_hist[1]) %>% na.omit() %>%
      plot_ly(x = ~get(input$var_hist[1]), type = 'histogram') %>%
      layout(
        xaxis = list(
          title = input$var_hist)
      )
    
  })
  
  output$hist22 <- renderPlotly({
    req(length(input$var_hist) == 2)
    
    rctPlotData() %>%
      select(input$var_hist[2]) %>% na.omit() %>%
      plot_ly(x = ~get(input$var_hist[2]), type = 'histogram') %>%
      layout(
        xaxis = list(
          title = input$var_hist)
      )
    
  })
  
  output$hist31 <- renderPlotly({
    req(length(input$var_hist) == 3)
    
    rctPlotData() %>%
      select(input$var_hist[1]) %>% na.omit() %>%
      plot_ly(x = ~get(input$var_hist[1]), type = 'histogram') %>%
      layout(
        xaxis = list(
          title = input$var_hist)
      )
    
  })
  
  output$hist32 <- renderPlotly({
    req(length(input$var_hist) == 3)
    
    rctPlotData() %>%
      select(input$var_hist[2]) %>% na.omit() %>%
      plot_ly(x = ~get(input$var_hist[2]), type = 'histogram') %>%
      layout(
        xaxis = list(
          title = input$var_hist)
      )
    
  })
  
  output$hist33 <- renderPlotly({
    req(length(input$var_hist) == 3)
    
    rctPlotData() %>%
      select(input$var_hist[3]) %>% na.omit() %>%
      plot_ly(x = ~get(input$var_hist[3]), type = 'histogram') %>%
      layout(
        xaxis = list(
          title = input$var_hist)
      )
    
  })
  
  output$hist41 <- renderPlotly({
    req(length(input$var_hist) == 4)
    
    rctPlotData() %>%
      select(input$var_hist[1]) %>% na.omit() %>%
      plot_ly(x = ~get(input$var_hist[1]), type = 'histogram') %>%
      layout(
        xaxis = list(
          title = input$var_hist)
      )
    
  })
  
  output$hist42 <- renderPlotly({
    req(length(input$var_hist) == 4)
    
    rctPlotData() %>%
      select(input$var_hist[2]) %>% na.omit() %>%
      plot_ly(x = ~get(input$var_hist[2]), type = 'histogram') %>%
      layout(
        xaxis = list(
          title = input$var_hist)
      )
    
  })
  
  output$hist43 <- renderPlotly({
    req(length(input$var_hist) == 4)
    
    rctPlotData() %>%
      select(input$var_hist[3]) %>% na.omit() %>%
      plot_ly(x = ~get(input$var_hist[3]), type = 'histogram') %>%
      layout(
        xaxis = list(
          title = input$var_hist)
      )
    
  })
  output$hist44 <- renderPlotly({
    req(length(input$var_hist) == 4)
    
    rctPlotData() %>%
      select(input$var_hist[4]) %>% na.omit() %>%
      plot_ly(x = ~get(input$var_hist[4]), type = 'histogram') %>%
      layout(
        xaxis = list(
          title = input$var_hist)
      )
    
  })
  
  output$hist51 <- renderPlotly({
    req(length(input$var_hist) == 5)
    
    rctPlotData() %>%
      select(input$var_hist[1]) %>% na.omit() %>%
      plot_ly(x = ~get(input$var_hist[1]), type = 'histogram') %>%
      layout(
        xaxis = list(
          title = input$var_hist)
      )
    
  })
  
  output$hist52 <- renderPlotly({
    req(length(input$var_hist) == 5)
    
    rctPlotData() %>%
      select(input$var_hist[2]) %>% na.omit() %>%
      plot_ly(x = ~get(input$var_hist[2]), type = 'histogram') %>%
      layout(
        xaxis = list(
          title = input$var_hist)
      )
    
  })
  output$hist53 <- renderPlotly({
    req(length(input$var_hist) == 5)
    
    rctPlotData() %>%
      select(input$var_hist[3]) %>% na.omit() %>%
      plot_ly(x = ~get(input$var_hist[3]), type = 'histogram') %>%
      layout(
        xaxis = list(
          title = input$var_hist)
      )
    
  })
  output$hist54 <- renderPlotly({
    req(length(input$var_hist) == 5)
    
    rctPlotData() %>%
      select(input$var_hist[4]) %>% na.omit() %>%
      plot_ly(x = ~get(input$var_hist[4]), type = 'histogram') %>%
      layout(
        xaxis = list(
          title = input$var_hist)
      )
    
  })
  
  output$hist55 <- renderPlotly({
    req(length(input$var_hist) == 5)
    
    rctPlotData() %>%
      select(input$var_hist[5]) %>% na.omit() %>%
      plot_ly(x = ~get(input$var_hist[5]), type = 'histogram') %>%
      layout(
        xaxis = list(
          title = input$var_hist)
      )
    
  })
  
  output$hist61 <- renderPlotly({
    req(length(input$var_hist) == 6)
    
    rctPlotData() %>%
      select(input$var_hist[1]) %>% na.omit() %>%
      plot_ly(x = ~get(input$var_hist[1]), type = 'histogram') %>%
      layout(
        xaxis = list(
          title = input$var_hist)
      )
    
  })
  
  output$hist62 <- renderPlotly({
    req(length(input$var_hist) == 6)
    
    rctPlotData() %>%
      select(input$var_hist[2]) %>% na.omit() %>%
      plot_ly(x = ~get(input$var_hist[2]), type = 'histogram') %>%
      layout(
        xaxis = list(
          title = input$var_hist)
      )
    
  })
  output$hist63 <- renderPlotly({
    req(length(input$var_hist) == 6)
    
    rctPlotData() %>%
      select(input$var_hist[3]) %>% na.omit() %>%
      plot_ly(x = ~get(input$var_hist[3]), type = 'histogram') %>%
      layout(
        xaxis = list(
          title = input$var_hist)
      )
    
  })
  output$hist64 <- renderPlotly({
    req(length(input$var_hist) == 6)
    
    rctPlotData() %>%
      select(input$var_hist[4]) %>% na.omit() %>%
      plot_ly(x = ~get(input$var_hist[4]), type = 'histogram') %>%
      layout(
        xaxis = list(
          title = input$var_hist)
      )
    
  })
  
  output$hist65 <- renderPlotly({
    req(length(input$var_hist) == 6)
    
    rctPlotData() %>%
      select(input$var_hist[5]) %>% na.omit() %>%
      plot_ly(x = ~get(input$var_hist[5]), type = 'histogram') %>%
      layout(
        xaxis = list(
          title = input$var_hist)
      )
    
  })
  
  output$hist66 <- renderPlotly({
    req(length(input$var_hist) == 6)
    
    rctPlotData() %>%
      select(input$var_hist[6]) %>% na.omit() %>%
      plot_ly(x = ~get(input$var_hist[6]), type = 'histogram') %>%
      layout(
        xaxis = list(
          title = input$var_hist)
      )
    
  })
  
  # -----------------------------------------------
  # lines plot
  
  output$line_plt <- renderUI({
    req(input$var_line)
    
    if(length(input$var_line) == 1){
      tagList(
        fluidRow(),
        fluidRow(column(1),
                 column(11, plotlyOutput("line_var1", height = "600px"))),
        fluidRow()
      )
      
    } else if (length(input$var_line) >= 2) {
      tagList(
        fluidRow(),
        fluidRow(column(1),
                 column(11, plotlyOutput("line_varN", height = "600px"))),
        fluidRow()
      )
    }
  })
  
  # render for all line plot
  
  output$line_var1 <- renderPlotly({
    
    req(length(input$var_line) == 1, input$pays_line_plt)
    
    don <-  rctPlotData() %>%
      select(c("Country_Name", "Year", input$var_line)) %>%
      filter(Country_Name %in% input$pays_line_plt)
    
    p <- ggplot(don, aes(x = Year, y = get(input$var_line), colour = Country_Name)) +
      geom_line(linewidth = 1) + theme_minimal() + ylab(input$var_line)
    
    ggplotly(p, tooltip = c("text"))
  })
  
  output$line_varN <- renderPlotly({
    
    req(length(input$var_line) >= 2)
    
    don <-  rctPlotData() %>%
      select(c("Year", input$var_line))
    
    setDT(don)
    
    don <- aggregate(don[,  c(input$var_line), with = FALSE],
                     list(Year = don$Year),
                     mean, na.rm = TRUE)
    
    don <- reshape2::melt(don, id = "Year")
    
    p <- ggplot(data = don,
                aes(x = Year, y = value, colour = variable)) +
      geom_line(linewidth = 1) + theme_minimal()
    
    ggplotly(p, tooltip = c("text"))
  })
  
  # -----------------------------------------------
  # scatter plot
  
  output$scatter_plt <- renderUI({
    tagList(
      fluidRow(),
      fluidRow(column(12, plotlyOutput("scatter", height = "600px"))),
      fluidRow()
    )
  })
  
  # render for scatter
  
  output$scatter <- renderPlotly({
    
    req(input$scatter_var1, input$scatter_var2, input$scatter_grp, input$scatter_var1 != input$scatter_var2)
    
    
    
    don <-  rctPlotData() %>%
      select(c("Country_Name", y_var = input$scatter_var1, x_var = input$scatter_var2,
               grp = input$scatter_grp))
    
    don_agg <- aggregate(don[, c("y_var", "x_var")],
                         list(Country = don$Country_Name, Zone = don$grp),
                         mean, na.rm = TRUE)
    
    don_agg <- don_agg[!is.na(don_agg$y_var) & !is.na(don_agg$x_var),]
    
    don_agg$Zone <- as_factor(don_agg$Zone)
    
    y_label <- as.numeric(round(don_agg$y_var, 2))
    x_label <- as.numeric(round(don_agg$x_var, 2))
    c_label <- as.character(don_agg$Country)
    
    mytext = paste("y value = ", don_agg$y_var , "\n",
                   "x value = ", don_agg$x_var, "\n",
                   "Pays: ", don_agg$Country,  sep="")
    
    p <- ggplot(don_agg, aes(x = x_var,
                             y = y_var, color = Zone,
                             text = mytext)) +
      xlab(input$scatter_var2) + ylab(input$scatter_var1) + 
      ggtitle("Nuage de points") + 
      geom_point(na.rm = TRUE) +
      theme_minimal() + theme(plot.title = element_text(size = 16, color="#993333",  face="bold"))
    
    ggplotly(p, tooltip = c("text"))
    
  })
  
  # --------------------------------------------------------
  # Tree map rendering 
  
  output$uiTreeYr <- renderUI({
    
    req(input$treeNumVar)
    
    don <- rctPlotData() %>% 
      filter(!is.na(get(input$treeNumVar))) %>%
      select(Year)
    
    selectizeInput("treeYr", "Choisir l'annee :",
                   choices = sort(unique(don$Year)),
                   selected = max(unique(don$Year), na.rm = TRUE),
                   multiple = FALSE, width = '200px')
    
  })
  
  output$tree_plt <- renderD3tree2({
    
    req(input$treeNumVar, input$treeGrp, input$treeYr)
    
    don <-  rctPlotData() %>% filter(Year == input$treeYr) %>%
      select(grp = input$treeGrp, 'Country_Name', var =  input$treeNumVar)
    
    setDT(don)
    
    don <- don[!is.na(var)]
    
    don$labels = paste(don$Country_Name, round(don$var, 2), sep = "\n")
    
    if(nrow(don) != 0){
      
      p <- treemap(don,
                   
                   # data
                   index = c("grp", "labels"),
                   vSize = "var",
                   type="index",
                   
                   # Main
                   title="",
                   palette="Dark2",
                   
                   # Borders:
                   border.col=c("black", "grey", "grey"),             
                   border.lwds=c(1,0.5,0.1),                         
                   
                   # Labels
                   fontsize.labels=c(0.7, 0.4, 0.3),
                   fontcolor.labels=c("white", "white", "black"),
                   fontface.labels=1,            
                   bg.labels=c("transparent"),              
                   align.labels=list( c("center", "center"), c("left", "top"), c("right", "bottom")),                                  
                   overlap.labels=0.5#, inflate.labels=T   
                   
      )
      
      d3tree2( p ,  rootname = "General")
      
    }
    
  })
  
  
  # Map
  output$map_plt <- renderUI({
    req(input$var_map)
    
    if(input$interMap == FALSE){
      
      tagList(
        plotOutput("map", width = "100%")
      )
    } else if(input$interMap == TRUE){
      tagList(
        leafletOutput("mapInter", width = "100%")
      )
    }
  })
  
  output$uiMapYr <- renderUI({
    
    req(input$var_map)
    
    don <- rctPlotData() %>% 
      filter(!is.na(get(input$var_map))) %>%
      select(Year)
    
    selectizeInput("mapYr", "Choisir l'annee :",
                   choices = sort(unique(don$Year)),
                   selected = max(unique(don$Year), na.rm = TRUE),
                   multiple = FALSE, width = '200px')
  })
  
  
  ## render des graphic pour le map
  
  output$map <- renderPlot({
    req(input$var_map, input$mapYr)
    
    data <- rctPlotData() %>%
      filter(Year == input$mapYr) %>%
      select('Country_Name', 'Country_Code',  var = input$var_map) %>% na.omit() %>%
      
      setDT(data)
    
    data <- data[!is.na(var)]
    
    datamap <- left_join(world_map, data, by = "Country_Code")
    
    if(nrow(datamap) != 0){
      
      ggplot(datamap, aes(long, lat, group = group))+
        geom_polygon(aes(fill = var), color = "white")+
        scale_fill_viridis_c(option = "C")
      
    }
    
    
  })
  
  
  ## render des graphic pour le map interactif
  
  output$mapInter <- renderLeaflet({
    req(input$var_map, input$mapYr)
    
    data <- rctPlotData() %>%
      filter(Year == input$mapYr) %>%
      select('Country_Name', 'Country_Code',  var = input$var_map) %>% na.omit() %>%
      
      setDT(data)
    
    data <- data[!is.na(var)]
    
    data_map1 <- data_map %>%
      rename(Country_Code = iso3) %>%
      left_join(data, by = "Country_Code")
    
    if(nrow(data_map1) != 0){
      
      pal <- colorNumeric(
        palette = "Spectral",
        domain = data_map1$var,
        reverse = TRUE)
      
      labels <- sprintf(
        "<strong>%s</strong><br/>value = %g",
        data_map1$name, data_map1$var
      ) %>% lapply(htmltools::HTML)
      
      map <- leaflet() %>%
        addTiles() %>%
        addPolygons(data = data_map1,
                    label = ~labels,
                    popup = ~paste0("variable : ", var),
                    fill = TRUE,
                    color = "white",
                    weight = 2,
                    opacity = 1,
                    dashArray = 3,
                    fillOpacity = 0.7,
                    fillColor = ~pal(data_map1$var),
                    highlightOptions = highlightOptions(fillOpacity = 0.7,
                                                        bringToFront = TRUE)) %>%
        addLegend(pal = pal, values = data_map1$var,
                  opacity = 0.7, 
                  title = "variable", position = "bottomright") %>%
        setView(lat = 1, lng = 20, zoom = 3 )
      
      map
      
    }
    
    
  })
  
  
  # ******************************************************************************************
  #                /* Reactives: Catching Inputs for Tax effort Estimation */
  # ******************************************************************************************
  
  
  output$InpSelecUser_ui <- renderUI({
    tagList(
      selectInput("ModelChoice", h5("Choice of the model"), 
                  choices = c("Battese and Coelli (1992)" = "Bc92", 
                              "Kumbhakar (1990)" = "Kh90",
                              "Modeified Kumbhakar (1990)" = "Kh90m", 
                              "Four components" = "4comp"), 
                  selected = "Bc92", multiple = FALSE),
      selectizeInput("v.dependent", h5("Dependent variable"), 
                     choices = names(num.df_finale()),
                     selected =  "Total tax revenue as % GDP" , multiple = FALSE),
      selectizeInput("v.predictor", h5("Predictor variables"), 
                     choices = names(num.df_finale()),
                     selected = c("GDP per capita (constant 2015 US$) [NY.GDP.PCAP.KD]", 
                                  "Agriculture, forestry, and fishing, value added (% of GDP) [NV.AGR.TOTL.ZS]",
                                  "Trade (% of GDP) [NE.TRD.GNFS.ZS]", 
                                  "Total natural resources rents (% of GDP) [NY.GDP.TOTL.RT.ZS]"),  multiple = TRUE),
      checkboxInput("trendcb", label = "Trend", value = TRUE),
      selectizeInput("Sample", h5("Sample"), choices = unique(df_finale()$Country_Name), 
                     selected = cSSA, multiple = TRUE),
      h5("Sample Period", align = "center"),
      fluidRow(
        column(5, numericInput("t.start", label = "Start", 
                               min = 1980, max = 2022, value = 1980)),
        column(5, numericInput("t.end", label = "End", 
                               min = 1980, max = 2022, value = 2022))
      ),
      fluidRow(numericInput("mLag", h5("Appliquer un lag"), 
                            value = 1, min = 0, max = 3, width = "50%")), 
      actionButton("applymodel", "Apply")
    )
  })
  
  lapply(c("InpSelecUser_ui"),
         function(x) outputOptions(output, x, suspendWhenHidden = FALSE))
  
  dependent_variable <- reactive({
    out <- input$v.dependent
    out
  })
  
  predictor_variable <- reactive({
    out <- input$v.predictor
    out
  })
  
  var_out_stata <- reactive({
    out <- append(predictor_variable(), dependent_variable() , 0)
    out <- append(out, c("Country_Name", "Year", "Country_Code") , 0)
    out
  })
  
  sel_sample <- reactive({
    out <- input$Sample
    out
  })
  t.period <- reactive({
    out <- list(debut = input$t.start, fin = input$t.end)
    out
  })
  
  # ------------------------------------------------------------
  #           User's selection for SFA estimation
  #-------------------------------------------------------------
  
  rct_selData <- reactive({
    req(input$v.dependent, input$v.predictor, input$trendcb, input$Sample, input$t.start, input$t.end, input$mLag, df_finale(), var_out_stata())
    df <- df_finale() %>%
      filter(`Country_Name` %in% c(input$Sample)) %>%
      filter(Year %in% c(input$t.start:input$t.end)) %>%
      select(var_out_stata())
    
    
    if(input$mLag > 0){
      df <- df %>%
        arrange(Country_Code, Year) %>% 
        group_by(Country_Code) %>%
        mutate(
          across(
            .cols = all_of(predictor_variable()),
            .fns = ~ lag(.x, n =input$mLag),
            .names = "Lag {.col}"
          ) 
        ) %>%
        select(-c(predictor_variable()))
    } else {
      df <- df %>%
        arrange(Country_Code, Year)
    }
    
    
  }) %>%
    bindCache(input$v.dependent, input$v.predictor, input$trendcb, input$Sample, input$t.start, input$t.end, input$mLag) %>%
    bindEvent(input$applymodel, ignoreNULL = FALSE)
  
  
  
  # *************************************************************************************
  #                /* Outputs Tax effort estimation - Tab Stat Desc  */
  # *************************************************************************************
  
  output$StatSelVar <- renderDataTable({
    req(input$v.dependent, input$v.predictor, input$Sample, input$t.start, input$t.end, input$mLag, rct_selData())
    df <- rct_selData()
    df <- df[, -c(1, 2, 3)]
    tmp <- do.call(data.frame, 
                   list(Mean = round(apply(df, 2, mean, na.rm = TRUE), 2),
                        SD = round(apply(df, 2, sd, na.rm = TRUE),2),
                        Median = round(apply(df, 2, median, na.rm = TRUE),2),
                        Min = round(apply(df, 2, min, na.rm = TRUE),2),
                        Max = round(apply(df, 2, max, na.rm = TRUE),2),
                        Obs = apply(df, 2, function(x) length(which(!is.na(x)))),
                        na = apply(df, 2, function(x) length(which(is.na(x))))
                   ))
    tmp <- data.frame("Variables" = row.names(tmp), tmp)
    datatable(tmp, rownames = FALSE, extensions = c('Buttons', 'Scroller', 'Responsive'), 
              options = list(
                dom = 'Bfrtip', buttons = c('colvis', 'excel', 'pdf'), 
                deferRender = TRUE,
                scrollY = 200,
                scroller = TRUE))
  })
  
  
  # -------------------------------------------
  # Tax effort Second generation model 
  # -------------------------------------------
  
  DataSecGenMod <- reactive({
    req(rct_selData(), input$v.dependent)
    df <- copy(rct_selData())
    df$Id <- c(1:uniqueN(levels(as.factor(df$Country_Code))))[as.factor(df$Country_Code)]
    predictor_var <- names(rct_selData())[-c(1:4)]

    df <- df %>%
      select(Id, Country_Name, Year, Country_Code, dependent_variable(), everything())
    
    # data.table solution to replace all 0 with NA (without making error)
    setDT(df)
    for (i in c(dependent_variable(), predictor_var)) {
      set(df, which(df[[i]] == 0), i, NA)
    }
    
    # if(input$mLag > 0){
    #   df <- df %>%
    #     arrange(Id, Year) %>% 
    #     group_by(Id) %>%
    #     mutate(
    #       across(
    #         .cols = all_of(predictor_variable()),
    #         .fns = ~ lag(.x, n =input$mLag),
    #         .names = "Lag {.col}"
    #       ) 
    #     ) %>%
    #     select(-c(predictor_variable()))
    # } else {
    #   df <- df %>%
    #     arrange(Id, Year)
    # }
    
    # suppress all strange characters from names 
    names(df) <- gsub("[^a-zA-Z]", "", names(df))
    names(df) <- substr(names(df), start = 1, stop = 15)
    
    df <- df %>% rename(ISO3 = CountryCode)
    
    setDF(df)
  })
  
  output$Seetable <- renderDataTable({
    datatable(DataSecGenMod())
  })
  
  # EstimationSfa <- reactive({
  #   
  #   model_data <<- DataSecGenMod() %>%
  #     mutate(trend = Year - min(Year, na.rm = TRUE) +1)
  # 
  #   dep <- names(model_data)[5]
  #   pred <- names(model_data)[-c(1:5, ncol(model_data))]
  #   
  #   if(input$trendcb == TRUE){
  #     form <- as.formula(paste(paste0("log( ", dep, " )"),
  #                              paste(paste("log( ", pred, " )", collapse = " + "), "+ trend"),
  #                              sep = " ~ "))
  #   } else {
  #     form <- as.formula(paste(paste0("log( ", dep, " )"),
  #                              paste("log( ", pred, " )", collapse = " + "),
  #                              sep = " ~ "))
  #   }
  #   
  #   
  #   if(input$ModelChoice == "Bc92") {
  #     model <- npsf::sf(formula = form,
  #                       data = model_data, it = c("Id", "Year"),
  #                       prod = TRUE, model = "BC1992",
  #                       eff.time.invariant = FALSE, mean.u.0i.zero = TRUE)
  #     return(model)
  #     
  #   } else if(input$ModelChoice == "Kh90") {
  #     
  #     model <- npsf::sf(formula = form,
  #                       data = model_data, it = c("Id", "Year"),
  #                       prod = TRUE,
  #                       eff.time.invariant = FALSE, mean.u.0i.zero = TRUE)
  #     return(model)
  #   } else if(input$ModelChoice == "Kh90m") {
  #     model <- npsf::sf(formula = form,
  #                       data = model_data, it = c("Id", "Year"),
  #                       prod = TRUE, model = "K1990modified",
  #                       eff.time.invariant = FALSE, mean.u.0i.zero = TRUE)
  #     return(model)
  #   }
  # })
  
  
  EstimationSfa <- reactive({
    
    model_data <<- DataSecGenMod() %>%
      mutate(trend = Year - min(Year, na.rm = TRUE) +1)
    
    dep <- names(model_data)[5]
    pred <- names(model_data)[-c(1:5, ncol(model_data))]
    
    if(input$trendcb == TRUE){
      form <- as.formula(paste(paste0("log( ", dep, " )"),
                               paste(paste("log( ", pred, " )", collapse = " + "), "+ trend"),
                               sep = " ~ "))
      
    } else if(input$trendcb == FALSE){ 
      
      form <- as.formula(paste(paste0("log( ", dep, " )"),
                               paste("log( ", pred, " )", collapse = " + "),
                               sep = " ~ "))
    } else {
      
      form <- as.formula(paste(paste0("log( ", dep, " )"),
                               paste("log( ", pred, " )", collapse = " + "),
                               sep = " ~ "))
    }
    
    
    if(input$ModelChoice == "Bc92") {
      model <- npsf::sf(formula = form,
                        data = model_data, it = c("Id", "Year"),
                        prod = TRUE, model = "BC1992",
                        eff.time.invariant = FALSE, mean.u.0i.zero = TRUE)
      return(model)
      
    } else if(input$ModelChoice == "Kh90") {
      
      model <- npsf::sf(formula = form,
                        data = model_data, it = c("Id", "Year"),
                        prod = TRUE,
                        eff.time.invariant = FALSE, mean.u.0i.zero = TRUE)
      return(model)
    } else if(input$ModelChoice == "Kh90m") {
      model <- npsf::sf(formula = form,
                        data = model_data, it = c("Id", "Year"),
                        prod = TRUE, model = "K1990modified",
                        eff.time.invariant = FALSE, mean.u.0i.zero = TRUE)
      return(model)
    } else if(input$ModelChoice == "4comp") {
      
      model <- npsf::sf(formula = form,
                        data = model_data, it = c("Id", "Year"),
                        prod = TRUE, model = "4comp",
                        eff.time.invariant = FALSE, mean.u.0i.zero = TRUE, R = 500, lmtol = 1e-3, maxit = 500)
      return(model)
      
    }
  })
  
  output$ModSecGen <- renderPrint({
    summary(EstimationSfa())
    
  })
  
  ResTeScdModel <- reactive({
    req(EstimationSfa())
    
    if(input$ModelChoice == "4comp") {
      
      df_est <- DataSecGenMod() %>%
        rename(t = Year, id = Id) %>%
        left_join((EstimationSfa()$efficiencies_it), by = c("id", "t")) %>%
        select(c(Country_Code = ISO3, names(EstimationSfa()$efficiencies_it))) %>%
        rename(Year = t, Id = id)
      
      out <- df_finale() %>% 
        right_join(df_est, by = c("Country_Code", "Year")) %>% 
        select(-c("Id")) %>% 
        rename(Persistent = te_i, 
               Transient =  te_it,
               Overall = te_over)
      
    } else {
      
      df_est <- DataSecGenMod() %>%
        left_join(efficiencies(EstimationSfa()), by = c("Id", "Year")) %>%
        select(c(Country_Code = ISO3, names(efficiencies(EstimationSfa()))))
      
      # Or alternatively 
      
      # df_est <- DataSecGenMod() %>%
      #   left_join((EstimationSfa()$efficiencies), by = c("Id", "Year")) %>%
      #   select(c(Country_Code = ISO3, names(EstimationSfa()$efficiencies)))
      
      out <- df_finale() %>% 
        right_join(df_est, by = c("Country_Code", "Year")) %>% 
        select(-c("Id")) %>% 
        rename("Tax effort - Jondrow et al. (1982)" = JLMS, 
               "Tax effort - Battese and Coelli (1988)" = BC, 
               "Tax effort - Mode of conditional distribution" = Mode)
    }
    
   
  })
  
  output$tabsum_sg <- renderDataTable({
    te.analysis <- ResTeScdModel()
    
    
    if(input$ModelChoice == "4comp") {
      
      te.analysis <- te.analysis %>%
        select(c(Persistent, Transient, Overall))
      
    } else {
      
      te.analysis <- te.analysis %>%
        select(c("Tax effort - Jondrow et al. (1982)", 
                 "Tax effort - Mode of conditional distribution", 
                 "Tax effort - Battese and Coelli (1988)"))
      
    }
    
    
    tmp <- do.call(data.frame, 
                   list(Mean = round(apply(te.analysis, 2, mean, na.rm = TRUE), 2),
                        SD = round(apply(te.analysis, 2, sd, na.rm = TRUE),2),
                        Median = round(apply(te.analysis, 2, median, na.rm = TRUE),2),
                        Min = round(apply(te.analysis, 2, min, na.rm = TRUE),2),
                        Max = round(apply(te.analysis, 2, max, na.rm = TRUE),2)
                   ))
    tmp <- data.frame("Variables" = row.names(tmp), tmp)
    datatable(tmp, rownames = FALSE, 
              extensions = c('Buttons'), 
              options = list(
                dom = 'Bfrtip',
                buttons = c('colvis', 'excel', 'pdf')))
  })

  output$TeGrEv <- renderPlot({
    df <- ResTeScdModel()
    
    if(input$ModelChoice == "4comp") {
    
    df <- aggregate(df[, c("Overall")],
                    list(Year = df$Year), mean, na.rm = TRUE)
    } else {
      df <- aggregate(df[, c("Tax effort - Battese and Coelli (1988)")],
                      list(Year = df$Year), mean, na.rm = TRUE)
    }

    df <- df
    tmp <- reshape2::melt(df, id = "Year")
    ggplot(data = tmp,
           aes(x = Year, y = value,
               ymin = (min(value, na.rm = TRUE) - (0.1*min(value, na.rm = TRUE))),
               ymax = ifelse(max(value, na.rm = TRUE) <= 1,
                             min((max(value, na.rm = TRUE) + (0.1*max(value, na.rm = TRUE))), 1,
                                 na.rm = TRUE),
                             (max(value, na.rm = TRUE) + (0.2*max(value, na.rm = TRUE))))
           )) +
      geom_line(size = 1) + theme_minimal() +
      theme(legend.position = "bottom")
  })

  output$CorVarTeSg_ui <- renderUI({
    selectizeInput("CorVarTeSg", "Select y variable", choices = names(ResTeScdModel()),
                   selected = c("External debt stocks (% of GNI) [DT.DOD.DECT.GN.ZS]"), multiple = FALSE)
  })
  
  lapply(c("CorVarTeSg_ui"),
         function(x) outputOptions(output, x, suspendWhenHidden = FALSE))
  
  output$corr_te_macvarSg <- renderPlot({
    req(input$CorVarTeSg)
    
    if(input$ModelChoice == "4comp") {
      
      don <- ResTeScdModel() %>%
        select(xvar = "Overall", yvar = input$CorVarTeSg)
      
    } else {
      don <- ResTeScdModel() %>%
        select(xvar = "Tax effort - Battese and Coelli (1988)", yvar = input$CorVarTeSg)
    }
    
      
      don <- don[complete.cases(don), ]
    
      ggplot(don, aes(x= xvar, y=yvar) ) + 
      geom_point(color=rgb(0.5, 0.8, 0.9, 0.7), size = 2) +
      geom_smooth(method=lm, color='#2C3E50') + 
      xlab("Tax effort - Battese and Coelli (1988)") + 
      ylab(input$CorVarTeSg)
      
  })
  
  output$tabTeSg <- renderDataTable(server = FALSE, {
    te.analysis <- ResTeScdModel()
    
    
    if(input$ModelChoice == "4comp") {
      
      te.analysis <- te.analysis %>%
        select(c(var_out_stata(), Persistent, Transient, Overall))
      
      
    } else {
      te.analysis <- te.analysis %>%
        select(c(var_out_stata(), "Tax effort - Jondrow et al. (1982)",
                 "Tax effort - Mode of conditional distribution",
                 "Tax effort - Battese and Coelli (1988)"))
      
    }
    
    datatable(te.analysis, rownames = FALSE, 
              extensions = c('Buttons'), 
              options = list(
                dom = 'Bfrtip',
                buttons = c('colvis', 'excel', 'pdf'),
                deferRender = TRUE,
                scrollX = 400,
                scrollY = 700,
                scroller = TRUE
                ))
  })
  
  
  
  output$avgTE <- renderDataTable(server = FALSE, {
    
    te.analysis <- ResTeScdModel()
    
    if(input$ModelChoice == "4comp") {
      
      te.analysis <- te.analysis %>%
        select(c(var_out_stata(), Persistent, Transient, `Tax effort` = Overall))
      
      
    } else {
      te.analysis <- te.analysis %>%
        select(c(var_out_stata(), "Tax effort - Jondrow et al. (1982)",
                 "Tax effort - Mode of conditional distribution",
                 `Tax effort` = "Tax effort - Battese and Coelli (1988)"))
      
    }
    
    te.analysis <- dataApp %>%
      select(Country_Code, Country_Name, Year, `Total tax revenue as % GDP`, `Total natural resources rents (% of GDP) [NY.GDP.TOTL.RT.ZS]`)
    
    dfmutate <- mutate(te.analysis, y10 = cut(Year, breaks = 4, dig.lab = 4, include.lowest = TRUE))
    mod <- levels(dfmutate$y10)
    dfagg <- aggregate(`Tax effort` ~ Country_Name, 
                       data = dfmutate, mean, na.rm = TRUE)
    
  })
  
  
  
  
  
  
  
  
  
  
  
  
  }































































































































































































































shinyApp(ui = ui, server = server)

