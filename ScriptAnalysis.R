

library(data.table)
library(readxl)
library(tidyverse)
library(sf)


setwd("C:/Users/aadama/OneDrive - Université Clermont Auvergne/UEMOA/App")

GRD <- read_excel("data/GRD.xls")

GRD[ GRD$iso == "KSV", "iso"] <- "XKX"
GRD[ GRD$iso == "WBG", "iso"] <- "PSE"

GRD <- GRD %>%
  rename(Country_Code = iso)

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
  select(Country_Code, Country_Name, Region, `Income group`, `Lending category`, everything())
  
CountriesWorldList <- fread("data/Identifier.csv", 
                            stringsAsFactors = FALSE, na.strings = c("" ,"n/a", "NA")) 


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


dataApp <- GRD %>%
  left_join(Identifier, by = "Country_Code")
remove(CLASS, groupsWB)



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
  select(Country_Code = iso3, Country, year, everything()) %>%
  select(-WEOCountryCode)


remove(dfweo)

dataApp <- dataApp %>%
  left_join(dfweoPanel, by = c("Country_Code", "year"))

hdi <- fread("data/human-development-index.csv", stringsAsFactors = FALSE, na.strings = c("", "n\a", "NA", "..", "n/a", "--"))

hdi <- hdi %>%
  filter(!is.na(Code)) %>%
  rename(year = Year, Country_Code = Code) %>%
  select(-Entity)

dataApp <- dataApp %>%
  left_join(hdi,  by = c("Country_Code", "year"))

wdi <- fread("data/wdi/Data.csv", stringsAsFactors = FALSE, na.strings = c("", "n\a", "NA", "..", "n/a", "--"))

wdi <- wdi[!is.na(`Country Code`), ] %>%
  rename(Country_Code = `Country Code`, year = Time) %>%
  select(-`Time Code`, `Country Name`)

# OECD Revenue Statistics African Countries

oecdafr <- fread("data/RS_AFR_24112023.csv", stringsAsFactors = FALSE, na.strings = c("", "n\a", "NA", "..", "n/a", "--"))

oecdafrm <- fread("data/RS_AFR_24112023_00_15.csv", stringsAsFactors = FALSE, na.strings = c("", "n\a", "NA", "..", "n/a", "--"))

oecdafr <- oecdafr %>%
  select(-c(`Unit Code`, Unit, `PowerCode Code`, PowerCode, `Reference Period Code`, `Reference Period`, `Flag Codes`, Flags, YEA, TAX, GOV))

oecdafr <- pivot_wider(oecdafr, id_cols = c(COU, Country, Year, `Level of government`), names_from = c(`Revenue category`, Indicator), values_from = Value)

dataApp <- dataApp %>%
  left_join(wdi,  by = c("Country_Code", "year")) %>%
  filter(year<2023)

c("Total tax and non-tax revenue", "Total tax revenue", 
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
  
  

###### -----

### Analysis

dfTest <- dataApp %>% 
  filter(Region %in% "Sub-Saharan Africa") %>%
  select(Country_Name, Year, Country_Code, 
         `Non-resource tax excluding social contributions as % of GDP`, 
         `GDP per capita (constant 2015 US$) [NY.GDP.PCAP.KD]`, 
         `Agriculture, forestry, and fishing, value added (% of GDP) [NV.AGR.TOTL.ZS]`, 
         `Trade (% of GDP) [NE.TRD.GNFS.ZS]`, 
         `Total natural resources rents (% of GDP) [NY.GDP.TOTL.RT.ZS]`
  )

dfTest <- dataApp %>% 
  filter(Region %in% "Sub-Saharan Africa") %>%
  select(Country_Name, Year, Country_Code, 
         `Non-resource tax excluding social contributions as % of GDP`, 
         `GDP per capita (constant 2015 US$) [NY.GDP.PCAP.KD]`, 
         `Agriculture, forestry, and fishing, value added (% of GDP) [NV.AGR.TOTL.ZS]`, 
         `Trade (% of GDP) [NE.TRD.GNFS.ZS]`, 
         `Total natural resources rents (% of GDP) [NY.GDP.TOTL.RT.ZS]`
  )

dfTest$Id <- c(1:uniqueN(levels(as.factor(dfTest$Country_Code))))[as.factor(dfTest$Country_Code)]

dfTest <- dfTest %>%
  select(Id, everything())

# data.table solution to replace all 0 with NA (without making error)
setDT(dfTest)
for (i in c(names(dfTest)[5:9])) {
  set(dfTest, which(dfTest[[i]] == 0), i, NA)
}

# # lagging the output variable
# dfTest[, "unLagDep" := `Non-resource tax excluding social contributions`]
# dfTest <- dfTest[order(Country_Name, Year)]
# dfTest <- dfTest[, `Non-resource tax excluding social contributions` := shift(get("unLagDep"),
#                                       n = 1,
#                                       fill = NA, type = "lag"),
#          by = "Country_Name"]


covari <- names(dfTest)[6:(ncol(dfTest))]
# DF <- dfTest %>% mutate_all(lead)

dfTest <- dfTest %>%
  arrange(Id, Year) %>% 
  group_by(Id) %>%
  mutate(
    across(
      .cols = all_of(covari),
      .fns = ~ lag(.x, n =1),
      .names = "Lag {.col}"
    )
  ) %>%
  select(-c(covari))



dfTest$trend <- dfTest$Year - min(dfTest$Year) + 1

estim <- npsf::sf(
  formula = log(`Non-resource tax excluding social contributions as % of GDP`) ~ log(`Lag GDP per capita (constant 2015 US$) [NY.GDP.PCAP.KD]`) + 
    log(`Lag Agriculture, forestry, and fishing, value added (% of GDP) [NV.AGR.TOTL.ZS]`) + log(`Lag Trade (% of GDP) [NE.TRD.GNFS.ZS]`),
  data = dfTest, it = c("Id", "Year"),
  prod = TRUE, model = "4comp",
  eff.time.invariant = FALSE, mean.u.0i.zero = TRUE, R = 500, lmtol = 1e-3, maxit = 500)
summary(estim)


allcountries4 <- (estim)

allcountries4SSA <- (estim)

estim <- npsf::sf(
  formula = log(`Non-resource tax excluding social contributions as % of GDP`) ~ log(`Lag GDP per capita (constant 2015 US$) [NY.GDP.PCAP.KD]`) + 
    log(`Lag Agriculture, forestry, and fishing, value added (% of GDP) [NV.AGR.TOTL.ZS]`) + log(`Lag Trade (% of GDP) [NE.TRD.GNFS.ZS]`) + 
    log(`Lag Total natural resources rents (% of GDP) [NY.GDP.TOTL.RT.ZS]`) + trend + I(trend^2),
  data = dfTest, it = c("Id", "Year"),
  prod = TRUE, model = "BC1992",
  eff.time.invariant = FALSE, mean.u.0i.zero = TRUE)


estim <- npsf::sf(
  formula = log(`Non-resource tax excluding social contributions as % of GDP`) ~ log(`Lag GDP per capita (constant 2015 US$) [NY.GDP.PCAP.KD]`) + 
    log(`Lag Agriculture, forestry, and fishing, value added (% of GDP) [NV.AGR.TOTL.ZS]`) + log(`Lag Trade (% of GDP) [NE.TRD.GNFS.ZS]`) + 
    log(`Lag Total natural resources rents (% of GDP) [NY.GDP.TOTL.RT.ZS]`) + I(0.5*log(`Lag GDP per capita (constant 2015 US$) [NY.GDP.PCAP.KD]`)^2) + 
    I(0.5*log(`Lag Agriculture, forestry, and fishing, value added (% of GDP) [NV.AGR.TOTL.ZS]`)^2) + 
    I(0.5*log(`Lag Trade (% of GDP) [NE.TRD.GNFS.ZS]`)^2) + I(0.5*log(`Lag Total natural resources rents (% of GDP) [NY.GDP.TOTL.RT.ZS]`)^2) +
    trend + I(0.5*trend^2),
  data = dfTest, it = c("Id", "Year"),
  prod = TRUE,
  eff.time.invariant = FALSE, mean.u.0i.zero = TRUE)

summary(estim$efficiencies_it)

summary(frontier::efficiencies(estim))

dfTest <- stats::aggregate(dataApp[, c("Non-resource tax excluding social contributions" ,
                                       "GDP per capita, PPP (constant 2017 international $) [NY.GDP.PCAP.PP.KD]")],
                           list(Group = dataApp$`Income group` ), mean, na.rm = TRUE)

dep <- "Non-resource tax excluding social contributions"
form <- as.formula(paste(paste0("log( ", dep, " )"),
                         paste(paste("log( ", covari, " )", collapse = " + "), "+ trend"),
                         sep = " ~ "))


## lm 

LM <- lm(formula = log(`Non-resource tax excluding social contributions as % of GDP`) ~ log(`Lag GDP per capita (constant 2015 US$) [NY.GDP.PCAP.KD]`) + 
     log(`Lag Agriculture, forestry, and fishing, value added (% of GDP) [NV.AGR.TOTL.ZS]`) + log(`Lag Trade (% of GDP) [NE.TRD.GNFS.ZS]`) + 
     log(`Lag Total natural resources rents (% of GDP) [NY.GDP.TOTL.RT.ZS]`) + trend,
   data = dfTest)


dataPanel <- pdata.frame(dfTest, index = c("Id", "Year"))
p <- plm(formula = log(Non.resource.tax.excluding.social.contributions.as...of.GDP) ~ log(Lag.GDP.per.capita..constant.2015.US....NY.GDP.PCAP.KD.) + 
           log(Lag.Agriculture..forestry..and.fishing..value.added....of.GDP...NV.AGR.TOTL.ZS.) + log(Lag.Trade....of.GDP...NE.TRD.GNFS.ZS.) + 
           log(Lag.Total.natural.resources.rents....of.GDP...NY.GDP.TOTL.RT.ZS.) + trend,
         data = dataPanel, model = "random", effect = "time")

###### -----

### Analysis

UEMOA <- Identifier[Identifier$Country_Name %in% c("Benin", "Burkina Faso", "Côte d’Ivoire", "Guinea-Bissau", "Mali", "Niger", "Senegal", "Togo"), "Country_Code"]

UEMOA <- as.vector(UEMOA$Country_Code)

# Table preambule

View(GRD[, c(4, 5, 10, 15:(ncol(GRD)-4))])

TaxVarGRD <- names(GRD)[15:(ncol(GRD)-4)]
c("Revenue excluding grants and social contributions",
  "Total Resource Revenue", "Total Non-Resource Revenue including social contributions", "Taxes excluding social contributions",
  "Resource taxes",  "Direct taxes excluding social contributions and resource revenue", 
  
  )

datTaxWaemu <- GRD %>%
  filter(Country_Code %in% UEMOA) %>%
  select(c(4, 5, 10, 15:(ncol(GRD)-4)))

datTaxWaemu <- datTaxWaemu %>%
  group_by(year) %>%
  add_row(country = "WAEMU", Country_Code = "WAEMU", year = row_number()==1, summarise(., across(where(is.numeric), sum)))


add_summary_rows <- function(.data, ...) {
  group_modify(.data, function(x, y) bind_rows(x, summarise(x, ...)))
}

dfNew1 <- datTaxWaemu %>% 
  group_by(year) %>% 
  add_summary_rows(
    country = "WAEMU", Country_Code = "WAEMU", across(where(is.numeric), ~ mean(., na.rm = TRUE))
  )


dfNew <- datTaxWaemu %>% 
  group_by(year) %>% 
  summarise(country = "WAEMU", Country_Code = "WAEMU", across(where(is.numeric), ~ mean(., na.rm = TRUE)))

dataBen <- datTaxWaemu %>%
  filter(Country_Code %in% "BEN")

dataBen <- pivot_longer(data = dataBen, cols = `Revenue including social contributions`:Grants, names_to = "Tax Variables", values_to = "value")

dataBen <- pivot_wider(data = dataBen, id_cols = c(country, Country_Code, `Tax Variables`), names_from = year, values_from = value)

# 
# DATAmARIO <- read_excel("C:/Users/aadama/OneDrive - Université Clermont Auvergne/UEMOA/data/SSA Dataset Final 1705 Cv2.xlsx") %>%
#   filter(Year == 2014) %>%
#   select(Country, `Trading Bloc`) %>%
#   select(Country, `Trading Bloc`)
# 
# fwrite(DATAmARIO, "data/TradingBloc.csv")

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


oecdgrd <- GRD %>%
  full_join(oecdafrFinal, by = c("Country_Code", "year")) %>%

oecdgrd <- oecdgrd %>% 
  mutate(
    `Total tax and non-tax revenue as % of GDP` = case_when(
      !is.na(`Total tax and non-tax revenue Revenue as % of GDP Federal or Central government`) ~ `Total tax and non-tax revenue Revenue as % of GDP Federal or Central government`, 
      is.na(`Total tax and non-tax revenue Revenue as % of GDP Federal or Central government`) ~ `Revenue including social contributions`
      ),
    `Total tax revenueas % GDP` = case_when(
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
    `Indirect as % of GDP` = Indirect,
    `5000 Taxes on goods and services` = case_when(
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
    
  ) %>%
  select(Country_Code, country)
  
  
  
# X <- split(x = oecdafr, f = ~ oecdafr$Indicator + oecdafr$`Level of government`)
# 

# # X <- split.data.frame(oecdafr, oecdafr$Indicator)
# 
# list2env(X, envir = .GlobalEnv)
# 
# to.rm <- unlist(eapply(.GlobalEnv, function(x) is.data.frame(x) && nrow(x) < 1))
# rm(list = names(to.rm)[to.rm], envir = .GlobalEnv)


testMerge <- X[["Revenue as % of GDP.Federal or Central government"]] %>%
  left_join(GRD, by = c("Country_Code", "year"))

cUEMOA <- c("BEN", "BFA", "CIV", "GNB", "MLI", "NER", "SEN", "TGO")

oecdafrSmall <- oecdafr %>%
  filter(Indicator %in% "Revenue as % of GDP")

dTaxUEMOA1 <- oecdafrSmall %>% 
  # select(Country_Name, Country_Code, Year, all_of(macrovar), all_of(TaxVarGRD)) %>%
  filter(COU %in% cUEMOA)

dTaxUEMOA <- oecdafrSmall %>% 
  # select(Country_Name, Country_Code, Year, all_of(macrovar), all_of(TaxVarGRD)) %>%
  filter(COU %in% cUEMOA) %>%
  group_by(Year, `Level of government`, `Revenue category`, Indicator) %>% 
  add_summary_rows(
    Country = "WAEMU", COU = "WAEMU", across(where(is.numeric), ~ mean(., na.rm = TRUE))
  )

datTax <- pivot_wider(dTaxUEMOA, id_cols = c(COU, Country, `Revenue category`, `Level of government`), names_from = c(Year), values_from = Value)



library(readr)

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

bceao <- pivot_wider(bceao, id_cols = c(PAYS, ANNEE), 
                       names_from = c(LIBELLE, `UNITE DE MESURE`, MAGNITUDE, `TYPE SERIE`),
                       names_sep = " ", values_from = value)

bceaoVar <- c("Droits et taxes a l'importation F. CFA Milliards Flux", "PRODUIT INTERIEUR BRUT (PIB) NOMINAL F. CFA Milliards Flux", 
"valeur ajoutee du secteur primaire F. CFA Milliards Flux", "Poids (%)†: Secteur primaire Pourcentage Aucune Ratio",
"Recettes totales et dons FCFA Milliards Flux",                                                    
"Recettes totales hors dons FCFA Milliards Flux",                                                   
"Recettes courantes FCFA Milliards Flux",                                                           
"Recettes fiscales FCFA Milliards Flux",                                                            
"Impots sur le commerce exterieur FCFA Milliards Flux",                                             
"Autres impots et taxes FCFA Milliards Flux",                                            
"Impots directs FCFA Milliards Flux",                                                      
"Recettes non fiscales FCFA Milliards Flux", 
"Recettes fiscales rapportees au PIB*100 Pourcentage Aucune Ratio"
)

# "Taux de pression fiscale Pourcentage Aucune Ratio" is all NA
# "Recettes fiscales rapportees au PIB*100 Pourcentage Aucune Ratio", calculated values match values in the variable. 
# The variable has a lot of missing values even though the numerators and denominators are available, hence the choice to go with the calculated values.  

bceao <- bceao %>%
  select(PAYS, ANNEE, all_of(bceaoVar))


bceao <- bceao %>%
  mutate(
    
    `Recettes fiscales % PIB` = 100*(`Recettes fiscales FCFA Milliards Flux`/`PRODUIT INTERIEUR BRUT (PIB) NOMINAL F. CFA Milliards Flux`), 
    `valeur ajoutee du secteur primaire % PIB` = 100*(`valeur ajoutee du secteur primaire F. CFA Milliards Flux`/`PRODUIT INTERIEUR BRUT (PIB) NOMINAL F. CFA Milliards Flux`)
  )

# Test of values

# "Recettes fiscales rapportees au PIB*100 Pourcentage Aucune Ratio" and "Poids (%)†: Secteur primaire Pourcentage Aucune Ratio": 
#   calculated values match values in the variables or very similar. 
#   The original variables have lot of missing values even though the numerators and denominators are available, hence the choice to go with the calculated variables  

  
# bceao %>%
#   select(PAYS, ANNEE, `Recettes fiscales FCFA Milliards Flux`, 
#          `Recettes fiscales rapportees au PIB*100 Pourcentage Aucune Ratio`, `Recettes fiscales % PIB`, `PRODUIT INTERIEUR BRUT (PIB) NOMINAL F. CFA Milliards Flux`) %>%
#   View()
#  
# 
# bceao %>%
#   select(PAYS, ANNEE, `valeur ajoutee du secteur primaire F. CFA Milliards Flux`, 
#          `Poids (%)†: Secteur primaire Pourcentage Aucune Ratio`, `valeur ajoutee du secteur primaire % PIB`, `PRODUIT INTERIEUR BRUT (PIB) NOMINAL F. CFA Milliards Flux`) %>%
#   View()  


bceao <- bceao %>%
  mutate(
    `Droits et taxes a l'importation % PIB` = 100*(`Droits et taxes a l'importation F. CFA Milliards Flux`/`PRODUIT INTERIEUR BRUT (PIB) NOMINAL F. CFA Milliards Flux`), 
    `Recettes totales et dons % PIB` = 100*(`Recettes totales et dons FCFA Milliards Flux`/`PRODUIT INTERIEUR BRUT (PIB) NOMINAL F. CFA Milliards Flux`),
    `Impots sur le commerce exterieur % PIB` = 100*(`Impots sur le commerce exterieur FCFA Milliards Flux`/ `PRODUIT INTERIEUR BRUT (PIB) NOMINAL F. CFA Milliards Flux`),
    `Impots directs % PIB` = 100*(`Impots directs FCFA Milliards Flux`/`PRODUIT INTERIEUR BRUT (PIB) NOMINAL F. CFA Milliards Flux`),
    `Recettes non fiscales % PIB` = 100*(`Recettes non fiscales FCFA Milliards Flux`/`PRODUIT INTERIEUR BRUT (PIB) NOMINAL F. CFA Milliards Flux`)
  )

bceao %>%
  select(PAYS, ANNEE, `Recettes totales et dons % PIB`, `Recettes non fiscales % PIB`, 
         `Impots directs % PIB`, `Impots sur le commerce exterieur % PIB`, `Droits et taxes a l'importation % PIB`, everything()) %>%
  filter(PAYS %in% "GUINEE BISSAU") %>%
  View()


bceao %>%
  select(PAYS, ANNEE, `Recettes totales et dons % PIB`, `Recettes non fiscales % PIB`, 
         `Impots directs % PIB`, `Impots sur le commerce exterieur % PIB`, `Droits et taxes a l'importation % PIB`, everything()) %>%
  filter(PAYS %in% "BENIN") %>%
  View()



C:/Users/aadama/OneDrive - Université Clermont Auvergne/UEMOA/App"










