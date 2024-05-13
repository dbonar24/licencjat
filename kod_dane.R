####################
#####Biblioteki#####
####################
install.packages('plm')
install.packages('bitops')
install.packages("gplots", dependencies = TRUE)
install.packages("pivottabler")
install.packages("cowplot")
install.packages("moments")
library(cowplot)
library(pivottabler)
library(bitops)
library(gplots)
library(tidyverse)
library(readxl)
library(ggplot2)
library(envalysis)
library(plm)
library(moments)

########################
#####Obszar roboczy#####
########################

setwd("C:/Users/Dawid/Desktop/Licencjat/data")

##############
#####Dane#####
##############

eps_market_data = read.csv("EPS_full_data.csv", sep = ";")

###########################################################
#####Definiujemy zbiór państw OECD które leżą w Europe#####
###########################################################

europe_country = c("Austria", "Belgium", "Czechia", "Denmark", "Estonia", "Finland", "France", "Germany", "Greece", "Hungary", "Ireland", "Iceland", "Italy", "Netherlands", "Norway", "Poland", "Portugal", "Slovak Republic", "Slovenia", "Spain", "Sweden", "Switzerland", "United Kingdom")
europe_country = sort(europe_country)
europe_country

############################
#####Filtrowanie danych#####
############################
#Environmental Policy Strignency

EPS_total = eps_market_data %>%
              filter(Variable == "Environmental Policy Stringency",
                     Country %in% europe_country,
                     Year >= 1995) %>%
              select(Country, Year, Value) %>%
              arrange(Country, Year) %>%
              rename("EPS" = "Value")
EPS_total
#Market based

market_based = eps_market_data %>%
                filter(Variable == "Market based policies",
                       Country %in% europe_country,
                       Year >= 1995) %>%
                select(Country, Year, Value) %>%
                arrange(Country, Year) %>%
                rename("Market" = "Value")
market_based 

#Non-market based

non_market_based = eps_market_data %>%
  filter(Variable == "Non-market based policies",
         Country %in% europe_country,
         Year >= 1995) %>%
  select(Country, Year, Value) %>%
  arrange(Country, Year) %>%
  rename("Non_market" = "Value")

#Technology

technology = eps_market_data %>%
  filter(Variable == "Technology support policies",
         Country %in% europe_country,
         Year >= 1995) %>%
  select(Country, Year, Value) %>%
  arrange(Country, Year) %>%
  rename("Tech" = "Value")

technology

#KOFGI - wskaźnik globalizacji

kofgi = read_xlsx(path = "KOFGI.xlsx", sheet = 1)

kofgi_europe_countries = c("Austria", "Belgium", "Czech Republic", "Denmark", "Estonia", "Finland", "France", "Germany", "Greece", "Hungary", "Ireland", "Iceland", "Italy", "Netherlands", "Norway", "Poland", "Portugal", "Slovak Republic", "Slovenia", "Spain", "Sweden", "Switzerland", "United Kingdom")

kofgi_selected = kofgi %>%
                  filter(country %in% kofgi_europe_countries,
                         year >= 1995 & year <= 2020) %>%
                  select(country, year, KOFGI) %>%
                  arrange(country, year) %>%
                  rename("Country" = "country",
                         "Year" = "year")

kofgi_selected

kofgi_selected$Country = str_replace(kofgi_selected$Country, "Czech Republic", "Czechia")

unique(kofgi_selected$Country)

kofgi_df = as.data.frame(kofgi_selected)
kofgi_df

#GDP per capita

gdp_per_capita = read_csv("GDP.csv")

gdp_selected = gdp_per_capita %>%
                select(-c("Country Code", "Indicator Name", "Indicator Code", "...68"))

name_table = cbind(gdp_selected["Country Name"], seq(from = 1, to = 266, by = 1))
colnames(name_table) = c("Country Name", "Number")
gdp_selected["Country Name"] = seq(from = 1, to = 266, by = 1)

#FDI % of GDP

FDI = read_csv("FDI.csv")

fdi_selected = FDI %>%
  select(-c("Country Code", "Indicator Name", "Indicator Code", "...68"))

fdi_selected["Country Name"] = seq(from = 1, to = 266, by = 1)

#Renewable energy consumption (% of energy consumption)

REC = read_csv("REC.csv")

rec_selected = REC %>%
  select(-c("Country Code", "Indicator Name", "Indicator Code", "...68"))

rec_selected["Country Name"] = seq(from = 1, to = 266, by = 1)

#Industry value added

INDUSTRY = read_csv("INDUSTRY.csv")

industry_selected = INDUSTRY %>%
  select(-c("Country Code", "Indicator Name", "Indicator Code", "2023", "...69"))

industry_selected["Country Name"] = seq(from = 1, to = 266, by = 1)

#Kod testowy z łazarski online courses
data = gdp_selected
data2 = fdi_selected
data3 = rec_selected
data4 = industry_selected
id<-gdp_selected$`Country Name`
c<-length(id)
c # to indicate the number of countries
a<-1960 #beginning of observation period
b<-2022 #end of observation period
year<-c(a:b)
year
d<-length(year)
d
matrix<-matrix(0, nrow=(c*d), ncol=10) ##number of columns should be at least 3 (cross-section indicator, year indicator, and at least one variable)
year.v<-rep(year, c)
year.v
for (n in 1:(c*d)){
  matrix[n,2]<-year.v[n]
}

matrix
country.v<-rep(id, each=d)
country.v
country.v[31]
for (n in 1:(c*d)){
  matrix[n,1]<-country.v[n]
}
### zmienna GDP
m1<-data[,-c(1)]
m2<-t(m1)
m2.v<-as.vector(m2)
for (n in 1:(c*d)){
  matrix[n,3]<-m2.v[n]
}
### zmienna FDI
m3 = data2[,-c(1)]
m4 = t(m3)
m4.v = as.vector(m4)
for (n in 1:(c*d)){
  matrix[n,4] = m4.v[n]
}
### zmienna REC
m5 = data3[,-c(1)]
m6 = t(m5)
m6.v = as.vector(m6)
for (n in 1:(c*d)) {
  matrix[n,5] = m6.v[n]
}
### zmienna INDUSTRY
m7 = data4[,-c(1)]
m8 = t(m7)
m8.v = as.vector(m8)
for (n in 1:(c*d)) {
  matrix[n,6] = m8.v[n]
}
#### strukturyzowanie macierzy z danymi
world_bank_data = matrix[,-c(7:10)]
world_bank_data = as.data.frame(world_bank_data)
world_bank_data$V1 = as.numeric(world_bank_data$V1)
world_bank_data$V2 = as.numeric(world_bank_data$V2)
world_bank_data$V3 = as.numeric(world_bank_data$V3)
world_bank_data$V4 = as.numeric(world_bank_data$V4)
world_bank_data$V5 = as.numeric(world_bank_data$V5)
world_bank_data$V6 = as.numeric(world_bank_data$V6)
colnames(world_bank_data) = c("country", "year", "GDP", "FDI", "REC", "INDUSTRY")

#inner join nazw z ich liczbowymi odpowiednikami

name_table
world_bank_data = world_bank_data %>%
                inner_join(name_table, by = join_by(country == Number))

#Dane zostały zrestrukturyzowane, przejdzmy do filtrowania

world_bank_data_filtered = world_bank_data %>%
                select(`Country Name`, year, GDP, FDI, REC, INDUSTRY) %>%
                filter(`Country Name` %in% europe_country,
                       year >= 1995 & year <= 2020) %>%
                arrange(`Country Name`, year) %>%
                rename("Country" = `Country Name`)

world_bank_data_filtered
tail(world_bank_data_filtered)

#Macierz zmiennych objaśniających
X_matrix_PBA = cbind(world_bank_data_filtered, technology, market_based, non_market_based, kofgi_df, EPS_total)
X_matrix_PBA
X_matrix_corrected_PBA = X_matrix_PBA[,-c(7,8,10,11,13,14,16,17,19,20)]
X_matrix_corrected  = X_matrix_corrected_PBA %>%
                          filter(Country != "Norway",
                                 Country != "Iceland")



#Emisje CO2 podejściami CBA I PBA

PBA = read_csv("PBA.csv")
CBA = read_csv("CBA.csv")

PBA_eu_countries = c("Austria", "Belgium", "Czechia", "Denmark", "Estonia", "Finland", "France", "Germany", "Greece", "Hungary", "Ireland", "Italy", "Netherlands", "Poland", "Portugal", "Slovakia", "Slovenia", "Spain", "Sweden", "Switzerland", "United Kingdom")

PBA_selected = PBA %>%
                filter(Year >= 1995 & Year <= 2020,
                       Entity %in% PBA_eu_countries) %>%
                select(Entity, Year, `Annual CO2 emissions (per capita)`) %>%
                rename("PBA" = `Annual CO2 emissions (per capita)`) %>%
                arrange(Entity, Year)

CBA_eu_countries = c("Austria", "Belgium", "Czechia", "Denmark", "Estonia", "Finland", "France", "Germany", "Greece", "Hungary", "Ireland", "Italy", "Netherlands", "Poland", "Portugal", "Slovakia", "Slovenia", "Spain", "Sweden", "Switzerland", "United Kingdom")

CBA_selected = CBA %>%
                filter(Year >= 1995 & Year <= 2020,
                       Entity %in% CBA_eu_countries,
                       Entity != "Norway") %>%
                select(Entity, Year, `Per capita consumption-based CO2 emissions`) %>%
                rename("CBA" = `Per capita consumption-based CO2 emissions`) %>%
                arrange(Entity, Year)

#Tworze dwie macierze do modeli dla zmiennych CBA I PBA

PBA_model = cbind(X_matrix_corrected, PBA_selected)
PBA_model = PBA_model[,-c(12,13)]

CBA_model = cbind(X_matrix_corrected, CBA_selected)
CBA_model = CBA_model[,-c(12,13)]
CBA_model 
#Dodanie zmiennych opartych na wskasnikach EPS od 2005 roku
CBA_model = CBA_model %>%
              mutate(Tech_2008 = case_when(
                                    year >= 2008 ~ Tech,
                                    year < 2008 ~ 0),
                     Market_2008 = case_when(
                                    year >= 2008 ~ Market,
                                    year < 2008 ~ 0),
                     Non_market_2008 = case_when(
                                    year >= 2008 ~ Non_market,
                                    year < 2008 ~ 0),
                     EPS_2008 = case_when(
                                    year >= 2008 ~ EPS,
                                    year < 2008 ~ 0))

PBA_model = PBA_model %>%
              mutate(Tech_2008 = case_when(
                                    year >= 2008 ~ Tech,
                                    year < 2008 ~ 0),
                    Market_2008 = case_when(
                                    year >= 2008 ~ Market,
                                    year < 2008 ~ 0),
                    Non_market_2008 = case_when(
                                    year >= 2008 ~ Non_market,
                                    year < 2008 ~ 0),
                    EPS_2008 = case_when(
                                    year >= 2008 ~ EPS,
                                    year < 2008 ~ 0))
                  
                                                
              
#Eksportuje dane z R do csv
write.csv(CBA_model, "C:/Users/Dawid/Desktop/Licencjat/data\\CBA_model.csv")
write.csv(PBA_model, "C:/Users/Dawid/Desktop/Licencjat/data\\PBA_model.csv")
#write.csv(PBA_selected, "C:/Users/Dawid/Desktop/Licencjat/data\\PBA_selected_data.csv")
#write.csv(CBA_selected, "C:/Users/Dawid/Desktop/Licencjat/data\\CBA_selected_data.csv")
#Funkcja do tworzenia wykresów
statistic_plots = function(data_, var_, tytul_wykresu_, jednostka_, all_stats, dynamics_, file_name_, want_to_save) {
  PBA_model_var = data_ %>%
                  select(Country, year)
  PBA_model_var = cbind(PBA_model_var, data_[var_])

  Panel = as.data.frame(spread(PBA_model_var, key = year, value = var_))
  id_country = as.vector(Panel[,1])
  Panel = Panel[,-1]
  row.names(Panel) = id_country
  print(Panel)
  #Statystyki opisowe dla wybranej zmiennej
  if (all_stats == TRUE) {
    summary_stats = matrix(0, nrow = 26, ncol = 9)
    years = c(1995:2020)
    colnames(summary_stats) = c("min", "max", "avg", "median", "Q1", "Q3", "sd", "V", "Asymetria")
    #min
    for (n in 1:26) {
    summary_stats[n,1] = min(Panel[,n])
      }
    #max
    for (n in 1:26) {
      summary_stats[n,2] = max(Panel[,n])
      }
    #avg
    summary_stats[,3] = colMeans(Panel)
    #median
    me_matrix = as.vector(apply(Panel, 2, median))
    me_matrix
    summary_stats[,4] = me_matrix
    #kwartyle
    Q1_matrix = as.vector(apply(Panel, 2, quantile, probs = c(0.25)))
    Q1_matrix
    Q3_matrix = as.vector(apply(Panel, 2, quantile, probs = c(0.75)))
    Q3_matrix 
    summary_stats[,5] = Q1_matrix
    summary_stats[,6] = Q3_matrix
    #odchylenie standardowe
    for (n in 1:26) {
      summary_stats[n,7] = sd(Panel[,n])
    }
    #wsp zmiennosci
    summary_stats[,8] = summary_stats[,7] / summary_stats[,3]
    #wsp skosnosci
    for (n in 1:26) {
    summary_stats[n,9] = 3*((summary_stats[n,3] - summary_stats[n,4])/summary_stats[n,7])
    }
    #Jeszcze kolumna lat
    summary_stats = as.data.frame(cbind(years, summary_stats))
    print(summary_stats)
    #CSV
    if (want_to_save == TRUE) {
    choosen_stats = summary_stats %>%
                      filter(years %in% c(1995, 2000, 2020)) %>%
                      select(years, min, max, avg, sd, V, median, Asymetria, Q1, Q3)
    write_csv(choosen_stats, file_name_)
    }
    #Dane pod indeks dynamiki
    if (dynamics_ == TRUE) {
    jednopodst = as.data.frame(summary_stats[,4] / summary_stats[1,4])
    dane_wykres_jednopodst = cbind(jednopodst, years)
    colnames(dane_wykres_jednopodst) = c("Index", "year")
    wykres_jednopodst = ggplot(dane_wykres_jednopodst, aes(x = year, y = Index)) +
                          geom_line() +
                          theme_minimal() +
                          theme(plot.title = element_text(hjust = 0.5),
                                panel.background = element_rect(fill = NA),
                                legend.position = "right") +
                          labs(title = "Indeks jednopodstawowy")
    srednia = as.matrix(summary_stats[,4])
    chain_matrix = matrix(0, nrow = 25, ncol = 1)
    for (i in 0:24) {
      chain_matrix[1+i,] = srednia[2+i,] / srednia[1+i,]
    }
    chain_matrix = as.data.frame(chain_matrix)
    dane_wykres_chain = cbind(chain_matrix, seq(1996, 2020, by = 1))
    colnames(dane_wykres_chain) = c("Index", "year")
    wykres_lancuch = ggplot(dane_wykres_chain, aes(x = year, y = Index)) +
                          geom_line() +
                          theme_minimal() +
                          theme(plot.title = element_text(hjust = 0.5),
                                panel.background = element_rect(fill = NA),
                                legend.position = "right") +
                          labs(title = "Indeks lancuchowy")
    print(plot_grid(wykres_jednopodst, wykres_lancuch, ncol = 2))
    }
    #Wykres liniowy dla tych zmiennych
    wykresik = ggplot(summary_stats, aes(x = years)) +
                  geom_line(aes(y = max, color = "Maksimum")) +
                  geom_line(aes(y = Q3, color = "Kwartyl trzeci")) +
                  geom_line(aes(y = avg, color = "Srednia")) +
                  geom_line(aes(y = median, color = "Mediana")) +
                  geom_line(aes(y = Q1, color = "Kwartyl pierwszy")) +
                  geom_line(aes(y = min, color = "Minimum")) +
                  labs(title = tytul_wykresu_, x = "Lata", y = jednostka_, colour = "Statystyki") +
                  scale_color_manual(values = c("Maksimum" = "darkred", "Kwartyl trzeci" = "orange", "Srednia" = "darkgreen",
                                            "Mediana" = "darkblue", "Kwartyl pierwszy" = "blue", "Minimum" = "red"),
                                 breaks = c("Maksimum", "Kwartyl trzeci", "Srednia", "Mediana", "Kwartyl pierwszy", "Minimum")) +
                  theme_minimal() +
                  theme(plot.title = element_text(hjust = 0.5),
                      panel.background = element_rect(fill = NA),
                      legend.position = "right")
    print(wykresik)
  } else if (all_stats == FALSE) {
    summary_stats = matrix(0, nrow = 26, ncol = 7)
    years = c(1995:2020)
    colnames(summary_stats) = c("min", "max", "avg", "median", "sd", "V", "Asymetria")
    #min
    for (n in 1:26) {
      summary_stats[n,1] = min(Panel[,n])
    }
    #max
    for (n in 1:26) {
      summary_stats[n,2] = max(Panel[,n])
    }
    #avg
    summary_stats[,3] = colMeans(Panel)
    #median
    me_matrix = as.vector(apply(Panel, 2, median))
    me_matrix
    summary_stats[,4] = me_matrix
    #odchylenie standardowe
    for (n in 1:26) {
      summary_stats[n,5] = sd(Panel[,n])
    }
    #Wsp zmiennosci
    summary_stats[,6] = summary_stats[,5] / summary_stats[,3]
    #wsp skosnosci
    for (n in 1:26) {
      summary_stats[n,7] = 3*((summary_stats[n,3] - summary_stats[n,4])/summary_stats[n,5])
    }
    summary_stats = as.data.frame(cbind(years, summary_stats))
    print(summary_stats)
    #Jeszcze kolumna lat
    if (want_to_save == TRUE) {
    choosen_stats = summary_stats %>%
                      filter(years %in% c(1995, 2000, 2020)) %>%
                      select(years, min, max, avg, sd, V, median, Asymetria)
    write_csv(choosen_stats, file_name_)
    }
    #Dynamika
    if (dynamics_ == TRUE) {
      jednopodst = as.data.frame(summary_stats[,4] / summary_stats[1,4])
      dane_wykres_jednopodst = cbind(jednopodst, years)
      colnames(dane_wykres_jednopodst) = c("Index", "year")
      wykres_jednopodst = ggplot(dane_wykres_jednopodst, aes(x = year, y = Index)) +
        geom_line() +
        theme_minimal() +
        theme(plot.title = element_text(hjust = 0.5),
              panel.background = element_rect(fill = NA),
              legend.position = "right") +
        labs(title = "Indeks jednopodstawowy")
      srednia = as.matrix(summary_stats[,4])
      chain_matrix = matrix(0, nrow = 25, ncol = 1)
      for (i in 0:24) {
        chain_matrix[1+i,] = srednia[2+i,] / srednia[1+i,]
      }
      chain_matrix = as.data.frame(chain_matrix)
      dane_wykres_chain = cbind(chain_matrix, seq(1996, 2020, by = 1))
      colnames(dane_wykres_chain) = c("Index", "year")
      wykres_lancuch = ggplot(dane_wykres_chain, aes(x = year, y = Index)) +
        geom_line() +
        theme_minimal() +
        theme(plot.title = element_text(hjust = 0.5),
              panel.background = element_rect(fill = NA),
              legend.position = "right") +
        labs(title = "Indeks łańcuchowy")
      print(plot_grid(wykres_jednopodst, wykres_lancuch, ncol = 2))
    }
    #Wykres liniowy dla tych zmiennych
    wykresik = ggplot(as.data.frame(summary_stats), aes(x = years)) +
      geom_line(aes(y = max, color = "Maksimum")) +
      geom_line(aes(y = avg, color = "Srednia")) +
      geom_line(aes(y = median, color = "Mediana")) +
      geom_line(aes(y = min, color = "Minimum")) +
      labs(title = tytul_wykresu_, x = "Lata", y = jednostka_, colour = "Statystyki") +
      scale_color_manual(values = c("Maksimum" = "darkred", "Srednia" = "darkgreen",
                                    "Mediana" = "darkblue", "Minimum" = "red"),
                         breaks = c("Maksimum", "Srednia", "Mediana", "Minimum")) +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5),
            panel.background = element_rect(fill = NA),
            legend.position = "right")
    print(wykresik)
    print(summary_stats)  
    }
}
#Wykresy dla innych statystyk
statistic_plots(data_ = PBA_model, var_ = "GDP", tytul_wykresu_ = "PKB na osobę", jednostka = "$", all_stats = TRUE, dynamics_ = TRUE, file_name_ = "GDP_data.csv", want_to_save = FALSE)
statistic_plots(data_ = PBA_model, var_ = "FDI", tytul_wykresu_ = "BIZ jako % w udziale PKB", jednostka_ = "%", all_stats = TRUE, dynamics_ = FALSE, want_to_save = FALSE, file_name_ = "FDI_data.csv")
statistic_plots(data_ = PBA_model, var_ = "REC", tytul_wykresu_ = "Odnawialne źródła energii", jednostka = "%", all_stats = TRUE, dynamics = TRUE, want_to_save = FALSE, file_name_ = "REC_data.csv")
statistic_plots(data_ = PBA_model, var_ = "KOFGI", tytul_wykresu_ = "Indeks globalizacji KOFGI", jednostka_ = "pkt", all_stats = TRUE, dynamics = TRUE, want_to_save = FALSE, file_name_ = "KOFGI_data.csv")
statistic_plots(data_ = CBA_model, var_ = "CBA", tytul_wykresu_ = "CO2 mierzone podejściem konsumpcyjnym", jednostka_ = "metric tons per capita", all_stats = TRUE, dynamics = TRUE, want_to_save = FALSE, file_name_ = "CBA_Data.csv")
statistic_plots(data_ = PBA_model, var_ = "PBA", tytul_wykresu_ = "CO2 mierzone podejściem produkcyjnym", jednostka_ = "metric tons per capita", all_stats = TRUE, dynamics = TRUE, want_to_save = FALSE, file_name_ = "PBA_data.csv")
statistic_plots(data_ = PBA_model, var_ = "Tech", tytul_wykresu_ = "Instrumenty technologiczne", jednostka_ = "pkt", all_stats = FALSE, dynamics = TRUE, want_to_save = FALSE, file_name_ = "TECH_data.csv")
statistic_plots(data_ = PBA_model, var_ = "Market", tytul_wykresu_ = "Instrumenty rynkowe", jednostka_ = "pkt", all_stats = FALSE, dynamics_ = TRUE, want_to_save = FALSE, file_name_ = "MARKET_data.csv")
statistic_plots(data_ = PBA_model, var_ = "Non_market", tytul_wykresu_ = "Instrumenty nierynkowe", jednostka_ = "pkt", all_stats = FALSE, dynamics_ = TRUE, want_to_save = FALSE, file_name_ = "NON_MARKET_data.csv")
statistic_plots(data_ = PBA_model, var_ = "INDUSTRY", tytul_wykresu_ = "Industrializacja", jednostka_ = "%", all_stats = TRUE, dynamics_ = TRUE, want_to_save = FALSE, file_name_ = "INDUSTRY_data.csv")
statistic_plots(data_ = PBA_model, var_ = "EPS", tytul_wykresu_ = "Wskaźnik EPS", jednostka_ = "pkt", all_stats = FALSE, dynamics_ = FALSE, want_to_save = FALSE, file_name_ = "EPS_data.csv")

#######################################
######Wybrane modele do analizy########
#######################################

  #Formula modeli

    #Formula dla modeli z podgrupami EPS
    model_formula_PBA = as.formula(log(PBA) ~ log(GDP) + log(REC) + log(KOFGI) + FDI + Tech + Market + Non_market)
    model_formula_CBA = as.formula(log(CBA) ~ log(GDP) + log(REC) + log(KOFGI) + FDI + Tech + Market + Non_market)
    #Formula dla modeli z calosciowym wskaznikiem EPS
    model_formula_PBA_EPS = as.formula(log(PBA) ~ log(GDP) + log(REC) + log(KOFGI) + FDI + EPS)
    model_formula_CBA_EPS = as.formula(log(CBA) ~ log(GDP) + log(REC) + log(KOFGI) + FDI + EPS)
    #Formula dla modeli z podgrupami EPS i przelacznikiem
    model_formula_PBA_prz = as.formula(log(PBA) ~ log(GDP) + log(REC) + log(KOFGI) + FDI + Tech + Market + Non_market + Tech_2008 + Market_2008 + Non_market_2008)
    model_formula_CBA_prz = as.formula(log(CBA) ~ log(GDP) + log(REC) + log(KOFGI) + FDI + Tech + Market + Non_market + Tech_2008 + Market_2008 + Non_market_2008)
    #Formula dla modeli z calosciowym wskaznikiem EPS i przelacznikiem
    model_formula_PBA_EPS_prz = as.formula(log(PBA) ~ log(GDP) + log(REC) + log(KOFGI) + FDI + EPS + EPS_2008)
    model_formula_CBA_EPS_prz = as.formula(log(CBA) ~ log(GDP) + log(REC) + log(KOFGI) + FDI + EPS + EPS_2008)
    
  #Model regresji przy uzyciu metody MNK
  
  ols_model_PBA = lm(model_formula_PBA, data = PBA_model)
  summary(ols_model_PBA)
  ols_model_CBA = lm(model_formula_CBA, data = CBA_model)
  summary(ols_model_CBA)
  
###EFEKTY STALE####
  
  #Efekty stałe dla 1995 - 2020
    
    #Model z podgrupami EPS
    log_fixed_95_20_PBA = plm(model_formula_PBA, data = PBA_model, index = c("Country", "year"), model = "within", effect = "individual")
    summary(log_fixed_95_20_PBA)
    within_intercept(log_fixed_95_20_PBA)
    log_fixed_95_20_CBA = plm(model_formula_CBA, data = CBA_model, index = c("Country", "year"), model = "within", effect = "individual")
    summary(log_fixed_95_20_CBA)
    within_intercept(log_fixed_95_20_CBA)
    
    #Model z calosciowym EPS
    log_fixed_95_20_PBA_EPS = plm(model_formula_PBA_EPS, data = PBA_model, index = c("Country", "year"), model = "within", effect = "individual")
    summary(log_fixed_95_20_PBA_EPS)
    within_intercept(log_fixed_95_20_PBA_EPS)
    log_fixed_95_20_CBA_EPS = plm(model_formula_CBA_EPS, data = CBA_model, index = c("Country", "year"), model = "within", effect = "individual")
    summary(log_fixed_95_20_CBA_EPS)
    within_intercept(log_fixed_95_20_CBA_EPS)
    
    #Model z przełącznikiem
      
      #Modele z podgrupami EPS
      
      log_fixed_95_20_PBA_prz = plm(model_formula_PBA_prz, data = PBA_model, index = c("Country", "year"), model = "within", effect = "individual")
      summary(log_fixed_95_20_PBA_prz)
      within_intercept(log_fixed_95_20_PBA_prz)
      log_fixed_95_20_CBA_prz = plm(model_formula_CBA_prz, data = CBA_model, index = c("Country", "year"), model = "within", effect = "individual")
      summary(log_fixed_95_20_CBA_prz)
      within_intercept(log_fixed_95_20_CBA_prz)
      
      #Modele z calosciowym EPS
      
      log_fixed_95_20_PBA_EPS_prz = plm(model_formula_PBA_EPS_prz, data = PBA_model, index = c("Country", "year"), model = "within", effect = "individual")
      summary(log_fixed_95_20_PBA_EPS_prz)
      within_intercept(log_fixed_95_20_PBA_EPS_prz)
      log_fixed_95_20_CBA_EPS_prz = plm(model_formula_CBA_EPS_prz, data = CBA_model, index = c("Country", "year"), model = "within", effect = "individual")
      summary(log_fixed_95_20_CBA_EPS_prz)
      within_intercept(log_fixed_95_20_CBA_EPS_prz)
      
  #Efekty stale dla 1995-2008
  
  PBA_log_95_08 = PBA_model %>%
                    filter(year >= 1995 & year < 2008)
  CBA_log_95_08 = CBA_model %>%
                    filter(year >= 1995 & year < 2008)
      
      #Model z podgrupami EPS
      
      log_fixed_95_08_PBA = plm(model_formula_PBA, data = PBA_log_95_08, index = c("Country", "year"), model = "within", effect = "individual")
      summary(log_fixed_95_08_PBA)
      within_intercept(log_fixed_95_08_PBA)
      log_fixed_95_08_CBA = plm(model_formula_CBA, data = CBA_log_95_08, index = c("Country", "year"), model = "within", effect = "individual")
      summary(log_fixed_95_08_CBA)
      within_intercept(log_fixed_95_08_CBA)
      
      #Model z calosciowym EPS

      log_fixed_95_08_PBA_EPS = plm(model_formula_PBA_EPS, data = PBA_log_95_08, index = c("Country", "year"), model = "within", effect = "individual")
      summary(log_fixed_95_08_PBA_EPS)
      within_intercept(log_fixed_95_08_PBA_EPS)
      log_fixed_95_08_CBA_EPS = plm(model_formula_CBA_EPS, data = CBA_log_95_08, index = c("Country", "year"), model = "within", effect = "individual")
      summary(log_fixed_95_08_CBA_EPS)
      within_intercept(log_fixed_95_08_CBA_EPS)      
      
  #Efekty stale dla 2005-2020
  
  PBA_log_08_20 = PBA_model %>%
                    filter(year >= 2008)
  CBA_log_08_20 = CBA_model %>%
                    filter(year >= 2008)
      
      #Model z podgrupami EPS
      
      log_fixed_08_20_PBA = plm(model_formula_PBA, data = PBA_log_08_20, index = c("Country", "year"), model = "within", effect = "individual")
      summary(log_fixed_08_20_PBA)
      within_intercept(log_fixed_08_20_PBA)
      log_fixed_08_20_CBA = plm(model_formula_CBA, data = CBA_log_08_20, index = c("Country", "year"), model = "within", effect = "individual")
      summary(log_fixed_08_20_CBA)
      within_intercept(log_fixed_08_20_CBA)
      
      #Model z calosciowym EPS
      
      log_fixed_08_20_PBA_EPS = plm(model_formula_PBA_EPS, data = PBA_log_08_20, index = c("Country", "year"), model = "within", effect = "individual")
      summary(log_fixed_08_20_PBA_EPS)
      within_intercept(log_fixed_08_20_PBA_EPS)
      log_fixed_08_20_CBA_EPS = plm(model_formula_CBA_EPS, data = CBA_log_08_20, index = c("Country", "year"), model = "within", effect = "individual")
      summary(log_fixed_08_20_CBA_EPS)
      within_intercept(log_fixed_08_20_CBA_EPS)

###EFEKTY LOSOWE####
      
      #Efekty losowe dla 1995 - 2020
      
      #Model z podgrupami EPS
      log_random_95_20_PBA = plm(model_formula_PBA, data = PBA_model, index = c("Country", "year"), model = "random", effect = "individual", random.method = "nerlove")
      summary(log_random_95_20_PBA)
      log_random_95_20_CBA = plm(model_formula_CBA, data = CBA_model, index = c("Country", "year"), model = "random", effect = "individual", random.method = "nerlove")
      summary(log_random_95_20_CBA)
      
      #Model z calosciowym EPS
      log_random_95_20_PBA_EPS = plm(model_formula_PBA_EPS, data = PBA_model, index = c("Country", "year"), model = "random", effect = "individual", random.method = "nerlove")
      summary(log_random_95_20_PBA_EPS)
      log_random_95_20_CBA_EPS = plm(model_formula_CBA_EPS, data = CBA_model, index = c("Country", "year"), model = "random", effect = "individual", random.method = "nerlove")
      summary(log_random_95_20_CBA_EPS)
      
      #Model z przełącznikiem
      
          #Modele z podgrupami EPS
          
          log_random_95_20_PBA_prz = plm(model_formula_PBA_prz, data = PBA_model, index = c("Country", "year"), model = "random", effect = "individual", random.method = "nerlove")
          summary(log_random_95_20_PBA_prz)
          log_random_95_20_CBA_prz = plm(model_formula_CBA_prz, data = CBA_model, index = c("Country", "year"), model = "random", effect = "individual", random.method = "nerlove")
          summary(log_random_95_20_CBA_prz)
          
          #Modele z calosciowym EPS
          
          log_random_95_20_PBA_EPS_prz = plm(model_formula_PBA_EPS_prz, data = PBA_model, index = c("Country", "year"), model = "random", effect = "individual", random.method = "nerlove")
          summary(log_random_95_20_PBA_EPS_prz)
          log_random_95_20_CBA_EPS_prz = plm(model_formula_CBA_EPS_prz, data = CBA_model, index = c("Country", "year"), model = "random", effect = "individual", random.method = "nerlove")
          summary(log_random_95_20_CBA_EPS_prz)

      
      #Efekty losowe dla 1995-2008
      
  
          #Model z podgrupami EPS
          
          log_random_95_08_PBA = plm(model_formula_PBA, data = PBA_log_95_08, index = c("Country", "year"), model = "random", effect = "individual", random.method = "nerlove")
          summary(log_random_95_08_PBA)
          log_random_95_08_CBA = plm(model_formula_CBA, data = CBA_log_95_08, index = c("Country", "year"), model = "random", effect = "individual", random.method = "nerlove")
          summary(log_random_95_08_CBA)
      
          #Model z calosciowym EPS
          
          log_random_95_08_PBA_EPS = plm(model_formula_PBA_EPS, data = PBA_log_95_08, index = c("Country", "year"), model = "random", effect = "individual", random.method = "nerlove")
          summary(log_random_95_08_PBA_EPS)
          log_random_95_08_CBA_EPS = plm(model_formula_CBA_EPS, data = CBA_log_95_08, index = c("Country", "year"), model = "random", effect = "individual", random.method = "nerlove")
          summary(log_random_95_08_CBA_EPS)
      
      #Efekty losowe dla 2005-2020

      
          #Model z podgrupami EPS
          
          log_random_08_20_PBA = plm(model_formula_PBA, data = PBA_log_08_20, index = c("Country", "year"), model = "random", effect = "individual", random.method = "nerlove")
          summary(log_random_08_20_PBA)
          log_random_08_20_CBA = plm(model_formula_CBA, data = CBA_log_08_20, index = c("Country", "year"), model = "random", effect = "individual", random.method = "nerlove")
          summary(log_random_08_20_CBA)
      
          #Model z calosciowym EPS
          
          log_random_08_20_PBA_EPS = plm(model_formula_PBA_EPS, data = PBA_log_08_20, index = c("Country", "year"), model = "random", effect = "individual", random.method = "nerlove")
          summary(log_random_08_20_PBA_EPS)
          log_random_08_20_CBA_EPS = plm(model_formula_CBA_EPS, data = CBA_log_08_20, index = c("Country", "year"), model = "random", effect = "individual", random.method = "nerlove")
          summary(log_random_08_20_CBA_EPS)
      
######
#Modele bez FDI
######
          #Formula dla modeli z podgrupami EPS bez FDI
          model_formula_PBA_noFDI = as.formula(log(PBA) ~ log(GDP) + log(REC) + log(KOFGI) + Tech + Market + Non_market)
          model_formula_CBA_noFDI = as.formula(log(CBA) ~ log(GDP) + log(REC) + log(KOFGI) + Tech + Market + Non_market)
          #Formula dla modeli z calosciowym wskaznikiem EPS
          model_formula_PBA_EPS_noFDI = as.formula(log(PBA) ~ log(GDP) + log(REC) + log(KOFGI) + EPS)
          model_formula_CBA_EPS_noFDI = as.formula(log(CBA) ~ log(GDP) + log(REC) + log(KOFGI) + EPS)
          
          #Model z podgrupami EPS lata 1995 - 2020
          log_random_95_20_PBA_noFDI = plm(model_formula_PBA_noFDI, data = PBA_model, index = c("Country", "year"), model = "random", effect = "individual", random.method = "nerlove")
          summary(log_random_95_20_PBA_noFDI)
          log_random_95_20_CBA_noFDI = plm(model_formula_CBA_noFDI, data = CBA_model, index = c("Country", "year"), model = "random", effect = "individual", random.method = "nerlove")
          summary(log_random_95_20_CBA_noFDI)
          
          #Model z calosciowym EPS lata 1995 - 2020
          log_random_95_20_PBA_EPS_noFDI = plm(model_formula_PBA_EPS_noFDI, data = PBA_model, index = c("Country", "year"), model = "random", effect = "individual", random.method = "nerlove")
          summary(log_random_95_20_PBA_EPS_noFDI)
          log_random_95_20_CBA_EPS_noFDI = plm(model_formula_CBA_EPS_noFDI, data = CBA_model, index = c("Country", "year"), model = "random", effect = "individual", random.method = "nerlove")
          summary(log_random_95_20_CBA_EPS_noFDI)
          
          #Model z podgrupami EPS lata 2008-2020
          
          log_random_08_20_PBA_noFDI = plm(model_formula_PBA_noFDI, data = PBA_log_08_20, index = c("Country", "year"), model = "random", effect = "individual", random.method = "nerlove")
          summary(log_random_08_20_PBA_noFDI)
          log_random_08_20_CBA_noFDI = plm(model_formula_CBA_noFDI, data = CBA_log_08_20, index = c("Country", "year"), model = "random", effect = "individual", random.method = "nerlove")
          summary(log_random_08_20_CBA_noFDI)
          
          #Model z calosciowym EPS lata 2008-2020
          
          log_random_08_20_PBA_EPS_noFDI = plm(model_formula_PBA_EPS_noFDI, data = PBA_log_08_20, index = c("Country", "year"), model = "random", effect = "individual", random.method = "nerlove")
          summary(log_random_08_20_PBA_EPS_noFDI)
          log_random_08_20_CBA_EPS_noFDI = plm(model_formula_CBA_EPS_noFDI, data = CBA_log_08_20, index = c("Country", "year"), model = "random", effect = "individual", random.method = "nerlove")
          summary(log_random_08_20_CBA_EPS_noFDI)
          
          #Model z podgrupami EPS lata 1995 - 2008
          
          log_random_95_08_PBA_noFDI = plm(model_formula_PBA_noFDI, data = PBA_log_95_08, index = c("Country", "year"), model = "random", effect = "individual", random.method = "nerlove")
          summary(log_random_95_08_PBA_noFDI)
          log_random_95_08_CBA_noFDI = plm(model_formula_CBA_noFDI, data = CBA_log_95_08, index = c("Country", "year"), model = "random", effect = "individual", random.method = "nerlove")
          summary(log_random_95_08_CBA_noFDI)
          
          #Model z calosciowym EPS lata 1995 - 2008
          
          log_random_95_08_PBA_EPS_noFDI = plm(model_formula_PBA_EPS_noFDI, data = PBA_log_95_08, index = c("Country", "year"), model = "random", effect = "individual", random.method = "nerlove")
          summary(log_random_95_08_PBA_EPS_noFDI)
          log_random_95_08_CBA_EPS_noFDI = plm(model_formula_CBA_EPS_noFDI, data = CBA_log_95_08, index = c("Country", "year"), model = "random", effect = "individual", random.method = "nerlove")
          summary(log_random_95_08_CBA_EPS_noFDI)
          
  #Definiuje fukcje ktora roznicuje mi wybrane kolumny
  diff_function = function(vector_, data_frame_) {
  for (col_name in vector_) {
    # Tworzenie nowej kolumny z dopiskiem "_diff"
    new_col_name <- paste0(col_name, "_diff")
    
    # Wykonywanie funkcji przy użyciu mutate
    data_frame_ <- data_frame_ %>%
                    group_by(Country) %>%
                    mutate(!!new_col_name := c(NA, diff(.data[[col_name]]))) %>%
                    ungroup()
  }
  print(data_frame_)
  }

#####
##Grupowanie wszystkich modeli
#####
fixed_standard_models = c("log_fixed_95_20_PBA", "log_fixed_95_20_CBA", "log_fixed_95_08_PBA", "log_fixed_95_08_CBA", "log_fixed_08_20_PBA", "log_fixed_08_20_CBA")
fixed_EPS_models = c("log_fixed_95_20_PBA_EPS", "log_fixed_95_20_CBA_EPS", "log_fixed_95_08_PBA_EPS", "log_fixed_95_08_CBA_EPS", "log_fixed_08_20_PBA_EPS", "log_fixed_08_20_CBA_EPS")
fixed_przelacznik_standard_models = c("log_fixed_95_20_PBA_prz", "log_fixed_95_20_CBA_prz")
fixed_przelacznik_EPS_models = c("log_fixed_95_20_PBA_EPS_prz", "log_fixed_95_20_CBA_EPS_prz")
random_standard_models = c("log_random_95_20_PBA", "log_random_95_20_CBA", "log_random_95_08_PBA", "log_random_95_08_CBA", "log_random_08_20_PBA", "log_random_08_20_CBA")
random_EPS_models = c("log_random_95_20_PBA_EPS", "log_random_95_20_CBA_EPS", "log_random_95_08_PBA_EPS", "log_random_95_08_CBA_EPS", "log_random_08_20_PBA_EPS", "log_random_08_20_CBA_EPS") 
random_przelacznik_standard_models = c("log_random_95_20_PBA_prz", "log_random_95_20_CBA_prz") 
random_przelacznik_EPS_models = c("log_random_95_20_PBA_EPS_prz", "log_random_95_20_CBA_EPS_prz")
random_standard_models_noFDI = c("log_random_95_20_PBA_noFDI", "log_random_95_20_CBA_noFDI", "log_random_95_08_PBA_noFDI", "log_random_95_08_CBA_noFDI", "log_random_08_20_PBA_noFDI", "log_random_08_20_CBA_noFDI")
random_EPS_models_noFDI = c("log_random_95_20_PBA_EPS_noFDI", "log_random_95_20_CBA_EPS_noFDI", "log_random_95_08_PBA_EPS_noFDI", "log_random_95_08_CBA_EPS_noFDI", "log_random_08_20_PBA_EPS_noFDI", "log_random_08_20_CBA_EPS_noFDI")
standard_models = c("log_fixed_95_20_PBA", "log_fixed_95_20_CBA", "log_fixed_95_08_PBA", "log_fixed_95_08_CBA", "log_fixed_08_20_PBA", "log_fixed_08_20_CBA", "log_random_95_20_PBA", "log_random_95_20_CBA", "log_random_95_08_PBA", "log_random_95_08_CBA", "log_random_08_20_PBA", "log_random_08_20_CBA")
EPS_models = c("log_fixed_95_20_PBA_EPS", "log_fixed_95_20_CBA_EPS", "log_fixed_95_08_PBA_EPS", "log_fixed_95_08_CBA_EPS", "log_fixed_08_20_PBA_EPS", "log_fixed_08_20_CBA_EPS", "log_random_95_20_PBA_EPS", "log_random_95_20_CBA_EPS", "log_random_95_08_PBA_EPS", "log_random_95_08_CBA_EPS", "log_random_08_20_PBA_EPS", "log_random_08_20_CBA_EPS")
standard_przelacznik_models = c("log_fixed_95_20_PBA_prz", "log_fixed_95_20_CBA_prz", "log_random_95_20_PBA_prz", "log_random_95_20_CBA_prz")
EPS_przelacznik_models = c("log_fixed_95_20_PBA_EPS_prz", "log_fixed_95_20_CBA_EPS_prz", "log_random_95_20_PBA_EPS_prz", "log_random_95_20_CBA_EPS_prz")
#Raportowanie wyników modeli standardowych
result_matrix_standard_models = matrix(0, nrow = 9, ncol = 12)
rownames(result_matrix_standard_models) = c("constant", "log_GDP", "log_REC", "log_KOFGI", "FDI", "Tech", "Market", "Nonmarket", "R_squared")
colnames(result_matrix_standard_models) = standard_models
coeff_p_value_function_fixed = function(model_) {
    result_coef = matrix(0, nrow = length(model_$coefficients) + 2, ncol = 1)
    intercept = round(within_intercept(model_)[1], digits = 3)
    error = attr(within_intercept(model_), "se")
    pre_intercept_p_value = round(2 * dt(x = intercept/error, df = nrow(model_$model) - length(model_) + 1), digits = 3)
    intercept_p_value = str_replace_all(paste("(", pre_intercept_p_value, ")"), " ", "")
    intercept_full = paste(intercept, intercept_p_value)
    result_coef[1,] = intercept_full
      for (i in 1:length(model_$coefficients)) {
      pre_p_value = round(summary(model_)$coefficients[i,4], digits = 3)
      p_value = str_replace_all(paste("(",pre_p_value,")"), " ", "")
      coeff = round(model_$coefficients[i], digits = 3)
      coeff_p_value = paste(coeff, p_value)
      result_coef[i+1,] = coeff_p_value
      }
    result_coef[length(model_$coefficients) + 2,] = round(r.squared(model_), digits = 3)
   print(as.matrix(result_coef))
  }
coeff_p_value_function_random = function(model_) {
  result_coef = matrix(0, nrow = length(model_$coefficients) + 1, ncol = 1)
  for (i in 1:length(model_$coefficients)) {
    pre_p_value = round(summary(model_)$coefficients[i,4], digits = 3)
    p_value = str_replace_all(paste("(",pre_p_value,")"), " ", "")
    coeff = round(model_$coefficients[i], digits = 3)
    coeff_p_value = paste(coeff, p_value)
    result_coef[i,] = coeff_p_value
  }
  result_coef[length(model_$coefficients) + 1,] = round(r.squared(model_), digits = 3)
  print(as.matrix(result_coef))
}
  for (i in (1:6)) {
    result_matrix_standard_models[,i] = coeff_p_value_function_fixed(get(fixed_standard_models[i]))
  }
  for (i in (1:6)) {
    result_matrix_standard_models[,i+6] = coeff_p_value_function_random(get(random_standard_models[i]))
  }
######
#Raportowanie wyników modeli z calosciowym wskaznikiem EPS#
######
result_matrix_EPS_models = matrix(0, nrow = 7, ncol = 12)
rownames(result_matrix_EPS_models) = c("constant", "log_GDP", "log_REC", "log_KOFGI", "FDI", "EPS", "R_squared")
colnames(result_matrix_EPS_models) = EPS_models
  for (i in (1:6)) {
    result_matrix_EPS_models[,i] = coeff_p_value_function_fixed(get(fixed_EPS_models[i]))
}
  for (i in (1:6)) {
    result_matrix_EPS_models[,i+6] = coeff_p_value_function_random(get(random_EPS_models[i]))
  }
######
#Raportowanie wyników modeli przelacznikowych standardowych#
######
result_matrix_przelacznik_models = matrix(0, nrow = 12, ncol = 4)
rownames(result_matrix_przelacznik_models) = c("constant", "log_GDP", "log_REC", "log_KOFGI", "FDI", "Tech", "Market", "Nonmarket", "Tech_2008", "Market_2008", "Non_market_2008", "R_squared")
colnames(result_matrix_przelacznik_models) = standard_przelacznik_models
  for (i in (1:2)) {
    result_matrix_przelacznik_models[,i] = coeff_p_value_function_fixed(get(fixed_przelacznik_standard_models[i]))
  }
  for (i in (1:2)) {
    result_matrix_przelacznik_models[,i+2] = coeff_p_value_function_random(get(random_przelacznik_standard_models[i]))
  }
######
#Raportowanie wyników modeli przelacznikowycg z calosciowym wskaznikiem EPS#
######
result_matrix_przelacznik_EPS_models = matrix(0, nrow = 8, ncol = 4)
rownames(result_matrix_przelacznik_EPS_models) = c("constant", "log_GDP", "log_REC", "log_KOFGI", "FDI", "EPS", "EPS_2008", "R_squared")
colnames(result_matrix_przelacznik_EPS_models) = EPS_przelacznik_models
  for (i in (1:2)) {
    result_matrix_przelacznik_EPS_models[,i] = coeff_p_value_function_fixed(get(fixed_przelacznik_EPS_models[i]))
  }
  for (i in (1:2)) {
    result_matrix_przelacznik_EPS_models[,i+2] = coeff_p_value_function_random(get(random_przelacznik_EPS_models[i]))
  }
#####
#Raportowanie wynikow modeli standardowych bez FDI
result_matrix_no_FDI_standard = matrix(0, nrow = 8, ncol = 6)
rownames(result_matrix_no_FDI_standard) = c("constant", "log_GDP", "log_REC", "log_KOFGI", "Tech", "Market", "Non_market", "R_squared")
colnames(result_matrix_no_FDI_standard) = random_standard_models_noFDI
  for(i in 1:6) {
    result_matrix_no_FDI_standard[,i] = coeff_p_value_function_random(get(random_standard_models_noFDI[i]))
  }
#Raportowanie wyników modeli EPS bez FDI
result_matrix_EPS_no_FDI_standard = matrix(0, nrow = 6, ncol = 6)
rownames(result_matrix_EPS_no_FDI_standard) = c("constant", "log_GDP", "log_REC", "log_KOFGI", "EPS", "R_squared")
colnames(result_matrix_EPS_no_FDI_standard) = random_EPS_models_noFDI
  for(i in 1:6) {
    result_matrix_EPS_no_FDI_standard[,i] = coeff_p_value_function_random(get(random_EPS_models_noFDI[i]))
  }
#Eksport modeli do csv
# write.csv(result_matrix_standard_models, "C:/Users/Dawid/Desktop/Licencjat/data\\standard_models.csv")
# write.csv(result_matrix_EPS_models, "C:/Users/Dawid/Desktop/Licencjat/data\\EPS_models.csv")
# write.csv(result_matrix_przelacznik_models, "C:/Users/Dawid/Desktop/Licencjat/data\\przelacznik_models.csv")
# write.csv(result_matrix_przelacznik_EPS_models, "C:/Users/Dawid/Desktop/Licencjat/data\\EPS_przelacznik_models.csv")
#write.csv(result_matrix_EPS_no_FDI_standard, "C:/Users/Dawid/Desktop/Licencjat/data\\EPS_models_noFDI.csv") 
#write.csv(result_matrix_no_FDI_standard, "C:/Users/Dawid/Desktop/Licencjat/data\\standard_models_noFDI.csv")

#Testy Hausmanna dla par modeli
log_08_20_CBA = phtest(log_fixed_08_20_CBA, log_random_08_20_CBA, method ="chisq")
log_08_20_PBA = phtest(log_fixed_08_20_PBA, log_random_08_20_PBA, method ="chisq")
log_95_08_CBA = phtest(log_fixed_95_08_CBA, log_random_95_08_CBA, method ="chisq")
log_95_08_PBA = phtest(log_fixed_95_08_PBA, log_random_95_08_PBA, method ="chisq") 
log_95_20_CBA = phtest(log_fixed_95_20_CBA, log_random_95_20_CBA, method ="chisq") 
log_95_20_PBA = phtest(log_fixed_95_20_PBA, log_random_95_20_PBA, method ="chisq") 
log_95_20_CBA_EPS = phtest(log_fixed_95_20_CBA_EPS, log_random_95_20_CBA_EPS, method ="chisq") 
log_95_20_PBA_EPS = phtest(log_fixed_95_20_PBA_EPS, log_random_95_20_PBA_EPS, method ="chisq")
log_95_08_CBA_EPS = phtest(log_fixed_95_08_CBA_EPS, log_random_95_08_CBA_EPS, method ="chisq")
log_95_08_PBA_EPS = phtest(log_fixed_95_08_PBA_EPS, log_random_95_08_PBA_EPS, method ="chisq")
log_08_20_CBA_EPS = phtest(log_fixed_08_20_CBA_EPS, log_random_08_20_CBA_EPS, method ="chisq")
log_08_20_PBA_EPS = phtest(log_fixed_08_20_PBA_EPS, log_random_08_20_PBA_EPS, method ="chisq")
log_95_20_CBA_EPS_prz = phtest(log_fixed_95_20_CBA_EPS_prz, log_random_95_20_CBA_EPS_prz, method ="chisq") 
log_95_20_PBA_EPS_prz = phtest(log_fixed_95_20_PBA_EPS_prz, log_random_95_20_PBA_EPS_prz, method ="chisq") 
log_95_20_CBA_prz = phtest(log_fixed_95_20_CBA_prz, log_random_95_20_CBA_prz, method ="chisq") 
log_95_20_PBA_prz = phtest(log_fixed_95_20_PBA_prz, log_random_95_20_PBA_prz, method ="chisq") 
tests = c("log_08_20_CBA", "log_08_20_PBA", "log_95_08_CBA", "log_95_08_PBA", "log_95_20_CBA", "log_95_20_PBA",
          "log_95_20_CBA_EPS", "log_95_20_PBA_EPS", "log_95_08_CBA_EPS", "log_95_08_PBA_EPS", "log_08_20_CBA_EPS", "log_08_20_PBA_EPS",
          "log_95_20_CBA_EPS_prz", "log_95_20_PBA_EPS_prz", "log_95_20_CBA_prz", "log_95_20_PBA_prz")

hausmann_result = matrix(0, nrow = 16, ncol = 3)
colnames(hausmann_result) = c("Model", "p-value", "Result")
for (i in (1:16)) {
  hausmann_result[i,1] = tests[i]
  hausmann_result[i,2] = get(tests[i])$p.value
  hausmann_result[i,3] = if (get(tests[i])$p.value >= 0.05){
                                paste("Random model")
                          } else {
                                paste("Fixed effects")
                            }
}
#Zapisujemy wyniki
#write.csv(hausmann_result, "C:/Users/Dawid/Desktop/Licencjat/data\\hausmann_test.csv")




