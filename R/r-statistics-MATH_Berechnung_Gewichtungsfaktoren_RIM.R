# =============================================================================/
# r-statistics-MATH_Berechnung_Gewichtungsfaktoren_RIM.R
#
# Skript zur Berechnung der Gewichtungsfaktoren
# 
# R-Script - Master Thesis - Blockchain-ID
#
# Author: wackt1.bfh@gmail.com
# Date: 12.06.2022
# =============================================================================/

# -----------------------------------------------------------------------------/
# IMPORT PACKAGES ----
# -----------------------------------------------------------------------------/
library('readxl')
library('xlsx')
library('tidyverse') 
library('iterake')

# -----------------------------------------------------------------------------/
# MAIN ----
# -----------------------------------------------------------------------------/

# read input ----
# Antwortdatensatz ohne Gewichte
data_full_preprocessed <- read.csv('Final_Preprocessed_Data_Master-Thesis_Blockchain-ID_MATH_2022_CSV.csv', sep = ';')

# Metadatensaetze
data_metadata_geschlecht_CH <- read_excel('www.bfs.admin.ch_asset_de_su-d-01.05.03.01.01.xlsx')
data_metadata_alter_CH <- read_excel('www.bfs.admin.ch_asset_de_je-d-01.02.03.02.xlsx')
data_metadata_bildung_CH <- read_excel('www.bfs.admin.ch_asset_de_su-d-01.05.07.03.01.01.xlsx')
data_metadata_parteien_NR19 <- read_excel('www.bfs.admin.ch_asset_de_je-d-17.02.02.03.01.xlsx')

# preprocess ----
# Berechnung Ist-Werte

# Ist-Wert Geschlecht
ist_geschlecht <-  data_full_preprocessed %>%
  dplyr::rename('Geschlecht' = Q2.1) %>%
    dplyr::count(Geschlecht) %>%
    dplyr::filter(!Geschlecht %in% c('Divers', 'Keine Antwort')) %>%
    dplyr::mutate(Istprozent = 100*n/sum(n))
  
# Ist-Wert Sprache
ist_sprache <-  data_full_preprocessed %>%
    dplyr::count(UserLanguage) %>%
    dplyr::mutate(Istprozent = 100*n/sum(n))

# Ist-Wert Altersgruppen
ist_altersgruppe <-  data_full_preprocessed %>%
  dplyr::count(Altersgruppe) %>%
  dplyr::mutate(Istprozent = 100*n/sum(n))
  
# Ist-Wert Bildungsabschluss
ist_bildung <-  data_full_preprocessed %>%
  dplyr::filter(!Bildungsabschluss %in% c('Andere', 'Keine Antwort')) %>%
    dplyr::count(Bildungsabschluss) %>%
    dplyr::mutate(Istprozent = 100*n/sum(n))

# Ist-Wert Partei
ist_partei <-  data_full_preprocessed %>%
  dplyr::count(Partei) %>%
  dplyr::mutate(Istprozent = 100*n/sum(n))
  

# Berechnung Sollwerte Geschlecht, Altersgruppe, Bildungsabschluss

# Bereite Externe Quelle auf
df_metadata_raw <- data_metadata_geschlecht_CH %>%
  dplyr::select(1, 4) %>%
  dplyr::rename('Kategorie' = 1,
                'SollSchweizer' = 2) %>%
  dplyr::mutate(SollSchweizer = as.numeric(SollSchweizer))

# Sollwerte Geschlecht
soll_geschlecht <- df_metadata_raw %>%
  dplyr::filter(Kategorie %in% c('Männer', 'Frauen')) %>%
  dplyr::mutate(Geschlecht = ifelse(Kategorie == 'Männer', 'Männlich', 'Weiblich')) %>%
  dplyr::mutate(Sollprozent = 100*SollSchweizer/sum(SollSchweizer))

# Sollwerte Altersgruppe
soll_altersgruppe <- df_metadata_raw %>%
  dplyr::filter(Kategorie %in% c('18-24 Jahre', '25-39 Jahre','40-54 Jahre', '55-64 Jahre', '65+ Jahre')) %>%
  dplyr::mutate(Altersgruppe = ifelse(Kategorie %in% c('18-24 Jahre', '25-39 Jahre'), '18-39 Jahre',
                                      ifelse(Kategorie == '65+ Jahre', Kategorie,
                                             '40-64 Jahre'))) %>%
  dplyr::group_by(Altersgruppe) %>%
  dplyr::summarise(SollSchweizer = sum(SollSchweizer)) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(Sollprozent = 100*SollSchweizer/sum(SollSchweizer))

# Sollwerte Bildungsabschluss
soll_bildungsabschluss <- df_metadata_raw %>%
  dplyr::filter(Kategorie %in% c('Obligatorische Schule',
                                 'Sekundarstufe II',
                                 'Tertiärstufe')) %>%
  dplyr::rename(Bildungsabschluss = Kategorie) %>%
  dplyr::mutate(Sollprozent = 100*SollSchweizer/sum(SollSchweizer))

# Berechnung Sollwerte Sprache
soll_sprache <- data.frame(UserLanguage = c('DE', 'FR'),
                           Sollprozent = c(75.13, 24.87))

# Berechnung Sollwerte Partei
soll_partei <- data_metadata_parteien_NR19 %>%
  filter(!row_number() %in% c(1, 26)) %>%
  dplyr::rename('Wahlanteil2019' = ...28,
                'Partei' = 1) %>%
  dplyr::select(Partei, Wahlanteil2019) %>%
  dplyr::filter(Wahlanteil2019 != '*') %>%
  # Fuer individuelle Partieangaben, fuege Gleiche zusammen
  dplyr::mutate(Partei = ifelse(Partei == 'FDP 3)', 'FDP', Partei),
                Partei = ifelse(Partei == 'GPS', 'Grüne', Partei),
                Partei = ifelse(Partei == 'CVP', 'Die Mitte', Partei),
                Partei = ifelse(Partei == 'GLP', 'glp', Partei)) %>%

  dplyr::mutate(Wahlanteil2019 = as.numeric(Wahlanteil2019)) %>%
  # Behalte nur Hauptparteien als Kategorien
  dplyr::mutate(Partei = ifelse(Partei %in% c('Keine Partei', 'glp', 'FDP', 'Die Mitte', 'SVP', 'SP', 'Grüne'),
                                Partei,
                                'Andere')) %>%
  dplyr::group_by(Partei) %>%
  dplyr::summarise(Sollprozent = sum(Wahlanteil2019)) %>%
  dplyr::ungroup()

# Ist und Soll fuer Sprachanteil
sprache_gewichtung <- ist_sprache %>%
  dplyr::left_join(soll_sprache) %>%
  dplyr::mutate(Gewicht = Sollprozent / Istprozent)

# Ist und Soll fuer Altersgruppe
altersgruppe_gewichtung <- ist_altersgruppe %>%
  dplyr::left_join(soll_altersgruppe) %>%
  dplyr::mutate(Gewicht = Sollprozent / Istprozent)

# Ist und Soll fuer Bildungsabschluss
bildungsabschluss_gewichtung <- ist_bildung%>%
  dplyr::left_join(soll_bildungsabschluss) %>%
  dplyr::mutate(Gewicht = Sollprozent / Istprozent)

# Ist und Soll fuer Geschlecht
geschlecht_gewichtung <- ist_geschlecht %>%
  dplyr::left_join(soll_geschlecht) %>%
  dplyr::mutate(Gewicht = Sollprozent / Istprozent)

# Ist und Soll fuer Partei
partei_gewichtung <- ist_partei %>%
  dplyr::left_join(soll_partei) %>%
  dplyr::mutate(Gewicht = Sollprozent / Istprozent)

# raking / RIM weighting factors ----

# Liste um nicht gewichtbare Antworten ('keine Antwort') nicht ausschliessen zu muessen 
liste.partei = c('Andere', 'Die Mitte', 'FDP', 'glp', 'Grüne', 'SP', 'SVP', 'SVP')
liste.geschlecht = c('Männlich', 'Weiblich')

# Ersetze Parteien die nicht gewichtbar sind:
# Annahme: Alle Personen verteilen sich entsprechend dem vorherrschenden politischen Spektrum
data_clean_party <- data_full_preprocessed %>%
  dplyr::filter(Partei == 'Keine Partei') %>%
  dplyr::mutate(Partei = liste.partei[row_number() %% 8 +1])

data_full_cleaned <- data_full_preprocessed  %>%
  dplyr::filter(Partei != 'Keine Partei') %>%
  dplyr::bind_rows(data_clean_party)

# Ersetze Geschlecht das nicht gewichtbar ist
# Annahme: Alle Personen verteilen sich entsprechend der vorherrschenden Geschlechtsverteilung
data_clean_sex <- data_full_cleaned %>%
  dplyr::filter(Q2.1 %in% c('Divers', 'Keine Antwort')) %>%
  dplyr::mutate(Q2.1 = liste.geschlecht[row_number() %% 2 +1])

data_full_cleaned <- data_full_cleaned  %>%
  dplyr::filter(!Q2.1 %in% c('Divers', 'Keine Antwort')) %>%
  dplyr::bind_rows(data_clean_sex)

# Berechne RIM Gewichtungsfaktoren mithilfe universe Objekt
uni <- universe(
  
  # Weise data Datensatz zu
  data = data_full_cleaned,
  
  # Definiere Gewichtungskategorien
  # Geschlechtskategorie
  category(
    name = "Q2.1",
    buckets = soll_geschlecht[['Geschlecht']],
    targets = soll_geschlecht[['Sollprozent']],
    sum.1 = TRUE # erzwingt dass Summe der buckets und targets 100 % ergibt
  ),
  
  # Sprachkategorie
  category(
    name = "UserLanguage",
    buckets = soll_sprache[['UserLanguage']],
    targets = soll_sprache[['Sollprozent']],
    sum.1 = TRUE # erzwingt dass Summe der buckets und targets 100 % ergibt
  ),
  
  # Parteikategorie
  category(
    name = "Partei",
    buckets = soll_partei[['Partei']],
    targets = soll_partei[['Sollprozent']],
    sum.1 = TRUE # erzwingt dass Summe der buckets und targets 100 % ergibt
  )
  
)

# Berechne Gewichtungsfaktoren mit iterake (Datensatz mit zusaetzlicher Kolonne weights)
gewichtungsfaktoren_RIM <- iterake(universe = uni, 
                  max.wgt = 3.3,
                  threshold = 1e-10, 
                  max.iter = 100000)

# Berechne Anzahl Datensaetze pro Gewichtungskategorie
n_abs_pro_strata <- data_full_cleaned %>%
  dplyr::select(Q2.1, UserLanguage,  Partei) %>%
  dplyr::count(Q2.1, UserLanguage, Partei)

# Extrahiere Gewichte
weights <- gewichtungsfaktoren_RIM %>% dplyr::distinct(Q2.1, UserLanguage, Partei, weight) %>%
  dplyr::arrange(desc(weight)) %>%
  dplyr::left_join(n_abs_pro_strata)

# Design Effekt (sollte < 1.5 sein)
anesrake::generaldesigneffect(weights$weight)

# Uebersicht Raking Ergebnisse
compare_margins(
  universe = uni, 
  data = gewichtungsfaktoren_RIM, 
  weight = weight, 
  plot = TRUE
)

# save ----
write.csv(gewichtungsfaktoren_RIM, file = 'Final_Gewichtungsfaktoren_RIM_Data_Master-Thesis_Blockchain-ID_MATH_2022_CSV.csv',
           row.names = FALSE)

# =============================================================================/
# END ----
# =============================================================================/

