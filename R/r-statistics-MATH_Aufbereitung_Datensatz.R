# =============================================================================/
# r-statistics-MATH_Aufbereitung_Datensatz.R
#
# Skript zur Aufbereitung und Addition von Metadaten an grundbereinigten Datensatz
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

# -----------------------------------------------------------------------------/
# MAIN ----
# -----------------------------------------------------------------------------/

# read input ----
# Antwortdatensatz ohne Gewichte
data_full <- read_excel('Final_Data_Master-Thesis_Blockchain-ID_MATH_2022_Excel_Full.xlsx') %>%
  filter(!row_number() %in% c(1))

# Metadatensaetze
data_metadata_geschlecht_CH <- read_excel('www.bfs.admin.ch_asset_de_su-d-01.05.03.01.01.xlsx')
data_metadata_alter_CH <- read_excel('www.bfs.admin.ch_asset_de_je-d-01.02.03.02.xlsx')
data_metadata_bildung_CH <- read_excel('www.bfs.admin.ch_asset_de_su-d-01.05.07.03.01.01.xlsx')
data_metadata_parteien_NR19 <- read_excel('www.bfs.admin.ch_asset_de_je-d-17.02.02.03.01.xlsx')

# process ----
# Mapping fuer Bildungsbaskets zu Q2.5_numeric definieren
bildung_mapping <- data.frame(Q2.5 = c("Universität, ETH", "Fachhochschule, PH",
                                               "Höhere Fach- und Berufsausbildung (HF, HFP, HTL, etc.)",
                                               "Berufliche Grundausbildung (Berufslehre, Berufsschule, Handelsschule, etc.)",
                                               "Anderer Abschluss (Textfeld):", 
                                               "Allgemeinbildende Schulen (Gymnasiale Maturität, Maturitätsschule, Berufsmatura, FMS, DMS, etc.)", 
                                               "Keine Antwort", "Obligatorische Schule (Primarschule, Sekundarschule)", "(Noch) kein Abschluss"),
                              Bildungsabschluss = c('Tertiärstufe', 'Tertiärstufe', 'Tertiärstufe', 
                                                    'Sekundarstufe II', 'Andere', 'Sekundarstufe II', 'Keine Antwort',
                                                    'Obligatorische Schule', 'Andere')) 

# Mapping zu numerischer Ordinalskala von Altersgruppe und Q11.2
Altersgruppe.mapping <- data.frame(Altersgruppe = c('18-39 Jahre', '40-64 Jahre', '65+ Jahre'),
                                   Altersgruppe_numeric = c(1, 2, 3))

# Mapping fuer Frage Q11.2 (Ordinalskala)
Q11.2.mapping <- data.frame(Q11.2 = c('Ja', 'Eher ja', "Unentschlossen / Weiss nicht", 'Eher nein', 'Nein'),
                            Q11.2_numeric = c(5, 4, 3, 2, 1))

# Baskets generieren 
data_full <- data_full %>%
  
  # Baskets fuer Alter
  dplyr::mutate(Alter = 2022 - as.numeric(Q2.2)) %>%
  dplyr::mutate(Altersgruppe = ifelse(Alter %in% seq(18, 39), '18-39 Jahre',
                                      ifelse(Alter %in% seq(40, 64), '40-64 Jahre',
                                             '65+ Jahre'))) %>%
  
  # Baskets fuer Bildung
  dplyr::left_join(bildung_mapping) %>%
  
  # Baskets fuer Parteien
  dplyr::mutate(Partei = ifelse(Q3.1 %in% c('Keine Partei', 'glp', 'FDP', 'Die Mitte', 'SVP', 'SP', 'Grüne'),
                                Q3.1,
                                'Andere'))

# Numerische Skala für ordinalskalierte und nominalskalierte Variablen
data_full <- data_full %>%
  # Fuege numerische Altersgruppe an als Kolonne
  dplyr::left_join(Altersgruppe.mapping) %>%
  # Fuege numerische Q11.2 an als Kolonne
  dplyr::left_join(Q11.2.mapping) %>%
  # Fuege fuer nominale Variablen arbitraere Zahlenwerte basierend auf Fakorlevel zu
  dplyr::mutate(Q2.1_numeric = as.numeric(as.factor(Q2.1)),
                UserLanguage_numeric = as.numeric(as.factor(UserLanguage)),
                Bildungsabschluss_numeric = as.numeric(as.factor(Bildungsabschluss)),
                Partei_numeric = as.numeric(as.factor(Partei)),
                Q4.1_numeric = as.numeric(as.factor(Q4.1)),
                Q8.1_numeric = as.numeric(as.factor(Q8.1)),
                Q9.1_numeric = as.numeric(as.factor(Q9.1)))

# save ----
write.csv2(data_full, file = 'Final_Preprocessed_Data_Master-Thesis_Blockchain-ID_MATH_2022_CSV.csv',
           row.names = FALSE)

write.csv(data_full, file = 'Final_Preprocessed_Data_Master-Thesis_Blockchain-ID_MATH_2022_CSV_Komma.csv',
           row.names = FALSE)

# =============================================================================/
# END ----
# =============================================================================/

