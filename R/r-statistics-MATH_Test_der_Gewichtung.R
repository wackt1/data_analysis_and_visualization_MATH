# =============================================================================/
# r-statistics-MATH_Berechnung_Test_der_Gewichtung.R
#
# Skript zur Berechnung der Guete der Gewichtung
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
library('tidyverse')

# -----------------------------------------------------------------------------/
# MAIN ----
# -----------------------------------------------------------------------------/

# read input ----
# Antwortdatensatz ohne Gewichte
data_full_preprocessed <- read.csv('Final_Preprocessed_Data_Master-Thesis_Blockchain-ID_MATH_2022_CSV.csv', sep = ';')

# Antwortdatensatz mit Gewichten
data_full_withweights <- read.csv('Final_Gewichtungsfaktoren_RIM_Data_Master-Thesis_Blockchain-ID_MATH_2022_CSV.csv', sep = ',')

# preprocess ----
# Extrahiere Q4.1 Antwortmoeglichkeiten
list_Q4.1_short <- data.frame(Q4.1 = c('Ich habe nicht abgestimmt',"Keine Antwort" , "Mit 'Ja', Gesetz angenommen",
                     "Mit 'Nein', Gesetz abgelehnt"),
                     Stimmen =c(NA, NA, 'Ja', 'Nein'))

# Berechnung Ist-Werte
data_full_preprocessed %>%
  dplyr::select(Q2.1, UserLanguage, Altersgruppe, Bildungsabschluss, Partei) %>%
  dplyr::count(Q2.1, UserLanguage, Altersgruppe, Bildungsabschluss, Partei)

# Ist-Wert Abstimmung berechnen
ist_Q4.1 <-  data_full_preprocessed %>%
    dplyr::left_join(list_Q4.1_short) %>%
    dplyr::filter(!is.na(Stimmen)) %>%
    dplyr::count(Stimmen) %>%
    dplyr::mutate(Istprozent = 100*n/sum(n)) 

# Sollwerte Abstimmung definieren
soll_Q4.1<- data.frame(Stimmen = c('Ja', 'Nein'),
                           Sollprozent = c(35.6, 64.4))

# Soll und Ist kombinieren
Q4.1_delta <- ist_Q4.1 %>%
  dplyr::left_join(soll_Q4.1) %>%
  dplyr::mutate(Delta = Sollprozent - Istprozent)

# Gebe latex-Tabelle aus
print(knitr::kable(Q4.1_delta , "latex"))

# Ist-Wert Abstimmung RIM gewichtet
ist_Q4.1_gewichtet <-  data_full_withweights %>%
  dplyr::left_join(list_Q4.1_short) %>%
  dplyr::filter(!is.na(Stimmen)) %>%
  dplyr::group_by(Q2.1, UserLanguage, Partei, weight) %>%
  dplyr::count(Stimmen) %>%
  dplyr::mutate(n_weighted = n*as.numeric(weight)) %>%
  dplyr::ungroup() %>%
  dplyr::group_by(Stimmen) %>%
  dplyr::summarise(n = sum(n), n_weighted = sum(n_weighted)) %>%
  dplyr::mutate(Istprozent = 100*n/sum(n)) %>%
  dplyr::mutate(Istprozent_weighted = 100*n_weighted/sum(n_weighted))

# Gebe Tabelle aus
ist_Q4.1_gewichtet

# =============================================================================/
# END
# =============================================================================/

