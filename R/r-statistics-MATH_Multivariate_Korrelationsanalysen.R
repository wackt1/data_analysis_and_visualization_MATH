# =============================================================================/
# r-statistics-MATH_Multivariate_Korrelationsanalysen.R
#
# Skript zur Korrelationsanalyse
# 
# R-Script - Master Thesis - Blockchain-ID
#
# Author: wackt1.bfh@gmail.com
# Date: 12.06.2022
# =============================================================================/

# -----------------------------------------------------------------------------/
# IMPORT PACKAGES ----
# -----------------------------------------------------------------------------/
library('tidyverse')
library('ggpmisc')
library('kableExtra')
library('wCorr')
library('weights')
library('DescTools')

# -----------------------------------------------------------------------------/
# FUNCTIONS ----
# -----------------------------------------------------------------------------/

# Funktion um Kontingenzkoeffizient und p-Werte zu berechnen
corrFunc_contingency <- function(var1, var2, data) {
  chi <- chisq.test(data[,var1], data[,var2])
  CC <- DescTools::ContCoef(data[,var1], data[,var2])
  
  # Output
  data.frame(var1, var2, p.value = chi$p.value, CC = CC, 
             stringsAsFactors=FALSE)
}

# Gewichtet - Funktion um Kontingenzkoeffizient und p-Werte zu berechnen
corrFunc_contingency_weighted <- function(var1, var2, weight, data) {
  # Gewichtete Chi-squared Funktion
  chi <- weights::wtd.chi.sq(var1 = data[,var1], var2 = data[,var2], weight = data[,weight], mean1=FALSE)
  
  CC.unweighted <- unname(sqrt(chi['Chisq'] / (chi['Chisq'] + length(data[,var1]))))
  CC.weighted <- unname(sqrt(chi['Chisq'] / (chi['Chisq']+ sum(data[,weight]))))
  
  # Output
  data.frame(var1, var2, n = length(data[,var1]), chisq = chi['Chisq'], p.value = chi['p.value'], CC.w = CC.weighted, CC.unw = CC.unweighted,
             stringsAsFactors=FALSE)
}

# Funktion um Spearman Korrelationskoeffizient und p-Werte zu berechnen
corrFunc_spearman <- function(var1, var2, data) {
  result = cor.test(data[,var1], data[,var2], method = 'spearman')
  
  # Output
  data.frame(var1, var2, result[c("estimate","p.value")], 
             stringsAsFactors=FALSE)
}

# -----------------------------------------------------------------------------/
# VARIABLES ----
# -----------------------------------------------------------------------------/
col.order.spearman <- c('Fragestellung',
                        'Geschlecht Rs',
                        'Geschlecht p-Wert',
                        'Sprache Rs',
                        'Sprache p-Wert',
                        'Alter Rs',
                        'Alter p-Wert',
                        'Bildung Rs',
                        'Bildung p-Wert',
                        'Partei Rs',
                        'Partei p-Wert')

col.order.pearson <- c('Fragestellung',
                        'Geschlecht CC',
                        'Geschlecht p-Wert',
                        'Sprache CC',
                        'Sprache p-Wert',
                        'Alter CC',
                        'Alter p-Wert',
                        'Bildung CC',
                        'Bildung p-Wert',
                        'Partei CC',
                        'Partei p-Wert')

# -----------------------------------------------------------------------------/
# MAIN ----
# -----------------------------------------------------------------------------/

# --- read input ----
# Antwortdatensatz ohne Gewichte
data_full_preprocessed <- read.csv('Final_Preprocessed_Data_Master-Thesis_Blockchain-ID_MATH_2022_CSV.csv', sep = ';')

# Antwortdatensatz mit Gewichten
data_full_withweights <- read.csv('Final_Gewichtungsfaktoren_RIM_Data_Master-Thesis_Blockchain-ID_MATH_2022_CSV.csv', sep = ',')
 

# --- preprocess unweighted ----
# Entferne nicht benötigte Kolonnen und benenne numerische Kolonnen um
data_Qs_numeric <- data_full_preprocessed %>%
  select_if(is.numeric) %>%
  dplyr::select(-c('StartDate', 'EndDate', 'Duration..in.seconds.', 'Q2.2', 'Q2.3')) %>%
  dplyr::rename(Q11.2_1 = Q11.2_numeric)%>%
  dplyr::rename(Q4.1 = Q4.1_numeric,
                Q8.1 = Q8.1_numeric,
                Q9.1 = Q9.1_numeric)

# Waehle ordinale Datenkolonnen und erstelle neuen Datensatz
Qs_ordinal <- data_Qs_numeric %>% 
  dplyr::select(c(Q5.2_1, Q5.3_1, Q5.4_1, Q5.5_1, Q6.2_1, Q6.3_1, Q6.4_1, Q7.2_1, 
                  Q7.2_2, Q7.2_3, Q7.2_4, Q7.3_1, Q7.4_1, Q8.3_1,
                  Q8.4_1, Q8.5_1, Q8.6_1, Q8.7_1, Q8.8_1, Q9.3_1, Q9.4_1, Q9.5_1,
                  Q10.2_1, Q10.5_1, Q10.6_1,
                  Q10.7_1, Q10.8_1, Q11.2_1, Q11.3_1))

# Waehle nominale Datenkolonnen und erstelle neuen Datensatz
Qs_nominal <- data_Qs_numeric %>% 
  dplyr::select(c(Q4.1, Q8.1, Q9.1)) 

# Waehle Datenkolonnen mit unabhaengigen Variablen und erstelle neuen Datensatz
Qs_unabh_variablen <- data_Qs_numeric %>% 
  dplyr::select(c(Q2.1_numeric,
                  UserLanguage_numeric, 
                  Bildungsabschluss_numeric, 
                  Altersgruppe_numeric,
                  Partei_numeric))

# --- preprocess weighted ----
# Entferne nicht benötigte Kolonnen, benenne numerische Kolonnen um und behalte Gewichtungsfaktoren
data_Qs_numeric_w <- data_full_withweights %>%
  select_if(is.numeric) %>%
  dplyr::select(-c('StartDate', 'EndDate', 'Duration..in.seconds.', 'Q2.2', 'Q2.3')) %>%
  dplyr::rename(Q11.2_1 = Q11.2_numeric)%>%
  dplyr::rename(Q4.1 = Q4.1_numeric,
                Q8.1 = Q8.1_numeric,
                Q9.1 = Q9.1_numeric)

# Waehle ordinale Datenkolonnen und erstelle neuen Datensatz
Qs_ordinal_w <- data_Qs_numeric_w %>% 
  dplyr::select(c(Q5.2_1, Q5.3_1, Q5.4_1, Q5.5_1, Q6.2_1, Q6.3_1, Q6.4_1, Q7.2_1, 
                  Q7.2_2, Q7.2_3, Q7.2_4, Q7.3_1, Q7.4_1, Q8.3_1,
                  Q8.4_1, Q8.5_1, Q8.6_1, Q8.7_1, Q8.8_1, Q9.3_1, Q9.4_1, Q9.5_1,
                  Q10.2_1, Q10.5_1, Q10.6_1,
                  Q10.7_1, Q10.8_1, Q11.2_1, Q11.3_1))

# Waehle nominale Datenkolonnen und erstelle neuen Datensatz
Qs_nominal_w <- data_Qs_numeric_w %>% 
  dplyr::select(c(Q4.1, Q8.1, Q9.1)) 

# Waehle Datenkolonnen mit unabhaengigen Variablen und erstelle neuen Datensatz
Qs_unabh_variablen_w <- data_Qs_numeric_w %>% 
  dplyr::select(c(Q2.1_numeric,
                  UserLanguage_numeric, 
                  Bildungsabschluss_numeric, 
                  Altersgruppe_numeric,
                  Partei_numeric))

# --- analyze unweighted ----
# Definiere Variablen-Paare für welche Korrelationen berechnet werden sollen
vars_ordinal = data.frame(v1=names(Qs_ordinal), v2='Q2.1_numeric') %>%
  dplyr::bind_rows(data.frame(v1=names(Qs_ordinal), v2='Altersgruppe_numeric'))%>%
  dplyr::bind_rows(data.frame(v1=names(Qs_ordinal), v2='UserLanguage_numeric'))%>%
  dplyr::bind_rows(data.frame(v1=names(Qs_ordinal), v2='Bildungsabschluss_numeric'))%>%
  dplyr::bind_rows(data.frame(v1=names(Qs_ordinal), v2='Partei_numeric'))

vars_nominal = data.frame(v1=names(Qs_nominal), v2='Q2.1_numeric') %>%
  dplyr::bind_rows(data.frame(v1=names(Qs_nominal), v2='Altersgruppe_numeric'))%>%
  dplyr::bind_rows(data.frame(v1=names(Qs_nominal), v2='UserLanguage_numeric'))%>%
  dplyr::bind_rows(data.frame(v1=names(Qs_nominal), v2='Bildungsabschluss_numeric'))%>%
  dplyr::bind_rows(data.frame(v1=names(Qs_nominal), v2='Partei_numeric'))

# Wende die Korrelationsfunktion auf saemtliche Variablenreihen an
corrs_contingency = do.call(rbind, mapply(corrFunc_contingency, vars_nominal[,1], vars_nominal[,2],
                                          MoreArgs=list(data=data_Qs_numeric), 
                                          SIMPLIFY=FALSE))

corrs_spearman = do.call(rbind, mapply(corrFunc_spearman, vars_ordinal[,1], vars_ordinal[,2],
                              MoreArgs=list(data=data_Qs_numeric), 
                              SIMPLIFY=FALSE))

# Fuege Resultate zu Tabelle zusammen - Kontingenz
Korr_table_contingency <- corrs_contingency %>%
  tidyr::pivot_wider(names_from = var2, values_from = c(CC, p.value)) %>%
  dplyr::rename('Fragestellung' = var1,
                'Geschlecht CC' = "CC_Q2.1_numeric",
                'Geschlecht p-Wert' = "p.value_Q2.1_numeric",
                'Sprache CC' = "CC_UserLanguage_numeric",
                'Sprache p-Wert' = "p.value_UserLanguage_numeric" ,
                'Alter CC' = "CC_Altersgruppe_numeric" ,
                'Alter p-Wert' = "p.value_Altersgruppe_numeric" ,
                'Bildung CC' = "CC_Bildungsabschluss_numeric",
                'Bildung p-Wert' = "p.value_Bildungsabschluss_numeric",
                'Partei CC' = "CC_Partei_numeric",
                'Partei p-Wert' = "p.value_Partei_numeric" )%>%
  dplyr::select(col.order.pearson) %>%
  mutate(across(where(is.numeric), round, 3))

# Fuege Resultate zu Tabelle zusammen - Spearman
Korr_table_spearman <- corrs_spearman %>%
  tidyr::pivot_wider(names_from = var2, values_from = c(estimate, p.value)) %>%
  dplyr::rename('Fragestellung' = var1,
                'Geschlecht Rs' = "estimate_Q2.1_numeric",
                'Geschlecht p-Wert' = "p.value_Q2.1_numeric",
                'Sprache Rs' = "estimate_UserLanguage_numeric",
                'Sprache p-Wert' = "p.value_UserLanguage_numeric" ,
                'Alter Rs' = "estimate_Altersgruppe_numeric" ,
                'Alter p-Wert' = "p.value_Altersgruppe_numeric" ,
                'Bildung Rs' = "estimate_Bildungsabschluss_numeric",
                'Bildung p-Wert' = "p.value_Bildungsabschluss_numeric",
                'Partei Rs' = "estimate_Partei_numeric",
                'Partei p-Wert' = "p.value_Partei_numeric" )%>%
  dplyr::select(col.order.spearman) %>%
  mutate(across(where(is.numeric), round, 3))

# Gebe latex-Tabelle aus
print(knitr::kable(Korr_table_contingency, "latex"))

# Gebe latex-Tabelle aus
print(knitr::kable(Korr_table_spearman, "latex"))

# --- analyze weighted ----
# Definiere Variablen-Paare für welche Korrelationen berechnet werden sollen
vars_ordinal_w = data.frame(v1=names(Qs_ordinal_w), v2='Q2.1_numeric') %>%
  dplyr::bind_rows(data.frame(v1=names(Qs_ordinal_w), v2='Altersgruppe_numeric'))%>%
  dplyr::bind_rows(data.frame(v1=names(Qs_ordinal_w), v2='UserLanguage_numeric'))%>%
  dplyr::bind_rows(data.frame(v1=names(Qs_ordinal_w), v2='Bildungsabschluss_numeric'))%>%
  dplyr::bind_rows(data.frame(v1=names(Qs_ordinal_w), v2='Partei_numeric'))

vars_nominal_w = data.frame(v1=names(Qs_nominal_w), v2='Q2.1_numeric') %>%
  dplyr::bind_rows(data.frame(v1=names(Qs_nominal_w), v2='Altersgruppe_numeric'))%>%
  dplyr::bind_rows(data.frame(v1=names(Qs_nominal_w), v2='UserLanguage_numeric'))%>%
  dplyr::bind_rows(data.frame(v1=names(Qs_nominal_w), v2='Bildungsabschluss_numeric'))%>%
  dplyr::bind_rows(data.frame(v1=names(Qs_nominal_w), v2='Partei_numeric'))

# Wende die Korrelationsfunktion auf saemtliche Variablenreihen an
corrs_contingency_w = do.call(rbind,
                              mapply(corrFunc_contingency_weighted , # Funktion die verwendet wird
                                     vars_nominal_w[,1], # Erste Variable (Element aus Liste)
                                     vars_nominal_w[,2], # Zweite Variable (Element aus Liste)
                                     MoreArgs=list(data=data_Qs_numeric_w, weight = 'weight'),  # Argumente in Funktion die immer gleich sind
                                     SIMPLIFY=FALSE)) # Behalte Originalformat des Funktionsoutputs bei

# Gewichteter Spearman wurde aufgrund fehlender p-value-Funktion in SPSS berechnet und manuell eingefuegt

# Fuege Resultate zu Tabelle zusammen
Korr_table_contingency_w <- corrs_contingency_w %>%
  dplyr::select(-n, -n, -chisq, -CC.unw) %>%
  tidyr::pivot_wider(names_from = var2, values_from = c(CC.w, p.value)) %>%
  dplyr::rename('Fragestellung' = var1,
                'Geschlecht Rs' = "CC.w_Q2.1_numeric",
                'Geschlecht p-Wert' = "p.value_Q2.1_numeric",
                'Sprache Rs' = "CC.w_UserLanguage_numeric",
                'Sprache p-Wert' = "p.value_UserLanguage_numeric" ,
                'Alter Rs' = "CC.w_Altersgruppe_numeric" ,
                'Alter p-Wert' = "p.value_Altersgruppe_numeric" ,
                'Bildung Rs' = "CC.w_Bildungsabschluss_numeric",
                'Bildung p-Wert' = "p.value_Bildungsabschluss_numeric",
                'Partei Rs' = "CC.w_Partei_numeric",
                'Partei p-Wert' = "p.value_Partei_numeric" )%>%
  dplyr::select(col.order.spearman) %>%
  mutate(across(where(is.numeric), round, 3))

# Gebe latex-Tabelle aus
print(knitr::kable(Korr_table_contingency_w, "latex"))

# =============================================================================/
# END ----
# =============================================================================/

