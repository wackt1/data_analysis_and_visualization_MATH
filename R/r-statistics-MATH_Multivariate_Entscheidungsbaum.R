# =============================================================================/
# r-statistics-MATH_Multivariate_Entscheidungsbaum.R
#
# Skript für den Entscheidungsbaum
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
library('ggplot2')
library('rpart')
library('rpart.plot')

# -----------------------------------------------------------------------------/
# CONSTANTS ----
# -----------------------------------------------------------------------------/

fig.path <- ''

# -----------------------------------------------------------------------------/
# MAIN ----
# -----------------------------------------------------------------------------/

# read input ----
# Antwortdatensatz mit Gewichten
data_full_withweights <- read.csv('Final_Gewichtungsfaktoren_RIM_Data_Master-Thesis_Blockchain-ID_MATH_2022_CSV.csv', sep = ',')
 
# preprocess ----
# Faktorisiere Antworten Q11.2
data_full_withweights <- data_full_withweights %>%
  dplyr::mutate(Q11.2 = factor(Q11.2, levels = c('Ja', 'Eher ja', 'Unentschlossen / Weiss nicht', 'Eher nein', 'Nein'))) 

# Codiere numerische Werte um, damit durch Ausprobieren gefundene Naeherungen in Groesser-/Kleiner-Entscheidung effizienter dargestellt werden können
df.num.partei <- data.frame(Partei_numeric_neu = seq(8),
                            Partei_numeric = c(1, 6, 5, 8,  2, 3, 7, 4))

# Errechne Durchschnittswerte der zusammengefassten Indizes in Form von Fragegruppen (Cluster) und fuege diese Datensatz hinzu
data_full_withweights <- data_full_withweights %>%
  dplyr::mutate(ClusterQ8_10 = (Q10.2_1 + Q8.8_1 + Q10.7_1 + Q10.6_1 + Q8.7_1 + Q8.6_1)/6,
                ClusterQ7 = (Q7.2_4 + Q7.2_3)/2,
                ClusterQ5 = (Q5.3_1 + Q5.4_1 + Q5.5_1)/3,
                ClusterQ8 = (Q8.4_1 + Q8.3_1)/2,
                ClusterQ6 = (Q6.4_1 + Q6.3_1 + Q6.2_1)/3,
                ClusterQ9 = (Q9.3_1 + Q9.4_1)/2) %>%
  dplyr::left_join(df.num.partei )

# Entferne nicht benötigte Kolonnen
data.clustered <- data_full_withweights %>%
  dplyr::select(c(ClusterQ5, ClusterQ6, ClusterQ7, ClusterQ8, ClusterQ8_10, ClusterQ9,
                  Q11.2_numeric, Q8.5_1, Q11.3_1, Q10.5_1, Q10.8_1, Q7.2_2, Q7.4_1, Q7.2_1, Q7.3_1,
                  Q5.2_1, Q8.1_numeric, Q9.5_1, Partei_numeric, Partei_numeric_neu, UserLanguage_numeric, Bildungsabschluss_numeric,
                  Q9.1_numeric, Altersgruppe_numeric, Q2.1_numeric, weight))

# decision tree ----
fit <- rpart::rpart(formula = Q11.2_numeric ~ Altersgruppe_numeric + Q2.1_numeric +Partei_numeric_neu + UserLanguage_numeric + Bildungsabschluss_numeric,
                     data = data.clustered, weights = weight, method = 'poisson',
                     control = rpart.control(cp = 0.0035))

rpart.plot(fit, type = 2, extra = 0 + 100)

# Visualisierung für Master-Thesis manuell in Bildbearbeitungsprogrammen weiter zur besseren Lesbarkeit/Verstaendlichkeit modifiziert

# =============================================================================/
# END ----
# =============================================================================/

