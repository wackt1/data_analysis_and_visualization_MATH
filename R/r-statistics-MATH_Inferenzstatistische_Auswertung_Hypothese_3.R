# =============================================================================/
# r-statistics-MATH_Inferenzstatistische_Auswertung_Hypothese_3.R
#
# Skript zur gewichteten inferenzstatistischen Auswertung der Hypothese 3
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
library('readxl')

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

# PLZ-Datensatz der Schweiz ----
data_metadata_PLZ <- read_excel('Postleitzahlen-Schweiz.xlsx')

# Fasse PLZ-Metadatensatz zusammen
data_PLZ <- data_metadata_PLZ %>%
  dplyr::rename(PLZ = "Postleitzahl / Code Postal / Codice Postale") %>%
  dplyr::select(PLZ, Kanton) %>%
  dplyr::distinct() %>%
  # Entferne doppelte PLZ aus Metadatensatz
  dplyr::filter(!(PLZ == 8212 & Kanton == 'Zürich')) %>%
  dplyr::group_by(PLZ) %>%
  dplyr::mutate(ndouble = n()) %>%
  dplyr::ungroup() %>%
  dplyr::filter(ndouble != 2) 

# Ergaenze Datensatz mit PLZ und gruppiere nach SH + Zug oder restliche Kantone
data_full_ZGSH <- data_full_withweights %>%
  dplyr::rename(PLZ = Q2.3) %>%
  dplyr::left_join(data_PLZ) %>%
  dplyr::filter(!is.na(Kanton)) %>%
  dplyr::mutate(IstZGoderSH = ifelse(Kanton %in% c("Zug", "Schaffhausen"), 'ZGundSH', 'RestlicheKantone')) %>%
  dplyr::mutate(IstZGoderSH_numeric = ifelse(IstZGoderSH == 'ZGundSH', 1, 2))

# 9 Datensätze mit ungültigen/doppelten PLZ wurden entfernt

# preprocess ----
# Entferne nicht benötigte Kolonnen und faktorisiere Antworten Q11.2
df.H3.relevant <- data_full_ZGSH %>%
  dplyr::select(c(Q11.2, Q11.2_numeric, weight, IstZGoderSH, IstZGoderSH_numeric, Kanton)) %>%
  dplyr::mutate(Q11.2 = factor(Q11.2, levels = c('Ja', 'Eher ja', 'Unentschlossen / Weiss nicht', 'Eher nein', 'Nein'))) 

# visualize ----
p.H3.correlation <- ggplot(df.H3.relevant %>%
                             dplyr::mutate(IstZGoderSH = ifelse(IstZGoderSH == 'ZGundSH', 'ZG & SH',
                                                                'Restliche\nKantone')), 
                           aes(x = Q11.2_numeric, y = IstZGoderSH, weight = weight))+
  labs(x = 'Stimmabsicht', y = '')+
  geom_violin(show.legend = FALSE, fill = '#BCBCBC', col = '#BCBCBC')+
  geom_boxplot(width = 0.07)+
  theme_bw(base_size = 20)+
  scale_x_continuous(breaks = seq(5), labels = c('Nein', 'Eher nein', 'Unentschlossen', 'Eher ja', 'Ja'))+
  theme(axis.line = element_line(color='black'),
        plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.text.x = element_text(colour="black", size = 16),
        axis.text.y = element_text(colour="black", size = 16),
        axis.title.x = element_text(size = 18),
        axis.title.y = element_text(size = 18 ),
        plot.title = element_text(size = 20, hjust = 0.5),
        plot.margin = margin(0.2, 0.2, 0.2, -0.5, "cm"))

p.H3.correlation

# PNG Datei erstellen und oeffnen
png(paste0(fig.path, 'H3_correlation.png'),
    width     = 3000,
    height    = 1600,
    res = 300)

# Bild speichern
plot(p.H3.correlation)

# Device schliessen
dev.off()

# =============================================================================/
# END ----
# =============================================================================/

