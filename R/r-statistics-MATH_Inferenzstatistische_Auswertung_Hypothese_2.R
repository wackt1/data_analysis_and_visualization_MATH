# =============================================================================/
# r-statistics-MATH_Inferenzstatistische_Auswertung_Hypothese_2.R
#
# Skript zur gewichteten inferenzstatistischen Auswertung der Hypothese 2
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

# String fuer Farben
col.string <- c('#5da8a1', '#8dc2bd', '#BCBCBC', '#8dadc2', '#5d8aa8')

# -----------------------------------------------------------------------------/
# MAIN ----
# -----------------------------------------------------------------------------/

# read input ----
# Antwortdatensatz mit Gewichten
data_full_withweights <- read.csv('Final_Gewichtungsfaktoren_RIM_Data_Master-Thesis_Blockchain-ID_MATH_2022_CSV.csv', sep = ',')

# Datensatz mit allen vollstaendigen Fragen
data_Qs <- read_excel('Final_Data_Master-Thesis_Blockchain-ID_MATH_2022_Excel_Full.xlsx') %>%
  filter(row_number() %in% c(1))

# Datensatz mit Q Bezeichungen und vollstaendigem Titel
df_Qs <- data.frame(  Frage = colnames(data_Qs),
                      Name = paste0(stringr::str_extract(colnames(data_Qs), "[^_]+"), ' ', data_Qs[1, ]))

# preprocess ----
# Berechne Wichtigkeitsindex als Mittelwert von Q7.2_1, Q7.2_2, Q7.2_3 und Q7.2_4
df.H2.relevant_1 <- data_full_withweights %>%
  dplyr::select(c(Partei, Q7.2_1, Q7.2_2, Q7.2_3, Q7.2_4, Q11.2, Q11.2_numeric, weight)) %>%
  dplyr::mutate(Wichtigkeitsindex = (Q7.2_1 + Q7.2_2 + Q7.2_3 + Q7.2_4)/4) %>%
  dplyr::mutate(Q11.2 = factor(Q11.2, levels = c('Ja', 'Eher ja', 'Unentschlossen / Weiss nicht', 'Eher nein', 'Nein')))

# Berechne Voraussetzungsbewusstsein als Mittelwert von Q7.3_1 und Q7.4_1
df.H2.relevant_2 <- data_full_withweights %>%
  dplyr::select(c(Partei, Q7.3_1, Q7.4_1, Q11.2, Q11.2_numeric, weight)) %>%
  dplyr::mutate(Voraussetzungsbewusstsein = (Q7.3_1 + Q7.4_1)/2) %>%
  dplyr::mutate(Q11.2 = factor(Q11.2, levels = c('Ja', 'Eher ja', 'Unentschlossen / Weiss nicht', 'Eher nein', 'Nein'))) 

# visualize ----
# Visualisierung Wichtigkeit
p.H2.correlation_1 <- ggplot(df.H2.relevant_1, aes(x = Q11.2_numeric, y = Wichtigkeitsindex, weight = weight))+
  labs(x = 'Stimmabsicht', y = 'Wichtigkeit')+
  geom_violin(aes(group = Q11.2, fill = Q11.2, color = Q11.2), show.legend = FALSE)+
  geom_boxplot(aes(group = Q11.2), width = 0.1)+
  geom_smooth(method = 'lm', formula = y ~ x + I(x^2), color = '#cc0033', fill = '#cc0033', alpha = 0.1)+
  scale_y_continuous(breaks = c(5, 95), labels = c('Tief', 'Hoch'))+
  coord_cartesian(x = c(NA, 5.5), expand = FALSE)+
  scale_x_continuous(breaks = seq(5), labels = c('Nein', 'Eher nein', 'Unentschlossen', 'Eher ja', 'Ja'))+
  scale_fill_manual(values = col.string)+
  scale_color_manual(values = col.string)+
  theme_bw(base_size = 20)+
  theme(axis.line = element_line(color='black'),
        plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.text.x = element_text(colour="black", size = 16),
        axis.text.y = element_text(colour="black", size = 16),
        axis.title.x = element_text(size = 18),
        axis.title.y = element_text(size = 18 , vjust = -6),
        plot.title = element_text(size = 20, hjust = 0.5),
        plot.margin = margin(0.2, 0.2, 0.2, -0.5, "cm"))

p.H2.correlation_1

# PNG Datei erstellen und oeffnen
png(paste0(fig.path, 'H2_correlation_1.png'),
    width     = 3000,
    height    = 1600,
    res = 300)

# Bild speichern
plot(p.H2.correlation_1)

# Device schliessen
dev.off()

# Visualisierung Voraussetzungsbewusstsein
p.H2.correlation_2 <- ggplot(df.H2.relevant_2, aes(x = Q11.2_numeric, y = Voraussetzungsbewusstsein, weight = weight))+
  labs(x = 'Stimmabsicht', y = 'Voraussetzungs-\nbewusstsein')+
  geom_violin(aes(group = Q11.2, fill = Q11.2, color = Q11.2), show.legend = FALSE)+
  geom_boxplot(aes(group = Q11.2), width = 0.1)+
  geom_smooth(method = 'lm', formula = y ~ x + I(x^2), color = '#cc0033', fill = '#cc0033', alpha = 0.1)+
  scale_y_continuous(breaks = c(5, 95), labels = c('Tief', 'Hoch'))+
  coord_cartesian(x = c(NA, 5.5), expand = FALSE)+
  scale_x_continuous(breaks = seq(5), labels = c('Nein', 'Eher nein', 'Unentschlossen', 'Eher ja', 'Ja'))+
  scale_fill_manual(values = col.string)+
  scale_color_manual(values = col.string)+
  theme_bw(base_size = 20)+
  theme(axis.line = element_line(color='black'),
        plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.text.x = element_text(colour="black", size = 16),
        axis.text.y = element_text(colour="black", size = 16),
        axis.title.x = element_text(size = 18),
        axis.title.y = element_text(size = 18 , vjust = -10),
        plot.title = element_text(size = 20, hjust = 0.5),
        plot.margin = margin(0.2, 0.2, 0.2, -1, "cm"))

p.H2.correlation_2

# PNG Datei erstellen und oeffnen
png(paste0(fig.path, 'H2_correlation_2.png'),
    width     = 3000,
    height    = 1600,
    res = 300)

# Bild speichern
plot(p.H2.correlation_2)

# Device schliessen
dev.off()

# =============================================================================/
# END ----
# =============================================================================/

