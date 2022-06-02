# =============================================================================/
# r-statistics-MATH_Inferenzstatistische_Auswertung_Hypothese_1.R
#
# Skript zur gewichteten inferenzstatistischen Auswertung der Hypothese 1
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
# Berechne Selbsteinschaetzung Kompetenz als Mittelwert von Q8.3_1 und Q8.4_1
df.H1.relevant <- data_full_withweights %>%
  dplyr::select(c(Partei, Q8.1, Q8.3_1, Q8.4_1, Q8.6_1, Q10.8_1, Q11.2, Q11.2_numeric, weight)) %>%
  dplyr::mutate(Selbsteinschaetzung_Kompetenz = (Q8.3_1 + Q8.4_1)/2) %>%
  dplyr::mutate(Q11.2 = factor(Q11.2, levels = c('Ja', 'Eher ja', 'Unentschlossen / Weiss nicht', 'Eher nein', 'Nein'))) 

# visualize ----
p.H1.correlation <- ggplot(df.H1.relevant, aes(x = Q11.2_numeric, y = Selbsteinschaetzung_Kompetenz, weight = weight))+
  labs(x = 'Stimmabsicht', y = 'Selbsteinschätzung\nKompetenz')+
  geom_violin(aes(group = Q11.2, fill = Q11.2, color = Q11.2), show.legend = FALSE)+
  geom_boxplot(aes(group = Q11.2), width = 0.1)+
  geom_smooth(method = 'lm', formula = y ~ x + I(x^2), color = '#cc0033', fill = '#cc0033', alpha = 0.1)+
  scale_y_continuous(breaks = c(5, 95), labels = c('Tief', 'Hoch'))+
  coord_cartesian(x = c(NA, 5.4), expand = FALSE)+
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

p.H1.correlation

# PNG Datei erstellen und oeffnen
png(paste0(fig.path, 'H1_correlation.png'),
    width     = 3000,
    height    = 1600,
    res = 300)

# Bild speichern
plot(p.H1.correlation)

# Device schliessen
dev.off()

# =============================================================================/
# END ----
# =============================================================================/

