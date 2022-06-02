# =============================================================================/
# r-statistics-MATH_Deskriptive_Visualisierung_Q8.1.R
#
# Skript zur gewichteten deskriptiven Visualisierung von Q8.1
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
library('matrixStats')
library('readxl')

# -----------------------------------------------------------------------------/
# CONSTANTS ----
# -----------------------------------------------------------------------------/

fig.path <- ''

# Funktion fuer mehrzeilige Titel
wrapper <- function(x, ...) 
{
  paste(strwrap(x, ...), collapse = "\n")
}

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
df_Qs <- data.frame(Frage = colnames(data_Qs),
                    Name = paste0(stringr::str_extract(colnames(data_Qs), "[^_]+"), ' ', data_Qs[1, ]))

# preprocess ----
# Berechne gewichtete Anzahl und ungewichtete Anzahl
df.weighted.median <- data_full_withweights %>%
  dplyr::group_by(Q8.1) %>%
  dplyr::summarize(Q8.1_count_weighted = sum(weight),
                   Q8.1_count_unweighted = n()) %>%
  dplyr::ungroup()

# Berechne nach Partei gewichtete Anzahl und ungewichtete Anzahl
df.weighted.median.partei <- data_full_withweights %>%
  dplyr::mutate(Partei = ifelse(Partei_numeric == 6, 'Keine Partei', Partei)) %>%
  dplyr::group_by(Partei_numeric, Partei, Q8.1) %>%
  dplyr::summarize(Q8.1_count_weighted = sum(weight),
                   Q8.1_count_unweighted = n()) %>%
  dplyr::ungroup()

# visualize ----
# Frage 8.1 einfach
p.Q8.1.simple <- ggplot(df.weighted.median ,
                 aes(x = Q8.1_count_weighted, y = reorder(Q8.1, Q8.1_count_weighted))) +
  labs(y = '', x= 'n')+
  ggtitle(wrapper(subset(df_Qs, Frage == 'Q8.1')$Name , width = 62))+
  geom_col(fill = '#5d8aa8', width = 0.6)+
  geom_text(aes(label= paste0(round(100*Q8.1_count_weighted/1730, 2), ' %'),
                hjust = -0.2),
            size = 6)+
  theme_bw(base_size = 16)+
  scale_x_continuous(breaks = seq(0, 20)* 250, expand = expansion(mult = c(0.01,0.17)))+
  theme(axis.line = element_line(color='black'),
        axis.text.y = element_text(colour="black", size = 16),
        axis.text.x = element_text(colour="black", size = 16),
        plot.background = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.title.x = element_text(size = 16),
        plot.title = element_text(size = 20),
        strip.text = element_text(size=16,colour="black"),
        strip.background = element_rect(colour="white", fill="white"))

# PNG datei erstellen und oeffnen
png(paste0(fig.path, 'Q8.1_gewichteterMittelwert.png'),
    width     = 3000,
    height    = 1400,
    res = 300)

# Bild speichern
plot(p.Q8.1.simple)

# Device schliessen
dev.off()

# Frage 8.1 nach Parteien mit Werten mit gewichtetem Mittelwert
p.Q8.1.text <- ggplot(df.weighted.median.partei ,
                 aes(x = Q8.1_count_weighted, y = reorder(Q8.1, Q8.1_count_weighted))) +
  labs(y = '', x= 'n')+
  ggtitle(wrapper(subset(df_Qs, Frage == 'Q8.1')$Name , width = 62))+
  geom_text(aes(label= round(Q8.1_count_weighted),
                hjust = -0.2),
            size = 5)+
  theme_bw(base_size = 16)+
  geom_col(fill = '#5d8aa8', width = 0.6)+
  facet_wrap(~Partei)+ 
  scale_x_continuous(breaks = seq(0, 20)*50, expand = expansion(mult = c(0.01,0.25)))+
  theme(axis.line = element_line(color='black'),
        axis.text.y = element_text(colour="black", size = 14),
        axis.text.x = element_text(colour="black", size = 14),
        plot.background = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.x = element_text(size = 16),
        plot.title = element_text(size = 20),
        strip.text = element_text(size=16,colour="black"),
        strip.background = element_rect(colour="white", fill="white"))

# PNG Datei erstellen und oeffnen
png(paste0(fig.path, 'Q8.1_gewichteterMittelwert_ParteimitWerten.png'),
    width     = 3000,
    height    = 2000,
    res = 300)

# Bild speichern
plot(p.Q8.1.text)

# Device schliessen
dev.off()

# =============================================================================/
# END ----
# =============================================================================/

