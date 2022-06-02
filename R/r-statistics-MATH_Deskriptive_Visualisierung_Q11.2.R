# =============================================================================/
# r-statistics-MATH_Deskriptive_Visualisierung_Q11.2.R
#
# Skript zur gewichteten deskriptiven Visualisierung von Q11.2
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
library('MESS')

# -----------------------------------------------------------------------------/
# CONSTANTS ----
# -----------------------------------------------------------------------------/

fig.path <- ''

# Funktion fuer mehrzeilige Titel
wrapper <- function(x, ...) 
{
  paste(strwrap(x, ...), collapse = "\n")
}

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
# Berechne gewichtete Anzahl und ungewichtete Anzahl
df.weighted.median <- data_full_withweights %>%
  dplyr::group_by(Q11.2) %>%
  dplyr::summarize(Q11.2_count_weighted = sum(weight),
                   Q11.2_count_unweighted = n()) %>%
  dplyr::ungroup()%>%
  # Kategorie-Bezeichnungen anpassen
  dplyr::mutate(Q11.2 = ifelse(Q11.2 == 'Unentschlossen / Weiss nicht', 'Unentschlossen', Q11.2),
                Q11.2 = factor(Q11.2, levels = c( 'Ja', 'Eher ja', 'Unentschlossen', 'Eher nein', 'Nein')))

# Berechne nach Geschlecht gewichtete Anzahl und ungewichtete Anzahl
df.weighted.median.geschlecht <- data_full_withweights %>%
  dplyr::mutate(Q2.1 = ifelse(Q2.1_numeric %in% c(1, 2), 'Übrige', Q2.1)) %>%
  dplyr::group_by(Q2.1) %>%
  dplyr::mutate(n = sum(weight)) %>%
  dplyr::group_by(Q2.1_numeric, Q2.1, Q11.2, n) %>%
  dplyr::summarize(Q11.2_count_weighted = sum(weight),
                   Q11.2_count_unweighted = n()) %>%
  dplyr::ungroup() %>%
  # Berechne gerundete Prozente um exakt 100 zu erhalten
  dplyr::group_by(Q2.1) %>%
  dplyr::mutate(label = MESS::round_percent(Q11.2_count_weighted/n)) %>%
  dplyr::ungroup() %>%
  # Kategorie-Bezeichnungen anpassen
  dplyr::mutate(Q11.2 = ifelse(Q11.2 == 'Unentschlossen / Weiss nicht', 'Unentschlossen', Q11.2),
                Q11.2 = factor(Q11.2, levels = c( 'Ja', 'Eher ja', 'Unentschlossen', 'Eher nein', 'Nein')),
                Q2.1 = factor(Q2.1, levels = c('Weiblich', 'Männlich', 'Übrige')))

# Berechne nach Sprache gewichtete Anzahl und ungewichtete Anzahl
df.weighted.median.sprache <- data_full_withweights %>%
  dplyr::group_by(UserLanguage) %>%
  dplyr::mutate(n = sum(weight)) %>%
  dplyr::group_by(UserLanguage_numeric, UserLanguage, Q11.2, n) %>%
  dplyr::summarize(Q11.2_count_weighted = sum(weight),
                   Q11.2_count_unweighted = n()) %>%
  dplyr::ungroup()%>%
  # Berechne gerundete Prozente um exakt 100 zu erhalten
  dplyr::group_by(UserLanguage) %>%
  dplyr::mutate(label =  MESS::round_percent(Q11.2_count_weighted/n)) %>%
  dplyr::ungroup() %>%
  # Kategorie-Bezeichnungen anpassen
  dplyr::mutate(Q11.2 = ifelse(Q11.2 == 'Unentschlossen / Weiss nicht', 'Unentschlossen', Q11.2),
                Q11.2 = factor(Q11.2, levels = c( 'Ja', 'Eher ja', 'Unentschlossen', 'Eher nein', 'Nein')))

# Berechne nach Partei gewichtete Anzahl und ungewichtete Anzahl
df.weighted.median.partei <- data_full_withweights %>%
  dplyr::mutate(Partei = ifelse(Partei_numeric == 6, 'Keine Partei', Partei)) %>%
  dplyr::group_by(Partei) %>%
  dplyr::mutate(n = sum(weight)) %>%
  dplyr::group_by(Partei_numeric, Partei, Q11.2, n) %>%
  dplyr::summarize(Q11.2_count_weighted = sum(weight),
                   Q11.2_count_unweighted = n()) %>%
  dplyr::ungroup() %>%
  # Berechne gerundete Prozente um exakt 100 zu erhalten
  dplyr::group_by(Partei) %>%
  dplyr::mutate(label =  MESS::round_percent(Q11.2_count_weighted/n)) %>%
  dplyr::ungroup() %>%
  # Kategorie-Bezeichnungen anpassen
  dplyr::mutate(Q11.2 = ifelse(Q11.2 == 'Unentschlossen / Weiss nicht', 'Unentschlossen', Q11.2),
                Q11.2 = factor(Q11.2, levels = c( 'Ja', 'Eher ja', 'Unentschlossen', 'Eher nein', 'Nein')))

# visualize ----
# Frage 11.2 mit Werten mit gewichtetem Mittelwert
p.Q11.2.simple <- ggplot(df.weighted.median,
                             aes(x = Q11.2_count_weighted, y = '', fill = Q11.2)) +
  labs(y = '', x= '50 %', fill = 'Stimmabsicht:')+
  ggtitle(wrapper(subset(df_Qs, Frage == 'Q11.2')$Name , width = 65))+
  geom_bar(width = 0.6, position='fill', stat='identity')+
  theme_bw(base_size = 19)+
  geom_text(aes(x = Q11.2_count_weighted/1730,
                label= paste0(round(100*Q11.2_count_weighted/1730), ' %')),
            size = 7,
            position = position_stack(vjust = 0.5))+
  scale_fill_manual(values = col.string)+
  scale_y_discrete(limits=rev) +
  theme(axis.line = element_line(color='black'),
        axis.text.y = element_text(colour="black", size = 16),
        plot.background = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.title.x = element_text(size = 16),
        plot.title = element_text(size = 20),
        strip.text = element_text(size=16,colour="black"),
        strip.background = element_rect(colour="white", fill="white"),
        legend.position = 'bottom')+
  guides(fill = guide_legend(reverse=TRUE))

p.Q11.2.simple

# PNG Datei erstellen und oeffnen
png(paste0(fig.path, 'Q11.2_Stimmabsicht.png'),
    width     = 3000,
    height    = 1400,
    res = 300)

# Bild speichern
plot(p.Q11.2.simple)

# Device schliessen 
dev.off()

# Frage 11.2 nach Geschlecht mit Werten mit gewichtetem Mittelwert
p.Q11.2.geschlecht <- ggplot(df.weighted.median.geschlecht %>%
         dplyr::filter(Q2.1 != 'Übrige'),
       aes(x = Q11.2_count_weighted, y = Q2.1, fill = Q11.2,
           label =  paste0(label, ' %')))+
  labs(y = '', x= '50 %', fill = 'Stimmabsicht:')+
  ggtitle(wrapper(subset(df_Qs, Frage == 'Q11.2')$Name , width = 65))+
  geom_bar(width = 0.6, position='fill', stat='identity')+
  geom_text(size = 6, position = position_fill(vjust=0.5))+
  scale_fill_manual(values = col.string)+
  guides(fill = guide_legend(reverse=TRUE))+
  theme_bw(base_size = 19)+
  theme(axis.line = element_line(color='black'),
        axis.text.y = element_text(colour="black", size = 16),
        axis.text.x = element_blank(),
        plot.background = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.title.x = element_text(size = 16),
        plot.title = element_text(size = 20),
        strip.text = element_text(size=16,colour="black"),
        strip.background = element_rect(colour="white", fill="white"),
        legend.position = 'bottom')

p.Q11.2.geschlecht

# PNG Datei erstellen und oeffnen
png(paste0(fig.path, 'Q11.2_Stimmabsicht_Geschlecht.png'),
    width     = 3000,
    height    = 1600,
    res = 300)

# Bild speichern
plot(p.Q11.2.geschlecht)

# Device schliessen 
dev.off()

# Frage 11.2 nach Sprache mit Werten mit gewichtetem Mittelwert
p.Q11.2.sprache <- ggplot(df.weighted.median.sprache,
                             aes(x = Q11.2_count_weighted, y = UserLanguage,
                                 fill = Q11.2,
                                 label = paste0(label, ' %')))+
  scale_y_discrete(limits=rev) +
  labs(y = '', x= '50 %', fill = 'Stimmabsicht:')+
  ggtitle(wrapper(subset(df_Qs, Frage == 'Q11.2')$Name , width = 65))+
  geom_bar(width = 0.6, position='fill', stat='identity')+
  geom_text(size = 6, position = position_fill(vjust=0.5))+
  scale_fill_manual(values = col.string)+
  guides(fill = guide_legend(reverse=TRUE))+
  theme_bw(base_size = 19)+
  theme(axis.line = element_line(color='black'),
        axis.text.y = element_text(colour="black", size = 16),
        axis.text.x = element_blank(),
        plot.background = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.title.x = element_text(size = 16),
        plot.title = element_text(size = 20),
        strip.text = element_text(size=16,colour="black"),
        strip.background = element_rect(colour="white", fill="white"),
        legend.position = 'bottom')

p.Q11.2.sprache

# PNG Datei erstellen und oeffnen
png(paste0(fig.path, 'Q11.2_Stimmabsicht_sprache.png'),
    width     = 3000,
    height    = 1600,
    res = 300)

# Bild speichern
plot(p.Q11.2.sprache)

# Device schliessen
dev.off()

# Frage 11.2 nach Parteien mit Werten mit gewichtetem Mittelwert
p.Q11.2.partei <- ggplot(df.weighted.median.partei %>%
                              dplyr::mutate(Partei = factor(Partei, levels = c(
                             'glp', 'FDP', 'Die Mitte', 'SP', 'Grüne', 'Keine Partei', 'SVP', 'Andere'))),
                              aes(x = Q11.2_count_weighted, y = Partei, fill = Q11.2,
                              label = ifelse(label < 4, '', paste0(label, '%'))))+
  scale_y_discrete(limits=rev) +
  labs(y = '', x= '50 %', fill = 'Stimmabsicht:')+
  ggtitle(wrapper(subset(df_Qs, Frage == 'Q11.2')$Name , width = 65))+
  geom_bar(width = 0.6, position='fill', stat='identity')+
  geom_text(size = 5.5, position = position_fill(vjust=0.5))+
  scale_fill_manual(values = col.string)+
  guides(fill = guide_legend(reverse=TRUE))+
  theme_bw(base_size = 18)+
  theme(axis.line = element_line(color='black'),
        axis.text.y = element_text(colour="black", size = 16),
        axis.text.x = element_blank(),
        plot.background = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.title.x = element_text(size = 16),
        plot.title = element_text(size = 20),
        strip.text = element_text(size=16,colour="black"),
        strip.background = element_rect(colour="white", fill="white"),
        legend.position = 'bottom')

p.Q11.2.partei

# PNG Datei erstellen und oeffnen
png(paste0(fig.path, 'Q11.2_Stimmabsicht_partei.png'),
    width     = 3000,
    height    = 2700,
    res = 300)

# Bild speichern
plot(p.Q11.2.partei)

# Device schliessen
dev.off()

# =============================================================================/
# END ----
# =============================================================================/

