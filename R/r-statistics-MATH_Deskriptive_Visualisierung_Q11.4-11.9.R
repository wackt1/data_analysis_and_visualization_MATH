# =============================================================================/
# r-statistics-MATH_Deskriptive_Visualisierung_Q11.4-11.9.R
#
# Skript zur gewichteten deskriptiven Visualisierung von Q11.4-11.9
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
library('matrixStats')
library('readxl')
library('tidytext')
library('RColorBrewer')

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
df_Qs <- data.frame(  Frage = colnames(data_Qs),
                      Name = paste0(stringr::str_extract(colnames(data_Qs), "[^_]+"), ' ', data_Qs[1, ]))

# preprocess ----
# Sortiere alle Antworten in Dagegen- und Dafuer-Argumente und behalte Q11.2
df.summary.Qs11 <- data_full_withweights %>%
  dplyr::select(Q11.2, Q11.4, Q11.5, Q11.6, Q11.7, Q11.8, Q11.9, weight) %>%
  dplyr::mutate(Q11.2_against = ifelse(Q11.2 %in% c('Ja', 'Eher ja'), Q11.5,
                                   ifelse(Q11.2 %in% c('Nein', 'Eher nein'), Q11.6, Q11.9)),
                Q11.2_for = ifelse(Q11.2 %in% c('Ja', 'Eher ja'), Q11.4,
                                   ifelse(Q11.2 %in% c('Nein', 'Eher nein'), Q11.7, Q11.8)),
                Q11.2 = ifelse(Q11.2 %in% c('Ja', 'Eher ja'), 'Ja & Eher ja',
                                       ifelse(Q11.2 %in% c('Nein', 'Eher nein'), 'Nein & Eher nein', 'Unentschlossen')),
                ID = row_number()) %>%
  # Kuerze einen String in Dafuer-Argumenten damit Kommatrennung der Argumente funktioniert
  dplyr::mutate(Q11.2_for = stringr::str_replace(Q11.2_for,
                                                 ' \\(E-Voting, E-Collecting, etc.\\)',
                                                 '')) 

# Berechne gewichtete Anzahl und ungewichtete Anzahl Q11.2
df.numbers <- df.summary.Qs11 %>%
  dplyr::select(weight, ID, Q11.2) %>%
  dplyr::group_by(Q11.2) %>%
  dplyr::summarise(num.pers = sum(weight),
                   num.ids = n()) %>%
  dplyr::ungroup()

# Berechne gewichtete Anzahl und ungewichtete Anzahl der Gegen-Argumente
df.summary.Qs11.against <- df.summary.Qs11 %>%
  # Separiere kommagetrennte Mehrauswahl in Kolonnen
  tidyr::separate(col = Q11.2_against,  sep = ",", into = c(
    'against1', 'against2', 'against3', 'against4', 'against5', 'against6', 'against7', 'against8', 'against9', 'against10')) %>%
  # Mehrauswahl in einzelne Zeilen umwandeln, um Anzahl einzelner Gruende zu bestimmen
  tidyr::pivot_longer(cols = c( 'against1', 'against2', 'against3', 'against4', 'against5', 'against6', 'against7', 'against8', 'against9', 'against10'),
    names_to = 'against_n',
    values_to = 'againstvalues') %>%
  drop_na(againstvalues) %>%
  # Gewichtete und ungewichtete Anzahl
  dplyr::group_by(againstvalues, Q11.2) %>%
  dplyr::summarise(n = sum(weight),
                   nunweighted = n()) %>%
  dplyr::ungroup() %>%
  # Fuege Kolonne mit n und ngewichtet in Bezug auf Q11.2-Kategorie hinzu
  dplyr::left_join(df.numbers) %>%
  dplyr::mutate(Against_ordered = tidytext::reorder_within(againstvalues, by = n/num.pers, within = Q11.2)) %>%
  dplyr::mutate(Q11.2 = factor(Q11.2, levels = c('Nein & Eher nein', 'Unentschlossen', 'Ja & Eher ja')))

# Berechne gewichtete Anzahl und ungewichtete Anzahl der Dafuer-Argumente
df.summary.Qs11.for <- df.summary.Qs11 %>%
  # Separiere kommagetrennte Mehrauswahlin Kolonnen
  tidyr::separate(col = Q11.2_for,  sep = ",", into = c(
    'for1', 'for2', 'for3', 'for4', 'for5', 'for6', 'for7', 'for8', 'for9', 'for10')) %>%
  # Kolonnen zu Einzelnen Reihen (ID kann mehrere Reihen haben, long format)
  tidyr::pivot_longer(cols = c('for1', 'for2', 'for3', 'for4', 'for5', 'for6', 'for7', 'for8', 'for9', 'for10'),
                      names_to = 'for_n',
                      values_to = 'forvalues')%>%
  drop_na(forvalues)%>%
  # Gewichtete und ungewichtete Anzahl
  dplyr::group_by(forvalues, Q11.2) %>%
  dplyr::summarise(n = sum(weight),
                   nunweighted = n()) %>%
  dplyr::ungroup() %>%
  # Fuege Kolonne mit n und ngewichtet in Bezug auf Q11.2-Kategorie hinzu
  dplyr::left_join(df.numbers) %>%
  dplyr::mutate(For_ordered = tidytext::reorder_within(forvalues, by = n/num.pers, within = Q11.2)) %>%
  dplyr::mutate(Q11.2 = factor(Q11.2, levels = c( 'Ja & Eher ja', 'Unentschlossen', 'Nein & Eher nein')))

# visualize ----
# Plot fuer Gruende Dafuer
p.Q11.for <- ggplot(df.summary.Qs11.for,
                aes(y = For_ordered,
                    x = 100*n/num.pers, fill = forvalues))+
  labs(title = 'Zustimmungsgründe staatliche Blockchain-ID\nQ11.4, Q11.7, Q11.8', y = '', x = 'Anteil in % der Antworten pro Gruppe', fill = '')+
  geom_col(show.legend = FALSE, width=0.3)+
  scale_fill_viridis_d()+
  scale_x_continuous(breaks = seq(0, 10)*10, expand = expansion(mult = c(0.02,0.02))) +
  scale_y_discrete(expand = expansion(mult = c(0.02,0.1))) +
  facet_wrap(Q11.2~., scales = 'free_y', strip.position = "left", ncol  = 1)+
  theme_bw(base_size = 20)+ 
  geom_text(aes(y = For_ordered,
                x = 0,
                label= paste0(forvalues, ' (', round(100*n/num.pers, 0), '%)')),
            hjust = "left",
            vjust = -0.8,
            size = 4,
            position = position_dodge(width = 0.9))+
  theme(axis.line = element_line(color='black'),
        axis.text.y = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.ticks.x = element_blank(),
        plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.title.x = element_text(size = 18),
        axis.title.y = element_blank(),
        plot.title = element_text(size = 20),
        legend.position = 'bottom')+ 
  guides(fill= guide_legend(ncol = 1))

# PNG datei erstellen und oeffnen
png(paste0(fig.path, 'Q11.Zustimmungsgruende.png'),
    width     = 2500,
    height    = 3500,
    res = 300)

# Bild speichern
plot(p.Q11.for)

# Datei schliessen
dev.off()

# Plot fuer Gruende Dagegen
p.Q11.against <- ggplot(df.summary.Qs11.against,
       aes(y = Against_ordered,
           x = 100*n/num.pers, fill = againstvalues))+
  labs(title = 'Ablehnungsgründe staatliche Blockchain-ID\nQ11.5, Q11.6, Q11.9', y = '', x = 'Anteil in % der Antworten pro Gruppe', fill = '')+
  geom_col(show.legend = FALSE, width=0.3)+
  scale_fill_viridis_d()+
  scale_x_continuous(breaks = seq(0, 10)*10, expand = expansion(mult = c(0.02,0.02))) +
  scale_y_discrete(expand = expansion(mult = c(0.02,0.1))) +
  facet_wrap(Q11.2~., scales = 'free_y', strip.position = "left", ncol  = 1)+
  theme_bw(base_size = 20)+
  geom_text(aes(y = Against_ordered,
                x = 0,
                label= paste0(againstvalues, ' (', round(100*n/num.pers, 0), '%)')),
            hjust = "left",
            vjust = -0.8,
            size = 4,
            position = position_dodge(width = 0.9))+
  theme(axis.line = element_line(color='black'),
        axis.text.y = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.ticks.x = element_blank(),
        plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.title.x = element_text(size = 18),
        axis.title.y = element_blank(),
        plot.title = element_text(size = 20),
        legend.position = 'bottom')+ 
  guides(fill= guide_legend(ncol = 1))

# PNG datei erstellen und oeffnen
png(paste0(fig.path, 'Q11.Ablehnungsgruende.png'),
    width     = 2500,
    height    = 3500,
    res = 300)

# Bild speichern
plot(p.Q11.against)

# Datei schliessen
dev.off()

# =============================================================================/
# END ----
# =============================================================================/

