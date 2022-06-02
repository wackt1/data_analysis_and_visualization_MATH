# =============================================================================/
# r-statistics-MATH_Deskriptive_Visualisierung_Zustimmungsfragen.R
#
# Skript zur gewichteten deskriptiven Visualisierung von Zustimmungsfragen
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
# MAIN
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

#------------------------------------------------------------------------------/
# Q5.2_1 ----
## * preprocess Q5.2_1 ----

df.weighted.mean.partei.Q5.2_1 <- data_full_withweights %>%
  # Numerisch 6 steht fuer 'keine Partei' (wurde ersetzt fuer RIM weighting)
  dplyr::mutate(Partei = ifelse(Partei_numeric == 6, 'Keine Partei', Partei)) %>%
  dplyr::mutate(Mittelwert = matrixStats::weightedMean(Q5.2_1, w = weight)) %>%
  dplyr::group_by(Partei_numeric, Partei, Mittelwert) %>%
  dplyr::summarise(Q5.2_1_median = matrixStats::weightedMedian(Q5.2_1, w = weight),
                   Q5.2_1_mean = matrixStats::weightedMean(Q5.2_1, w = weight)) %>%
  dplyr::ungroup() %>%
  dplyr::arrange(desc(Q5.2_1_mean))

data.weighted.mean.partei.Q5.2_1 <- df.weighted.mean.partei.Q5.2_1 %>%
  dplyr::bind_rows(data.frame(Partei = 'Mittelwert',
                              Q5.2_1_mean = df.weighted.mean.partei.Q5.2_1$Mittelwert[1])) %>%
  # Definiere Breite der Linie im Plot
  dplyr::mutate(Mittelwert = ifelse(Partei == 'Mittelwert', 0.03, 0.12))

## * visualize Q5.2_1 ----

p.Q5.2_1 <- ggplot(data.weighted.mean.partei.Q5.2_1 ,
       aes(x = (Q5.2_1_mean-50)*2, y = reorder(Partei, Q5.2_1_mean))) +
  labs(y = '', x= 'Zustimmungswert')+
  ggtitle(wrapper(subset(df_Qs, Frage == 'Q5.2_1')$Name , width = 68))+
  geom_col(show.legend = FALSE,
           aes(fill = factor(Mittelwert)),
           width = length(data.weighted.mean.partei.Q5.2_1 ) * data.weighted.mean.partei.Q5.2_1 $Mittelwert)+
  geom_text(aes(label=paste0(round((Q5.2_1_mean-50)*2, 1)),
                fontface = ifelse(Partei == 'Mittelwert', 'bold.italic', 'plain')), 
                size = 6,
                hjust = -0.2, show.legend = FALSE)+
  scale_x_continuous(breaks = c(0, 20, 40, 60, 80, 100),labels = c('0\nNeutral', 20, 40, 60, 80, 100),
                     expand = expansion(mult = c(0.05,0.1)))+
  scale_fill_manual(values = c( '#00cc99', '#5d8aa8'))+ #'#e52b50', 
  scale_y_discrete(breaks = c("Grüne","SP","Andere","Die Mitte","Keine Partei","glp","SVP","FDP", "Mittelwert"), 
                   labels = c("Grüne","SP","Andere","Die Mitte","Keine Partei","glp","SVP","FDP", expression(bolditalic("Mittelwert \u00F8"))))+
  theme_bw(base_size = 20)+
  theme(axis.line = element_line(color='black'),
        axis.text.y = element_text(colour="black", size = 18),
        plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.text.x = element_text(colour="black", size = 16),
        axis.title.x = element_text(size = 16),
        plot.title = element_text(size = 20))

p.Q5.2_1 

# PNG Datei erstellen und oeffnen
png(paste0(fig.path, 'Q5.2_1_gewichteterMittelwert.png'),
    width     = 3000,
    height    = 1450,
    res = 300)

# Bild speichern
plot(p.Q5.2_1)

# Device schliessen
dev.off()

## * stacked barplot Q5.2_1 ----

p2.Q5.2_1 <- ggplot(data_full_withweights %>% dplyr::arrange(Q5.2_1) %>%
                      dplyr::select(Q5.2_1, weight) %>%
                      dplyr::mutate(Q5.2_1_adjusted = (Q5.2_1-50)*2)%>%
                      dplyr::mutate(fillcol = ifelse(Q5.2_1_adjusted <0, 'Ablehnung', 'Zustimmung'),
                                    fillcol = ifelse(Q5.2_1_adjusted == 0, 'Neutral', fillcol)),
             aes(x = Q5.2_1_adjusted, y = weight)) +
  labs(y = 'n', x= 'Zustimmungswert', fill = '')+
  geom_bar( stat = 'identity', aes(fill = fillcol)) +
  scale_fill_manual(values = c('#A87B5D', 'gray','#5d8aa8'))+
  theme_bw(base_size = 20)+
  theme(axis.line = element_line(color='black'),
        axis.text.y = element_text(colour="black", size = 18),
        axis.text.x = element_text(colour="black", size = 18),
        plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 18),
        plot.title = element_text(size = 20))

p2.Q5.2_1

# PNG Datei erstellen und oeffnen
png(paste0(fig.path, 'Q5.2_1_Skewness.png'),
    width     = 3200,
    height    = 1350,
    res = 300)


# Bild speichern
plot(p2.Q5.2_1)

# Device schliessen
dev.off()

#------------------------------------------------------------------------------/
# Q5.3_1 ----
## * preprocess Q5.3_1 ----

df.weighted.mean.partei.Q5.3_1 <- data_full_withweights %>%
  dplyr::mutate(Partei = ifelse(Partei_numeric == 6, 'Keine Partei', Partei)) %>%
  dplyr::mutate(Mittelwert = matrixStats::weightedMean(Q5.3_1, w = weight)) %>%
  dplyr::group_by(Partei_numeric, Partei, Mittelwert) %>%
  dplyr::summarise(Q5.3_1_median = matrixStats::weightedMedian(Q5.3_1, w = weight),
                   Q5.3_1_mean = matrixStats::weightedMean(Q5.3_1, w = weight)) %>%
  dplyr::ungroup() %>%
  dplyr::arrange(desc(Q5.3_1_mean))

data.weighted.mean.partei.Q5.3_1 <- df.weighted.mean.partei.Q5.3_1 %>%
  dplyr::bind_rows(data.frame(Partei = 'Mittelwert',
                              Q5.3_1_mean = df.weighted.mean.partei.Q5.3_1$Mittelwert[1])) %>%
  dplyr::mutate(Mittelwert = ifelse(Partei == 'Mittelwert', 0.03, 0.12))

## * visualize Q5.3_1 ----

p.Q5.3_1 <- ggplot(data.weighted.mean.partei.Q5.3_1 ,
                   aes(x = (Q5.3_1_mean-50)*2, y = reorder(Partei, Q5.3_1_mean))) +
  labs(y = '', x= 'Zustimmungswert')+
  ggtitle(wrapper(subset(df_Qs, Frage == 'Q5.3_1')$Name , width = 64))+
  geom_col(show.legend = FALSE,
           aes(fill = factor(Mittelwert)),
           width = length(data.weighted.mean.partei.Q5.3_1 ) * data.weighted.mean.partei.Q5.3_1 $Mittelwert)+
  geom_text(aes(label= round((Q5.3_1_mean-50)*2, 1),
                fontface = ifelse(Partei == 'Mittelwert', 'bold.italic', 'plain')), 
            size = 6,
            hjust = -0.2, show.legend = FALSE)+
  scale_x_continuous( breaks = c(-10, 0, 10, 20, 30, 40, 50),labels = c(-10, '0\nNeutral', 10, 20, 30, 40, 50),
                      expand = expansion(mult = c(0.05,0.1)))+
  scale_fill_manual(values = c( '#00cc99', '#5d8aa8'))+ #'#e52b50', 
  scale_y_discrete(breaks = c("Grüne","SP","Andere","Die Mitte","Keine Partei","glp","SVP","FDP", "Mittelwert"), 
                   labels = c("Grüne","SP","Andere","Die Mitte","Keine Partei","glp","SVP","FDP", expression(bolditalic("Mittelwert \u00F8"))))+
  theme_bw(base_size = 20)+
  theme(axis.line = element_line(color='black'),
        axis.text.y = element_text(colour="black", size = 18),
        plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.text.x = element_text(colour="black", size = 16),
        axis.title.x = element_text(size = 16),
        plot.title = element_text(size = 20))

p.Q5.3_1 

# PNG Datei erstellen und oeffnen
png(paste0(fig.path, 'Q5.3_1_gewichteterMittelwert.png'),
    width     = 3000,
    height    = 1450,
    res = 300)

# Bild speichern
plot(p.Q5.3_1)

# Device schliessen
dev.off()

#------------------------------------------------------------------------------/
# Q5.4_1 ----
## * preprocess Q5.4_1 ----

df.weighted.mean.partei.Q5.4_1 <- data_full_withweights %>%
  dplyr::mutate(Partei = ifelse(Partei_numeric == 6, 'Keine Partei', Partei)) %>%
  dplyr::mutate(Mittelwert = matrixStats::weightedMean(Q5.4_1, w = weight)) %>%
  dplyr::group_by(Partei_numeric, Partei, Mittelwert) %>%
  dplyr::summarise(Q5.4_1_median = matrixStats::weightedMedian(Q5.4_1, w = weight),
                   Q5.4_1_mean = matrixStats::weightedMean(Q5.4_1, w = weight)) %>%
  dplyr::ungroup() %>%
  dplyr::arrange(desc(Q5.4_1_mean))

data.weighted.mean.partei.Q5.4_1 <- df.weighted.mean.partei.Q5.4_1 %>%
  dplyr::bind_rows(data.frame(Partei = 'Mittelwert',
                              Q5.4_1_mean = df.weighted.mean.partei.Q5.4_1$Mittelwert[1])) %>%
  dplyr::mutate(Mittelwert = ifelse(Partei == 'Mittelwert', 0.03, 0.12))

## * visualize Q5.4_1 ----

p.Q5.4_1 <- ggplot(data.weighted.mean.partei.Q5.4_1 %>%
                     dplyr::mutate(fillcol = ifelse(Q5.4_1_mean <50, 'Ablehnung', 'Zustimmung'),
                                   fillcol = ifelse(Partei == 'Mittelwert', 'Mittelwert', fillcol)),
                   aes(x = (Q5.4_1_mean-50)*2, y = reorder(Partei, Q5.4_1_mean), fill = fillcol)) +
  labs(y = '', x= 'Zustimmungswert')+
  ggtitle(wrapper(subset(df_Qs, Frage == 'Q5.4_1')$Name , width = 60))+
  geom_col(show.legend = FALSE,
           aes(fill = factor(fillcol)),
           width = length(data.weighted.mean.partei.Q5.4_1 ) * data.weighted.mean.partei.Q5.4_1 $Mittelwert)+
  geom_text(aes(hjust = ifelse(Q5.4_1_mean < 50, 1.2, -0.2),
                label=round((Q5.4_1_mean-50)*2, 1),
                fontface = ifelse(Partei == 'Mittelwert', 'bold.italic', 'plain')), 
            size = 6,
            show.legend = FALSE)+
  scale_x_continuous( breaks = c(-10, 0, 10, 20, 30, 40),labels = c(-10, '0\nNeutral', 10, 20, 30, 40),
                      expand = expansion(mult = c(0.1,0.1)))+
  scale_fill_manual(values = c( '#A87B5D', '#00cc99', '#5d8aa8'))+ #'#e52b50', 
  scale_y_discrete(breaks = c("Grüne","SP","Andere","Die Mitte","Keine Partei","glp","SVP","FDP", "Mittelwert"), 
                   labels = c("Grüne","SP","Andere","Die Mitte","Keine Partei","glp","SVP","FDP", expression(bolditalic("Mittelwert \u00F8"))))+
  theme_bw(base_size = 20)+
  theme(axis.line = element_line(color='black'),
        axis.text.y = element_text(colour="black", size = 18),
        plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.text.x = element_text(colour="black", size = 16),
        axis.title.x = element_text(size = 16),
        plot.title = element_text(size = 20))

p.Q5.4_1 

# PNG Datei erstellen und oeffnen
png(paste0(fig.path, 'Q5.4_1_gewichteterMittelwert.png'),
    width     = 3000,
    height    = 1450,
    res = 300)

# Bild speichern
plot(p.Q5.4_1)

# Device schliessen
dev.off()

#------------------------------------------------------------------------------/
# Q5.5_1 ----
## * preprocess Q5.5_1 ----

df.weighted.mean.partei.Q5.5_1 <- data_full_withweights %>%
  dplyr::mutate(Partei = ifelse(Partei_numeric == 6, 'Keine Partei', Partei)) %>%
  dplyr::mutate(Mittelwert = matrixStats::weightedMean(Q5.5_1, w = weight)) %>%
  dplyr::group_by(Partei_numeric, Partei, Mittelwert) %>%
  dplyr::summarise(Q5.5_1_median = matrixStats::weightedMedian(Q5.5_1, w = weight),
                   Q5.5_1_mean = matrixStats::weightedMean(Q5.5_1, w = weight)) %>%
  dplyr::ungroup() %>%
  dplyr::arrange(desc(Q5.5_1_mean))

data.weighted.mean.partei.Q5.5_1 <- df.weighted.mean.partei.Q5.5_1 %>%
  dplyr::bind_rows(data.frame(Partei = 'Mittelwert',
                              Q5.5_1_mean = df.weighted.mean.partei.Q5.5_1$Mittelwert[1])) %>%
  dplyr::mutate(Mittelwert = ifelse(Partei == 'Mittelwert', 0.03, 0.12))

## * visualize Q5.5_1 ----

p.Q5.5_1 <- ggplot(data.weighted.mean.partei.Q5.5_1 %>%
                     dplyr::mutate(fillcol = ifelse(Q5.5_1_mean <50, 'Ablehnung', 'Zustimmung'),
                                   fillcol = ifelse(Partei == 'Mittelwert', 'Mittelwert', fillcol)),
                   aes(x = (Q5.5_1_mean-50)*2, y = reorder(Partei, Q5.5_1_mean), fill = fillcol)) +
  labs(y = '', x= 'Zustimmungswert')+
  ggtitle(wrapper(subset(df_Qs, Frage == 'Q5.5_1')$Name , width = 64))+
  geom_col(show.legend = FALSE,
           aes(fill = factor(fillcol)),
           width = length(data.weighted.mean.partei.Q5.5_1) * data.weighted.mean.partei.Q5.5_1$Mittelwert)+
  geom_text(aes(label= round((Q5.5_1_mean-50)*2, 1),
                fontface = ifelse(Partei == 'Mittelwert', 'bold.italic', 'plain')), 
            size = 6,
            hjust = -0.2, show.legend = FALSE)+
  scale_x_continuous( breaks = c(0, 20, 40, 60, 80, 100),labels = c('0\nNeutral', 20, 40, 60, 80, 100),
                      expand = expansion(mult = c(0.05,0.1)))+
  scale_fill_manual(values = c( '#00cc99', '#5d8aa8'))+ #'#e52b50', 
  scale_y_discrete(breaks = c("Grüne","SP","Andere","Die Mitte","Keine Partei","glp","SVP","FDP", "Mittelwert"), 
                   labels = c("Grüne","SP","Andere","Die Mitte","Keine Partei","glp","SVP","FDP", expression(bolditalic("Mittelwert \u00F8"))))+
  theme_bw(base_size = 20)+
  theme(axis.line = element_line(color='black'),
        axis.text.y = element_text(colour="black", size = 18),
        plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.text.x = element_text(colour="black", size = 16),
        axis.title.x = element_text(size = 16),
        plot.title = element_text(size = 20))

p.Q5.5_1 

# PNG Datei erstellen und oeffnen
png(paste0(fig.path, 'Q5.5_1_gewichteterMittelwert.png'),
    width     = 3000,
    height    = 1450,
    res = 300)

# Bild speichern
plot(p.Q5.5_1)

# Device schliessen
dev.off()

#------------------------------------------------------------------------------/
# Q6.2_1 ----
## * preprocess Q6.2_1 ----

df.weighted.mean.partei.Q6.2_1 <- data_full_withweights %>%
  dplyr::mutate(Partei = ifelse(Partei_numeric == 6, 'Keine Partei', Partei)) %>%
  dplyr::mutate(Mittelwert = matrixStats::weightedMean(Q6.2_1, w = weight)) %>%
  dplyr::group_by(Partei_numeric, Partei, Mittelwert) %>%
  dplyr::summarise(Q6.2_1_median = matrixStats::weightedMedian(Q6.2_1, w = weight),
                   Q6.2_1_mean = matrixStats::weightedMean(Q6.2_1, w = weight)) %>%
  dplyr::ungroup() %>%
  dplyr::arrange(desc(Q6.2_1_mean))

data.weighted.mean.partei.Q6.2_1 <- df.weighted.mean.partei.Q6.2_1 %>%
  dplyr::bind_rows(data.frame(Partei = 'Mittelwert',
                              Q6.2_1_mean = df.weighted.mean.partei.Q6.2_1$Mittelwert[1])) %>%
  dplyr::mutate(Mittelwert = ifelse(Partei == 'Mittelwert', 0.03, 0.12))

## * visualize Q6.2_1 ----

p.Q6.2_1 <- ggplot(data.weighted.mean.partei.Q6.2_1 ,
                   aes(x = (Q6.2_1_mean-50)*2, y = reorder(Partei, Q6.2_1_mean))) +
  labs(y = '', x= 'Zustimmungswert')+
  ggtitle(wrapper(subset(df_Qs, Frage == 'Q6.2_1')$Name , width = 70))+
  geom_col(show.legend = FALSE,
           aes(fill = factor(Mittelwert)),
           width = length(data.weighted.mean.partei.Q6.2_1) * data.weighted.mean.partei.Q6.2_1$Mittelwert)+
  geom_text(aes(label= round((Q6.2_1_mean-50)*2, 1),
                fontface = ifelse(Partei == 'Mittelwert', 'bold.italic', 'plain')), 
            size = 6,
            hjust = -0.2, show.legend = FALSE)+
  scale_x_continuous(breaks = c(0, 20, 40, 60, 80, 100),labels = c('0\nNeutral', 20, 40, 60, 80, 100),
                      expand = expansion(mult = c(0.05,0.1)))+
  scale_fill_manual(values = c( '#00cc99', '#5d8aa8'))+ 
  scale_y_discrete(breaks = c("Grüne","SP","Andere","Die Mitte","Keine Partei","glp","SVP","FDP", "Mittelwert"), 
                   labels = c("Grüne","SP","Andere","Die Mitte","Keine Partei","glp","SVP","FDP", expression(bolditalic("Mittelwert \u00F8"))))+
  theme_bw(base_size = 20)+
  theme(axis.line = element_line(color='black'),
        axis.text.y = element_text(colour="black", size = 18),
        plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.text.x = element_text(colour="black", size = 16),
        axis.title.x = element_text(size = 16),
        plot.title = element_text(size = 20))
p.Q6.2_1 

# PNG Datei erstellen und oeffnen
png(paste0(fig.path, 'Q6.2_1_gewichteterMittelwert.png'),
    width     = 3000,
    height    = 1450,
    res = 300)

# Bild speichern
plot(p.Q6.2_1)

# Device schliessen
dev.off()

#------------------------------------------------------------------------------/
# Q6.3_1 ----
## * preprocess Q6.3_1 ----

df.weighted.mean.partei.Q6.3_1 <- data_full_withweights %>%
  dplyr::mutate(Partei = ifelse(Partei_numeric == 6, 'Keine Partei', Partei)) %>%
  dplyr::mutate(Mittelwert = matrixStats::weightedMean(Q6.3_1, w = weight)) %>%
  dplyr::group_by(Partei_numeric, Partei, Mittelwert) %>%
  dplyr::summarise(Q6.3_1_median = matrixStats::weightedMedian(Q6.3_1, w = weight),
                   Q6.3_1_mean = matrixStats::weightedMean(Q6.3_1, w = weight)) %>%
  dplyr::ungroup() %>%
  dplyr::arrange(desc(Q6.3_1_mean))

data.weighted.mean.partei.Q6.3_1 <- df.weighted.mean.partei.Q6.3_1 %>%
  dplyr::bind_rows(data.frame(Partei = 'Mittelwert',
                              Q6.3_1_mean = df.weighted.mean.partei.Q6.3_1$Mittelwert[1])) %>%
  dplyr::mutate(Mittelwert = ifelse(Partei == 'Mittelwert', 0.03, 0.12))

## * visualize Q6.3_1 ----

p.Q6.3_1 <- ggplot(data.weighted.mean.partei.Q6.3_1,
                   aes(x = (Q6.3_1_mean-50)*2, y = reorder(Partei, Q6.3_1_mean))) +
  labs(y = '', x= 'Zustimmungswert')+
  ggtitle(wrapper(subset(df_Qs, Frage == 'Q6.3_1')$Name , width = 64))+
  geom_col(show.legend = FALSE,
           aes(fill = factor(Mittelwert)),
           width = length(data.weighted.mean.partei.Q6.3_1) * data.weighted.mean.partei.Q6.3_1$Mittelwert)+
  geom_text(aes(label= round((Q6.3_1_mean-50)*2, 1),
                fontface = ifelse(Partei == 'Mittelwert', 'bold.italic', 'plain')), 
            size = 6,
            hjust = -0.2, show.legend = FALSE)+
  scale_x_continuous( breaks = c(0, 20, 40, 60, 80, 100),labels = c('0\nNeutral', 20, 40, 60, 80, 100),
                      expand = expansion(mult = c(0.05,0.1)))+
  scale_fill_manual(values = c( '#00cc99', '#5d8aa8'))+ #'#e52b50', 
  scale_y_discrete(breaks = c("Grüne","SP","Andere","Die Mitte","Keine Partei","glp","SVP","FDP", "Mittelwert"), 
                   labels = c("Grüne","SP","Andere","Die Mitte","Keine Partei","glp","SVP","FDP", expression(bolditalic("Mittelwert \u00F8"))))+
  theme_bw(base_size = 20)+
  theme(axis.line = element_line(color='black'),
        axis.text.y = element_text(colour="black", size = 18),
        plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.text.x = element_text(colour="black", size = 16),
        axis.title.x = element_text(size = 16),
        plot.title = element_text(size = 20))

p.Q6.3_1 

# PNG Datei erstellen und oeffnen
png(paste0(fig.path, 'Q6.3_1_gewichteterMittelwert.png'),
    width     = 3000,
    height    = 1450,
    res = 300)

# Bild speichern
plot(p.Q6.3_1)

# Device schliessen
dev.off()

#------------------------------------------------------------------------------/
# Q6.4_1 ----
## * preprocess Q6.4_1 ----

df.weighted.mean.partei.Q6.4_1 <- data_full_withweights %>%
  dplyr::mutate(Partei = ifelse(Partei_numeric == 6, 'Keine Partei', Partei)) %>%
  dplyr::mutate(Mittelwert = matrixStats::weightedMean(Q6.4_1, w = weight)) %>%
  dplyr::group_by(Partei_numeric, Partei, Mittelwert) %>%
  dplyr::summarise(Q6.4_1_median = matrixStats::weightedMedian(Q6.4_1, w = weight),
                   Q6.4_1_mean = matrixStats::weightedMean(Q6.4_1, w = weight)) %>%
  dplyr::ungroup() %>%
  dplyr::arrange(desc(Q6.4_1_mean))

data.weighted.mean.partei.Q6.4_1 <- df.weighted.mean.partei.Q6.4_1 %>%
  dplyr::bind_rows(data.frame(Partei = 'Mittelwert',
                              Q6.4_1_mean = df.weighted.mean.partei.Q6.4_1 $Mittelwert[1])) %>%
  dplyr::mutate(Mittelwert = ifelse(Partei == 'Mittelwert', 0.03, 0.12))

## * visualize Q6.4_1 ----

p.Q6.4_1 <- ggplot(data.weighted.mean.partei.Q6.4_1 ,
                   aes(x = (Q6.4_1_mean-50)*2, y = reorder(Partei, Q6.4_1_mean))) +
  labs(y = '', x= 'Zustimmungswert')+
  ggtitle(wrapper(subset(df_Qs, Frage == 'Q6.4_1')$Name , width = 66))+
  geom_col(show.legend = FALSE,
           aes(fill = factor(Mittelwert)),
           width = length(data.weighted.mean.partei.Q6.4_1) * data.weighted.mean.partei.Q6.4_1$Mittelwert)+
  geom_text(aes(label= round((Q6.4_1_mean-50)*2, 1),
                fontface = ifelse(Partei == 'Mittelwert', 'bold.italic', 'plain')), 
            size = 6,
            hjust = -0.2, show.legend = FALSE)+
  scale_x_continuous( breaks = c(-10, 0, 10, 20, 30, 40),labels = c(-10, '0\nNeutral', 10, 20, 30, 40),
                      expand = expansion(mult = c(0.05,0.1)))+
  scale_fill_manual(values = c( '#00cc99', '#5d8aa8'))+ #'#e52b50', 
  scale_y_discrete(breaks = c("Grüne","SP","Andere","Die Mitte","Keine Partei","glp","SVP","FDP", "Mittelwert"), 
                   labels = c("Grüne","SP","Andere","Die Mitte","Keine Partei","glp","SVP","FDP", expression(bolditalic("Mittelwert \u00F8"))))+
  theme_bw(base_size = 20)+
  theme(axis.line = element_line(color='black'),
        axis.text.y = element_text(colour="black", size = 18),
        plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.text.x = element_text(colour="black", size = 16),
        axis.title.x = element_text(size = 16),
        plot.title = element_text(size = 20))

p.Q6.4_1 

# PNG Datei erstellen und oeffnen
png(paste0(fig.path, 'Q6.4_1_gewichteterMittelwert.png'),
    width     = 3000,
    height    = 1450,
    res = 300)

# Bild speichern
plot(p.Q6.4_1)

# Device schliessen
dev.off()

#------------------------------------------------------------------------------/
# Q7.2_1 ----
## * preprocess Q7.2_1 ----

df.weighted.mean.partei.Q7.2_1 <- data_full_withweights %>%
  dplyr::mutate(Partei = ifelse(Partei_numeric == 6, 'Keine Partei', Partei)) %>%
  dplyr::mutate(Mittelwert = matrixStats::weightedMean(Q7.2_1, w = weight)) %>%
  dplyr::group_by(Partei_numeric, Partei, Mittelwert) %>%
  dplyr::summarise(Q7.2_1_median = matrixStats::weightedMedian(Q7.2_1, w = weight),
                   Q7.2_1_mean = matrixStats::weightedMean(Q7.2_1, w = weight)) %>%
  dplyr::ungroup() %>%
  dplyr::arrange(desc(Q7.2_1_mean))

data.weighted.mean.partei.Q7.2_1 <- df.weighted.mean.partei.Q7.2_1 %>%
  dplyr::bind_rows(data.frame(Partei = 'Mittelwert',
                              Q7.2_1_mean = df.weighted.mean.partei.Q7.2_1$Mittelwert[1])) %>%
  dplyr::mutate(Mittelwert = ifelse(Partei == 'Mittelwert', 0.03, 0.12))

## * visualize Q7.2_1 ----

p.Q7.2_1 <- ggplot(data.weighted.mean.partei.Q7.2_1 ,
                   aes(x = (Q7.2_1_mean-50)*2, y = reorder(Partei, Q7.2_1_mean))) +
  labs(y = '', x= 'Zustimmungswert')+
  ggtitle(wrapper(subset(df_Qs, Frage == 'Q7.2_1')$Name , width = 64))+
  geom_col(show.legend = FALSE,
           aes(fill = factor(Mittelwert)),
           width = length(data.weighted.mean.partei.Q7.2_1) * data.weighted.mean.partei.Q7.2_1$Mittelwert)+
  geom_text(aes(label= round((Q7.2_1_mean-50)*2, 1),
                fontface = ifelse(Partei == 'Mittelwert', 'bold.italic', 'plain')), 
            size = 6,
            hjust = -0.2, show.legend = FALSE)+
  scale_x_continuous( breaks = c(0, 20, 40, 60, 80, 100),labels = c('0\nNeutral', 20, 40, 60, 80, 100),
                      expand = expansion(mult = c(0.05,0.1)))+
  scale_fill_manual(values = c( '#00cc99', '#5d8aa8'))+  
  scale_y_discrete(breaks = c("Grüne","SP","Andere","Die Mitte","Keine Partei","glp","SVP","FDP", "Mittelwert"), 
                   labels = c("Grüne","SP","Andere","Die Mitte","Keine Partei","glp","SVP","FDP", expression(bolditalic("Mittelwert \u00F8"))))+
  theme_bw(base_size = 20)+
  theme(axis.line = element_line(color='black'),
        axis.text.y = element_text(colour="black", size = 18),
        plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.text.x = element_text(colour="black", size = 16),
        axis.title.x = element_text(size = 16),
        plot.title = element_text(size = 20))

p.Q7.2_1 

# PNG Datei erstellen und oeffnen
png(paste0(fig.path, 'Q7.2_1_gewichteterMittelwert.png'),
    width     = 3000,
    height    = 1600,
    res = 300)

# Bild speichern
plot(p.Q7.2_1)

# Device schliessen
dev.off()

#------------------------------------------------------------------------------/
# Q7.2_2 ----
## * preprocess Q7.2_2 ----

df.weighted.mean.partei.Q7.2_2 <- data_full_withweights %>%
  dplyr::mutate(Partei = ifelse(Partei_numeric == 6, 'Keine Partei', Partei)) %>%
  dplyr::mutate(Mittelwert = matrixStats::weightedMean(Q7.2_2, w = weight)) %>%
  dplyr::group_by(Partei_numeric, Partei, Mittelwert) %>%
  dplyr::summarise(Q7.2_2_median = matrixStats::weightedMedian(Q7.2_2, w = weight),
                   Q7.2_2_mean = matrixStats::weightedMean(Q7.2_2, w = weight)) %>%
  dplyr::ungroup() %>%
  dplyr::arrange(desc(Q7.2_2_mean))

data.weighted.mean.partei.Q7.2_2 <- df.weighted.mean.partei.Q7.2_2 %>%
  dplyr::bind_rows(data.frame(Partei = 'Mittelwert',
                              Q7.2_2_mean = df.weighted.mean.partei.Q7.2_2$Mittelwert[1])) %>%
  dplyr::mutate(Mittelwert = ifelse(Partei == 'Mittelwert', 0.03, 0.12))

## * visualize Q7.2_2 ----

p.Q7.2_2 <- ggplot(data.weighted.mean.partei.Q7.2_2 %>%
                     dplyr::mutate(fillcol = ifelse(Q7.2_2_mean <50, 'Ablehnung', 'Zustimmung'),
                                   fillcol = ifelse(Partei == 'Mittelwert', 'Mittelwert', fillcol)),
                   aes(x = (Q7.2_2_mean-50)*2, y = reorder(Partei, Q7.2_2_mean))) +
  labs(y = '', x= 'Zustimmungswert')+
  ggtitle(wrapper(subset(df_Qs, Frage == 'Q7.2_2')$Name , width = 64))+
  geom_col(show.legend = FALSE,
           aes(fill = factor(fillcol)),
           width = length(data.weighted.mean.partei.Q7.2_2) * data.weighted.mean.partei.Q7.2_2$Mittelwert)+
  geom_text(aes(hjust = ifelse(Q7.2_2_mean < 50, 1.2, -0.2),
                label=round((Q7.2_2_mean-50)*2, 1),
                fontface = ifelse(Partei == 'Mittelwert', 'bold.italic', 'plain')), 
            size = 6,
            show.legend = FALSE)+
  scale_x_continuous( breaks = c(-10, 0, 10, 20, 30, 40, 50),labels = c(-10, '0\nNeutral', 10, 20, 30, 40, 50),
                      expand = expansion(mult = c(0.1,0.1)))+
  scale_fill_manual(values = c( '#A87B5D', '#00cc99', '#5d8aa8'))+ #'#e52b50', 
  scale_y_discrete(breaks = c("Grüne","SP","Andere","Die Mitte","Keine Partei","glp","SVP","FDP", "Mittelwert"), 
                   labels = c("Grüne","SP","Andere","Die Mitte","Keine Partei","glp","SVP","FDP", expression(bolditalic("Mittelwert \u00F8"))))+
  theme_bw(base_size = 20)+
  theme(axis.line = element_line(color='black'),
        axis.text.y = element_text(colour="black", size = 18),
        plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.text.x = element_text(colour="black", size = 16),
        axis.title.x = element_text(size = 16),
        plot.title = element_text(size = 20))

p.Q7.2_2 

# PNG Datei erstellen und oeffnen
png(paste0(fig.path, 'Q7.2_2_gewichteterMittelwert.png'),
    width     = 3000,
    height    = 1450,
    res = 300)

# Bild speichern
plot(p.Q7.2_2)

# Device schliessen
dev.off()

#------------------------------------------------------------------------------/
# Q7.2_3 ----
## * preprocess Q7.2_3 ----

df.weighted.mean.partei.Q7.2_3 <- data_full_withweights %>%
  dplyr::mutate(Partei = ifelse(Partei_numeric == 6, 'Keine Partei', Partei)) %>%
  dplyr::mutate(Mittelwert = matrixStats::weightedMean(Q7.2_3, w = weight)) %>%
  dplyr::group_by(Partei_numeric, Partei, Mittelwert) %>%
  dplyr::summarise(Q7.2_3_median = matrixStats::weightedMedian(Q7.2_3, w = weight),
                   Q7.2_3_mean = matrixStats::weightedMean(Q7.2_3, w = weight)) %>%
  dplyr::ungroup() %>%
  dplyr::arrange(desc(Q7.2_3_mean))

data.weighted.mean.partei.Q7.2_3 <- df.weighted.mean.partei.Q7.2_3 %>%
  dplyr::bind_rows(data.frame(Partei = 'Mittelwert',
                              Q7.2_3_mean = df.weighted.mean.partei.Q7.2_3$Mittelwert[1])) %>%
  dplyr::mutate(Mittelwert = ifelse(Partei == 'Mittelwert', 0.03, 0.12))

## * visualize Q7.2_3 ----

p.Q7.2_3 <- ggplot(data.weighted.mean.partei.Q7.2_3 %>%
                     dplyr::mutate(fillcol = ifelse(Q7.2_3_mean <50, 'Ablehnung', 'Zustimmung'),
                                   fillcol = ifelse(Partei == 'Mittelwert', 'Mittelwert', fillcol)),
                   aes(x = (Q7.2_3_mean-50)*2, y = reorder(Partei, Q7.2_3_mean))) +
  labs(y = '', x= 'Zustimmungswert')+
  ggtitle(wrapper(subset(df_Qs, Frage == 'Q7.2_3')$Name , width = 64))+
  geom_col(show.legend = FALSE,
           aes(fill = factor(fillcol)),
           width = length(data.weighted.mean.partei.Q7.2_3) * data.weighted.mean.partei.Q7.2_3$Mittelwert)+
  geom_text(aes(hjust = ifelse(Q7.2_3_mean < 50, 1.2, -0.2),
                label=round((Q7.2_3_mean-50)*2, 1),
                fontface = ifelse(Partei == 'Mittelwert', 'bold.italic', 'plain')), 
            size = 6,
            show.legend = FALSE)+
  scale_x_continuous( breaks = c(-10, 0, 10, 20, 30, 40, 50),labels = c(-10, '0\nNeutral', 10, 20, 30, 40, 50),
                      expand = expansion(mult = c(0.1,0.1)))+
  scale_fill_manual(values = c( '#A87B5D', '#00cc99', '#5d8aa8'))+ #'#e52b50', 
  scale_y_discrete(breaks = c("Grüne","SP","Andere","Die Mitte","Keine Partei","glp","SVP","FDP", "Mittelwert"), 
                   labels = c("Grüne","SP","Andere","Die Mitte","Keine Partei","glp","SVP","FDP", expression(bolditalic("Mittelwert \u00F8"))))+
  theme_bw(base_size = 20)+
  theme(axis.line = element_line(color='black'),
        axis.text.y = element_text(colour="black", size = 18),
        plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.text.x = element_text(colour="black", size = 16),
        axis.title.x = element_text(size = 16),
        plot.title = element_text(size = 20))


p.Q7.2_3 

# PNG Datei erstellen und oeffnen
png(paste0(fig.path, 'Q7.2_3_gewichteterMittelwert.png'),
    width     = 3000,
    height    = 1450,
    res = 300)

# Bild speichern
plot(p.Q7.2_3)

# Device schliessen
dev.off()

#------------------------------------------------------------------------------/
# Q7.2_4 ----
## * preprocess Q7.2_4 ----

df.weighted.mean.partei.Q7.2_4 <- data_full_withweights %>%
  dplyr::mutate(Partei = ifelse(Partei_numeric == 6, 'Keine Partei', Partei)) %>%
  dplyr::mutate(Mittelwert = matrixStats::weightedMean(Q7.2_4, w = weight)) %>%
  dplyr::group_by(Partei_numeric, Partei, Mittelwert) %>%
  dplyr::summarise(Q7.2_4_median = matrixStats::weightedMedian(Q7.2_4, w = weight),
                   Q7.2_4_mean = matrixStats::weightedMean(Q7.2_4, w = weight)) %>%
  dplyr::ungroup() %>%
  dplyr::arrange(desc(Q7.2_4_mean))

data.weighted.mean.partei.Q7.2_4 <- df.weighted.mean.partei.Q7.2_4 %>%
  dplyr::bind_rows(data.frame(Partei = 'Mittelwert',
                              Q7.2_4_mean = df.weighted.mean.partei.Q7.2_4$Mittelwert[1])) %>%
  dplyr::mutate(Mittelwert = ifelse(Partei == 'Mittelwert', 0.03, 0.12))

## * visualize Q7.2_4 ----

p.Q7.2_4 <- ggplot(data.weighted.mean.partei.Q7.2_4 ,
                   aes(x = (Q7.2_4_mean-50)*2, y = reorder(Partei, Q7.2_4_mean))) +
  labs(y = '', x= 'Zustimmungswert')+
  ggtitle(wrapper(subset(df_Qs, Frage == 'Q7.2_4')$Name , width = 64))+
  geom_col(show.legend = FALSE,
           aes(fill = factor(Mittelwert)),
           width = length(data.weighted.mean.partei.Q7.2_4) * data.weighted.mean.partei.Q7.2_4$Mittelwert)+
  geom_text(aes(label=round((Q7.2_4_mean-50)*2, 1),
                fontface = ifelse(Partei == 'Mittelwert', 'bold.italic', 'plain')), 
            size = 6,
            hjust = -0.2, show.legend = FALSE)+
  scale_x_continuous( breaks = c(-10, 0, 10, 20, 30, 40, 50),labels = c(-10, '0\nNeutral', 10, 20, 30, 40, 50),
                      expand = expansion(mult = c(0.05,0.1)))+
  scale_fill_manual(values = c( '#00cc99', '#5d8aa8'))+ 
  scale_y_discrete(breaks = c("Grüne","SP","Andere","Die Mitte","Keine Partei","glp","SVP","FDP", "Mittelwert"), 
                   labels = c("Grüne","SP","Andere","Die Mitte","Keine Partei","glp","SVP","FDP", expression(bolditalic("Mittelwert \u00F8"))))+
  theme_bw(base_size = 20)+
  theme(axis.line = element_line(color='black'),
        axis.text.y = element_text(colour="black", size = 18),
        plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.text.x = element_text(colour="black", size = 16),
        axis.title.x = element_text(size = 16),
        plot.title = element_text(size = 20))

p.Q7.2_4 

# PNG Datei erstellen und oeffnen
png(paste0(fig.path, 'Q7.2_4_gewichteterMittelwert.png'),
    width     = 3000,
    height    = 1600,
    res = 300)

# Bild speichern
plot(p.Q7.2_4)

# Device schliessen
dev.off()

#------------------------------------------------------------------------------/
# Q7.3_1 ----
## * preprocess Q7.3_1 ----

df.weighted.mean.partei.Q7.3_1 <- data_full_withweights %>%
  dplyr::mutate(Partei = ifelse(Partei_numeric == 6, 'Keine Partei', Partei)) %>%
  dplyr::mutate(Mittelwert = matrixStats::weightedMean(Q7.3_1, w = weight)) %>%
  dplyr::group_by(Partei_numeric, Partei, Mittelwert) %>%
  dplyr::summarise(Q7.3_1_median = matrixStats::weightedMedian(Q7.3_1, w = weight),
                   Q7.3_1_mean = matrixStats::weightedMean(Q7.3_1, w = weight)) %>%
  dplyr::ungroup() %>%
  dplyr::arrange(desc(Q7.3_1_mean))

data.weighted.mean.partei.Q7.3_1 <- df.weighted.mean.partei.Q7.3_1 %>%
  dplyr::bind_rows(data.frame(Partei = 'Mittelwert',
                              Q7.3_1_mean = df.weighted.mean.partei.Q7.3_1$Mittelwert[1])) %>%
  dplyr::mutate(Mittelwert = ifelse(Partei == 'Mittelwert', 0.03, 0.12))

## * visualize Q7.3_1 ----

p.Q7.3_1 <- ggplot(data.weighted.mean.partei.Q7.3_1 ,
                   aes(x = (Q7.3_1_mean-50)*2, y = reorder(Partei, Q7.3_1_mean))) +
  labs(y = '', x= 'Zustimmungswert')+
  ggtitle(wrapper(subset(df_Qs, Frage == 'Q7.3_1')$Name , width = 64))+
  geom_col(show.legend = FALSE,
           aes(fill = factor(Mittelwert)),
           width = length(data.weighted.mean.partei.Q7.3_1) * data.weighted.mean.partei.Q7.3_1$Mittelwert)+
  geom_text(aes(label=round((Q7.3_1_mean-50)*2, 1),
                fontface = ifelse(Partei == 'Mittelwert', 'bold.italic', 'plain')), 
            size = 6,
            hjust = -0.2, show.legend = FALSE)+
  scale_x_continuous( breaks = c(-10, 0, 10, 20, 30, 40, 50),labels = c(-10, '0\nNeutral', 10, 20, 30, 40, 50),
                      expand = expansion(mult = c(0.05,0.1)))+
  scale_fill_manual(values = c( '#00cc99', '#5d8aa8'))+ 
  scale_y_discrete(breaks = c("Grüne","SP","Andere","Die Mitte","Keine Partei","glp","SVP","FDP", "Mittelwert"), 
                   labels = c("Grüne","SP","Andere","Die Mitte","Keine Partei","glp","SVP","FDP", expression(bolditalic("Mittelwert \u00F8"))))+
  theme_bw(base_size = 20)+
  theme(axis.line = element_line(color='black'),
        axis.text.y = element_text(colour="black", size = 18),
        plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.text.x = element_text(colour="black", size = 16),
        axis.title.x = element_text(size = 16),
        plot.title = element_text(size = 20))


p.Q7.3_1 

# PNG Datei erstellen und oeffnen
png(paste0(fig.path, 'Q7.3_1_gewichteterMittelwert.png'),
    width     = 3000,
    height    = 1450,
    res = 300)

# Bild speichern
plot(p.Q7.3_1)

# Device schliessen
dev.off()

#------------------------------------------------------------------------------/
# Q7.4_1 ----
## * preprocess Q7.4_1 ----

df.weighted.mean.partei.Q7.4_1 <- data_full_withweights %>%
  dplyr::mutate(Partei = ifelse(Partei_numeric == 6, 'Keine Partei', Partei)) %>%
  dplyr::mutate(Mittelwert = matrixStats::weightedMean(Q7.4_1, w = weight)) %>%
  dplyr::group_by(Partei_numeric, Partei, Mittelwert) %>%
  dplyr::summarise(Q7.4_1_median = matrixStats::weightedMedian(Q7.4_1, w = weight),
                   Q7.4_1_mean = matrixStats::weightedMean(Q7.4_1, w = weight)) %>%
  dplyr::ungroup() %>%
  dplyr::arrange(desc(Q7.4_1_mean))

data.weighted.mean.partei.Q7.4_1 <- df.weighted.mean.partei.Q7.4_1 %>%
  dplyr::bind_rows(data.frame(Partei = 'Mittelwert',
                              Q7.4_1_mean = df.weighted.mean.partei.Q7.4_1$Mittelwert[1])) %>%
  dplyr::mutate(Mittelwert = ifelse(Partei == 'Mittelwert', 0.03, 0.12))

## * visualize Q7.4_1 ----

p.Q7.4_1 <- ggplot(data.weighted.mean.partei.Q7.4_1 ,
                   aes(x = (Q7.4_1_mean-50)*2, y = reorder(Partei, Q7.4_1_mean))) +
  labs(y = '', x= 'Zustimmungswert')+
  ggtitle(wrapper(subset(df_Qs, Frage == 'Q7.4_1')$Name , width = 60))+
  geom_col(show.legend = FALSE,
           aes(fill = factor(Mittelwert)),
           width = length(data.weighted.mean.partei.Q7.4_1) * data.weighted.mean.partei.Q7.4_1$Mittelwert)+
  geom_text(aes(label=round((Q7.4_1_mean-50)*2, 1),
                fontface = ifelse(Partei == 'Mittelwert', 'bold.italic', 'plain')), 
            size = 6,
            hjust = -0.2, show.legend = FALSE)+
  scale_x_continuous( breaks = c(0, 20, 40, 60),labels = c('0\nNeutral', 20, 40, 60),
                      expand = expansion(mult = c(0.05,0.1)))+
  scale_fill_manual(values = c( '#00cc99', '#5d8aa8'))+ 
  scale_y_discrete(breaks = c("Grüne","SP","Andere","Die Mitte","Keine Partei","glp","SVP","FDP", "Mittelwert"), 
                   labels = c("Grüne","SP","Andere","Die Mitte","Keine Partei","glp","SVP","FDP", expression(bolditalic("Mittelwert \u00F8"))))+
  theme_bw(base_size = 20)+
  theme(axis.line = element_line(color='black'),
        axis.text.y = element_text(colour="black", size = 18),
        plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.text.x = element_text(colour="black", size = 16),
        axis.title.x = element_text(size = 16),
        plot.title = element_text(size = 20))

p.Q7.4_1 

# PNG Datei erstellen und oeffnen
png(paste0(fig.path, 'Q7.4_1_gewichteterMittelwert.png'),
    width     = 3000,
    height    = 1450,
    res = 300)

# Bild speichern
plot(p.Q7.4_1)

# Device schliessen
dev.off()

#------------------------------------------------------------------------------/
# Q8.3_1 ----
## * preprocess Q8.3_1 ----

df.weighted.mean.partei.Q8.3_1 <- data_full_withweights %>%
  dplyr::mutate(Partei = ifelse(Partei_numeric == 6, 'Keine Partei', Partei)) %>%
  dplyr::mutate(Mittelwert = matrixStats::weightedMean(Q8.3_1, w = weight)) %>%
  dplyr::group_by(Partei_numeric, Partei, Mittelwert) %>%
  dplyr::summarise(Q8.3_1_median = matrixStats::weightedMedian(Q8.3_1, w = weight),
                   Q8.3_1_mean = matrixStats::weightedMean(Q8.3_1, w = weight)) %>%
  dplyr::ungroup() %>%
  dplyr::arrange(desc(Q8.3_1_mean))

data.weighted.mean.partei.Q8.3_1 <- df.weighted.mean.partei.Q8.3_1 %>%
  dplyr::bind_rows(data.frame(Partei = 'Mittelwert',
                              Q8.3_1_mean = df.weighted.mean.partei.Q8.3_1$Mittelwert[1])) %>%
  dplyr::mutate(Mittelwert = ifelse(Partei == 'Mittelwert', 0.03, 0.12))

## * visualize Q8.3_1 ----

p.Q8.3_1 <- ggplot(data.weighted.mean.partei.Q8.3_1 %>%
                     dplyr::mutate(fillcol = ifelse(Q8.3_1_mean <50, 'Ablehnung', 'Zustimmung'),
                                   fillcol = ifelse(Partei == 'Mittelwert', 'Mittelwert', fillcol)),
                   aes(x = (Q8.3_1_mean-50)*2, y = reorder(Partei, Q8.3_1_mean))) +
  labs(y = '', x= 'Zustimmungswert')+
  ggtitle(wrapper(subset(df_Qs, Frage == 'Q8.3_1')$Name , width = 64))+
  geom_col(show.legend = FALSE,
           aes(fill = factor(fillcol)),
           width = length(data.weighted.mean.partei.Q8.3_1) * data.weighted.mean.partei.Q8.3_1$Mittelwert)+
  geom_text(aes(hjust = ifelse(Q8.3_1_mean < 50, 1.2, -0.2),
                label=round((Q8.3_1_mean-50)*2, 1),
                fontface = ifelse(Partei == 'Mittelwert', 'bold.italic', 'plain')), 
            size = 6,
            show.legend = FALSE)+
  scale_x_continuous( breaks = c(-10, 0, 10, 20, 30, 40),labels = c(-10, '0\nNeutral', 10, 20, 30, 40),
                      expand = expansion(mult = c(0.1,0.1)))+
  scale_fill_manual(values = c( '#A87B5D', '#00cc99', '#5d8aa8'))+  
  scale_y_discrete(breaks = c("Grüne","SP","Andere","Die Mitte","Keine Partei","glp","SVP","FDP", "Mittelwert"), 
                   labels = c("Grüne","SP","Andere","Die Mitte","Keine Partei","glp","SVP","FDP", expression(bolditalic("Mittelwert \u00F8"))))+
  theme_bw(base_size = 20)+
  theme(axis.line = element_line(color='black'),
        axis.text.y = element_text(colour="black", size = 18),
        plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.text.x = element_text(colour="black", size = 16),
        axis.title.x = element_text(size = 16),
        plot.title = element_text(size = 20))

p.Q8.3_1 

# PNG Datei erstellen und oeffnen
png(paste0(fig.path, 'Q8.3_1_gewichteterMittelwert.png'),
    width     = 3000,
    height    = 1300,
    res = 300)

# Bild speichern
plot(p.Q8.3_1)

# Device schliessen
dev.off()

#------------------------------------------------------------------------------/
# Q8.4_1 ----
## * preprocess Q8.4_1 ----

df.weighted.mean.partei.Q8.4_1 <- data_full_withweights %>%
  dplyr::mutate(Partei = ifelse(Partei_numeric == 6, 'Keine Partei', Partei)) %>%
  dplyr::mutate(Mittelwert = matrixStats::weightedMean(Q8.4_1, w = weight)) %>%
  dplyr::group_by(Partei_numeric, Partei, Mittelwert) %>%
  dplyr::summarise(Q8.4_1_median = matrixStats::weightedMedian(Q8.4_1, w = weight),
                   Q8.4_1_mean = matrixStats::weightedMean(Q8.4_1, w = weight)) %>%
  dplyr::ungroup() %>%
  dplyr::arrange(desc(Q8.4_1_mean))

data.weighted.mean.partei.Q8.4_1 <- df.weighted.mean.partei.Q8.4_1 %>%
  dplyr::bind_rows(data.frame(Partei = 'Mittelwert',
                              Q8.4_1_mean = df.weighted.mean.partei.Q8.4_1$Mittelwert[1])) %>%
  dplyr::mutate(Mittelwert = ifelse(Partei == 'Mittelwert', 0.03, 0.12))

## * visualize Q8.4_1 ----

p.Q8.4_1 <- ggplot(data.weighted.mean.partei.Q8.4_1 %>%
                     dplyr::mutate(fillcol = ifelse(Q8.4_1_mean <50, 'Ablehnung', 'Zustimmung'),
                                   fillcol = ifelse(Partei == 'Mittelwert', 'Mittelwert', fillcol)),
                   aes(x = (Q8.4_1_mean-50)*2, y = reorder(Partei, Q8.4_1_mean))) +
  labs(y = '', x= 'Zustimmungswert')+
  ggtitle(wrapper(subset(df_Qs, Frage == 'Q8.4_1')$Name , width = 64))+
  geom_col(show.legend = FALSE,
           aes(fill = factor(fillcol)),
           width = length(data.weighted.mean.partei.Q8.4_1) * data.weighted.mean.partei.Q8.4_1$Mittelwert)+
  geom_text(aes(hjust = ifelse(Q8.4_1_mean < 50, 1.2, -0.2),
                label=round((Q8.4_1_mean-50)*2, 1),
                fontface = ifelse(Partei == 'Mittelwert', 'bold.italic', 'plain')), 
            size = 6,
            show.legend = FALSE)+
  scale_x_continuous( breaks = c(-40, -30, -20, -10, 0, 10, 20, 30, 40),labels = c(-40, -30, -20, -10, '0\nNeutral', 10, 20, 30, 40),
                      expand = expansion(mult = c(0.1,0.1)))+
  scale_fill_manual(values = c( '#A87B5D', '#cc0033', '#5d8aa8'))+  
  scale_y_discrete(breaks = c("Grüne","SP","Andere","Die Mitte","Keine Partei","glp","SVP","FDP", "Mittelwert"), 
                   labels = c("Grüne","SP","Andere","Die Mitte","Keine Partei","glp","SVP","FDP", expression(bolditalic("Mittelwert \u00F8"))))+
  theme_bw(base_size = 20)+
  theme(axis.line = element_line(color='black'),
        axis.text.y = element_text(colour="black", size = 18),
        plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.text.x = element_text(colour="black", size = 16),
        axis.title.x = element_text(size = 16),
        plot.title = element_text(size = 20))

p.Q8.4_1 

# PNG Datei erstellen und oeffnen
png(paste0(fig.path, 'Q8.4_1_gewichteterMittelwert.png'),
    width     = 3000,
    height    = 1450,
    res = 300)

# Bild speichern
plot(p.Q8.4_1)

# Device schliessen
dev.off()

#------------------------------------------------------------------------------/
# Q8.5_1 ----
## * preprocess Q8.5_1 ----

df.weighted.mean.partei.Q8.5_1 <- data_full_withweights %>%
  dplyr::mutate(Partei = ifelse(Partei_numeric == 6, 'Keine Partei', Partei)) %>%
  dplyr::mutate(Mittelwert = matrixStats::weightedMean(Q8.5_1, w = weight)) %>%
  dplyr::group_by(Partei_numeric, Partei, Mittelwert) %>%
  dplyr::summarise(Q8.5_1_median = matrixStats::weightedMedian(Q8.5_1, w = weight),
                   Q8.5_1_mean = matrixStats::weightedMean(Q8.5_1, w = weight)) %>%
  dplyr::ungroup() %>%
  dplyr::arrange(desc(Q8.5_1_mean))

data.weighted.mean.partei.Q8.5_1 <- df.weighted.mean.partei.Q8.5_1 %>%
  dplyr::bind_rows(data.frame(Partei = 'Mittelwert',
                              Q8.5_1_mean = df.weighted.mean.partei.Q8.5_1$Mittelwert[1])) %>%
  dplyr::mutate(Mittelwert = ifelse(Partei == 'Mittelwert', 0.03, 0.12))

## * visualize Q8.5_1 ----

p.Q8.5_1 <- ggplot(data.weighted.mean.partei.Q8.5_1 %>%
                     dplyr::mutate(fillcol = ifelse(Q8.5_1_mean <50, 'Ablehnung', 'Zustimmung'),
                                   fillcol = ifelse(Partei == 'Mittelwert', 'Mittelwert', fillcol)),
                   aes(x = (Q8.5_1_mean-50)*2, y = reorder(Partei, Q8.5_1_mean))) +
  labs(y = '', x= 'Zustimmungswert')+
  ggtitle(wrapper(subset(df_Qs, Frage == 'Q8.5_1')$Name , width = 64))+
  geom_col(show.legend = FALSE,
           aes(fill = factor(fillcol)),
           width = length(data.weighted.mean.partei.Q8.5_1) * data.weighted.mean.partei.Q8.5_1$Mittelwert)+
  geom_text(aes(hjust = ifelse(Q8.5_1_mean < 50, 1.2, -0.2),
                label=round((Q8.5_1_mean-50)*2, 1),
                fontface = ifelse(Partei == 'Mittelwert', 'bold.italic', 'plain')), 
            size = 6,
            show.legend = FALSE)+
  scale_x_continuous( breaks = c(-40, -30, -20, -10, 0, 10, 20, 30, 40),labels = c(-40, -30, -20, -10, '0\nNeutral', 10, 20, 30, 40),
                      expand = expansion(mult = c(0.15,0.1)))+
  scale_fill_manual(values = c( '#A87B5D', '#cc0033', '#5d8aa8'))+  
  scale_y_discrete(breaks = c("Grüne","SP","Andere","Die Mitte","Keine Partei","glp","SVP","FDP", "Mittelwert"), 
                   labels = c("Grüne","SP","Andere","Die Mitte","Keine Partei","glp","SVP","FDP", expression(bolditalic("Mittelwert \u00F8"))))+
  theme_bw(base_size = 20)+
  theme(axis.line = element_line(color='black'),
        axis.text.y = element_text(colour="black", size = 18),
        plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.text.x = element_text(colour="black", size = 16),
        axis.title.x = element_text(size = 16),
        plot.title = element_text(size = 20))

p.Q8.5_1

# PNG Datei erstellen und oeffnen
png(paste0(fig.path, 'Q8.5_1_gewichteterMittelwert.png'),
    width     = 3000,
    height    = 1300,
    res = 300)

# Bild speichern
plot(p.Q8.5_1)

# Device schliessen
dev.off()

#------------------------------------------------------------------------------/
# Q8.6_1 ----
## * preprocess Q8.6_1 ----

df.weighted.mean.partei.Q8.6_1 <- data_full_withweights %>%
  dplyr::mutate(Partei = ifelse(Partei_numeric == 6, 'Keine Partei', Partei)) %>%
  dplyr::mutate(Mittelwert = matrixStats::weightedMean(Q8.6_1, w = weight)) %>%
  dplyr::group_by(Partei_numeric, Partei, Mittelwert) %>%
  dplyr::summarise(Q8.6_1_median = matrixStats::weightedMedian(Q8.6_1, w = weight),
                   Q8.6_1_mean = matrixStats::weightedMean(Q8.6_1, w = weight)) %>%
  dplyr::ungroup() %>%
  dplyr::arrange(desc(Q8.6_1_mean))

data.weighted.mean.partei.Q8.6_1 <- df.weighted.mean.partei.Q8.6_1 %>%
  dplyr::bind_rows(data.frame(Partei = 'Mittelwert',
                              Q8.6_1_mean = df.weighted.mean.partei.Q8.6_1$Mittelwert[1])) %>%
  dplyr::mutate(Mittelwert = ifelse(Partei == 'Mittelwert', 0.03, 0.12))

## * visualize Q8.6_1 ----

p.Q8.6_1 <- ggplot(data.weighted.mean.partei.Q8.6_1%>%
                     dplyr::mutate(fillcol = ifelse(Q8.6_1_mean <50, 'Ablehnung', 'Zustimmung'),
                                   fillcol = ifelse(Partei == 'Mittelwert', 'Mittelwert', fillcol)) ,
                   aes(x = (Q8.6_1_mean-50)*2, y = reorder(Partei, Q8.6_1_mean))) +
  labs(y = '', x= 'Zustimmungswert')+
  ggtitle(wrapper(subset(df_Qs, Frage == 'Q8.6_1')$Name , width = 64))+
  geom_col(show.legend = FALSE,
           aes(fill = factor(fillcol)),
           width = length(data.weighted.mean.partei.Q8.6_1) * data.weighted.mean.partei.Q8.6_1$Mittelwert)+
  geom_text(aes(hjust = ifelse(Q8.6_1_mean< 50, 1.2, -0.2),
                label=round((Q8.6_1_mean-50)*2, 1),
                fontface = ifelse(Partei == 'Mittelwert', 'bold.italic', 'plain')), 
            size = 6,
            show.legend = FALSE)+
  scale_x_continuous( breaks = c(-40, -30, -20, -10, 0, 10, 20, 30, 40),labels = c(-40, -30, -20, -10, '0\nNeutral', 10, 20, 30, 40),
                      expand = expansion(mult = c(0.15,0.1)))+
  scale_fill_manual(values = c( '#A87B5D', '#00CC99', '#5d8aa8'))+  
  scale_y_discrete(breaks = c("Grüne","SP","Andere","Die Mitte","Keine Partei","glp","SVP","FDP", "Mittelwert"), 
                   labels = c("Grüne","SP","Andere","Die Mitte","Keine Partei","glp","SVP","FDP", expression(bolditalic("Mittelwert \u00F8"))))+
  theme_bw(base_size = 20)+
  theme(axis.line = element_line(color='black'),
        axis.text.y = element_text(colour="black", size = 18),
        plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.text.x = element_text(colour="black", size = 16),
        axis.title.x = element_text(size = 16),
        plot.title = element_text(size = 20))

p.Q8.6_1 

# PNG Datei erstellen und oeffnen
png(paste0(fig.path, 'Q8.6_1_gewichteterMittelwert.png'),
    width     = 3000,
    height    = 1450,
    res = 300)

# Bild speichern
plot(p.Q8.6_1)

# Device schliessen
dev.off()

#------------------------------------------------------------------------------/
# Q8.7_1 ----
## * preprocess Q8.7_1 ----

df.weighted.mean.partei.Q8.7_1 <- data_full_withweights %>%
  dplyr::mutate(Partei = ifelse(Partei_numeric == 6, 'Keine Partei', Partei)) %>%
  dplyr::mutate(Mittelwert = matrixStats::weightedMean(Q8.7_1, w = weight)) %>%
  dplyr::group_by(Partei_numeric, Partei, Mittelwert) %>%
  dplyr::summarise(Q8.7_1_median = matrixStats::weightedMedian(Q8.7_1, w = weight),
                   Q8.7_1_mean = matrixStats::weightedMean(Q8.7_1, w = weight)) %>%
  dplyr::ungroup() %>%
  dplyr::arrange(desc(Q8.7_1_mean))

data.weighted.mean.partei.Q8.7_1 <- df.weighted.mean.partei.Q8.7_1 %>%
  dplyr::bind_rows(data.frame(Partei = 'Mittelwert',
                              Q8.7_1_mean = df.weighted.mean.partei.Q8.7_1$Mittelwert[1])) %>%
  dplyr::mutate(Mittelwert = ifelse(Partei == 'Mittelwert', 0.03, 0.12))

## * visualize Q8.7_1 ----

p.Q8.7_1 <- ggplot(data.weighted.mean.partei.Q8.7_1%>%
                     dplyr::mutate(fillcol = ifelse(Q8.7_1_mean <50, 'Ablehnung', 'Zustimmung'),
                                   fillcol = ifelse(Partei == 'Mittelwert', 'Mittelwert', fillcol)) ,
                   aes(x = (Q8.7_1_mean-50)*2, y = reorder(Partei, Q8.7_1_mean))) +
  labs(y = '', x= 'Zustimmungswert')+
  ggtitle(wrapper(subset(df_Qs, Frage == 'Q8.7_1')$Name , width = 64))+
  geom_col(show.legend = FALSE,
           aes(fill = factor(fillcol)),
           width = length(data.weighted.mean.partei.Q8.7_1) * data.weighted.mean.partei.Q8.7_1$Mittelwert)+
  geom_text(aes(hjust = ifelse(Q8.7_1_mean< 50, 1.2, -0.2),
                label=round((Q8.7_1_mean-50)*2, 1),
                fontface = ifelse(Partei == 'Mittelwert', 'bold.italic', 'plain')), 
            size = 6,
            show.legend = FALSE)+
  scale_x_continuous( breaks = c(-40, -30, -20, -10, 0, 10, 20, 30, 40),labels = c(-40, -30, -20, -10, '0\nNeutral', 10, 20, 30, 40),
                      expand = expansion(mult = c(0.15 ,0.1)))+
  scale_fill_manual(values = c( '#A87B5D', '#00CC99', '#5d8aa8'))+  
  scale_y_discrete(breaks = c("Grüne","SP","Andere","Die Mitte","Keine Partei","glp","SVP","FDP", "Mittelwert"), 
                   labels = c("Grüne","SP","Andere","Die Mitte","Keine Partei","glp","SVP","FDP", expression(bolditalic("Mittelwert \u00F8"))))+
  theme_bw(base_size = 20)+
  theme(axis.line = element_line(color='black'),
        axis.text.y = element_text(colour="black", size = 18),
        plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.text.x = element_text(colour="black", size = 16),
        axis.title.x = element_text(size = 16),
        plot.title = element_text(size = 20))

p.Q8.7_1 

# PNG Datei erstellen und oeffnen
png(paste0(fig.path, 'Q8.7_1_gewichteterMittelwert.png'),
    width     = 3000,
    height    = 1450,
    res = 300)

# Bild speichern
plot(p.Q8.7_1)

# Device schliessen
dev.off()

#------------------------------------------------------------------------------/
# Q8.8_1 ----
## * preprocess Q8.8_1 ----

df.weighted.mean.partei.Q8.8_1 <- data_full_withweights %>%
  dplyr::mutate(Partei = ifelse(Partei_numeric == 6, 'Keine Partei', Partei)) %>%
  dplyr::mutate(Mittelwert = matrixStats::weightedMean(Q8.8_1, w = weight)) %>%
  dplyr::group_by(Partei_numeric, Partei, Mittelwert) %>%
  dplyr::summarise(Q8.8_1_median = matrixStats::weightedMedian(Q8.8_1, w = weight),
                   Q8.8_1_mean = matrixStats::weightedMean(Q8.8_1, w = weight)) %>%
  dplyr::ungroup() %>%
  dplyr::arrange(desc(Q8.8_1_mean))

data.weighted.mean.partei.Q8.8_1 <- df.weighted.mean.partei.Q8.8_1 %>%
  dplyr::bind_rows(data.frame(Partei = 'Mittelwert',
                              Q8.8_1_mean = df.weighted.mean.partei.Q8.8_1$Mittelwert[1])) %>%
  dplyr::mutate(Mittelwert = ifelse(Partei == 'Mittelwert', 0.03, 0.12))

## * visualize Q8.8_1 ----

p.Q8.8_1 <- ggplot(data.weighted.mean.partei.Q8.8_1%>%
                     dplyr::mutate(fillcol = ifelse(Q8.8_1_mean <50, 'Ablehnung', 'Zustimmung'),
                                   fillcol = ifelse(Partei == 'Mittelwert', 'Mittelwert', fillcol)) ,
                   aes(x =(Q8.8_1_mean-50)*2, y = reorder(Partei, Q8.8_1_mean))) +
  labs(y = '', x= 'Zustimmungswert')+
  ggtitle(wrapper(subset(df_Qs, Frage == 'Q8.8_1')$Name , width = 64))+
  geom_col(show.legend = FALSE,
           aes(fill = factor(fillcol)),
           width = length(data.weighted.mean.partei.Q8.8_1) * data.weighted.mean.partei.Q8.8_1$Mittelwert)+
  geom_text(aes(hjust = ifelse(Q8.8_1_mean< 50, 1.2, -0.2),
                label=round((Q8.8_1_mean-50)*2, 1),
                fontface = ifelse(Partei == 'Mittelwert', 'bold.italic', 'plain')), 
            size = 6,
            show.legend = FALSE)+
  scale_x_continuous( breaks = c(-40, -30, -20, -10, 0, 10, 20, 30, 40),labels = c(-40, -30, -20, -10, '0\nNeutral', 10, 20, 30, 40),
                      expand = expansion(mult = c(0.15 ,0.1)))+
  scale_fill_manual(values = c( '#A87B5D', '#00CC99', '#5d8aa8'))+  
  scale_y_discrete(breaks = c("Grüne","SP","Andere","Die Mitte","Keine Partei","glp","SVP","FDP", "Mittelwert"), 
                   labels = c("Grüne","SP","Andere","Die Mitte","Keine Partei","glp","SVP","FDP", expression(bolditalic("Mittelwert \u00F8"))))+
  theme_bw(base_size = 20)+
  theme(axis.line = element_line(color='black'),
        axis.text.y = element_text(colour="black", size = 18),
        plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.text.x = element_text(colour="black", size = 16),
        axis.title.x = element_text(size = 16),
        plot.title = element_text(size = 20))

p.Q8.8_1 

# PNG Datei erstellen und oeffnen
png(paste0(fig.path, 'Q8.8_1_gewichteterMittelwert.png'),
    width     = 3000,
    height    = 1450,
    res = 300)

# Bild speichern
plot(p.Q8.8_1)

# Device schliessen
dev.off()

#------------------------------------------------------------------------------/
# Q9.3_1 ----
## * preprocess Q9.3_1 ----

df.weighted.mean.partei.Q9.3_1 <- data_full_withweights %>%
  dplyr::mutate(Partei = ifelse(Partei_numeric == 6, 'Keine Partei', Partei)) %>%
  dplyr::mutate(Mittelwert = matrixStats::weightedMean(Q9.3_1, w = weight)) %>%
  dplyr::group_by(Partei_numeric, Partei, Mittelwert) %>%
  dplyr::summarise(Q9.3_1_median = matrixStats::weightedMedian(Q9.3_1, w = weight),
                   Q9.3_1_mean = matrixStats::weightedMean(Q9.3_1, w = weight)) %>%
  dplyr::ungroup() %>%
  dplyr::arrange(desc(Q9.3_1_mean))

data.weighted.mean.partei.Q9.3_1 <- df.weighted.mean.partei.Q9.3_1 %>%
  dplyr::bind_rows(data.frame(Partei = 'Mittelwert',
                              Q9.3_1_mean = df.weighted.mean.partei.Q9.3_1$Mittelwert[1])) %>%
  dplyr::mutate(Mittelwert = ifelse(Partei == 'Mittelwert', 0.03, 0.12))

## * visualize Q9.3_1 ----

p.Q9.3_1 <- ggplot(data.weighted.mean.partei.Q9.3_1 ,
                   aes(x = (Q9.3_1_mean-50)*2, y = reorder(Partei, Q9.3_1_mean))) +
  labs(y = '', x= 'Zustimmungswert')+
  ggtitle(wrapper(subset(df_Qs, Frage == 'Q9.3_1')$Name , width = 64))+
  geom_col(show.legend = FALSE,
           aes(fill = factor(Mittelwert)),
           width = length(data.weighted.mean.partei.Q9.3_1) * data.weighted.mean.partei.Q9.3_1$Mittelwert)+
  geom_text(aes(label=round((Q9.3_1_mean-50)*2, 1),
                fontface = ifelse(Partei == 'Mittelwert', 'bold.italic', 'plain')), 
            size = 6,
            hjust = -0.2, show.legend = FALSE)+
  scale_x_continuous( breaks = c(0, 20, 40, 60, 80, 100),labels = c('0\nNeutral', 20, 40, 60, 80, 100),
                      expand = expansion(mult = c(0.05,0.1)))+
  scale_fill_manual(values = c( '#00cc99', '#5d8aa8'))+ #'#e52b50', 
  scale_y_discrete(breaks = c("Grüne","SP","Andere","Die Mitte","Keine Partei","glp","SVP","FDP", "Mittelwert"), 
                   labels = c("Grüne","SP","Andere","Die Mitte","Keine Partei","glp","SVP","FDP", expression(bolditalic("Mittelwert \u00F8"))))+
  theme_bw(base_size = 20)+
  theme(axis.line = element_line(color='black'),
        axis.text.y = element_text(colour="black", size = 18),
        plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.text.x = element_text(colour="black", size = 16),
        axis.title.x = element_text(size = 16),
        plot.title = element_text(size = 20))

p.Q9.3_1 

# PNG Datei erstellen und oeffnen
png(paste0(fig.path, 'Q9.3_1_gewichteterMittelwert.png'),
    width     = 3000,
    height    = 1450,
    res = 300)

# Bild speichern
plot(p.Q9.3_1)

# Device schliessen
dev.off()

#------------------------------------------------------------------------------/
# Q9.4_1 ----
## * preprocess Q9.4_1 ----

df.weighted.mean.partei.Q9.4_1 <- data_full_withweights %>%
  dplyr::mutate(Partei = ifelse(Partei_numeric == 6, 'Keine Partei', Partei)) %>%
  dplyr::mutate(Mittelwert = matrixStats::weightedMean(Q9.4_1, w = weight)) %>%
  dplyr::group_by(Partei_numeric, Partei, Mittelwert) %>%
  dplyr::summarise(Q9.4_1_median = matrixStats::weightedMedian(Q9.4_1, w = weight),
                   Q9.4_1_mean = matrixStats::weightedMean(Q9.4_1, w = weight)) %>%
  dplyr::ungroup() %>%
  dplyr::arrange(desc(Q9.4_1_mean))

data.weighted.mean.partei.Q9.4_1 <- df.weighted.mean.partei.Q9.4_1 %>%
  dplyr::bind_rows(data.frame(Partei = 'Mittelwert',
                              Q9.4_1_mean = df.weighted.mean.partei.Q9.4_1$Mittelwert[1])) %>%
  dplyr::mutate(Mittelwert = ifelse(Partei == 'Mittelwert', 0.03, 0.12))

## * visualize Q9.4_1 ----

p.Q9.4_1 <- ggplot(data.weighted.mean.partei.Q9.4_1 ,
                   aes(x = (Q9.4_1_mean-50)*2, y = reorder(Partei, Q9.4_1_mean))) +
  labs(y = '', x= 'Zustimmungswert')+
  ggtitle(wrapper(subset(df_Qs, Frage == 'Q9.4_1')$Name , width = 64))+
  geom_col(show.legend = FALSE,
           aes(fill = factor(Mittelwert)),
           width = length(data.weighted.mean.partei.Q9.4_1) * data.weighted.mean.partei.Q9.4_1$Mittelwert)+
  geom_text(aes(label=round((Q9.4_1_mean-50)*2, 1),
                fontface = ifelse(Partei == 'Mittelwert', 'bold.italic', 'plain')), 
            size = 6,
            hjust = -0.2, show.legend = FALSE)+
  scale_x_continuous( breaks = c(0, 20, 40, 60, 80, 100),labels = c('0\nNeutral', 20, 40, 60, 80, 100),
                      expand = expansion(mult = c(0.05,0.1)))+
  scale_fill_manual(values = c( '#00cc99', '#5d8aa8'))+ #'#e52b50', 
  scale_y_discrete(breaks = c("Grüne","SP","Andere","Die Mitte","Keine Partei","glp","SVP","FDP", "Mittelwert"), 
                   labels = c("Grüne","SP","Andere","Die Mitte","Keine Partei","glp","SVP","FDP", expression(bolditalic("Mittelwert \u00F8"))))+
  theme_bw(base_size = 20)+
  theme(axis.line = element_line(color='black'),
        axis.text.y = element_text(colour="black", size = 18),
        plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.text.x = element_text(colour="black", size = 16),
        axis.title.x = element_text(size = 16),
        plot.title = element_text(size = 20))

p.Q9.4_1 

# PNG Datei erstellen und oeffnen
png(paste0(fig.path, 'Q9.4_1_gewichteterMittelwert.png'),
    width     = 3000,
    height    = 1450,
    res = 300)

# Bild speichern
plot(p.Q9.4_1)

# Device schliessen
dev.off()

#------------------------------------------------------------------------------/
# Q9.5_1 ----
## * preprocess Q9.5_1 ----

df.weighted.mean.partei.Q9.5_1 <- data_full_withweights %>%
  dplyr::mutate(Partei = ifelse(Partei_numeric == 6, 'Keine Partei', Partei)) %>%
  dplyr::mutate(Mittelwert = matrixStats::weightedMean(Q9.5_1, w = weight)) %>%
  dplyr::group_by(Partei_numeric, Partei, Mittelwert) %>%
  dplyr::summarise(Q9.5_1_median = matrixStats::weightedMedian(Q9.5_1, w = weight),
                   Q9.5_1_mean = matrixStats::weightedMean(Q9.5_1, w = weight)) %>%
  dplyr::ungroup() %>%
  dplyr::arrange(desc(Q9.5_1_mean))

data.weighted.mean.partei.Q9.5_1 <- df.weighted.mean.partei.Q9.5_1 %>%
  dplyr::bind_rows(data.frame(Partei = 'Mittelwert',
                              Q9.5_1_mean = df.weighted.mean.partei.Q9.5_1$Mittelwert[1])) %>%
  dplyr::mutate(Mittelwert = ifelse(Partei == 'Mittelwert', 0.03, 0.12))

## * visualize Q9.5_1 ----

p.Q9.5_1 <- ggplot(data.weighted.mean.partei.Q9.5_1 ,
                   aes(x = (Q9.5_1_mean-50)*2, y = reorder(Partei, Q9.5_1_mean))) +
  labs(y = '', x= 'Zustimmungswert')+
  ggtitle(wrapper(subset(df_Qs, Frage == 'Q9.5_1')$Name , width = 64))+
  geom_col(show.legend = FALSE,
           aes(fill = factor(Mittelwert)),
           width = length(data.weighted.mean.partei.Q9.5_1) * data.weighted.mean.partei.Q9.5_1$Mittelwert)+
  geom_text(aes(label=round((Q9.5_1_mean-50)*2, 1),
                fontface = ifelse(Partei == 'Mittelwert', 'bold.italic', 'plain')), 
            size = 6,
            hjust = -0.2, show.legend = FALSE)+
  scale_x_continuous( breaks = c(0, 20, 40, 60, 80, 100),labels = c('0\nNeutral', 20, 40, 60, 80, 100),
                      expand = expansion(mult = c(0.05,0.1)))+
  scale_fill_manual(values = c( '#00cc99', '#5d8aa8'))+ #'#e52b50', 
  scale_y_discrete(breaks = c("Grüne","SP","Andere","Die Mitte","Keine Partei","glp","SVP","FDP", "Mittelwert"), 
                   labels = c("Grüne","SP","Andere","Die Mitte","Keine Partei","glp","SVP","FDP", expression(bolditalic("Mittelwert \u00F8"))))+
  theme_bw(base_size = 20)+
  theme(axis.line = element_line(color='black'),
        axis.text.y = element_text(colour="black", size = 18),
        plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.text.x = element_text(colour="black", size = 16),
        axis.title.x = element_text(size = 16),
        plot.title = element_text(size = 20))

p.Q9.5_1 

# PNG Datei erstellen und oeffnen
png(paste0(fig.path, 'Q9.5_1_gewichteterMittelwert.png'),
    width     = 3000,
    height    = 1450,
    res = 300)

# Bild speichern
plot(p.Q9.5_1)

# Device schliessen
dev.off()

#------------------------------------------------------------------------------/
# Q10.2_1 ----
## * preprocess Q10.2_1 ----

df.weighted.mean.partei.Q10.2_1 <- data_full_withweights %>%
  dplyr::mutate(Partei = ifelse(Partei_numeric == 6, 'Keine Partei', Partei)) %>%
  dplyr::mutate(Mittelwert = matrixStats::weightedMean(Q10.2_1, w = weight)) %>%
  dplyr::group_by(Partei_numeric, Partei, Mittelwert) %>%
  dplyr::summarise(Q10.2_1_median = matrixStats::weightedMedian(Q10.2_1, w = weight),
                   Q10.2_1_mean = matrixStats::weightedMean(Q10.2_1, w = weight)) %>%
  dplyr::ungroup() %>%
  dplyr::arrange(desc(Q10.2_1_mean))

data.weighted.mean.partei.Q10.2_1 <- df.weighted.mean.partei.Q10.2_1 %>%
  dplyr::bind_rows(data.frame(Partei = 'Mittelwert',
                              Q10.2_1_mean = df.weighted.mean.partei.Q10.2_1$Mittelwert[1])) %>%
  dplyr::mutate(Mittelwert = ifelse(Partei == 'Mittelwert', 0.03, 0.12))

## * visualize Q10.2_1 ----

p.Q10.2_1 <- ggplot(data.weighted.mean.partei.Q10.2_1%>%
                      dplyr::mutate(fillcol = ifelse(Q10.2_1_mean <50, 'Ablehnung', 'Zustimmung'),
                                    fillcol = ifelse(Partei == 'Mittelwert', 'Mittelwert', fillcol)) ,
                   aes(x = (Q10.2_1_mean-50)*2, y = reorder(Partei, Q10.2_1_mean))) +
  labs(y = '', x= 'Zustimmungswert')+
  ggtitle(wrapper(subset(df_Qs, Frage == 'Q10.2_1')$Name , width = 64))+
  geom_col(show.legend = FALSE,
           aes(fill = factor(fillcol)),
           width = length(data.weighted.mean.partei.Q10.2_1) * data.weighted.mean.partei.Q10.2_1$Mittelwert)+
  geom_text(aes(hjust = ifelse( Q10.2_1_mean < 50, 1.2, -0.2),
                label=round(( Q10.2_1_mean-50)*2, 1),
                fontface = ifelse(Partei == 'Mittelwert', 'bold.italic', 'plain')), 
            size = 6,
            show.legend = FALSE)+
  scale_x_continuous( breaks = c(-20, 0, 20, 40),labels = c(-20, '0\nNeutral', 20,  40),
                      expand = expansion(mult = c(0.15 ,0.1)))+
  scale_fill_manual(values = c( '#A87B5D', '#00cc99', '#5d8aa8'))+ #'#e52b50', 
  scale_y_discrete(breaks = c("Grüne","SP","Andere","Die Mitte","Keine Partei","glp","SVP","FDP", "Mittelwert"), 
                   labels = c("Grüne","SP","Andere","Die Mitte","Keine Partei","glp","SVP","FDP", expression(bolditalic("Mittelwert \u00F8"))))+
  theme_bw(base_size = 20)+
  theme(axis.line = element_line(color='black'),
        axis.text.y = element_text(colour="black", size = 18),
        plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.text.x = element_text(colour="black", size = 16),
        axis.title.x = element_text(size = 16),
        plot.title = element_text(size = 20))

p.Q10.2_1 

# PNG Datei erstellen und oeffnen
png(paste0(fig.path, 'Q10.2_1_gewichteterMittelwert.png'),
    width     = 3000,
    height    = 1450,
    res = 300)

# Bild speichern
plot(p.Q10.2_1)

# Device schliessen
dev.off()

#------------------------------------------------------------------------------/
# Q10.5_1 ----
## * preprocess Q10.5_1 ----

df.weighted.mean.partei.Q10.5_1 <- data_full_withweights %>%
  dplyr::mutate(Partei = ifelse(Partei_numeric == 6, 'Keine Partei', Partei)) %>%
  dplyr::mutate(Mittelwert = matrixStats::weightedMean(Q10.5_1, w = weight)) %>%
  dplyr::group_by(Partei_numeric, Partei, Mittelwert) %>%
  dplyr::summarise(Q10.5_1_median = matrixStats::weightedMedian(Q10.5_1, w = weight),
                   Q10.5_1_mean = matrixStats::weightedMean(Q10.5_1, w = weight)) %>%
  dplyr::ungroup() %>%
  dplyr::arrange(desc(Q10.5_1_mean))

data.weighted.mean.partei.Q10.5_1 <- df.weighted.mean.partei.Q10.5_1 %>%
  dplyr::bind_rows(data.frame(Partei = 'Mittelwert',
                              Q10.5_1_mean = df.weighted.mean.partei.Q10.5_1$Mittelwert[1])) %>%
  dplyr::mutate(Mittelwert = ifelse(Partei == 'Mittelwert', 0.03, 0.12))

## * visualize Q10.5_1 ----

p.Q10.5_1 <- ggplot(data.weighted.mean.partei.Q10.5_1 ,
                   aes(x = (Q10.5_1_mean-50)*2, y = reorder(Partei, Q10.5_1_mean))) +
  labs(y = '', x= 'Zustimmungswert')+
  ggtitle(wrapper(subset(df_Qs, Frage == 'Q10.5_1')$Name , width = 64))+
  geom_col(show.legend = FALSE,
           aes(fill = factor(Mittelwert)),
           width = length(data.weighted.mean.partei.Q10.5_1) * data.weighted.mean.partei.Q10.5_1$Mittelwert)+
  geom_text(aes(label=round((Q10.5_1_mean-50)*2, 1),
                fontface = ifelse(Partei == 'Mittelwert', 'bold.italic', 'plain')), 
            size = 6,
            hjust = -0.2, show.legend = FALSE)+
  scale_x_continuous( breaks = c(0, 20, 40, 60, 80, 100),labels = c('0\nNeutral', 20, 40, 60, 80, 100),
                      expand = expansion(mult = c(0.05,0.1)))+
  scale_fill_manual(values = c( '#00cc99', '#5d8aa8'))+ #'#e52b50', 
  scale_y_discrete(breaks = c("Grüne","SP","Andere","Die Mitte","Keine Partei","glp","SVP","FDP", "Mittelwert"), 
                   labels = c("Grüne","SP","Andere","Die Mitte","Keine Partei","glp","SVP","FDP", expression(bolditalic("Mittelwert \u00F8"))))+
  theme_bw(base_size = 20)+
  theme(axis.line = element_line(color='black'),
        axis.text.y = element_text(colour="black", size = 18),
        plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.text.x = element_text(colour="black", size = 16),
        axis.title.x = element_text(size = 16),
        plot.title = element_text(size = 20))

p.Q10.5_1 

# PNG Datei erstellen und oeffnen
png(paste0(fig.path, 'Q10.5_1_gewichteterMittelwert.png'),
    width     = 3000,
    height    = 1450,
    res = 300)

# Bild speichern
plot(p.Q10.5_1)

# Device schliessen
dev.off()

#------------------------------------------------------------------------------/
# Q10.6_1 ----
## * preprocess Q10.6_1 ----

df.weighted.mean.partei.Q10.6_1 <- data_full_withweights %>%
  dplyr::mutate(Partei = ifelse(Partei_numeric == 6, 'Keine Partei', Partei)) %>%
  dplyr::mutate(Mittelwert = matrixStats::weightedMean(Q10.6_1, w = weight)) %>%
  dplyr::group_by(Partei_numeric, Partei, Mittelwert) %>%
  dplyr::summarise(Q10.6_1_median = matrixStats::weightedMedian(Q10.6_1, w = weight),
                   Q10.6_1_mean = matrixStats::weightedMean(Q10.6_1, w = weight)) %>%
  dplyr::ungroup() %>%
  dplyr::arrange(desc(Q10.6_1_mean))

data.weighted.mean.partei.Q10.6_1 <- df.weighted.mean.partei.Q10.6_1 %>%
  dplyr::bind_rows(data.frame(Partei = 'Mittelwert',
                              Q10.6_1_mean = df.weighted.mean.partei.Q10.6_1$Mittelwert[1])) %>%
  dplyr::mutate(Mittelwert = ifelse(Partei == 'Mittelwert', 0.03, 0.12))

## * visualize Q10.6_1 ----

p.Q10.6_1 <- ggplot(data.weighted.mean.partei.Q10.6_1 ,
                   aes(x = (Q10.6_1_mean-50)*2, y = reorder(Partei, Q10.6_1_mean))) +
  labs(y = '', x= 'Zustimmungswert')+
  ggtitle(wrapper(subset(df_Qs, Frage == 'Q10.6_1')$Name , width = 64))+
  geom_col(show.legend = FALSE,
           aes(fill = factor(Mittelwert)),
           width = length(data.weighted.mean.partei.Q10.6_1) * data.weighted.mean.partei.Q10.6_1$Mittelwert)+
  geom_text(aes(label=round((Q10.6_1_mean-50)*2, 1),
                fontface = ifelse(Partei == 'Mittelwert', 'bold.italic', 'plain')), 
            size = 6,
            hjust = -0.2, show.legend = FALSE)+
  scale_x_continuous( breaks = c(0, 20, 40, 60, 80, 100),labels = c('0\nNeutral', 20, 40, 60, 80, 100),
                      expand = expansion(mult = c(0.05,0.1)))+
  scale_fill_manual(values = c( '#00cc99', '#5d8aa8'))+ #'#e52b50', 
  scale_y_discrete(breaks = c("Grüne","SP","Andere","Die Mitte","Keine Partei","glp","SVP","FDP", "Mittelwert"), 
                   labels = c("Grüne","SP","Andere","Die Mitte","Keine Partei","glp","SVP","FDP", expression(bolditalic("Mittelwert \u00F8"))))+
  theme_bw(base_size = 20)+
  theme(axis.line = element_line(color='black'),
        axis.text.y = element_text(colour="black", size = 18),
        plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.text.x = element_text(colour="black", size = 16),
        axis.title.x = element_text(size = 16),
        plot.title = element_text(size = 20))

p.Q10.6_1 

# PNG Datei erstellen und oeffnen
png(paste0(fig.path, 'Q10.6_1_gewichteterMittelwert.png'),
    width     = 3000,
    height    = 1450,
    res = 300)

# Bild speichern
plot(p.Q10.6_1)

# Device schliessen
dev.off()

#------------------------------------------------------------------------------/
# Q10.7_1 ----
## * preprocess Q10.7_1 ----

df.weighted.mean.partei.Q10.7_1 <- data_full_withweights %>%
  dplyr::mutate(Partei = ifelse(Partei_numeric == 6, 'Keine Partei', Partei)) %>%
  dplyr::mutate(Mittelwert = matrixStats::weightedMean(Q10.7_1, w = weight)) %>%
  dplyr::group_by(Partei_numeric, Partei, Mittelwert) %>%
  dplyr::summarise(Q10.7_1_median = matrixStats::weightedMedian(Q10.7_1, w = weight),
                   Q10.7_1_mean = matrixStats::weightedMean(Q10.7_1, w = weight)) %>%
  dplyr::ungroup() %>%
  dplyr::arrange(desc(Q10.7_1_mean))

data.weighted.mean.partei.Q10.7_1 <- df.weighted.mean.partei.Q10.7_1 %>%
  dplyr::bind_rows(data.frame(Partei = 'Mittelwert',
                              Q10.7_1_mean = df.weighted.mean.partei.Q10.7_1$Mittelwert[1])) %>%
  dplyr::mutate(Mittelwert = ifelse(Partei == 'Mittelwert', 0.03, 0.12))

## * visualize Q10.7_1 ----

p.Q10.7_1 <- ggplot(data.weighted.mean.partei.Q10.7_1 %>%
                      dplyr::mutate(fillcol = ifelse(Q10.7_1_mean <50, 'Ablehnung', 'Zustimmung'),
                                    fillcol = ifelse(Partei == 'Mittelwert', 'Mittelwert', fillcol)),
                   aes(x = (Q10.7_1_mean-50)*2, y = reorder(Partei, Q10.7_1_mean))) +
  labs(y = '', x= 'Zustimmungswert')+
  ggtitle(wrapper(subset(df_Qs, Frage == 'Q10.7_1')$Name , width = 64))+
  geom_col(show.legend = FALSE,
           aes(fill = factor(fillcol)),
           width = length(data.weighted.mean.partei.Q10.7_1) * data.weighted.mean.partei.Q10.7_1$Mittelwert)+
  geom_text(aes(hjust = ifelse(Q10.7_1_mean < 50, 1.2, -0.2),
                label=round((Q10.7_1_mean-50)*2, 1),
                fontface = ifelse(Partei == 'Mittelwert', 'bold.italic', 'plain')), 
            size = 6,
            show.legend = FALSE)+
  scale_x_continuous( breaks = c(-20, 0, 20,  40, 60, 80),labels = c(-20, '0\nNeutral', 20,  40, 60, 80),
                      expand = expansion(mult = c(0.1,0.1)))+
  scale_fill_manual(values = c( '#A87B5D', '#00cc99', '#5d8aa8'))+ #'#e52b50', 
  scale_y_discrete(breaks = c("Grüne","SP","Andere","Die Mitte","Keine Partei","glp","SVP","FDP", "Mittelwert"), 
                   labels = c("Grüne","SP","Andere","Die Mitte","Keine Partei","glp","SVP","FDP", expression(bolditalic("Mittelwert \u00F8"))))+
  theme_bw(base_size = 20)+
  theme(axis.line = element_line(color='black'),
        axis.text.y = element_text(colour="black", size = 18),
        plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.text.x = element_text(colour="black", size = 16),
        axis.title.x = element_text(size = 16),
        plot.title = element_text(size = 20))

p.Q10.7_1 

# PNG Datei erstellen und oeffnen
png(paste0(fig.path, 'Q10.7_1_gewichteterMittelwert.png'),
    width     = 3000,
    height    = 1550,
    res = 300)

# Bild speichern
plot(p.Q10.7_1)

# Device schliessen
dev.off()

#------------------------------------------------------------------------------/
# Q10.8_1 ----
## * preprocess Q10.8_1 ----

df.weighted.mean.partei.Q10.8_1 <- data_full_withweights %>%
  dplyr::mutate(Partei = ifelse(Partei_numeric == 6, 'Keine Partei', Partei)) %>%
  dplyr::mutate(Mittelwert = matrixStats::weightedMean(Q10.8_1, w = weight)) %>%
  dplyr::group_by(Partei_numeric, Partei, Mittelwert) %>%
  dplyr::summarise(Q10.8_1_median = matrixStats::weightedMedian(Q10.8_1, w = weight),
                   Q10.8_1_mean = matrixStats::weightedMean(Q10.8_1, w = weight)) %>%
  dplyr::ungroup() %>%
  dplyr::arrange(desc(Q10.8_1_mean))

data.weighted.mean.partei.Q10.8_1 <- df.weighted.mean.partei.Q10.8_1 %>%
  dplyr::bind_rows(data.frame(Partei = 'Mittelwert',
                              Q10.8_1_mean = df.weighted.mean.partei.Q10.8_1$Mittelwert[1])) %>%
  dplyr::mutate(Mittelwert = ifelse(Partei == 'Mittelwert', 0.03, 0.12))

## * visualize Q10.8_1 ----

p.Q10.8_1 <- ggplot(data.weighted.mean.partei.Q10.8_1 %>%
                      dplyr::mutate(fillcol = ifelse(Q10.8_1_mean <50, 'Ablehnung', 'Zustimmung'),
                                    fillcol = ifelse(Partei == 'Mittelwert', 'Mittelwert', fillcol)),
                   aes(x = (Q10.8_1_mean-50)*2, y = reorder(Partei, Q10.8_1_mean))) +
  labs(y = '', x= 'Zustimmungswert')+
  ggtitle(wrapper(subset(df_Qs, Frage == 'Q10.8_1')$Name , width = 64))+
  geom_col(show.legend = FALSE,
           aes(fill = factor(fillcol)),
           width = length(data.weighted.mean.partei.Q10.8_1) * data.weighted.mean.partei.Q10.8_1$Mittelwert)+
  geom_text(aes(hjust = ifelse(Q10.8_1_mean < 50, 1.2, -0.2),
                label=round((Q10.8_1_mean-50)*2, 1),
                fontface = ifelse(Partei == 'Mittelwert', 'bold.italic', 'plain')), 
            size = 6,
            show.legend = FALSE)+
  scale_x_continuous( breaks = c(-20, 0, 20,  40, 60, 80),labels = c(-20, '0\nNeutral', 20,  40, 60, 80),
                      expand = expansion(mult = c(0.1,0.1)))+
  scale_fill_manual(values = c( '#A87B5D', '#00cc99', '#5d8aa8'))+ #'#e52b50', 
  scale_y_discrete(breaks = c("Grüne","SP","Andere","Die Mitte","Keine Partei","glp","SVP","FDP", "Mittelwert"), 
                   labels = c("Grüne","SP","Andere","Die Mitte","Keine Partei","glp","SVP","FDP", expression(bolditalic("Mittelwert \u00F8"))))+
  theme_bw(base_size = 20)+
  theme(axis.line = element_line(color='black'),
        axis.text.y = element_text(colour="black", size = 18),
        plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.text.x = element_text(colour="black", size = 16),
        axis.title.x = element_text(size = 16),
        plot.title = element_text(size = 20))

p.Q10.8_1 

# PNG Datei erstellen und oeffnen
png(paste0(fig.path, 'Q10.8_1_gewichteterMittelwert.png'),
    width     = 3000,
    height    = 1450,
    res = 300)

# Bild speichern
plot(p.Q10.8_1)

# Device schliessen
dev.off()

#------------------------------------------------------------------------------/
# Q11.3_1 ----
## * preprocess Q11.3_1 ----

df.weighted.mean.partei.Q11.3_1 <- data_full_withweights %>%
  dplyr::mutate(Partei = ifelse(Partei_numeric == 6, 'Keine Partei', Partei)) %>%
  dplyr::mutate(Mittelwert = matrixStats::weightedMean(Q11.3_1, w = weight)) %>%
  dplyr::group_by(Partei_numeric, Partei, Mittelwert) %>%
  dplyr::summarise(Q11.3_1_median = matrixStats::weightedMedian(Q11.3_1, w = weight),
                   Q11.3_1_mean = matrixStats::weightedMean(Q11.3_1, w = weight)) %>%
  dplyr::ungroup() %>%
  dplyr::arrange(desc(Q11.3_1_mean))

data.weighted.mean.partei.Q11.3_1 <- df.weighted.mean.partei.Q11.3_1 %>%
  dplyr::bind_rows(data.frame(Partei = 'Mittelwert',
                              Q11.3_1_mean = df.weighted.mean.partei.Q11.3_1$Mittelwert[1])) %>%
  dplyr::mutate(Mittelwert = ifelse(Partei == 'Mittelwert', 0.03, 0.12))

## * visualize Q11.3_1 ----

p.Q11.3_1 <- ggplot(data.weighted.mean.partei.Q11.3_1 ,
                   aes(x = (Q11.3_1_mean-50)*2, y = reorder(Partei, Q11.3_1_mean))) +
  labs(y = '', x= 'Zustimmungswert')+
  ggtitle(wrapper(subset(df_Qs, Frage == 'Q11.3_1')$Name , width = 64))+
  geom_col(show.legend = FALSE,
           aes(fill = factor(Mittelwert)),
           width = length(data.weighted.mean.partei.Q11.3_1) * data.weighted.mean.partei.Q11.3_1$Mittelwert)+
  geom_text(aes(label=paste0('+ ', round((Q11.3_1_mean-50)*2, 1)),
                fontface = ifelse(Partei == 'Mittelwert', 'bold.italic', 'plain')), 
            size = 6,
            hjust = -0.2, show.legend = FALSE)+
  scale_x_continuous( breaks = c(0, 10, 20, 30, 40, 50, 60),labels = c('0\nNeutral',  10, 20, 30, 40, 50, 60),
                      expand = expansion(mult = c(0.05,0.1)))+
  scale_fill_manual(values = c( '#00cc99', '#5d8aa8'))+ #'#e52b50', 
  scale_y_discrete(breaks = c("Grüne","SP","Andere","Die Mitte","Keine Partei","glp","SVP","FDP", "Mittelwert"), 
                   labels = c("Grüne","SP","Andere","Die Mitte","Keine Partei","glp","SVP","FDP", expression(bolditalic("Mittelwert \u00F8"))))+
  theme_bw(base_size = 20)+
  theme(axis.line = element_line(color='black'),
        axis.text.y = element_text(colour="black", size = 18),
        plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.text.x = element_text(colour="black", size = 16),
        axis.title.x = element_text(size = 16),
        plot.title = element_text(size = 20))

p.Q11.3_1 

# PNG Datei erstellen und oeffnen
png(paste0(fig.path, 'Q11.3_1_gewichteterMittelwert.png'),
    width     = 3000,
    height    = 1450,
    res = 300)

# Bild speichern
plot(p.Q11.3_1)

# Device schliessen
dev.off()
#------------------------------------------------------------------------------/

# =============================================================================/
# END ----
# =============================================================================/

