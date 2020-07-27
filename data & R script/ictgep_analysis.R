########################
### package installs ###
########################

install.packages("psych")
install.packages("psychTools")
library(psych)
library(psychTools)

# necessary mapping packages
install.packages(c("cowplot", "googleway", "ggplot2", "ggrepel", "ggspatial", 
                   "libwgeom", "sf", "rnaturalearth", "rnaturalearthdata", "rgeos"))
library("ggplot2")
theme_set(theme_bw())
library("sf")
library("rnaturalearth")
library("rnaturalearthdata")
library("rgeos")

# some more useful packages
install.packages('dplyr')
library('dplyr')

# for correlation comparison magic
installed.packages('cocor')
library(cocor)

install.packages('rstudioapi')
library(rstudioapi)
install.packages('tidyverse')
library(tidyverse) # ggplot2, dplyr, tidyr, readr, purrr, tibble
install.packages('magrittr')
library(magrittr) # pipes
install.packages('lintr')
library(lintr) # code linting
install.packages('raster')
library(raster) # raster handling (needed for relief)
install.packages('viridis')
library(viridis) # viridis color scale
install.packages('dichromat')
library('dichromat')

# font package
install.packages("extrafont")
library(extrafont)
font_import()

########################
##### NOTES/CREDIT #####
########################
# shoutout to these webpages for being helpful guides for mapping in R:
# https://www.r-spatial.org/r/2018/10/25/ggplot2-sf-2.html
# https://timogrossenbacher.ch/2019/04/bivariate-maps-with-ggplot2-and-sf/
  

# store the world (except Antarctica)! 
world <- ne_countries(scale = "medium", returnclass = "sf")
world <- subset(world, name_long != 'Antarctica')

# set theme for maps
# set defaults
default_font_family <- 'Tahoma'
default_font_color <- '#22211d'
default_background_color <- '#f5f5f2'
theme_map <- function(...) {
  theme_minimal() +
    theme(
      text = element_text(family = default_font_family,
                          color = default_font_color),
      # remove all axes
      axis.line = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks = element_blank(),
      # add a subtle grid
      # panel.grid.major = element_line(color = "#dbdbd9", size = 0.2),
      # panel.grid.minor = element_blank(),
      # background colors
      plot.background = element_rect(fill = default_background_color,
                                     color = NA),
      panel.background = element_rect(fill = default_background_color,
                                      color = NA),
      legend.background = element_rect(fill = default_background_color,
                                       color = NA),
      # borders and margins
      plot.margin = unit(c(.5, .5, .2, .5), "cm"),
      panel.border = element_blank(),
      panel.spacing = unit(c(-.1, 0.2, .2, 0.2), "cm"),
      # titles
      legend.title = element_text(size = 9, margin = margin(-.2,0,.2,0, unit = 'cm')),
      legend.text = element_text(size = 9, hjust = 0,
                                 color = default_font_color),
      legend.position="bottom", 
      legend.box = "horizontal" ,
      plot.title = element_text(size = 14, hjust = 0.5,
                                color = default_font_color),
      plot.subtitle = element_text(size = 9, hjust = 0.5,
                                   color = default_font_color,
                                   margin = margin(b = .5,
                                                   t = 0,
                                                   l = 2,
                                                   unit = "cm"),
                                   debug = F),
      # captions
      plot.caption = element_text(size = 7,
                                  hjust = .5,
                                  margin = margin(t = 0.2,
                                                  b = 0,
                                                  unit = "cm"),
                                  color = "#939184"),
      ...
    )
}

# ict data 
# source:  http://data.uis.unesco.org/Index.aspx?DataSetCode=EDULIT_DS&popupcustomise=true&lang=en#
# path from source to data: education --> completion --> percentage of tertiary graduates --> percentage of female graduates by field of study
# note on country codes: to join the various data frames, it essential to include the iso_a3 coutnry codes in each of the original data frames,
# so be sure to select 'customise' --> 'table options' and finally, check the box to use codes for country
ict_raw <- read.csv("./data & R script/data/ict.csv")
# the distinct country codes in case you need them
ict_countries <- levels(ict_raw[,'LOCATION'])
# get average across all years that each country has data for (2013-2019)
ict_averages <- aggregate(ict_raw[,'Value'], list(ict_raw$LOCATION), mean)
# rename columns
colnames(ict_averages) <- c('iso_a3', 'average_ict')

# ict + world !!!
ict_world <- merge(world, ict_averages, by.x='iso_a3', by.y='iso_a3', all.x = TRUE)
# note: many tidbits borrowed from this blog post: https://timogrossenbacher.ch/2019/04/bivariate-maps-with-ggplot2-and-sf/
ggplot(data = ict_world) +
  # it is essential to have the empty 'colour' string here so that we can include the 'no data' legend
  geom_sf(aes(fill = average_ict, colour="")) +
  scale_fill_viridis_c(option = "viridis", name = '% of graduates\n who are women',
                       guide = guide_colorbar(title.position = 'top', title.hjust = 0.5,
                                              label.hjust = 0.5, ticks = FALSE), na.value = '#d6d6d2') +
  scale_colour_manual(values=NA) +              
  guides(colour=guide_legend("\nNo data", override.aes=list(colour="#d6d6d2"), title.position = 'top')) +
  labs(x = NULL,
       y = NULL,
       title = "ICT Gender Equality Paradox",
       subtitle = "Percent of women among ICT graduates (2013-2019)",
       caption = "Data: UNESCO Institute For Statistics, displaying the average of a country's available data from 2013-2019") +
  theme_map()

# ggi (2013-2019) + world !!!
# data download: https://tcdata360-backend.worldbank.org/api/v1/datasets/743/dump.csv
# data hub: https://tcdata360.worldbank.org/indicators/af52ebe9?country=BRA&indicator=27959&viz=line_chart&years=2006,2018
ggi_raw <- read.csv("./data & R script/data/ggi.csv", header = TRUE)
# reduces the data frame to contain only the relevant data, aka the ggi index value for 2018
ggi_index <- subset(ggi_raw, Indicator=='Overall Global Gender Gap Index' & Subindicator.Type== 'Index')
# find average GGI from 2013-2018
ggi_index$avg_ggi <- rowMeans(ggi_index[,13:17], na.rm = TRUE)

# combine ggi & world data (all.x = true performs an outer join to preserve the countries that do not appear in the ggi data)
ggi_world <- merge(world, ggi_index, by.x='iso_a3', by.y='Country.ISO3', all.x = TRUE)

# map to visualize 2018 ggi index
# note: many tidbits borrowed from this blog post: https://timogrossenbacher.ch/2019/04/bivariate-maps-with-ggplot2-and-sf/
ggplot(data = ggi_world) +
  geom_sf(aes(fill = avg_ggi, colour="")) +
  scale_fill_viridis_c(option = "viridis", name = 'GGI',
                       guide = guide_colorbar(title.position = 'top', title.hjust = 0.5,
                                              label.hjust = 0.5, ticks = FALSE), na.value = '#d6d6d2') +
  scale_colour_manual(values=NA) +              
  guides(colour=guide_legend("No data", override.aes=list(colour="#d6d6d2"), title.position = 'top')) +
  labs(x = NULL,
       y = NULL,
       title = "Global Gender Gap Index (GGGI) (2013-2018)",
       subtitle = "GGGI considers four outcomes of gender disparity:\nEconomic Participation and Opportunity, Educational Attainment, Health and Survival and Political Empowerment",
       caption = "Data: World Bank, 2020. Note: Gender parity is represented by a GGGI of 1.0") +
  theme_map()

#############################
##### organize the data #####
#############################
# percent_of_women data
percent_of_women <- read.csv("./data & R script/data/percent_of_women.csv", header = TRUE)
percent_of_women <- subset(percent_of_women, Indicator == 'Percentage of female graduates from tertiary education graduating from Information and Communication Technologies programmes, female (%)')
percent_of_women_avg <- aggregate(percent_of_women[,'Value'], list(percent_of_women$LOCATION), mean)
# percent_of_men data
percent_of_men <- read.csv("./data & R script/data/percent_of_men.csv", header = TRUE)
percent_of_men <- subset(percent_of_men, Indicator == 'Percentage of male graduates from tertiary education graduating from Information and Communication Technologies programmes, male (%)')
percent_of_men_avg <- aggregate(percent_of_men[,'Value'], list(percent_of_men$LOCATION), mean)
# add a/(a+b) column
percent_of_ict_adjusted <- percent_of_women_avg
percent_of_ict_adjusted$b <- percent_of_men_avg[,'x']
colnames(percent_of_ict_adjusted) <- cs(iso_a3, a, b)
# add a column for the percent_of_ict adjusted percentage
percent_of_ict_adjusted$adjusted <- 100*percent_of_ict_adjusted[,'a']/(percent_of_ict_adjusted[,'a']+percent_of_ict_adjusted[,'b'])
# add a column for the disparity percentage
percent_of_ict_adjusted$disparity <- percent_of_ict_adjusted[,'a'] - percent_of_ict_adjusted[,'b']


# percent_of_ict data + gggi data
ict_ggi <- merge(ict_averages, ggi_index, by.x='iso_a3', by.y='Country.ISO3')

# append the gggi data
ict_ggi_adjusted <- merge(percent_of_ict_adjusted, ggi_index, by.x='iso_a3', by.y='Country.ISO3')

# store all relevant columns in the same data frame
three_correlations <- merge(x = percent_of_ict_adjusted, y = ict_ggi[, c('iso_a3', 'average_ict','avg_ggi')], by = 'iso_a3')
# remove countries that have missing data for any of the six variables
three_correlations <- na.exclude(three_correlations)

###################
#### normality ####
###################

# run shaprio-wilk tests of normality on each of the variables
shapiro.test(three_correlations[,'a'])
shapiro.test(three_correlations[,'b'])
shapiro.test(three_correlations[,'average_ict'])
shapiro.test(three_correlations[,'adjusted'])
shapiro.test(three_correlations[,'disparity'])
shapiro.test(three_correlations[,'avg_ggi'])

####################
##### outliers #####
####################

outlier_test <- three_correlations[, cs(iso_a3, a, b, average_ict, avg_ggi)]
# make the rownames the country codes so we can visualize this in the output plot
rownames(outlier_test) <- outlier_test[,'iso_a3']
# remove the iso_a3 column so that we can run the outlier test
outlier_test <- outlier_test[,-1]
# change colnames for purposes of the output table
colnames(outlier_test) <- cs(variable_1, variable_2, variable_3, variable_6)
# run the test and plot the results
d2 <- outlier(outlier_test, bad = 7)
# this will print any country/countries we should remove
d2[d2 > 25]
outlier.d2 <- data.frame(outlier_test,d2)
pairs.panels(outlier.d2,bg=c("yellow","blue")[(d2 > 25)+1],pch=21)

# remove Oman from ict_ggi, ict_ggi_adjusted, and three_correlations
ict_ggi <- subset(ict_ggi, iso_a3 != 'OMN')
ict_ggi_adjusted <- subset(ict_ggi_adjusted, iso_a3 != 'OMN')
three_correlations <- subset(three_correlations, iso_a3 != 'OMN')

############################
##### data description #####
############################
# remove iso_a3 column and reorder columns for consistency
three_correlations <- three_correlations[, cs(a, b, average_ict, adjusted, disparity, avg_ggi)]
# change colnames for purposes of the output table
colnames(three_correlations) <- cs(variable_1, variable_2, variable_3, variable_4, variable_5, variable_6)
# descriptive statistics
describe(three_correlations)
# visualize
pairs.panels(three_correlations)
# change colnames back 
colnames(three_correlations) <- cs(a, b, average_ict, adjusted, disparity, avg_ggi)


########################
##### ICT Method 1 #####
########################

# uses simply the percentage of ict graduates who are women

# so that we can label the dots with the actual country name
rownames(ict_ggi) <- ict_ggi[,'Country.Name']

# plot it
ggplot(ict_ggi, aes(x=avg_ggi, y=average_ict, label = rownames(ict_ggi))) + 
  geom_label(size = 2) +
  scale_color_distiller(palette = "YlGn", type = "seq", direction = -1) +
  geom_smooth(method=lm) +
  labs(x = 'Global Gender Gap Index (GGGI) (2013-2018)',
       y = 'Percent women among ICT graduates (2013-2019)',
       title = "ICT Gender Equality Paradox (Method 1)",
       caption = "Data: UNESCO Institute For Statistics, displaying the average of a country's available data from 2013-2019") +
theme_map() + theme(axis.title=element_text(size=8))

# Pearson's R for ict_cor_1 = -0.40
cor.test(x = ict_ggi[,'average_ict'], y = ict_ggi[,'avg_ggi'])


########################
##### ICT Method 2 #####
########################

# The formula used for calculating the propensity of women to graduate with STEM degrees was a /(a + b), 
# where a is the percentage of women who graduate with STEM [ICT] degrees (relative to all women graduating) and b is the percentage 
# of men who graduate with STEM [ICT] degrees (relative to all men graduating).

# so that we can label the dots with the actual country name
rownames(ict_ggi_adjusted) <- ict_ggi_adjusted[,'Country.Name']

# plot it
ggplot(ict_ggi_adjusted, aes(x=avg_ggi, y=adjusted, label = rownames(ict_ggi_adjusted))) + 
  geom_label(size = 2) +
  scale_color_distiller(palette = "YlGn", type = "seq", direction = -1) +
  geom_smooth(method=lm) +
  labs(x = 'Global Gender Gap Index (GGGI) (2013-2018)',
       y = 'Expected percent of women among ICT graduates, accounting for\n gender differences in overall graduation numbers',
       title = "ICT Gender Equality Paradox (Method 2)",
       subtitle = "Adjusting for gender differences in overall graduation numbers",
       caption = "Data: UNESCO Institute For Statistics, displaying the average of a country's available data from 2013-2019") +
  theme_map() + theme(axis.title=element_text(size=8), axis.text.x = element_text(), axis.text.y = element_text())

# Spearman's for ict_cor_2 = -0.46
cor.test(x = ict_ggi_adjusted[,'adjusted'], y = ict_ggi_adjusted[,'avg_ggi'])

########################
##### ICT Method 3 #####
########################

### a correlation plot based on disparity ###

# plot it
ggplot(ict_ggi_adjusted, aes(x=avg_ggi, y=disparity, label = rownames(ict_ggi_adjusted))) + 
  geom_label(size = 2) +
  scale_color_distiller(palette = "YlGn", type = "seq", direction = -1) +
  geom_smooth(method=lm) +
  labs(x = 'Global Gender Gap Index (GGGI) (2013-2018)',
       y = 'Disparity between the percentage of women and men who graduate from ICT programs',
       title = "ICT Gender Equality Paradox (Method 3)",
       subtitle = 'Gender disparity in ICT degree attainment:\npercentage of women who earn ICT degrees minus percentage of men who earn ICT degrees',
       caption = "Data: UNESCO Institute For Statistics, utilizing the average of a country's available data from 2013-2019") +
  theme_map() + theme(axis.title=element_text(size=8), axis.text.x = element_text(), axis.text.y = element_text())

# Pearson's R for ict_cor_3 = -0.32
cor.test(x = ict_ggi_adjusted[,'disparity'], y = ict_ggi_adjusted[,'avg_ggi'])

################################
#### correlation comparison ####
################################

# In this situation, we want to compare the correlation between AB and AC,
# with A being the GGGI and B and C being the different measures of women in ICT
# So, we have a dependent overlapping correlation

# using cocor, with the cocor.dep.groups.overlap formula:

# is there a difference between the Stoet & Geary and Thinkpiece methods? NO
cocor(formula = ~avg_ggi + average_ict | avg_ggi + adjusted, data = three_correlations)

# is there a difference between the Thinkpiece methods and the disparity method? NO
cocor(formula = ~avg_ggi + average_ict | avg_ggi + disparity, data = three_correlations)

# is there a difference between the Stoet & Geary method and the disparity method? YES
cocor(formula = ~avg_ggi + adjusted | avg_ggi + disparity, data = three_correlations)

