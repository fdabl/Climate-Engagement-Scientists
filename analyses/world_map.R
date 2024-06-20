library(dplyr)
library(tidyr)
library(ggplot2)
library(RColorBrewer)

cols <- brewer.pal(3, 'Set1')[c(2, 1)]
cols <- brewer.pal(8, 'Set1')
cols <- c('#ff7f0e', '#E69F00', '#D55E00', '#A65628', '#029E73', '#56B4E9', '#0072B2')

# dat_text has all observations (i.e., also for countries with less than 10 observations)
# and we cannot share it due to privacy reasons
dat_text <- read.csv('../../../data/barriers_text_final.csv')
mapping_tib <- read.csv('../data/map_data.csv') # data for making map

countries_aggreg <- dat_text %>%
  select(starts_with('Country')) %>%
  pivot_longer(cols = everything(), values_drop_na = TRUE) %>%
  group_by(value) %>%
  summarise(count = n())

# Define the breaks for the categories
breaks <- c(0, 10, 50, 100, 200, 500, 1000, Inf)

# Define labels for the categories
labels <- c('< 10', '10-50', '51-100', '101-200', '201-500', '501-1000', '> 1000')

countries_aggreg <- countries_aggreg %>%
  mutate(
    category = cut(count, breaks = breaks, labels = labels, include.lowest = TRUE)
  )

# Recode country names --> Has to match with country names in mapping_tib
countries_aggreg[countries_aggreg$value == 'Brunei Darussalam',]$value <- 'Brunei' 
countries_aggreg[countries_aggreg$value == "Lao People's Democratic Republic",]$value <- 'Laos' 
countries_aggreg[countries_aggreg$value == 'Libyan Arab Jamahiriya',]$value <- 'Libya' 
countries_aggreg[countries_aggreg$value == 'Republic of Moldova',]$value <- 'Moldova' 
countries_aggreg[countries_aggreg$value == 'Russian Federation',]$value <- 'Russia' 
countries_aggreg[countries_aggreg$value == 'Trinidad and Tobago',]$value <- 'Trinidad' 
countries_aggreg[countries_aggreg$value == 'United States of America',]$value <- 'USA' 
countries_aggreg[countries_aggreg$value == 'United Kingdom of Great Britain and Northern Ireland',]$value <- 'UK' 
countries_aggreg[countries_aggreg$value == 'United Republic of Tanzania',]$value <- 'Tanzania' 
countries_aggreg[countries_aggreg$value == 'United States of America',]$value <- 'USA' 
countries_aggreg[countries_aggreg$value == 'Venezuela, Bolivarian Republic of...',]$value <- 'Venezuela' 
countries_aggreg[countries_aggreg$value == 'Viet Nam',]$value <- 'Vietnam' 

mapping_tib <- mapping_tib %>%
  mutate(country_recoded = countries_aggreg$value[match(mapping_tib$country, countries_aggreg$value)]) %>%
  mutate(country_recoded = ifelse(!is.na(country_recoded), country_recoded, country))

world_subset <- mapping_tib %>% 
  left_join(countries_aggreg, by = join_by('country_recoded' == 'value'), multiple = 'first')

p <- world_subset %>%
  mutate(text = paste('Country: ', country_recoded, '\nSample size (n): ', category, sep='')) %>%
  ggplot(aes(x = long, y = lat, group = group, fill = factor(category), text = text)) +
  geom_polygon(size = 0, alpha = 0.9) +
  theme_void() +
  scale_fill_manual(
    values = cols,
    na.value = '#f0f0f0', name = 'Number of observations per country', 
    breaks = labels,
    labels = labels,
    guide = guide_legend(
      keyheight = unit(3, units = 'mm'),
      keywidth = unit(10, units = 'mm'),
      label.position = 'bottom',
      title.position = 'top', nrow = 1
    )) +
  labs(title = '') +
  theme(legend.position = c(0.51, 0.09), plot.title = element_text(hjust = 0.5)) +
  coord_fixed(1.3)

ggsave('../figures/figureEx1_worldmap.pdf', p, width = 9, height = 7)
