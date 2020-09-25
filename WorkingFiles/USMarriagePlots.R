library(USAboundaries)
library(sf)
library(tidyverse)

marriage <- read_csv('/Users/loganjensen/Downloads/siyeh-state-marriage-rate/data/state_marriage_rates_90_95_99_16.csv')

marriage <- marriage %>%
  filter(state != 'Alaska', state != 'Hawaii') %>%
  mutate(state_name = state)

usstates <- us_states(states = c('Alabama', 'Arizona','Arkansas','California','Colorado','Connecticut','Delaware','Florida','Georgia','Idaho','Illinois','Indiana','Iowa','Kansas','Kentucky','Louisiana','Maine','Maryland','Massachusetts','Michigan','Minnesota','Mississippi','Missouri','Montana','Nebraska','Nevada','New Hampshire','New jersey','New Mexico','New York','North Carolina','North Dakota','Ohio','Oklahoma','Oregon','Pennsylvania','Rhode Island','South Carolina', 'South Dakota','Tennessee','Texas','Utah','Vermont','Virginia','Washington','West Virginia','Wisconsin','Wyoming'))

usastuff <- map('usa', plot = TRUE)
usasf <- st_as_sf(usastuff)

marriage <- marriage %>%
  left_join(usstates, by = 'state_name', )

marriage2 <- marriage %>%
  gather(`1990`, `1995`, `1999`, `2000`, `2001`, `2002`, `2003`, `2004`, `2005`, `2006`, `2007`, `2008`, `2009`, `2010`, `2011`, `2012`, `2013`, `2014`, `2015`, `2016`, key = 'year', value = 'rate') %>%
  mutate(year = as.numeric(year))
  
marriage2 %>%
  #filter(state != 'Nevada') %>%
  ggplot() +
  geom_sf(aes(fill = rate, frame = year)) 
  facet_wrap(~year)

  
gganimate(marriage2 %>%
            ggplot(aes(frame=year)) +
            geom_sf(aes(fill = rate)), interval = .2)

