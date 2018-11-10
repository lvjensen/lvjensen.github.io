---
title: "CASE STUDY TITLE"
author: "YOUR NAME"
date: "November 09, 2018"
output:
  html_document:  
    keep_md: true
    toc: true
    toc_float: true
    code_folding: hide
    fig_height: 6
    fig_width: 12
    fig_align: 'center'
---






```r
# Use this R-Chunk to import all your datasets!
wisc <- read_spss("http://www.ssc.wisc.edu/nsfh/wave3/NSFH3%20Apr%202005%20release/main05022005.sav")
wisc2 <- as.tibble(wisc)

german <- read_dta("https://byuistats.github.io/M335/data/heights/germanconscr.dta")
bavar <- read_dta("https://byuistats.github.io/M335/data/heights/germanprison.dta")

wageheight <- read_csv("https://github.com/hadley/r4ds/raw/master/data/heights.csv")


worlddata <- tempfile(fileext = '.xlsx')
download("https://byuistats.github.io/M335/data/heights/Height.xlsx", mode = 'wb', destfile = worlddata)
worlddata <- read_excel(worlddata, skip = 2)

south <- read.dbf("~/Desktop/B6090.DBF")
```


## Data Wrangling


```r
# Use this R-Chunk to clean & wrangle your data!
worlddata2 <- worlddata%>%
  gather('1800':'2011', key = 'year_decade', value = 'heightscm')%>%
  filter(!is.na(heightscm))%>%
  mutate(heightsin = heightscm*0.3937008)%>%
  separate(year_decade, into = c('Century','decade'),sep = 2, remove = FALSE)%>%
  separate(decade, into = c('Decade', 'Year'), sep = 1)
  
wisc3 <- wisc2%>%
  mutate(heightsin = RT216F*12+RT216I, year_decade = 1900 + DOBY, heightscm = heightsin/0.3937008, study = 'National Survey')%>%
  select(year_decade, heightsin, heightscm, study)

bavar2 <- bavar%>%
  mutate(year_decade = bdec, heightscm = height, heightsin = heightscm*0.3937008, study = 'Bavarian')%>%
  select(year_decade, heightsin, heightscm, study)

german2 <- german%>%
  mutate(year_decade = bdec, heightscm = height, heightsin = height*0.3937008, study = 'Bavarian Germans')%>%
  select(year_decade, heightsin, heightscm, study)

wageheight2 <- wageheight%>%
  mutate(year_decade = 1950, heightsin = height, heightscm = heightsin/0.3937008, study = 'BLS')%>%
  select(year_decade, heightsin, heightscm, study)

south2 <- south%>%
  mutate(year_decade = GEBJ, heightscm = CMETER, heightsin = CMETER*0.3937008, study = 'South German Soldiers')%>%
  select(year_decade, heightsin, heightscm, study)

alld <- bind_rows(bavar2, wisc3, german2, wageheight2, south2)

alld$years <- cut(alld$year_decade, c(1700, 1800, 1900, 2000), labels = c('18th Century','19th Century','20th Century'))
```

## Data Visualization


```r
# Use this R-Chunk to plot & visualize your data!

world <- worlddata2%>%
  mutate(Country = case_when(
      `Continent, Region, Country` == 'Germany' ~ "Germany", 
      TRUE ~ 'Other'
      ))%>%
  group_by(year_decade, Country)%>%
  summarise(countrymean = mean(heightsin))
  
    
worlddata2%>%
    mutate(Country = case_when(
      `Continent, Region, Country` == 'Germany' ~ "Germany", 
      TRUE ~ 'Other'
      ))%>%
    ggplot()+
    geom_point(mapping = aes(x = year_decade, y = heightsin, color = Country))+
    geom_line(data = world, aes(x = year_decade, y = countrymean, group = Country, color = Country))+
  scale_color_manual(values = c('dodgerblue','darkgray'))+
  labs(title = "Germany's Increase in Height is More Extreme Than Rest of World", x = 'Year', y = 'Heights(in Inches)')+
  theme_economist()
```

![](CaseStudy5_files/figure-html/plot_data-1.png)<!-- -->

```r
alld%>%
  filter(heightsin<96 & heightsin>45&!is.na(years))%>%
  dplyr::mutate(study = fct_relevel(study, c('South German Soldiers','Bavarian Germans','Bavarian','National Survey','BLS')))%>%
  ggplot()+
  geom_jitter(mapping = aes(y = heightsin, x = study, color = study))+
  labs(title = 'No Large Increase in Heights since 18th Century', x = 'Century',y = 'Heights (in inches)')+
  theme_economist()+
  guides(fill = FALSE)
```

![](CaseStudy5_files/figure-html/plot_data-2.png)<!-- -->
## Conclusions

The first plot I created helps to describe the overall increase in heights over the years. Germany is highlighted, and according to the visualization, had a muc faster increase in heights over most other countries. The 'Other' variable in this plot shows all other countries or territories. 

My second plot is a visualization to compare all the other plots over the past few centuries. They are all in order of the years they were taken. Evidently, it appears as though there hasn't been much change. One could argue that there seem to be both taller and shorter people more recently, giving us more variance over the spectrum.
