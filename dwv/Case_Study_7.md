---
title: "Case Study 7"
author: "Logan Jensen"
date: "July 19, 2018"
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
download("http://scriptures.nephi.org/downloads/lds-scriptures.csv.zip", "docs/data/scriptures.zip", mode = "wb")
unzip("docs/data/scriptures.zip", exdir = file.path(getwd(),"docs/data"))
file.remove("docs/data/scriptures.zip")
```

```
## [1] TRUE
```

```r
scriptures <- read_csv("docs/data/lds-scriptures.csv")
bmnames <- read_rds(gzcon(url("https://byuistats.github.io/M335/data/BoM_SaviorNames.rds")))
```


## Data Wrangling


```r
bm <- scriptures %>%
  filter(volume_title == 'Book of Mormon')

newscript <- bm$scripture_text %>%
  str_c(collapse = '', sep = '') %>%
  str_replace_all(pattern = "\\.", replacement = ' ') %>%
  str_replace_all(pattern = "\\?", replacement = ' ') %>%
  str_replace_all(pattern = "\\,", replacement = ' ') %>%
  str_replace_all(pattern = "\\!", replacement = ' ') %>%
  str_replace_all(pattern = "\\;", replacement = ' ') %>%
  str_replace_all(pattern = "\\:", replacement = ' ') %>%
  str_replace_all(pattern = "\\(", replacement = ' ') %>%
  str_replace_all(pattern = "\\)", replacement = ' ')

names <- bmnames %>%
  arrange(desc(nchar))
```


```r
nmavg <- bm %>%
  group_by(volume_id, book_title, chapter_id, verse_id) %>%
  summarise(wordcountbm = stri_stats_latex(scripture_text)['Words'])

bmavg <- nmavg %>%
  group_by(book_title) %>%
  summarise(bookwords = sum(wordcountbm), numverse = n())
  
jnames <- names$name %>%
  str_replace_all(pattern = "\\,", replacement = ' ')

scripturetext <- newscript
```




```r
for (i in seq_along(jnames)){
  scripturetext <- scripturetext%>%
    str_replace_all(jnames[i], str_c('__', i, '__'))
}

observations <- scripturetext %>%
  str_split("__[0-9]+__") %>%
  unlist()
names_id <- scripturetext %>%
  str_match_all("__[1-9]+__")

breaks1 <- c(1:4066)

scrips <- tibble(
  breaks = breaks1, 
  OBS = observations)
```




```r
scripbooks <- scrips %>%
  mutate(book_title = case_when(
    breaks < 463 ~ '1 Nephi', 
    breaks >=463 & breaks < 1057 ~ '2 Nephi', 
    breaks <1210 & breaks >= 1057 ~ 'Jacob', 
    breaks >= 1210 & breaks < 1233 ~ 'Enos', 
    breaks >= 1233 & breaks <1241 ~ 'Jarom',
    breaks >=1241 & breaks <1262 ~ 'Omni', 
    breaks >=1262 & breaks <1277 ~ 'Words of Mormon', 
    breaks >=1277 & breaks <1737 ~ 'Mosiah', 
    breaks >=1737 & breaks <2757 ~ 'Alma', 
    breaks >= 2757 & breaks <2966 ~ 'Helaman', 
    breaks >= 2966 & breaks < 3422 ~ '3 Nephi', 
    breaks >= 3422 & breaks <3465 ~ '4 Nephi', 
    breaks >= 3465 & breaks < 3658 ~ 'Mormon', 
    breaks >= 3658 & breaks <3886 ~ 'Ether',
    breaks >= 3886 ~ 'Moroni'
  )) %>%
  group_by(breaks, OBS, book_title) %>%
  summarise(wordcount = stri_stats_latex(OBS)['Words'])




bookscr <- scripbooks %>%
  group_by(book_title) %>%
  summarise(avgword = mean(wordcount), avgbreak = mean(breaks), obscount = n())


bookscr2 <- bookscr %>%
  left_join(bmavg, by = 'book_title') %>%
  mutate(avgwordbook = obscount/numverse)
```




```r
scripbooks %>%
  mutate(bookorder = fct_relevel(book_title, c('1 Nephi','2 Nephi','Jacob','Enos','Jarom','Omni','Words of Mormon','Mosiah','Alma','Helaman','3 Nephi','4 Nephi','Mormon','Ether','Moroni'))) %>%
  ggplot()+
  geom_point(mapping = aes(x = breaks, y = wordcount, color = fct_relevel(book_title, c('1 Nephi','2 Nephi','Jacob','Enos','Jarom','Omni','Words of Mormon','Mosiah','Alma','Helaman','3 Nephi','4 Nephi','Mormon','Ether','Moroni'))))+
  geom_point(data = bookscr, aes(x = avgbreak, y = avgword, size = 1.1))+
  geom_vline(xintercept = c(463, 1057, 1210, 1233, 1241, 1261, 1277, 1737, 2757, 2966, 3422, 3465, 3658, 3886), linetype = 'dotted')+
  theme_economist()+
  labs(x = 'Occurences', y = 'Words Between Names', color = 'Book', title = 'Distribution of Occurences of the Names of Jesus Christ', caption = 'Black Point Gives Average of the Book')+
  scale_y_continuous(trans = "sqrt")+
  guides(size = FALSE)+
  theme(legend.position = 'right')
```

![](Case_Study_7_files/figure-html/unnamed-chunk-5-1.png)<!-- -->

```r
bookscr2 %>%
  mutate(bookorder = fct_relevel(book_title, c('1 Nephi','2 Nephi','Jacob','Enos','Jarom','Omni','Words of Mormon','Mosiah','Alma','Helaman','3 Nephi','4 Nephi','Mormon','Ether','Moroni'))) %>%
  ggplot()+
  geom_point(mapping = aes(x = bookorder, y = avgword, size = numverse, color = bookorder))+
  geom_hline(data = scripbooks, yintercept = mean(scripbooks$wordcount))+
  theme_economist()+
  labs(x = 'Book', y = 'Average Words Between Names', size = 'Number of Verses', color = 'Book', title = 'The Book of Mormon Averages only 65 Words \n Between Each Name of the Savior', caption = 'Black Line indicates mean throughout the Book of Mormon')+
  theme(legend.position = 'right')+
  guides(color = 'none')
```

![](Case_Study_7_files/figure-html/unnamed-chunk-5-2.png)<!-- -->


## Conclusions
I have included two plots. The first one shows the distribution of the appearances of the names of the Savior throughout the Book of Mormon. I felt like it was common for the middle books to spread out the occurrences of the appearances more frequently than the first books and the last books. My second plot shows the average more clearly, with the nmber of verses in each book indicating the size of the point. It's clear to note that Helaman has the hightest amount of words between appearances at over 100. The average throughout the entire Book of Mormon is 64.5.
