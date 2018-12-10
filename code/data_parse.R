library(tidyverse)
library(lubridate)
library(MatchIt)
library(ade4)

setwd("/Users/lisalebovici/Documents/Duke/Fall18/STA640/hw/CausalTJ")
load("data/key_mappings.Rdata")
load("data/pitches_full4.Rdata")
people <- read_csv("data/People.csv")
TJ <- read_csv("data/TJ.csv")

##### ADD MLBID TO PEOPLE
people <- people %>%
  left_join(select(key_mappings, key_mlbam, key_bbref), by = c("bbrefID" = "key_bbref")) %>%
  rename("pitcher" = "key_mlbam") %>%
  mutate(bday = ymd(paste(birthYear, birthMonth, birthDay))) %>%
  select(pitcher, bday, height, weight, throws)

##### ADD PERSONAL DATA TO PITCHES
pitches.full4 <- pitches.full4 %>% 
  left_join(people, by = "pitcher")

##### ADD TJ TO PITCHES
TJ.once <- TJ %>% group_by(pitcher) %>%
  mutate(n = n()) %>%
  filter(n < 2) %>%
  select(pitcher, surgery_date, return_date)

pitches.full4 <- pitches.full4 %>% 
  left_join(TJ.once, by = "pitcher") %>%
  mutate(datetime = as.Date(datetime))

max_pitches <- pitches.full4 %>%
  group_by(pitcher, pitch_type) %>%
  summarize(n = n(), speed = mean(start_speed, na.rm = TRUE)) %>%
  group_by(pitcher) %>%
  mutate(pct_thrown = n/sum(n)) %>%
  filter(pct_thrown >= 0.1) %>%
  filter(speed == max(speed)) %>%
  ungroup() %>%
  select(pitcher, pitch_type) %>%
  rename(fastest_pitch = pitch_type)

##### TREATMENT UNITS

pitches.TJ <- pitches.full4 %>%
  filter(!is.na(surgery_date)) %>%
  group_by(pitcher) %>%
  filter(surgery_date > min(datetime)) %>%
  mutate(before = datetime < surgery_date) %>%
  group_by(pitcher, before) %>%
  mutate(final = max(datetime), first = min(datetime)) %>%
  group_by(pitcher) %>%
  mutate(final = min(final), first = (max(first)))

buff.pre.days <- 365
buff.post.days <- 365
measure.period <- 365

units.TJ <- pitches.TJ %>% group_by(pitcher) %>%
  filter( (before == TRUE & datetime >  final - buff.pre.days - measure.period & datetime < final - buff.pre.days ) |
            (before == FALSE & datetime > first + buff.post.days & datetime < first + buff.post.days + measure.period)) %>%
  inner_join(max_pitches, by = 'pitcher') %>%
  mutate(age = as.numeric(round((surgery_date - bday)/365))) %>%
  group_by(pitcher, age, height, weight, throws, fastest_pitch, before) %>%
  summarize(velo = weighted.mean(start_speed, w = pitch_type == fastest_pitch, na.rm = TRUE),
            pitches = sum(before),
            starter = sum(inning.x==1*before)) %>%
  group_by(pitcher) %>%
  mutate(pitches = max(pitches),
         starter = max(starter) > 0) %>%
  ungroup() %>%
  mutate(before = factor(ifelse(before == TRUE, "before", "after"), levels = c("before", "after")))  %>% 
  spread(before, velo) %>%
  mutate(TJ = 1)


##### CONTROL UNITS

pitches.NOTJ <- pitches.full4 %>%
  filter(is.na(surgery_date)) %>%
  inner_join(max_pitches, by = 'pitcher') %>%
  mutate(year = year(datetime),
         age = year(datetime) - year(bday) + 2) %>% 
  group_by(pitcher, year, age, height, weight, throws, fastest_pitch) %>%
  summarize(velo = weighted.mean(start_speed, w = pitch_type == fastest_pitch),
            pitches = n(),
            starter = sum(inning.x==1)>0) %>%
  mutate(year4 = year + 4)

units.NOTJ <- pitches.NOTJ %>% inner_join(pitches.NOTJ, by = c('pitcher', 'height', 'weight', 'throws',
                                                               'fastest_pitch', 'year4' = 'year')) %>%
  ungroup() %>%
  select(pitcher, age.x, height, weight, throws, fastest_pitch, pitches.x, starter.x, velo.x, velo.y) %>%
  setNames(c('pitcher', 'age', 'height', 'weight', 'throws', 'fastest_pitch', 'pitches', 'starter', 'before', 'after')) %>%
  mutate(TJ = 0)

# dat <- units.TJ %>% rbind(units.NOTJ) %>%
#   filter(!is.na(before), !is.nan(before), !is.nan(after)) %>%
#   filter(!is.na(height))

dat <- units.TJ %>% rbind(units.NOTJ) %>%
  filter(!is.na(before), !is.nan(before), !is.na(after), !is.nan(after))

# dat <- dat %>% mutate(S = !is.na(after))

match <- matchit(TJ ~ age + height + weight + throws +
                   fastest_pitch + pitches + starter + before,
                 data = dat, method = "nearest", distance = "logit",
                 ratio = 10, replace = TRUE)

control_matches <- sort(as.numeric(unique(as.vector(match$match.matrix))))

dat <- dat %>% filter(TJ == 1) %>% rbind(dat[control_matches,])


##### SECOND SET OF TREATMENT / CONTROL UNITS FOR D-I-D PARALLEL TREND ASSUMPTION #####

units.TJ2 <- pitches.TJ %>% group_by(pitcher) %>%
  filter( (before == TRUE & ((datetime > final - buff.pre.days - measure.period & datetime < final - buff.pre.days) |
                               (datetime > final - 2*buff.pre.days - 2*measure.period & datetime < final - 2*buff.pre.days - measure.period)) ) |
            (before == FALSE & datetime > first + buff.post.days & datetime < first + buff.post.days + measure.period)) %>%
  inner_join(max_pitches, by = 'pitcher') %>%
  mutate(age = as.numeric(round((surgery_date - bday)/365)),
         period = case_when(before == TRUE & datetime > final - 2*buff.pre.days - 2*measure.period & datetime < final - 2*buff.pre.days - measure.period ~ "before2",
                            before == TRUE & datetime > final - buff.pre.days - measure.period & datetime < final - buff.pre.days ~ "before",
                            before == FALSE ~ "after")) %>%
  group_by(pitcher, age, height, weight, throws, fastest_pitch, period) %>%
  summarize(velo = weighted.mean(start_speed, w = pitch_type == fastest_pitch, na.rm = TRUE),
            pitches = sum(before),
            starter = sum(inning.x==1*before)) %>%
  group_by(pitcher) %>%
  mutate(pitches = max(pitches),
         starter = max(starter) > 0) %>%
  ungroup() %>%
  mutate(period = factor(period, levels = c("before2", "before", "after")))  %>%
  spread(period, velo) %>%
  mutate(TJ = 1)

pitches.NOTJ2 <- pitches.full4 %>%
  filter(is.na(surgery_date)) %>%
  inner_join(max_pitches, by = 'pitcher') %>%
  mutate(year = year(datetime),
         age = year(datetime) - year(bday) - 1) %>%
  group_by(pitcher, year, age, height, weight, throws, fastest_pitch) %>%
  summarize(velo = weighted.mean(start_speed, w = pitch_type == fastest_pitch),
            pitches = n(),
            starter = sum(inning.x==1)>0) %>%
  mutate(year4 = year + 4,
         year2 = year - 2)

units.NOTJ2 <- pitches.NOTJ2 %>% inner_join(pitches.NOTJ2, by = c('pitcher', 'height', 'weight', 'throws',
                                                                'fastest_pitch', 'year4' = 'year')) %>%
  inner_join(pitches.NOTJ2, c('pitcher', 'height', 'weight', 'throws',
                             'fastest_pitch', 'year2.x' = 'year')) %>%
  ungroup() %>%
  select(pitcher, age.x, height, weight, throws, fastest_pitch, pitches.x, starter.x, velo, velo.x, velo.y) %>%
  setNames(c('pitcher', 'age', 'height', 'weight', 'throws', 'fastest_pitch', 'pitches', 'starter', 'before2', 'before', 'after')) %>%
  mutate(TJ = 0)

dat2 <- units.TJ2 %>% rbind(units.NOTJ2) %>%
  filter(!is.na(before), !is.na(after), !is.na(before2), !is.nan(before), !is.nan(after), !is.nan(before2))

match2 <- matchit(TJ ~ age + height + weight + throws +
                   fastest_pitch + pitches + starter + before2,
                 data = dat2, method = "nearest", distance = "logit",
                 ratio = 10, replace = TRUE)

control_matches2 <- sort(as.numeric(unique(as.vector(match2$match.matrix))))

dat2 <- dat2 %>% filter(TJ == 1) %>% rbind(dat2[control_matches2,])


##### CONTROL UNITS FOR SENSITIVITY ANALYSIS #####

units.NOTJ = map_dfr(2008:2018, function(year) {
  print(year)
  
  pitches.full4 %>%
    filter(is.na(surgery_date)) %>%
    inner_join(max_pitches, by = 'pitcher') %>%
    group_by(pitcher) %>%
    mutate(surgery_date = ymd(paste0(year, "06", "01")),
           return_date = surgery_date + 485) %>% # 15 months
    filter(surgery_date > min(datetime)) %>%
    mutate(before = case_when(datetime < surgery_date ~ TRUE,
                              datetime >= return_date ~ FALSE)) %>%
    filter(!is.na(before)) %>%
    group_by(pitcher, before) %>%
    mutate(final = max(datetime), first = min(datetime)) %>%
    group_by(pitcher) %>%
    mutate(final = min(final), first = max(first)) %>%
    filter( (before == TRUE & datetime >  final - buff.pre.days - measure.period & datetime < final - buff.pre.days ) |
              (before == FALSE & datetime > first + buff.post.days & datetime < first + buff.post.days + measure.period)) %>%
    mutate(age = as.numeric(round((surgery_date - bday)/365))) %>%
    group_by(pitcher, age, height, weight, throws, fastest_pitch, before) %>%
    summarize(velo = weighted.mean(start_speed, w = pitch_type == fastest_pitch, na.rm = TRUE),
              pitches = sum(before),
              starter = sum(inning.x==1*before)) %>%
    group_by(pitcher) %>%
    mutate(pitches = max(pitches),
           starter = max(starter) > 0) %>%
    ungroup() %>%
    mutate(before = factor(ifelse(before == TRUE, "before", "after"), levels = c("before", "after")))  %>% 
    spread(before, velo) %>%
    mutate(TJ = 0)
}) %>% na.omit()
