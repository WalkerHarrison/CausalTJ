library(tidyverse)
library(lubridate)

load("data/key_mappings.Rdata")
load("data/pitches_full5.Rdata")
people <- read_csv("data/People.csv")
TJ <- read_csv("data/TJ.csv")

##### ADD MLBID TO PEOPLE
people <- people %>%
  left_join(select(key_mappings, key_mlbam, key_bbref), by = c("bbrefID" = "key_bbref")) %>%
  rename("pitcher" = "key_mlbam") %>%
  mutate(bday = ymd(paste(birthYear, birthMonth, birthDay))) %>%
  select(pitcher, bday, height, weight, throws)

##### ADD PERSONAL DATA TO PITCHES
pitches.full5 <- pitches.full5 %>% 
  filter(!is.na(sv_id) & !(sv_id %in% c("", "_"))) %>%
  mutate(brk = sqrt(pfx_x^2 + pfx_z^2),
         datetime = as.character(as.POSIXct(sv_id, format = "%y%m%d_%H%M%S"))) %>%
  select(-pfx_x, -pfx_z, -sv_id) %>%
  left_join(people, by = "pitcher")

##### ADD TJ TO PITCHES
TJ.once <- TJ %>% group_by(pitcher) %>%
  mutate(n = n()) %>%
  filter(n < 2) %>%
  select(pitcher, surgery_date, return_date)

pitches.full5 <- pitches.full5 %>% 
  left_join(TJ.once, by = "pitcher") %>%
  mutate(datetime = as.Date(datetime))

max_pitches <- pitches.full5 %>%
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

pitches.TJ <- pitches.full5 %>%
  filter(!is.na(surgery_date)) %>%
  group_by(pitcher) %>%
  mutate(a = min(datetime)) %>%
  filter(surgery_date > min(datetime)) %>%
  mutate(before = datetime < surgery_date) %>%
  group_by(pitcher, before) %>%
  mutate(final = max(datetime), first = min(datetime)) %>%
  group_by(pitcher) %>%
  mutate(final = min(final), first = (max(first)))

buff.pre.days <- 90
buff.post.days <- 90
measure.period <- 365

units.TJ <- pitches.TJ %>% group_by(pitcher) %>%
  filter( (before == TRUE & datetime >  final - buff.pre.days - measure.period & datetime < final - buff.pre.days ) |
            (before == FALSE & datetime > first + buff.post.days & datetime < first + buff.post.days + measure.period)) %>%
  inner_join(max_pitches, by = 'pitcher') %>%
  mutate(age = as.numeric(round((surgery_date - bday)/365))) %>%
  group_by(pitcher, age, height, weight, throws, before) %>%
  summarize(velo = weighted.mean(start_speed, w = pitch_type == fastest_pitch, na.rm = TRUE),
            brk = weighted.mean(brk, w = before, na.rm = TRUE),
            pitches = sum(before),
            starter = sum(inning.x==1*before)) %>%
  group_by(pitcher) %>%
  mutate(pitches = max(pitches),
         brk = max(brk, na.rm = TRUE),
         starter = max(starter) > 0) %>%
  ungroup() %>%
  mutate(before = factor(ifelse(before == TRUE, "before", "after"), levels = c("before", "after")))  %>% 
  spread(before, velo) %>%
  mutate(TJ = 1)


##### CONTROL UNITS

pitches.NOTJ <- pitches.full5 %>%
  filter(is.na(surgery_date)) %>%
  inner_join(max_pitches, by = 'pitcher') %>%
  mutate(year = year(datetime),
         age = year(datetime) - year(bday) + 2) %>% 
  group_by(pitcher, year, age, height, weight, throws) %>%
  summarize(velo = weighted.mean(start_speed, w = pitch_type == fastest_pitch),
            brk = mean(brk, na.rm = TRUE),
            pitches = n(),
            starter = sum(inning.x==1)>0) %>%
  filter(pitches>100) %>%
  mutate(year4 = year + 4)

units.NOTJ <- pitches.NOTJ %>% left_join(pitches.NOTJ, by = c('pitcher', 'height', 'weight', 'throws',
                                                               'year4' = 'year')) %>% 
  ungroup() %>%
  select(pitcher, age.x, height, weight, throws, brk.x, pitches.x, starter.x, velo.x, velo.y) %>%
  setNames(c('pitcher', 'age', 'height', 'weight', 'throws', 'brk', 'pitches', 'starter', 'before', 'after')) %>%
  mutate(TJ = 0)
