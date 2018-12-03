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
  mutate(final = max(datetime), first = min(datetime)) %>% filter(pitcher == 276520) %>% View()
  group_by(pitcher) %>%
  mutate(final = min(final), first = (max(first)))

buff.pre.days <- 365
buff.post.days <- 365
measure.period <- 365

units.TJ <- pitches.TJ %>% group_by(pitcher) %>%
  filter( (before == TRUE & datetime >  final - buff.pre.days - measure.period & datetime < final - buff.pre.days ) |
          (before == FALSE & datetime > first + buff.post.days & datetime < first + buff.post.days + measure.period)) %>%
  inner_join(max_pitches, by = 'pitcher') %>%
  group_by(pitcher, before) %>%
  summarize(velo = weighted.mean(start_speed, w = pitch_type == fastest_pitch)) %>% 
  spread(before, velo) %>% 
  setNames(c('pitcher', 'after', 'before')) %>%
  select('pitcher', 'before', 'after')

units.TJ %>%
  filter(!is.na(before) & !is.na(after)) %>%
  View()

##### CONTROL UNITS

pitches.NOTJ <- pitches.full4 %>%
  filter(is.na(surgery_date)) %>%
  inner_join(max_pitches, by = 'pitcher') %>%
  group_by(pitcher, year = year(datetime)) %>%
  summarize(velo = weighted.mean(start_speed, w = pitch_type == fastest_pitch)) %>%
  mutate(year4 = year + 4)

units.NOTJ <- pitches.NOTJ %>% inner_join(pitches.NOTJ, by = c('pitcher', 'year4' = 'year')) %>% 
  select(-year4.y)


