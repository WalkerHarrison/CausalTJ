library(tidyverse)
library(lubridate)
library(cobalt)
library(MatchIt)
library(WeightIt)
library(survey)
library(ade4)

setwd("/Users/lisalebovici/Documents/Duke/Fall18/STA640/hw/project")
load("TJ.Rdata")
load("/Users/lisalebovici/Documents/Duke/Fall17/STA523/hw/Project-523_and_Me/key_mappings.Rdata")
people = read_csv("People.csv")

people = people %>%
  left_join(select(key_mappings, key_mlbam, key_bbref), by = c("bbrefID" = "key_bbref")) %>%
  rename("pitcher" = "key_mlbam") %>%
  select(pitcher, everything())

fb_codes <- c('FA', # Generic Fastball
              'FF', # Four-Seamer
              'FT', # Two-seamer
              'FC') # Cutter

head(pitches) %>% mutate(year = year(tfs_zulu),
                         fastball = ifelse(pitch_type %in% fastballs, 1, 0))


# lots of dupes?
distinct(pitches.full3) %>% group_by(year = substr(sv_id, 1, 2)) %>% summarize(n=n())

### USE THIS DF
pitches.full4 = distinct(pitches.full3) %>%
  filter(!is.na(sv_id) & !(sv_id %in% c("", "_"))) %>%
  mutate(datetime = as.character(as.POSIXct(sv_id, format = "%y%m%d_%H%M%S")))

starters = pitches.full4 %>%
  filter(inning.x == 1) %>%
  pull(pitcher) %>%
  unique()

starter_fb = pitches.full4 %>%
  filter(pitch_type %in% fb_codes,
         pitcher %in% starters)

max_pitches <- starter_fb %>%
  group_by(pitcher, pitch_type) %>%
  summarize(n = n(), speed = mean(start_speed, na.rm = TRUE)) %>%
  group_by(pitcher) %>%
  mutate(pct_thrown = n/sum(n)) %>%
  filter(pct_thrown >= 0.1) %>%
  filter(speed == max(speed)) %>%
  ungroup() %>%
  select(pitcher, pitch_type) %>%
  rename(fastest_pitch = pitch_type)

# these guys had TJ surgery multiple times - exclude for now
TJmultiple = TJ.full %>%
  group_by(pitcher) %>%
  summarize(n=n()) %>%
  filter(n>1) %>%
  pull(pitcher)

TJ.once = TJ.full %>% filter(!(pitcher %in% TJmultiple))

TJ.pitchers <- TJ.once %>% pull(pitcher)

# now just look at starters
pitches.starters = pitches.full4 %>%
  inner_join(max_pitches, by = "pitcher") %>%
  mutate(TJ = ifelse(pitcher %in% pull(TJ.once, pitcher), 1, 0)) %>%
  left_join(select(TJ.once, pitcher, surgery_date, return_date, throws), by = "pitcher") %>%
  left_join(select(people, pitcher, birthYear, height, weight), by = "pitcher") %>%
  mutate(age = year(datetime) - birthYear,
         age_at_surgery = year(surgery_date) - birthYear)

pitches.starters.TJ = pitches.starters %>%
  filter(TJ == 1) %>%
  mutate(before_surgery = ifelse(datetime < surgery_date, T, F))


pitches.starters.TJ %>%
  filter(pitch_type == fastest_pitch &
           ((year(datetime) == year(surgery_date) - 2) |
              (year(datetime) == year(surgery_date) + 2))) %>%
  pull(pitcher) %>% unique() %>% length()


prepost = pitches.starters.TJ %>%
  filter(pitch_type == fastest_pitch & ( (year(datetime) == year(surgery_date) - 2) | (year(datetime) == year(surgery_date) + 2) ))

prepost %>%
  group_by(pitcher, before_surgery) %>%
  summarize(n=n()) %>%
  arrange(pitcher, desc(before_surgery)) %>%
  spread(key=before_surgery, value=n) %>%
  na.omit() %>% View()




treatment = prepost %>%
  mutate(timeframe = ifelse(before_surgery, "preAvgFB", "postAvgFB")) %>%
  select(pitcher, timeframe, start_speed) %>%
  group_by(pitcher, timeframe) %>%
  summarize(avgFB = mean(start_speed, na.rm = TRUE)) %>%
  spread(key=timeframe, value=avgFB) %>%
  select(pitcher, preAvgFB, postAvgFB) %>%
  na.omit() %>%
  inner_join(prepost %>%
               select(pitcher, fastest_pitch, TJ, surgery_date, return_date, throws, birthYear, height, weight, age_at_surgery) %>%
               distinct(),
             by="pitcher") %>%
  ungroup()

controlFB = pitches.starters %>%
  filter(TJ == 0)

controlPitchers = controlFB %>% pull(pitcher) %>% unique()
cp = head(controlPitchers)


control = plyr::ldply(1:length(controlPitchers), function(x) {
  print(x)
  i = controlPitchers[x]
  
  df = controlFB %>%
    filter(pitcher == i) %>%
    filter(pitch_type == fastest_pitch)
  
  years = df %>%
    pull(datetime) %>%
    range() %>%
    year()
  years = seq(years[1], years[2])
  
  if (length(years) < 5) {
    return()
  }
  
  plyr::ldply((1+2):(length(years) - 2), function(j) {
    y = years[j]
    fb = df %>%
      filter((year(datetime) == y - 2) | (year(datetime) == y + 2)) %>%
      mutate(timeframe = ifelse(datetime < y, "preAvgFB", "postAvgFB")) %>%
      select(pitcher, timeframe, start_speed) %>%
      group_by(pitcher, timeframe) %>%
      summarize(avgFB = mean(start_speed, na.rm = TRUE)) %>%
      spread(key=timeframe, value=avgFB) %>%
      na.omit() %>% unlist() %>% c(., y)
    
    if (length(fb) != 4) { # we're missing pre or post data
      return()
    } else {
      return(fb)
    }
  })
})

control = test

control = control %>%
  rename(surgery_date = V1) %>%
  select(pitcher, preAvgFB, postAvgFB, surgery_date) %>%
  left_join(max_pitches, by="pitcher") %>%
  mutate(TJ = 0, return_date = NA,
         surgery_date = as.Date(ISOdate(surgery_date, 1, 1)) ) %>%
  left_join(select(people, pitcher, throws, birthYear, height, weight), by="pitcher") %>%
  mutate(age_at_surgery = year(surgery_date) - birthYear) %>%
  select(names(treatment))
  

data_all_cols = bind_rows(treatment, control)
data = data_all_cols %>%
  select(-c(pitcher, return_date, birthYear, surgery_date)) %>%
  select(postAvgFB, everything()) %>%
  rename(age = age_at_surgery)

################################################################################################################################################################

# test unconfoundedness assumption with lagged outcome
# p-value on TJ is not significant so we're gucci
lm.lagged = lm(preAvgFB ~ TJ*(. - postAvgFB), data = data)

#### direct regression estimator ####

bootstrap_lm = function(data, bootstrap = FALSE) {
  if (bootstrap) {
    df = sample_n(data, nrow(data), replace = TRUE)
  } else {
    df = data
  }
  
  # fit outcome model (try something besides linear w/ main & interaction effects?)
  lm1 = lm(postAvgFB ~ TJ*(.), data = df)
  df = df %>% mutate(pred_control = predict(lm1, type = "response",
                                            newdata = mutate(., TJ = 0)))
  
  ATT = df %>%
    filter(TJ == 1) %>%
    summarize(sum(postAvgFB - pred_control) / nrow(.)) %>%
    pull()
  
  return(ATT)
}

# get actual estimate for ATT
ATT.lm = bootstrap_lm(data)

# bootstrap to calculate standard error
set.seed(2018)
nsamps = 1000
bootstrap.lm =  sapply(1:nsamps, function(x) bootstrap_lm(data, TRUE))
SE.lm = sd(bootstrap.lm)

#### matching estimator ####

# calculate propensity score and do balance check
glm.ps = glm(TJ ~ . - postAvgFB, data = data, family = "binomial")
glm.predict = predict(lm.ps, type = "response")
data = data %>% mutate(ps = glm.predict)

ggplot(data, aes(x = ps, fill = as.factor(TJ))) +
  geom_histogram(alpha = 0.5) +
  labs(fill = "TJ surgery", x = "propensity score") +
  theme(axis.title.x = element_text(size=9),
        axis.title.y = element_text(size=9),
        legend.title = element_text(size=9))


# start with MatchIt - but eventually try to match multiple controls to treatment? can do with MatchIt

# love.plot(bal.tab(match), threshold = .1) # balance got worse...?

bootstrap_match <- function(data, bootstrap = FALSE){
  if (bootstrap) {
    df = sample_n(data, nrow(data), replace = TRUE)
  } else {
    df = data
  }
  
  df = df %>%
    mutate_if(is.character, factor) %>%
    select_if(is.factor) %>%
    as.data.frame() %>%
    acm.disjonctif() %>%
    bind_cols(df, .) %>%
    select(-c(fastest_pitch, throws))
  
  match <- matchit(TJ ~ preAvgFB + height + weight + age + fastest_pitch.FC +
              fastest_pitch.FF + fastest_pitch.FT + throws.L + throws.R,
            data = df, method = "nearest", distance = "logit", ratio = 6, replace = TRUE) # 6 control units per treated unit, with replacement
  
  TJ.matched <- match.data(match)
  
  estimate <- TJ.matched %>% group_by(TJ) %>%
    summarize(mean = mean(postAvgFB)) %>% pull(mean) %>% diff()
  
  return(estimate)
}

# get actual estimate for ATT
ATT.match = bootstrap_match(data)

# bootstrap to calculate standard error
set.seed(2018)
nsamps = 1000
bootstrap.match =  sapply(1:nsamps, function(x) bootstrap_match(data, TRUE))
SE.match = sd(bootstrap.match)

#### weighting estimator #### is this IPW or overlap?

bootstrap_weight <- function(data, bootstrap = FALSE){
  if (bootstrap) {
    df = sample_n(data, nrow(data), replace = TRUE)
  } else {
    df = data
  }
  
  df = df %>%
    mutate_if(is.character, factor) %>%
    select_if(is.factor) %>%
    as.data.frame() %>%
    acm.disjonctif() %>%
    bind_cols(df, .) %>%
    select(-c(fastest_pitch, throws))
  
  weight <- weightit(TJ ~ preAvgFB + height + weight + age + fastest_pitch.FC +
               fastest_pitch.FF + fastest_pitch.FT + throws.L + throws.R,
             data = df, method = "ps", estimand = "ATT")
  
  d.w <- svydesign(ids = ~1, weights = get.w(weight),
                   data = df)
  fit <- svyglm(postAvgFB ~ TJ, design = d.w)
  
  return(unname(fit$coefficients['TJ']))
}

love.plot(bal.tab(weight), threshold = .1) # reaaaalllly helps balance

# get actual estimate for ATT
ATT.weight = bootstrap_weight(data)

# bootstrap to calculate standard error
set.seed(2018)
nsamps = 1000
bootstrap.weight = sapply(1:nsamps, function(x) bootstrap_weight(data, TRUE))
SE.weight = sd(bootstrap.weight)

#### double robust estimator ####

#### ASD ####

data %>% group_by(TJ) %>%
  

