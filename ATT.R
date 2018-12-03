library(GGally)
library(cobalt)
library(MatchIt)
library(WeightIt)
library(survey)

dat2 <- dat %>% mutate(diff = after - before) %>% select(-c(pitcher, after)) %>%
  mutate(TJ = TJ == 1,
         throws = throws == "R")

ggpairs(dat2)

####### DIRECT REGRESSION ESTIMATOR
summary(lm(diff ~ ., data = dat2))

debit %>% mutate(prop.score = glm.predict) %>%
  ggplot(aes(prop.score, fill = debit_card1998)) +
  geom_histogram(alpha = 0.5)


####### 

match <- matchit(TJ ~ age + height + weight + throws + pitches + starter + before, 
                 data = dat2, method = "nearest", distance = "logit", ratio = 10, replace = FALSE)

TJ.matched <- match.data(match)

TJ.matched %>% group_by(TJ) %>%
  summarize(mean = mean(diff)) %>% pull(mean) %>% diff()

View(TJ.matched)

######

weight <- weightit(TJ ~ age + height + weight + throws + pitches + starter + before, 
                   data = dat2, method = "ps", estimand = "ATT")

d.w <- svydesign(ids = ~1, weights = get.w(weight),
                 data = dat2)

fit <- svyglm(diff ~ TJ, design = d.w)

fit$coefficients

#######

weight0 <- weightit(TJ ~ age + height + weight + throws + pitches + starter + before, 
                   data = dat2, method = "ps", estimand = "ATT")

lm(diff ~ ., data = dat2, weights = weight0$weights)

debitT <- debit.bs %>%
  filter(debit_card1998 == TRUE) %>%
  pull(spending1998) %>%
  mean()

debitF <- debit.bs %>%
  filter(debit_card1998 == TRUE) %>%
  mutate(debit_card1998 = FALSE) %>%
  mutate(pred = predict(lm.dr, newdata = .)) %>%
  dplyr::summarize(mean = mean(pred)) %>%
  pull(mean)

return(debitT - debitF)

##################################################


x <- pitches.TJ %>% 
  inner_join(max_pitches, by = 'pitcher') %>%
  filter(pitch_type == fastest_pitch) %>%
  group_by(pitcher) %>%
  filter(before == TRUE, datetime > final - 60) %>%
  select(pitcher, datetime, start_speed) %>%
  mutate(date = as.Date(datetime)) %>%
  mutate(date = as.numeric(date - max(date))) %>%
  mutate(start_speed = start_speed - mean(start_speed))

x %>% ggplot(aes(date, start_speed)) + 
  geom_point(alpha = 0.01) +
  geom_smooth() +
  scale_y_continuous(limits = c(-5, 5))
  

