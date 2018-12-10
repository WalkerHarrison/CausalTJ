# drop pitcher ID as covariate
datP = cbind(dat)
ggpairs(select(dat, height, weight)) # height and weight are related, only use one
dat = dat %>% select(-c(pitcher, height))

# treatment is ucl injury + TJ surgery
# include service_time?

# DID estimator - but need to stratify by covariates? model accomplishes stratification
# this is nonparmetric estimator
dat %>% group_by(TJ) %>% summarize(tBA = mean(after) - mean(before)) %>% pull(tBA) %>% diff()

################################################################################################

bootstrap_DID = function(data, bootstrap = FALSE) {
  if (bootstrap) {
    df = sample_n(filter(data, TJ == 1), nrow(filter(data, TJ == 1)), replace = TRUE) %>%
      rbind(sample_n(filter(data, TJ == 0), nrow(filter(data, TJ == 0)), replace = TRUE))
  } else {
    df = data
  }
  
  # each unit gets two rows, one for pre- and post-treatment
  dfDID = df %>% gather(key = "period", value = "velo", c("before", "after"))
  
  # fit outcome model
  m = lm(velo ~ TJ + period + TJ:period + age + weight +
           throws + fastest_pitch + starter, data = dfDID)
  
  # fit propensity score model
  lm.ps = glm(TJ ~ . - after, data = df, family = "binomial")
  df = df %>% mutate(ps = predict(lm.ps, type = "response"))
  
  # regression estimator
  df = df %>% mutate(
    predBefore = df %>% mutate(TJ = 0) %>%
      select(-c(after, ps)) %>%
      rename(velo = before) %>%
      mutate(period = "before") %>%
      select(names(dfDID)) %>%
      predict(m, type = "response", newdata = .),
    predAfter = df %>% mutate(TJ = 0) %>%
      select(-c(before, ps)) %>%
      rename(velo = after) %>%
      mutate(period = "after") %>%
      select(names(dfDID)) %>%
      predict(m, type = "response", newdata = .),
    w = ifelse(TJ == 1, 1, ps / (1 - ps)))
  
  t1 = df %>% summarize(sum(TJ * after) / sum(TJ)) %>% pull()
  
  t0reg = df %>%
    summarize(
      sum(TJ * before) / sum(TJ) + sum(TJ * (predAfter - predBefore)) / sum(TJ)) %>%
    pull()
  
  t0ipw = df %>% summarize(
    sum(TJ * before * w) / sum(TJ) + sum((1 - TJ) * (after - before) * w) / sum(TJ)) %>%
    pull()
  
  t0dr_reg = df %>%
    summarize(
      sum((TJ - ps) * (predAfter - predBefore) / (1 - ps)) / sum(TJ)) %>%
    pull()
  
  t0dr_ipw = df %>%
    summarize(
      sum((1 - TJ) * (after - before + predBefore - predAfter) * w) / sum(TJ)) %>%
    pull()
  
  t0dr = t0ipw + t0dr_reg
  
  regATT = t1 - t0reg
  ipwATT = t1 - t0ipw
  drATT  = t1 - t0dr
  
  return(list(t1 = t1, t0reg = t0reg, t0ipw = t0ipw, t0dr = t0dr))
  # return(list(regATT = regATT, ipwATT = ipwATT, drATT = drATT))
}

set.seed(2018)
thetas = unlist(bootstrap_DID(dat))
est.ATT = thetas[1] - thetas[-1]
print(est.ATT)
nsamps = 1000
bootstrap.ATT = sapply(1:nsamps, function(x) unlist(bootstrap_DID(dat, TRUE))) %>%
  apply(1, function(x) {.[1,] - x})
bootstrap.ATT = bootstrap.ATT[,-1]

apply(bootstrap.ATT, 2, function(x) {quantile(x, probs = c(0.025, 0.975))})

SE.ATT = apply(bootstrap.ATT, 2, sd)
est.ATT - 1.96*SE.ATT
est.ATT + 1.96*SE.ATT

apply(bootstrap.ATT, 2, mean)

#### test parallel trend assumption #####

datP2 = cbind(dat2)
ggpairs(select(dat2, height, weight)) # height and weight are related, only use one
dat2 = dat2 %>% select(-c(pitcher, height, after)) %>%
  rename(after = before, before = before2)

set.seed(2018)
thetas2 = unlist(bootstrap_DID(dat2))
est.ATT2 = thetas2[1] / thetas2[-1]
nsamps2 = 1000
bootstrap.ATT2 = sapply(1:nsamps2, function(x) unlist(bootstrap_DID(dat2, TRUE))) %>%
  apply(1, function(x) {.[1,] / x})
bootstrap.ATT2 = bootstrap.ATT2[,-1]

apply(bootstrap.ATT2, 2, function(x) {quantile(x, probs = c(0.025, 0.975))})

SE.ATT2 = apply(bootstrap.ATT2, 2, sd)
est.ATT2 - 1.96*SE.ATT2
est.ATT2 + 1.96*SE.ATT2

apply(bootstrap.ATT2, 2, mean)

#### extra stuff ####

# each unit gets two rows, one for pre- and post-treatment
datDID = dat %>% gather(key = "period", value = "velo", c("before", "after"))

# fit model
m = lm(velo ~ TJ + period + TJ:period + age + weight + throws + starter, data = datDID)

# get coefficient and standard error
m$coefficients["TJ:periodbefore"]
coef(summary(m))[, "Std. Error"]["TJ:periodbefore"]


df %>%
  ggplot(aes(ps, fill = factor(TJ))) + geom_histogram(alpha = 0.8) +
  labs(x = "propensity score", fill = "TJ surgery",
       title = "Propensity score by group")
  
#### ASD ####

dat_ASD = dat %>% mutate(starter = as.numeric(starter))
dat_ASD = dat_ASD %>%
  mutate_if(is.character, factor) %>%
  select_if(is.factor) %>%
  as.data.frame() %>%
  acm.disjonctif() %>%
  bind_cols(select_if(dat, is.numeric), .)
  
# fit propensity score model
lm.ps = glm(TJ ~ . - after, data = dat_ASD, family = "binomial")
dat_ASD = dat_ASD %>% mutate(ps = predict(lm.ps, type = "response"))

D = dat_ASD$TJ
weights_reg = 1
weights_ipw = dat_ASD %>%
  mutate(w = ifelse(TJ == 1, 1, ps / (1 - ps))) %>%
  pull(w)

calculate_ASD = function(weights) {
  dat_ASD %>%
    select(-c(TJ, after, ps)) %>%
    apply(2, function(x) {
      (abs(sum(D * x * weights) / sum(D * weights) -
             sum((1 - D) * x * weights) / sum((1 - D) * weights)) /
         sqrt(var((D * x)[which(D == 1)]) / sum(D * weights) +
                var(((1 - D) * x)[which(D == 0)]) / sum((1 - D) * weights))
       )
    })
  }

data.frame(Unweighted = calculate_ASD(weights_reg),
           IPW = calculate_ASD(weights_ipw)) %>%
  gather(weight) %>%
  mutate(weight = ordered(factor(weight), levels = c("Unweighted", "IPW"))) %>%
  ggplot(aes(x = weight, y = value)) +
  geom_boxplot() +
  geom_hline(aes(yintercept = 0.1), linetype = "dashed", color = "blue") +
  labs(y = "Abs. Standardized Difference") +
  theme(axis.title.x = element_blank())

