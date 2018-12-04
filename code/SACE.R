bootstrap_SACE <- function(){

  dat <- units.TJ %>% rbind(units.NOTJ) %>%
    filter(!is.na(before), !is.nan(before), !is.nan(after)) %>%
    filter(!is.na(height)) %>%
    mutate(diff = after - before) %>%
    select(-after, - pitcher) %>%
    mutate(S = !is.na(diff)) %>%
    group_by(TJ, S) %>% sample_n(100, replace = TRUE) %>% ungroup()
  
  glm.fit <- glm(TJ ~ ., data = dat %>% select(-diff, -brk), family = binomial)
  
  dat <- dat %>% mutate(w = predict(glm.fit, type = "response"))
  #View(dat)
  ###### step 1
  
  pi.LL <- dat %>% filter(TJ == 0) %>% summarize(pi = weighted.mean(S == 1, w = w)) %>% pull(pi)
  pi.DD <- dat %>% filter(TJ == 1) %>% summarize(pi = weighted.mean(S == 0, w = w)) %>% pull(pi)
  pi.LD <- 1 - pi.LL - pi.DD
  
  mu0.LL <- Y.0L <- dat %>% filter(TJ == 0, S == 1) %>% summarize(Y = weighted.mean(diff, w = w)) %>% pull(Y)
  
  ##### step 2
  
  PA.LL <- dat %>% filter(TJ == 0, S == 1) %>% 
    mutate(w = w/sum(w)) %>%
    summarize(p = list(approxfun(density(brk, weights = w)))) %>% pull(p) %>% .[[1]]
  
  PA.DD <- dat %>% filter(TJ == 1, S == 0) %>% 
    mutate(w = w/sum(w)) %>%
    summarize(p = list(approxfun(density(brk, weights = w)))) %>% pull(p) %>% .[[1]]
  
  #PA.LL <- dat %>% filter(TJ == 0, S == 1) %>% pull(brk) %>% density() %>% approxfun()
  #PA.DD <- dat %>% filter(TJ == 1, S == 0) %>% pull(brk) %>% density() %>% approxfun()
  PA <- dat %>% mutate(w = w/sum(w)) %>% summarize(p = list(approxfun(density(brk, weights = w)))) %>% pull(p) %>% .[[1]]
  PA.LD <- function(a){(PA(a) - pi.LL*PA.LL(a) - pi.DD*PA.DD(a))/pi.LD}
  
  #### step 3
  B.a <- function(a){pi.LL*PA.LL(a)/(pi.LL*PA.LL(a) + pi.LD*PA.LD(a))}
  
  ### step 4
  
  mu.a <- function(a){
    smooth <- dat %>% filter(TJ == 1, S == 1) %>% mutate(w = w/sum(w)) %>% loess(diff ~ brk, data = ., weights = w)
    return(predict(smooth, newdata = a))
    }
  
  ### step 5
  
  Ak <- seq(quantile(dat$brk, 0.25), quantile(dat$brk, 0.75), length.out = 10)
  mu.Ak <- mu.a(Ak)
  B.Ak <- B.a(Ak)
  B.Ak.inv <- 1 - B.Ak
  
  mu1.LL <- lm(mu.Ak ~ B.Ak +  B.Ak.inv - 1)$coefficients[1]
  
  ### step 6
  return(mu1.LL - mu0.LL)
  
}

a <- sapply(1:1000, function(x) bootstrap_SACE())
mean(a); quantile(a, .025); quantile(a, 0.975)


