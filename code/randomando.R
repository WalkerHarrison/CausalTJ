##### fake data

Z <- c(1, 1, 1, 0, 0, 0)
G <- c('LL', 'LD', 'DD', 'LL', 'LD', 'DD')
prop.zg <- rep(1/6, 6)
S <- c(1, 1, 0, 1, 0, 0)
Y <- c(5, 3, NA, 4, NA, NA)
prop.azg <- c(2/3, 1/3, 1/2, 2/3, 1/3, 1/2)

A2 <- data.frame(Z, G, prop.zg, S, Y, prop.azg)
B2 <- A2 %>% group_by(Z,S) %>% 
  summarize(prop.zs = sum(prop.zg), 
            Y = mean(Y), 
            prop.azs = mean(prop.azg)) %>% 
  arrange(desc(Z), desc(S))

A2
B2
