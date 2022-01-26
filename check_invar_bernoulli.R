library(rethinking)
library(zipfR)

ZM <- lnre("zm", alpha = 0.50, B = 0.2)
total_freq <- as.numeric(as.character(rlnre(ZM, n=100)))
hist(log(total_freq), breaks = 100, xaxt="n", main = "Histogram of total_freq", xlab = "total_freq")
axis(side = 1, at = log(total_freq), labels = total_freq)

s_plurals <- total_freq[1:(length(total_freq)/2)]
en_plurals <- total_freq[(length(total_freq)/2+1):length(total_freq)]

s_plurals_before_stress <- rbinom(rep(1, length(s_plurals)), 
                                  s_plurals, 
                                  rep(0.3, length(s_plurals)))

s_plurals_before_nostress <- s_plurals - s_plurals_before_stress

en_plurals_before_stress <- rbinom(rep(1, length(en_plurals)), 
                                  en_plurals, 
                                  rep(0.7, length(en_plurals)))

en_plurals_before_nostress <- en_plurals - en_plurals_before_stress

S <- c()
item <- c()
stress <- c()

item_index = 0
for (i in s_plurals_before_stress){
  item_index <- item_index + 1
  S <- c(S, rep(1, i))
  item <- c(item, rep(item_index, i))
  stress <- c(stress, rep(1, i))
}
item_index = 0
for (i in s_plurals_before_nostress){
  item_index <- item_index + 1
  S <- c(S, rep(1, i))
  item <- c(item, rep(item_index, i))
  stress <- c(stress, rep(2, i))
}

item_index = length(s_plurals)
for (i in en_plurals_before_stress){
  item_index <- item_index + 1
  S <- c(S, rep(0, i))
  item <- c(item, rep(item_index, i))
  stress <- c(stress, rep(1, i))
}
item_index = length(s_plurals)
for (i in en_plurals_before_nostress){
  item_index <- item_index + 1
  S <- c(S, rep(0, i))
  item <- c(item, rep(item_index, i))
  stress <- c(stress, rep(2, i))
}

m0 <- ulam(
  alist(
    S ~ dbern(prob),
    logit(prob) <- a[item] + b[stress],
    b[stress] ~ dnorm(0, 1),
    a[item] ~ dnorm(0, sigma),
    sigma ~ dexp(1)
  ), 
  data = list(S = as.integer(S), 
              item = as.integer(item), 
              stress = as.integer(stress)
  ), 
  chains = 4, cores = 4, iter = 4000
)

precis(m0)
post <- extract.samples(m0)
post$b12 <- post$b[,1] - post$b[,2]
plot(density(post$b12))
abline(v = 0, lty = "dashed")

m0b <- ulam(
  alist(
    S ~ dbern(prob),
    logit(prob) <- b[stress],
    b[stress] ~ dnorm(0, 1)
  ), 
  data = list(S = as.integer(S), 
              stress = as.integer(stress)
  ), 
  chains = 4, cores = 4, iter = 4000
)

precis(m0b, depth = 2)
post <- extract.samples(m0b)
post$b12 <- post$b[,1] - post$b[,2]
plot(density(post$b12), xlim = c(-2,0.5))
abline(v = 0, lty = "dashed")

