if (Sys.info()[1] == "Darwin"){
  f_path = "/Volumes/tensusers/timzee/other/"
} else {
  f_path = "/vol/tensusers/timzee/other/"
}

library(dagitty)
library(rethinking)


dagEN <- dagitty("dag {
  SS [unobserved]
  PL [unobserved]
  M [exposure]
  V [outcome]
  SS <- R -> V
  SS -> W -> M -> V
  W -> Ph -> V
  W -> F -> V
  SP -> V
  SS -> PR
  PL -> PR
  PR -> V
  SS -> nW -> nPh -> V
  nW -> nF -> V
}")

coordinates(dagEN) <- list( x=c(M=2,SS=1,W=1,F=3,Ph=3,nW=2,nPh=3,nF=3,V=5,PR=3,PL=4,R=4,SP=4), 
                            y=c(M=3,SS=1,W=4,F=5,Ph=4,nW=2,nPh=2,nF=3,V=4,PR=1,PL=1,R=0,SP=5) ) 
drawdag( dagEN )

par(mar=c(5.1, 4.1, 4.1, 2.1), mfrow=c(1,1))

##
d <- read.csv(paste(f_path, "OSvar2.csv", sep = ""), header = T, row.names = NULL)

d_dist <- table(d$item, d$s_plural)
d_dist2 <- d_dist[d_dist[,"0"] != 0 & d_dist[,"1"] != 0,]
d_dist3 <- data.frame(item=row.names(d_dist2), en=d_dist2[,"0"], s=d_dist2[,"1"])
d_dist3$prop <- d_dist3$s / (d_dist3$en + d_dist3$s)
d_dist4 <- d_dist3[d_dist3$prop > 0.1 & d_dist3$prop < 0.9,]

d2 <- d[d$item %in% d_dist4$item,]
d3 <- na.omit(d2)
pca <- prcomp(d3[, c("f2", "f3", "f4", "f5", "f6", "f7", "f8")])
pca_df <- as.data.frame(pca$x)
d4 <- cbind(d3, pca_df)
d4$item_i <- coerce_index(d4$item)
d4$next_s <- as.integer(ifelse(d4$next_sound == "s", 1, 2))
d4$next_stress_i <- ifelse(d4$next_stress == 1, 1, 2)

library(rethinking)

m0 <- ulam(
  alist(
    S ~ dbern(prob),
    logit(prob) <- a[item],
    a[item] ~ dnorm(a_bar, sigma),
    a_bar ~ dnorm(0, 2),
    sigma ~ dexp(1)
  ), data = list(S = d4$s_plural, item = d4$item_i), chains = 4, cores = 4
)

precis(m0)

m1 <- ulam(
  alist(
    S ~ dbern(prob),
    logit(prob) <- a[item] + b[ns],
    b[ns] ~ dnorm(0, 1),
    a[item] ~ dnorm(a_bar, sigma),
    a_bar ~ dnorm(0, 2),
    sigma ~ dexp(1)
  ), data = list(S = d4$s_plural, item = d4$item_i, ns = d4$next_s), chains = 4, cores = 4
)

precis(m1, depth = 2)

# decentered model
m1d <- ulam(
  alist(
    S ~ dbern(prob),
    logit(prob) <- a_bar + z[item]*sigma + b[ns],
    b[ns] ~ dnorm(0, 1),
    z[item] ~ dnorm(0, 1),
    a_bar ~ dnorm(0, 2),
    sigma ~ dexp(1),
    gq> vector[item]:a <<- a_bar + z*sigma
  ), data = list(S = d4$s_plural, item = d4$item_i, ns = d4$next_s), chains = 4, cores = 4
)

precis(m1d, depth = 2)

m3 <- ulam(
  alist(
    S ~ dbern(prob),
    logit(prob) <- a_bar + z[item]*sigma + b_pc1*PC1,
    b_pc1 ~ dnorm(0, 1),
    z[item] ~ dnorm(0, 1),
    a_bar ~ dnorm(0, 2),
    sigma ~ dexp(1),
    gq> vector[item]:a <<- a_bar + z*sigma
  ), data = list(S = d4$s_plural, item = d4$item_i, PC1 = d4$PC1), chains = 4, cores = 4
)

precis(m3, depth = 2)

m2 <- ulam(
  alist(
    S ~ dbern(prob),
    logit(prob) <- a_bar + z[item]*sigma + b[nst],
    b[nst] ~ dnorm(0, 1),
    z[item] ~ dnorm(0, 1),
    a_bar ~ dnorm(0, 2),
    sigma ~ dexp(1),
    gq> vector[item]:a <<- a_bar + z*sigma
  ), data = list(S = d4$s_plural, item = d4$item_i, nst = d4$next_stress_i), chains = 3, cores = 3
)

precis(m2, depth = 2)

m_bla <- quap(
  alist(
    S ~ dbern(prob),
    logit(prob) <- b[ns],
    b[ns] ~ dnorm(0, 1)
  ), data = list(S = d4$s_plural, ns = d4$next_stress_i)
)



