if (Sys.info()[1] == "Darwin"){
  f_path = "/Volumes/tensusers/timzee/"
} else {
  f_path = "/vol/tensusers/timzee/"
}

library(rethinking)

##
d <- read.csv(paste(f_path, "other/SonarVar.csv", sep = ""), header = T, row.names = NULL)

d_dist <- table(d$item, d$s_plural)
d_dist2 <- d_dist[d_dist[,"0"] != 0 & d_dist[,"1"] != 0,]
d_dist3 <- data.frame(item=row.names(d_dist2), en=d_dist2[,"0"], s=d_dist2[,"1"])
d_dist3$prop <- d_dist3$s / (d_dist3$en + d_dist3$s)
d_dist4 <- d_dist3[d_dist3$prop > 0.1 & d_dist3$prop < 0.9,]

d2 <- d[d$item %in% d_dist4$item,]


d3 <- na.omit(d2)
d3 <- d3[d3$next_vowel_length != 0,]
table(list(prosodic_break=d3$prosodic_break, next_stress=d3$next_stress, s_plural=d3$s_plural))

# 1: s-sound, vowel, consonant
d3$next_sound2 <- ifelse(d3$next_sound %in% c("s", "z"), 1, 
                         ifelse(d3$next_sound %in% c("@", "a", "A", "e", "E", "i", "I", "K", "L", "M", "o", "O", "y"), 2, 3))
d3$next_sound2 <- as.integer(d3$next_sound2)
d3$next_s <- ifelse(d3$next_sound %in% c("s", "z"), 1, 2)
d3$next_s <- as.integer(d3$next_s)
# 1: break - stress, 2: break - nostress, 3: nobreak - stress, 4: nobreak - nostress
d3$stress_interaction <- ifelse(d3$prosodic_break == "1" & d3$next_stress == "1", 1,
                                ifelse(d3$prosodic_break == "1" & d3$next_stress == "0", 2,
                                       ifelse(d3$prosodic_break == "0" & d3$next_stress == "1", 3, 4)))
d3$stress_interaction <- as.integer(d3$stress_interaction)
d3$sound_interaction <- ifelse(d3$prosodic_break == "1" & d3$next_sound2 == "1", 1,
                               ifelse(d3$prosodic_break == "1" & d3$next_sound2 == "2", 2,
                                      ifelse(d3$prosodic_break == "1" & d3$next_sound2 == "3", 3,
                                             ifelse(d3$prosodic_break == "0" & d3$next_sound2 == "1", 4,
                                                    ifelse(d3$prosodic_break == "0" & d3$next_sound2 == "2", 5, 6)))))
d3$sound_interaction <- as.integer(d3$sound_interaction)
d3$item_index <- coerce_index(d3$item)

# d3 still needs to be cleaned manually of a few French sentences
check_french <- d3[d3$item %in% c("compagnie", "ballon", "dessert", "directeur", "galerie", "luitenant", "redacteur", "sergeant") & d3$s_plural == 1,]

# 16145: Compagnies des Alpes baat al vijftien pretparken uit in Nederland  Duitsland  Frankrijk en Groot-Brittannië : onder meer het Franse Asterixpark en het dolfinarium in Harderwijk ( Nederland ) .
# 34370: Cette liste reprend quelques exemples d'offres on-line de compagnies d'assurances .
# 37630: Désormais  les directeurs des ressources humaines deviennent les responsables .
# 47134: D'autres compagnies l'excluent .
# 90132: Compagnies des Alpes baat al vijftien pretparken uit in Nederland  Duitsland  Frankrijk en Groot-Brittannië  waaronder het Franse Asterixpark .
# 94461: Galeries des Minimes  Minimenstraat 23

# 21343: Op bataljonsniveau waren dat de kapitein gevechtsinlichtingen en diens naast medewerkers de sergeant-majoor gevechtsinlichtingen en de sergeant-majoor militaire veiligheid ; bij de compagnie vervulde de compagnies - sergeant-majoor deze taak .
# 94998: Ook in dit geval vond men - zowel de compagniescommandant als diens compagnies sergeant-majoor - dit niet onder de termen vallen van een mogelijk strafbaar feit .
# 95010: Ik kreeg dat te horen van de compagnies sergeant-majoor  en toen heb ik die sergeant op het matje geroepen .


rm_lines <- c("16145", "34370", "37630", "47134", "90132", "94461", "21343", "94998", "95010")

d3 <- d3[!(row.names(d3) %in% c("16145", "34370", "37630", "47134", "90132", "94461", "21343", "94998", "95010")),]


# can't i get rid of a_bar?
m1 <- ulam(
  alist(
    S ~ dbern(prob),
    logit(prob) <- a[item] + b[stress] + g[sound],
    b[stress] ~ dnorm(0, 1),
    g[sound] ~ dnorm(0, 1),
    a[item] ~ dnorm(a_bar, sigma),
    a_bar ~ dnorm(0, 2),
    sigma ~ dexp(1)
  ), 
  data = list(S = d3$s_plural, 
              item = d3$item_index, 
              stress = d3$stress_interaction,
              sound = d3$next_s
              ), 
  chains = 4, cores = 4, iter = 4000, log_lik = T
)

saveRDS(object = m1, file = concat(f_path, "LIN_project/m1.rds"))

post <- extract.samples(m1)
# look at sound effects
post$g1_2 <- post$g[,1] - post$g[,2]
plot(density(post$g1_2), xlim=c(-0.2, 1))
abline(v = 0, lty="dotted")

g_mu <- apply(post$g, 2, mean)
g_pi <- apply(post$g, 2, PI)
 
plot(1:2, g_mu, ylim=c(-2,2), xaxt="n")
axis(side = 1, at = 1:2, labels = c("S", "NS"))
for (i in 1:2) lines(c(i,i), g_pi[,i])



# calculate difference of difference (interaction); see p. 178 of rethinking pdf
# see also p. 355 in rethinking pdf for similar analysis
post$b1_2 <- post$b[,1] - post$b[,2]
post$b3_4 <- post$b[,3] - post$b[,4]
plot(density(post$b3_4), xlim=c(-0.5, 0.75))
lines(density(post$b1_2), lty="dashed")
abline(v = 0, lty="dotted")
diffs <- list(db12 = post$b1_2, db34 = post$b3_4 )
plot(precis(diffs))

post$b1_2_3_4 <- post$b1_2 - post$b3_4
plot(density(post$b1_2_3_4))
abline(v=0, lty="dashed")
PI(post$b1_2_3_4, prob = .95)
PI(post$b1_2, prob = .95)
PI(post$b3_4, prob = .95)

b_mu <- apply(post$b, 2, mean)
b_pi <- apply(post$b, 2, PI)

plot(1:4, b_mu, ylim=c(-2,2), xaxt="n")
axis(side = 1, at = 1:4, labels = c("B-S", "B-NS", "NB-S", "NB-NS"))
for (i in 1:4) lines(c(i,i), b_pi[,i])

# very large confidence intervals, but differences are "significant"
# what is going on here?
pairs(m1)
# this shows b1 and b2 distributions very positively correlated 
# and b3 and b4 distributions very positively correlated
# let's see what happens if we plot posterior predictions
# see also p. 458 in rethinking pdf for similar analysis

p_link_custom <- function( item, treatment ) {
  logodds <- with( post , a_bar + a[,item] + b[,treatment] + g[,1] ) 
  return( inv_logit(logodds) )
}

p_raw <- mapply(p_link_custom, rep(1:max(d3$item_index), times = 4), rep(1:4, each = max(d3$item_index)))

p_ar <- array(data = as.vector(p_raw), dim = c(length(post$a_bar), max(d3$item_index), 4))

par(mfrow=c(2,1), mar=c(1,2,2,1))
for (k in 0:1){
  plot( NULL , xlim=c(1,4*6) , 
        ylim=c(0, 1) , 
        xlab="" ,
        ylab="prop(-s)" , xaxt="n" )
  for ( j in 1:(6-1) ) abline( v=(j-1)*4+4.5 , lwd=0.5 )
  for ( j in 1:6 ) {
    word_name <- d3[d3$item_index == k*6+j,]$item[1]
    text( (j-1)*4+2.5 , 1.1 , word_name , xpd=TRUE )
  }
  # draw in predictions
  for ( j in (1:6) ) {
    for (i in 1:4) {
      p_mu <- mean(p_ar[,k*6+j,i])
      p_ci <- PI(p_ar[,k*6+j,i], prob = .95)
      points(c((j-1)*4 + i), c(p_mu))
      lines(c((j-1)*4 + i, (j-1)*4 + i), p_ci)
    }
  }
}
par(mar=c(5.1,4.1,4.1,2.1), mfrow=c(1,1))

# still very large uncertainty
# does that suggest that the effect of condition differs 
# quite a bit from item to item?
# or because i'm used to dummy coded variables
# let's check:
# dBla = d3[,c("s_plural", "prosodic_break", "next_stress", "next_s", "item")]
# dBla$prosodic_break = as.factor(dBla$prosodic_break)
# dBla$next_stress = as.factor(dBla$next_stress)
# dBla$next_s = as.factor(dBla$next_s)
# dBla$item = as.factor(dBla$item)
# mBla = glmer(s_plural ~ next_stress*prosodic_break + next_s + (1|item), data = dBla, family = binomial(link = "logit"))
# plot(effect("next_stress*prosodic_break", mBla))
# summary(mBla)

m2 <- ulam(
  alist(
    S ~ dbern(prob),
    # linear model
    logit(prob) <- a[item,stress] + b[stress] + g[sound],
    # adaptive priors
    vector[4]:a[item] ~ multi_normal(0,Rho_item,sigma_item),
    # fixed priors
    b[stress] ~ dnorm(0, 1),
    g[sound] ~ dnorm(0, 1),
    sigma_item ~ dexp(1),
    Rho_item ~ dlkjcorr(4)
  ), 
  data = list(S = d3$s_plural, 
              item = d3$item_index, 
              stress = d3$stress_interaction,
              sound = d3$next_s), 
  chains = 4, cores = 4, iter = 4000, log_lik = T
)

saveRDS(object = m2, file = concat(f_path, "LIN_project/m2.rds"))

post2 <- extract.samples(m2)

post2$b1_2 <- post2$b[,1] - post2$b[,2]
post2$b3_4 <- post2$b[,3] - post2$b[,4]
plot(density(post2$b3_4), xlim=c(-0.5, 0.75))
lines(density(post2$b1_2), lty="dashed")
abline(v = 0, lty="dotted")
post2$b1_2_3_4 <- post2$b1_2 - post2$b3_4
plot(density(post2$b1_2_3_4))
abline(v = 0, lty="dotted")
# no more fixed effect

# plot random slopes
p_link_custom <- function( item, treatment ) {
  logodds <- with( post2 , a[,item,treatment] + b[,treatment] + g[,2] ) 
  return( inv_logit(logodds) )
}

p_raw <- mapply(p_link_custom, rep(1:max(d3$item_index), times = 4), rep(1:4, each = max(d3$item_index)))

p_ar <- array(data = as.vector(p_raw), dim = c(nrow(post2$b), max(d3$item_index), 4))

library(scales)
coln <- 6
rown <- 2 
par(mfrow=c(rown,1), mar=c(1,2,2,1))
for (k in 0:(rown-1)){
  plot( NULL , xlim=c(1,4*coln) , 
        ylim=c(0, 1) , 
        xlab="" ,
        ylab="Prop(-s)" , xaxt="n" )
  for ( j in 1:(coln-1) ) abline( v=(j-1)*4+4.5 , lwd=0.5 )
  for ( j in 1:coln ) {
    word_name <- d3[d3$item_index == k*coln+j,]$item[1]
    text( (j-1)*4+2.5 , 1.1 , word_name , xpd=TRUE )
#    if (word_name %in% c("ballon", "directeur", "luitenant", "redacteur", "sergeant")){
#      rect((j-1)*4+0.5, 0, (j-1)*4+4.5, 1, col = alpha("green", 0.5), border = NA)
#    } else if (word_name %in% c("admiraal", "bretel", "compagnie", "redacteur", "doorn")){
#      rect((j-1)*4+0.5, 0, (j-1)*4+4.5, 1, col = alpha("purple", 0.5), border = NA)
#    }
  }
  # draw in predictions
  for ( j in (1:coln) ) {
    for (i in 1:4) {
      p_mu <- mean(p_ar[,k*coln+j,i])
      p_ci <- PI(p_ar[,k*coln+j,i], prob = .95)
      points(c((j-1)*4 + i), c(p_mu))
      lines(c((j-1)*4 + i, (j-1)*4 + i), p_ci)
    }
  }
  if (k == 0){
    text( 21, PI(p_ar[,6,1], prob = .95)[2] + 0.05 , "B/S" , cex=0.8)
    text( 22 , PI(p_ar[,6,2], prob = .95)[1] - 0.05 , "B/N", cex=0.8 )
    text( 23 , PI(p_ar[,6,3], prob = .95)[2] + 0.05 , "N/S", cex=0.8 )
    text( 24 , PI(p_ar[,6,4], prob = .95)[1] - 0.05 , "N/N", cex=0.8  )
  }
}
par(mar=c(5.1,4.1,4.1,2.1), mfrow=c(1,1))

# okay, so clearly plural distribution of some nouns does
# not follow the pattern. Could it be that for those words
# the N/S condition more frequently contains prosodic breaks
# that are not indicated by punctuation.

# compare m1 and m2 
# see p. 362 in rethinking pdf for info on (dis)aggregated binomial models
# see p. 456 in rethinking pdf for info on WAIC measures for multilevel predictions

m0 <- ulam(
  alist(
    S ~ dbern(prob),
    logit(prob) <- a[item],
    a[item] ~ dnorm(a_bar, sigma),
    a_bar ~ dnorm(0, 2),
    sigma ~ dexp(1)
  ), 
  data = list(S = d3$s_plural, 
              item = d3$item_index), 
  chains = 4, cores = 4, iter = 2000, log_lik = T
)

saveRDS(object = m0, file = concat(f_path, "LIN_project/m0.rds"))

compare(m0, m1, m2, func = PSIS)
#      PSIS    SE dPSIS  dSE pPSIS weight
# m1 8559.6 89.21   0.0   NA  15.6      1
# m2 8572.2 89.11  12.6 9.88  38.1      0
# m0 8573.7 88.98  14.1 9.41  11.6      0

# m0 has 12 intercepts + 1 average + 1 sd = 14 actual parameters
#   not that much shrinkage, because a lot of variation item-specific
# m1 has (4 + 2) fixed + 12 intercepts + 1 average + 1 sd = 20 actual parameters
#   6 additional fixed parameters result in less variation explained by 
#   intercepts, so more shrinkage on those
# m2 has (4 + 2) fixed + 4x12 varying + 4 sd + 12 correlation = 70 actual parameters

# if we compare bigger datasets (0.05 > prop_s < 0.95):
# compare(m1,m2)
#       WAIC     SE dWAIC   dSE pWAIC weight
# m2 12310.3 150.40   0.0    NA  57.8   0.89
# m1 12314.4 150.66   4.2 14.64  23.9   0.11


library(dagitty)


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



