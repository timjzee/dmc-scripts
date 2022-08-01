if (Sys.info()[1] == "Darwin"){
  f_path = "/Volumes/tensusers/timzee/"
} else {
  f_path = "/vol/tensusers/timzee/"
}

library(rethinking)
library(stringr)
##
d <- read.csv(paste(f_path, "other/SonarVar2.csv", sep = ""), header = T, row.names = NULL)

d_dist <- table(d$item, d$s_plural)
d_dist2 <- d_dist[d_dist[,"0"] != 0 & d_dist[,"1"] != 0,]
d_dist3 <- data.frame(item=row.names(d_dist2), en=d_dist2[,"0"], s=d_dist2[,"1"])
d_dist3$prop <- d_dist3$s / (d_dist3$en + d_dist3$s)
d_dist4 <- d_dist3[d_dist3$prop > 0.05 & d_dist3$prop < 0.95,]

d2 <- d[d$item %in% d_dist4$item,]


d3 <- na.omit(d2)
# d3 <- d3[d3$next_vowel_length != 0,]
table(list(prosodic_break=d3$prosodic_break, next_stress=d3$next_stress, s_plural=d3$s_plural))

d3$next_stress2 <- d3$next_stress + 1

d3$stress_interaction <- ifelse(d3$prosodic_break == "1" & d3$next_stress == "1", 1,
                                ifelse(d3$prosodic_break == "1" & d3$next_stress == "0", 2,
                                       ifelse(d3$prosodic_break == "0" & d3$next_stress == "1", 3, 4)))
d3$stress_interaction <- as.integer(d3$stress_interaction)
d3$item_index <- coerce_index(d3$item)


rm_lines <- c("16145", "34370", "37630", "47134", "90132", "94461", "21343", "94998", "95010")
d3 <- d3[!(row.names(d3) %in% rm_lines),]
d3 <- d3[row.names(d3) != "46707",]

d3$sc <- coerce_index(d3$sub_corpus)

table(d3$sub_corpus, d3$stress_interaction)

m0.0 <- ulam(
  alist(
    S ~ dbern(prob),
    # linear model
    logit(prob) <- a[item] + g[corpus,stress] + b[stress],
    # adaptive priors
    vector[4]:g[corpus] ~ multi_normal(0,Rho_corpus,sigma_corpus),
    a[item] ~ dnorm(0, sigma_item),
    # fixed priors
    b[stress] ~ dnorm(0, 1),
    c(sigma_corpus, sigma_item) ~ dexp(1),
    Rho_corpus ~ dlkjcorr(4)
  ), 
  data = list(S = d3$s_plural,
              item = d3$item_index,
              corpus = d3$sc,
              stress = d3$stress_interaction
  ), 
  chains = 4, cores = 4, iter = 4000#, log_lik = T
)

m_post <- extract.samples(m0.0)

# plot random slopes
p_link_custom <- function( corpus, treatment ) {
  logodds <- with( m_post , g[,corpus,treatment] + b[,treatment]) 
  return( inv_logit(logodds) )
}

p_raw <- mapply(p_link_custom, rep(1:max(d3$sc), times = 4), rep(1:4, each = max(d3$sc)))

p_ar <- array(data = as.vector(p_raw), dim = c(nrow(m_post$b), max(d3$sc), 4))

corpus_numbers <- table(d3$sub_corpus, d3$stress_interaction)

library(scales)
coln <- 5
rown <- 4 
par(mfrow=c(rown,1), mar=c(2,1,1,3))
for (k in 0:(rown-1)){
  plot( NULL , xlim=c(1,4*coln) , 
        ylim=c(-0.1, 1) , 
        xlab="" ,
        ylab="" , xaxt="n", yaxt="n" )
  axis(side = 4, labels = c(0, 0.5, 1), at = c(0, 0.5, 1), las=1, cex.axis=0.8)
  #  mtext("Proporiton", side = 4, line = 0, padj=-8.5, las=1, cex=0.8)
  text(23, 0.5, "Proportion -s", xpd=T, srt=-90)
  for ( j in 1:(coln-1) ) abline( v=(j-1)*4+4.5 , lwd=0.5 )
  for ( j in 1:coln ) {
    word_name <- d3[d3$sc == k*coln+j,]$sub_corpus[1]
    text( (j-1)*4+2.5 , -0.22 , word_name , xpd=TRUE, cex=0.8)
  }
  # draw in predictions
  for ( j in (1:coln) ) {
    for (i in 1:4) {
      p_mu <- mean(p_ar[,k*coln+j,i])
      p_ci <- PI(p_ar[,k*coln+j,i], prob = .95)
      points(c((j-1)*4 + i), c(p_mu))
      lines(c((j-1)*4 + i, (j-1)*4 + i), p_ci)
    }
    word_name <- d3[d3$sc == k*coln+j,]$sub_corpus[1]
    text(x=(1:4+(j-1)*4),y=rep(-0.05, times=4), labels=as.character(corpus_numbers[word_name,]),cex=0.8)
  }
  if (k == 0){
    text( 17, PI(p_ar[,5,1], prob = .95)[2] + 0.05 , "B/S" , cex=0.7)
    text( 18 , PI(p_ar[,5,2], prob = .95)[1] - 0.05 , "B/N", cex=0.7 )
    text( 19 , PI(p_ar[,5,3], prob = .95)[2] + 0.05 , "N/S", cex=0.7 )
    text( 20 , PI(p_ar[,5,4], prob = .95)[1] - 0.05 , "N/N", cex=0.7  )
  }
}
par(mar=c(5.1,4.1,4.1,2.1), mfrow=c(1,1))


coln <- 5
rown <- 4 
par(mfrow=c(rown,1), mar=c(2,1,1,3))
for (k in 0:(rown-1)){
  plot( NULL , xlim=c(1,2*coln) , 
        ylim=c(0, 16) , 
        xlab="" ,
        ylab="" , xaxt="n", yaxt="n" )
  axis(side = 4, labels = c(0, 8, 16), at = c(0, 8, 16), las=1, cex.axis=0.8)
  #  mtext("Proporiton", side = 4, line = 0, padj=-8.5, las=1, cex=0.8)
  text(23, 0.5, "Proportion -s", xpd=T, srt=-90)
  for ( j in 1:(coln-1) ) abline( v=(j-1)*2+2.5 , lwd=0.5 )
  for ( j in 1:coln ) {
    word_name <- d3[d3$sc == k*coln+j,]$sub_corpus[1]
    text( (j-1)*2+1.5 , -2 , word_name , xpd=TRUE, cex=0.8)
  }
  # draw in predictions
  for ( j in (1:coln) ) {
    cond_diff <- p_ar[,k*coln+j,3] - p_ar[,k*coln+j,4]
    lines(density((j-1)*2 + 1.5 + cond_diff))
    abline(v=(j-1)*2 + 1.5, lty="dashed")
    p_above_0 <- length(cond_diff[cond_diff>0])/length(cond_diff)
    text(x=(j-1)*2 + 1, y=15, labels=as.character(round(p_above_0,3)) )
    text(x=(j-1)*2 + 2, y=15, labels=as.character(round(mean(cond_diff),3)))
  }
}
par(mar=c(5.1,4.1,4.1,2.1), mfrow=c(1,1))

# look at lexical predictors
var <- read.csv(paste(f_path, "other/var.csv", sep = ""), header = T, row.names = NULL)

getFreq <- function(i, sp){
  if (i %in% var$word){
    if (sp == 1){
      return(var[var$word == i,]$f_s[1])
    } else {
      return(var[var$word == i,]$f_en[1])
    }
  } else {
    return(NA)
  }
}

getPropPL <- function(i){
  if (i %in% var$word){
    return(var[var$word == i,]$prop_pl[1])
  } else {
    return(NA)
  }
}

getFreqPL <- function(i){
  if (i %in% var$word){
    return(var[var$word == i,]$f_pl[1])
  } else {
    return(NA)
  }
}

d3$wf <- mapply(function(x,y) getFreq(x,y), d3$item, d3$s_plural)
d3$prop_pl <- sapply(d3$item, function(x) getPropPL(x))
d3$f_pl <- sapply(d3$item, function(x) getFreqPL(x))

d3$log_f_pl <- log(d3$f_pl)

m0.1 <- ulam(
  alist(
    S ~ dbern(prob),
    # linear model
    logit(prob) <- a[item] + g[corpus] + b[stress] + b_f[stress]*freq,
    # adaptive priors
    g[corpus] ~ dnorm(0, sigma_corpus),
    a[item] ~ dnorm(0, sigma_item),
    # fixed priors
    b[stress] ~ dnorm(0, 1),
    b_f[stress] ~ dnorm(0, 1),
    c(sigma_corpus, sigma_item) ~ dexp(1)
  ), 
  data = list(S = d3$s_plural,
              item = d3$item_index,
              corpus = d3$sc,
              stress = d3$stress_interaction,
              freq = scale(d3$log_f_pl)
  ), 
  chains = 4, cores = 4, iter = 4000#, log_lik = T
)

m_post <- extract.samples(m0.1)

f_seq <- seq(from=min(scale(d3$log_f_pl)),to=max(scale(d3$log_f_pl)), length.out=30)
treatment <- 1:4
p_link_custom <- function() {
  p_tot <- c()
  for (f in f_seq){
    logodds <- with( m_post , b[,treatment] + b_f[,treatment]*f)
    p_tot <- c(p_tot, inv_logit(logodds))
  }
  return( p_tot )
}

p_raw <- p_link_custom()

p_ar <- array(data = as.vector(p_raw), dim = c(nrow(m_post$b), 4, 30))

boundary_effect_mu_12 <- apply(p_ar[,1,1:30] - p_ar[,2,1:30],2, mean)
boundary_effect_pi_12 <- apply(p_ar[,1,1:30] - p_ar[,2,1:30],2, PI)
boundary_effect_mu_34 <- apply(p_ar[,3,1:30] - p_ar[,4,1:30],2, mean)
boundary_effect_pi_34 <- apply(p_ar[,3,1:30] - p_ar[,4,1:30],2, PI)

plot(NULL, xlim=c(min(f_seq), max(f_seq)), xlab = "Plural Frequency", ylab = "Stress-Unstressed difference", 
     ylim=c(min(boundary_effect_pi_12, boundary_effect_pi_34), max(boundary_effect_pi_12, boundary_effect_pi_34)))

lines(f_seq, boundary_effect_mu_12)
shade(boundary_effect_pi_12, f_seq)
lines(f_seq, boundary_effect_mu_34, lty="dashed")
shade(boundary_effect_pi_34, f_seq)

m0.2 <- ulam(
  alist(
    S ~ dbern(prob),
    # linear model
    logit(prob) <- a[item] + g[corpus] + b[stress] + b_f[stress]*freq,
    # adaptive priors
    g[corpus] ~ dnorm(0, sigma_corpus),
    a[item] ~ dnorm(0, sigma_item),
    # fixed priors
    b[stress] ~ dnorm(0, 1),
    b_f[stress] ~ dnorm(0, 1),
    c(sigma_corpus, sigma_item) ~ dexp(1)
  ), 
  data = list(S = d3$s_plural,
              item = d3$item_index,
              corpus = d3$sc,
              stress = d3$stress_interaction,
              freq = scale(d3$prop_pl)
  ), 
  chains = 4, cores = 4, iter = 4000#, log_lik = T
)

m_post <- extract.samples(m0.2)

f_seq <- seq(from=min(scale(d3$prop_pl)),to=max(scale(d3$prop_pl)), length.out=30)
treatment <- 1:4
p_link_custom <- function() {
  p_tot <- c()
  for (f in f_seq){
    logodds <- with( m_post , b[,treatment] + b_f[,treatment]*f)
    p_tot <- c(p_tot, inv_logit(logodds))
  }
  return( p_tot )
}

p_raw <- p_link_custom()

p_ar <- array(data = as.vector(p_raw), dim = c(nrow(m_post$b), 4, 30))

boundary_effect_mu_12 <- apply(p_ar[,1,1:30] - p_ar[,2,1:30],2, mean)
boundary_effect_pi_12 <- apply(p_ar[,1,1:30] - p_ar[,2,1:30],2, PI)
boundary_effect_mu_34 <- apply(p_ar[,3,1:30] - p_ar[,4,1:30],2, mean)
boundary_effect_pi_34 <- apply(p_ar[,3,1:30] - p_ar[,4,1:30],2, PI)

plot(NULL, xlim=c(min(f_seq), max(f_seq)), xlab = "Plural Proportion", ylab = "Stress-Unstressed difference", 
     ylim=c(min(boundary_effect_pi_12, boundary_effect_pi_34), max(boundary_effect_pi_12, boundary_effect_pi_34)))

lines(f_seq, boundary_effect_mu_12)
shade(boundary_effect_pi_12, f_seq)
lines(f_seq, boundary_effect_mu_34, lty="dashed")
shade(boundary_effect_pi_34, f_seq)
