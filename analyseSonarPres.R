if (Sys.info()[1] == "Darwin"){
  f_path = "/Volumes/tensusers/timzee/"
} else {
  f_path = "/vol/tensusers/timzee/"
}

library(rethinking)
library(stringr)
##
d <- read.csv(paste(f_path, "other/SonarVar.csv", sep = ""), header = T, row.names = NULL)

d_dist <- table(d$item, d$s_plural)
d_dist2 <- d_dist[d_dist[,"0"] != 0 & d_dist[,"1"] != 0,]
d_dist3 <- data.frame(item=row.names(d_dist2), en=d_dist2[,"0"], s=d_dist2[,"1"])
d_dist3$prop <- d_dist3$s / (d_dist3$en + d_dist3$s)
d_dist4 <- d_dist3[d_dist3$prop > 0.1 & d_dist3$prop < 0.9,]

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
# d3 <- d3[row.names(d3) != "46707",]


m0 <- ulam(
  alist(
    S ~ dbern(prob),
    logit(prob) <- a[item] + b[stress],
    b[stress] ~ dnorm(0, 1),
    a[item] ~ dnorm(0, sigma),
    sigma ~ dexp(1)
  ), 
  data = list(S = d3$s_plural, 
              item = d3$item_index, 
              stress = d3$next_stress2
  ), 
  chains = 4, cores = 4, iter = 4000, log_lik = T
)

m0.5 <- ulam(
  alist(
    S ~ dbern(prob),
    logit(prob) <- a[item] + b[stress],
    b[stress] ~ dnorm(0, 1),
    a[item] ~ dnorm(0, sigma),
    sigma ~ dexp(1)
  ), 
  data = list(S = d3$s_plural, 
              item = d3$item_index, 
              stress = d3$stress_interaction
  ), 
  chains = 4, cores = 4, iter = 3000, log_lik = T
)

compare(m0, m0.5)

m0.5_post <- extract.samples(m0.5)

diffs <- list(
  "Boundary" = m0.5_post$b[,2] - m0.5_post$b[,1],
  "No Boundary" = m0.5_post$b[,4] - m0.5_post$b[,3]
)
plot(precis(diffs), xlab = "Log-odds difference (unstressed - stressed)")

m1 <- ulam(
  alist(
    S ~ dbern(prob),
    # linear model
    logit(prob) <- a[item,stress] + b[stress],
    # adaptive priors
    vector[4]:a[item] ~ multi_normal(0,Rho_item,sigma_item),
    # fixed priors
    b[stress] ~ dnorm(0, 1),
    sigma_item ~ dexp(1),
    Rho_item ~ dlkjcorr(4)
  ), 
  data = list(S = d3$s_plural, 
              item = d3$item_index, 
              stress = d3$stress_interaction
              ),
  chains = 4, cores = 4, iter = 4000, log_lik = T
)

m1_post <- extract.samples(m1)

# plot random slopes
p_link_custom <- function( item, treatment ) {
  logodds <- with( m1_post , a[,item,treatment] + b[,treatment]) 
  return( inv_logit(logodds) )
}

p_raw <- mapply(p_link_custom, rep(1:max(d3$item_index), times = 4), rep(1:4, each = max(d3$item_index)))

p_ar <- array(data = as.vector(p_raw), dim = c(nrow(m1_post$b), max(d3$item_index), 4))

library(scales)
coln <- 6
rown <- 2 
par(mfrow=c(rown,1), mar=c(2,1,1,3))
for (k in 0:(rown-1)){
  plot( NULL , xlim=c(1,4*coln) , 
        ylim=c(0, 1) , 
        xlab="" ,
        ylab="" , xaxt="n", yaxt="n" )
  axis(side = 4, labels = c(0, 0.5, 1), at = c(0, 0.5, 1), las=1, cex.axis=0.8)
#  mtext("Proporiton", side = 4, line = 0, padj=-8.5, las=1, cex=0.8)
  text(27, 0.5, "Proportion -s", xpd=T, srt=-90)
  for ( j in 1:(coln-1) ) abline( v=(j-1)*4+4.5 , lwd=0.5 )
  for ( j in 1:coln ) {
    word_name <- d3[d3$item_index == k*coln+j,]$item[1]
    text( (j-1)*4+2.5 , -0.12 , word_name , xpd=TRUE, cex=0.8)
    if (word_name %in% c("ballon", "kopie", "directeur", "redacteur")){
      rect((j-1)*4+0.5, 0, (j-1)*4+4.5, 1, col = alpha("green", 0.5), border = NA)
    #    } else if (word_name %in% c("admiraal", "bretel", "compagnie", "redacteur", "doorn")){
    #      rect((j-1)*4+0.5, 0, (j-1)*4+4.5, 1, col = alpha("purple", 0.5), border = NA)
    }
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
    text( 21, PI(p_ar[,6,1], prob = .95)[2] + 0.05 , "B/S" , cex=0.7)
    text( 22 , PI(p_ar[,6,2], prob = .95)[1] - 0.05 , "B/N", cex=0.7 )
    text( 23 , PI(p_ar[,6,3], prob = .95)[2] + 0.05 , "N/S", cex=0.7 )
    text( 24 , PI(p_ar[,6,4], prob = .95)[1] - 0.05 , "N/N", cex=0.7  )
  }
}
par(mar=c(5.1,4.1,4.1,2.1), mfrow=c(1,1))

