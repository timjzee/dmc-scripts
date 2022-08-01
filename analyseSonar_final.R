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

rm_lines <- c("16145", "34370", "37630", "47134", "90132", "94461", "21343", "94998", "95010")
d3 <- d3[!(row.names(d3) %in% rm_lines),]
d3 <- d3[row.names(d3) != "46707",]

d3$next_sound2 <- ifelse(d3$next_sound %in% c("s", "z"), 1, 
                         ifelse(d3$next_sound == "n", 2, 
                                ifelse(d3$next_sound %in% c("@", "a", "A", "e", "E", "i", "I", "K", "L", "M", "o", "O", "y"), 3, 4)))

d3$sound_interaction <- ifelse(d3$prosodic_break == "1" & d3$next_sound2 == "1", 1,
                               ifelse(d3$prosodic_break == "1" & d3$next_sound2 == "2", 2,
                                      ifelse(d3$prosodic_break == "1" & d3$next_sound2 == "3", 3,
                                             ifelse(d3$prosodic_break == "1" & d3$next_sound2 == "4", 4,
                                                    ifelse(d3$prosodic_break == "0" & d3$next_sound2 == "1", 5, 
                                                           ifelse(d3$prosodic_break == "0" & d3$next_sound2 == "2", 6, 
                                                                  ifelse(d3$prosodic_break == "0" & d3$next_sound2 == "3", 7, 8)))))))
d3$sound_interaction <- as.integer(d3$sound_interaction)

d3$stress_interaction <- ifelse(d3$prosodic_break == "1" & d3$next_stress == "1", 1,
                                ifelse(d3$prosodic_break == "1" & d3$next_stress == "0", 2,
                                       ifelse(d3$prosodic_break == "0" & d3$next_stress == "1", 3, 4)))
d3$stress_interaction <- as.integer(d3$stress_interaction)

d3$item_index <- coerce_index(d3$item)

d3$sc <- coerce_index(d3$sub_corpus)

m0 <- readRDS(file = "/Users/tim/Downloads/m0f.rds")

m0 <- ulam(
  alist(
    S ~ dbern(prob),
    logit(prob) <- a[item] + b[sc] + g[sound],
    g[sound] ~ dnorm(0, 1),
    a[item] ~ dnorm(0, sigma_a),
    b[sc] ~ dnorm(0, sigma_b),
    c(sigma_a, sigma_b) ~ dexp(1)
  ), 
  data = list(S = d3$s_plural, 
              item = d3$item_index, 
              sc = d3$sc,
              sound = d3$sound_interaction
  ), 
  chains = 4, cores = 4, iter = 4000, log_lik = T
)

saveRDS(object = m0, file = "/Users/tim/Downloads/m0f.rds")

m1 <- readRDS(file = "/Users/tim/Downloads/m1f.rds")

m1 <- ulam(
  alist(
    S ~ dbern(prob),
    logit(prob) <- a[item] + b[sc] + g[sound] + d[stress],
    g[sound] ~ dnorm(0, 1),
    d[stress] ~ dnorm(0, 1),
    a[item] ~ dnorm(0, sigma_a),
    b[sc] ~ dnorm(0, sigma_b),
    c(sigma_a, sigma_b) ~ dexp(1)
  ), 
  data = list(S = d3$s_plural, 
              item = d3$item_index, 
              sc = d3$sc,
              sound = d3$sound_interaction,
              stress = d3$stress_interaction
  ), 
  chains = 4, cores = 4, iter = 4000, log_lik = T
)

saveRDS(object = m1, file = "/Users/tim/Downloads/m1f.rds")

compare(m0, m1)

post <- extract.samples(m1)

# /s/ - /n/ contrasts
# prosodic break
break_sn <- post$g[,1] - post$g[,3]
# no break
nobreak_sn <- post$g[,5] - post$g[,7]

plot(density(break_sn))
lines(density(nobreak_sn))


diffs <- list(
  "Boundary" = post$d[,1] - post$d[,2],
  "No Boundary" = post$d[,3] - post$d[,4]
)

boundary_dens <- density(diffs$Boundary)
boundary_dens$ynorm <- boundary_dens$y / (max(boundary_dens$y)*1.2)
noboundary_dens <- density(diffs$`No Boundary`)
noboundary_dens$ynorm <- noboundary_dens$y / (max(noboundary_dens$y)*1.2)

par(mar=c(5,6,4,2) + 0.1)
plot(NULL, xlim=c(min(diffs$`No Boundary`), max(diffs$Boundary)), ylim=c(-0.25,2),yaxt="n",ylab="",xlab="Log-odds difference", main = "Effect of following stress")
axis(2, at = c(0.5, 1.5), labels = c("No Boundary", "Boundary"), las=2)
abline(v=0, lty="dashed")
abline(h=0.1, lty="dotted")
abline(h=1.1, lty="dotted")

arrows(x0 = -0.05, y0 = -0.2, x1 = min(diffs$`No Boundary`) + 0.03, y1 = -0.2, col = "darkgrey", lwd=3, length = 0.15)
text(x=-0.17,y=-0.06,labels = "fewer -s", col = "darkgrey", lwd=2, cex=1.3)
arrows(x0 = 0.05, y0 = -0.2, x1 = max(diffs$Boundary) - 0.03, y1 = -0.2, col = "darkgrey", lwd=3, length = 0.15)
text(x=0.17,y=-0.07,labels = "more -s", col = "darkgrey", lwd=2, cex=1.3)

polygon(x = c(boundary_dens$x, max(boundary_dens$x)), y = c(boundary_dens$ynorm + 1.1, 1.1), col = adjustcolor("grey",alpha.f=0.5), border = adjustcolor("grey",alpha.f=0.5))
polygon(x = c(noboundary_dens$x, max(noboundary_dens$x)), y = c(noboundary_dens$ynorm + 0.1, 0.1), col = adjustcolor("grey",alpha.f=0.5), border = adjustcolor("grey",alpha.f=0.5))
par(mar=c(5,4,4,2) + 0.1)

# random slopes
m2 <- ulam(
  alist(
    S ~ dbern(prob),
    # linear model
    logit(prob) <- a[item, stress] + b[sc] + g[sound] + d[stress],
    # adaptive priors
    vector[4]:a[item] ~ multi_normal(0, Rho_a, sigma_a),
    b[sc] ~ dnorm(0, sigma_b),
    # fixed priors
    d[stress] ~ dnorm(0, 1),
    g[sound] ~ dnorm(0, 1),
    c(sigma_a, sigma_b) ~ dexp(1),
    Rho_a ~ dlkjcorr(4)
  ), 
  data = list(S = d3$s_plural, 
              item = d3$item_index, 
              sc = d3$sc,
              sound = d3$sound_interaction,
              stress = d3$stress_interaction
  ), 
  chains = 4, cores = 4, iter = 4000#, log_lik = T
)

saveRDS(object = m2, file = "/Users/tim/Downloads/m2f.rds")

m_post <- extract.samples(m2)

# plot random slopes
p_link_custom <- function( item, treatment ) {
  logodds <- with( m_post , a[,item,treatment] + d[,treatment]) 
  return( inv_logit(logodds) )
}

p_raw <- mapply(p_link_custom, rep(1:max(d3$item_index), times = 4), rep(1:4, each = max(d3$item_index)))

p_ar <- array(data = as.vector(p_raw), dim = c(nrow(m_post$d), max(d3$item_index), 4))




noun_numbers <- table(d3$item, d3$stress_interaction)

library(scales)
coln <- 7
rown <- 3 
par(mfrow=c(rown,1), mar=c(2,3,1,1))
for (k in 0:(rown-1)){
  plot( NULL , xlim=c(1,4*coln) , 
        ylim=c(-0.1, 1) , 
        xlab="" ,
        ylab="" , xaxt="n", yaxt="n" )
  axis(side = 2, labels = c(0, 0.5, 1), at = c(0, 0.5, 1), las=1, cex.axis=0.8)
  text(-2, 0.5, "Proportion -s", xpd=T, srt=90)
  for ( j in 1:(coln-1) ) abline( v=(j-1)*4+4.5 , lwd=0.5 )
  for ( j in 1:coln ) {
    word_name <- d3[d3$item_index == k*coln+j,]$item[1]
    text( (j-1)*4+2.5 , 1.1 , word_name , xpd=TRUE, cex=0.8)
    if ((k+1) %% 2 != 0){
      if (j %in% seq(from = 1, to = coln, by = 2)){
        axis(side = 1, labels = c("B/S", "B/N", "N/S", "N/N"), at = (j-1)*4 + 1:4, cex.axis=0.6, padj = -1.2)
#        text(x = (j-1)*4 + 1:4, y = -0.22, labels = c("B/S", "B/N", "N/S", "N/N"), xpd=T, cex = 0.6)
      }
    } else {
      if (j %in% seq(from = 2, to = coln, by = 2)){
        axis(side = 1, labels = c("B/S", "B/N", "N/S", "N/N"), at = (j-1)*4 + 1:4, cex.axis=0.6, padj = -1.2)
#        text(x = (j-1)*4 + 1:4, y = -0.22, labels = c("B/S", "B/N", "N/S", "N/N"), xpd=T, cex = 0.6)
      }
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
    word_name <- d3[d3$item_index == k*coln+j,]$item[1]
    text(x=(1:4+(j-1)*4),y=rep(-0.05, times=4), labels=as.character(noun_numbers[word_name,]),cex=0.6)
  }
  if (k == 0){
    text( 17, PI(p_ar[,5,1], prob = .95)[2] + 0.05 , "B/S" , cex=0.7)
    text( 18 , PI(p_ar[,5,2], prob = .95)[1] - 0.05 , "B/N", cex=0.7 )
    text( 19 , PI(p_ar[,5,3], prob = .95)[2] + 0.05 , "N/S", cex=0.7 )
    text( 20 , PI(p_ar[,5,4], prob = .95)[1] - 0.05 , "N/N", cex=0.7  )
  }
}
par(mar=c(5.1,4.1,4.1,2.1), mfrow=c(1,1))

par(mar=c(2,2,2,2), mfrow=c(1,1))
plot( NULL, xlim=c(1.52,4*coln - 0.52), ylim=c(0.13,rown*1.2 - 0.13), xlab="", ylab="", xaxt="n", yaxt="n" )
abline( v=0.5 , lwd=0.5 )
abline( v=4*coln + 0.5 , lwd=0.5 )
for ( k in 1:rown ) {
  rect(0.5, k*1.2 - 0.1, 4*coln + 0.5, k*1.2, col = alpha("lightgrey", 1), border = "black")
  if (k %in% seq(from = 1, to = rown, by = 2)){
    axis(side = 2, labels = c(0, 0.5, 1), at = (k-1)*1.2 + c(0.1, 0.6, 1.1), cex.axis=0.6)
  } else {
    axis(side = 4, labels = c(0, 0.5, 1), at = (k-1)*1.2 + c(0.1, 0.6, 1.1), cex.axis=0.6)
  }
}
for ( k in 1:rown) {
  for ( j in 1:coln ) {
    word_name <- d3[d3$item_index == (k-1)*coln+j,]$item[1]
    text( (j-1)*4+2.5 , 1.2*(rown - (k-1) - 0.04), word_name , xpd=TRUE, cex=0.7)
    for (i in 1:4) {
      p_mu <- mean(p_ar[,(k-1)*coln+j,i])
      p_ci <- PI(p_ar[,(k-1)*coln+j,i], prob = .95)
      points(c((j-1)*4 + i), 1.2*rown - 1.2*k + p_mu + 0.1, pch = 16, cex=0.8)
      lines(c((j-1)*4 + i, (j-1)*4 + i), 1.2*rown - 1.2*k + p_ci + 0.1)
    }
    text(x=(1:4+(j-1)*4),y=rep(1.2*rown - 1.2*k + 0.05, times=4), labels=as.character(noun_numbers[word_name,]),cex=0.5)
  }
}
for ( j in 1:coln ) {
  if (j < coln){
    abline( v=(j-1)*4+4.5 , lwd=0.5 )
  }
  if (j %in% seq(from = 1, to = coln, by = 2)){
    axis(side = 3, labels = c("B/S", "B/N", "N/S", "N/N"), at = (j-1)*4 + 1:4, cex.axis=0.5, padj = 1.3)
  } else {
    axis(side = 1, labels = c("B/S", "B/N", "N/S", "N/N"), at = (j-1)*4 + 1:4, cex.axis=0.5, padj = -1.9)
  }
}
par(mar=c(5.1,4.1,4.1,2.1), mfrow=c(1,1))
