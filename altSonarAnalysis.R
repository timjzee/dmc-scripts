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

d3$pros_pl_interaction <- ifelse(d3$prosodic_break == "1" & d3$s_plural == "1", 1,
                                ifelse(d3$prosodic_break == "1" & d3$s_plural == "0", 2,
                                       ifelse(d3$prosodic_break == "0" & d3$s_plural == "1", 3, 4)))
d3$pros_pl_interaction <- as.integer(d3$pros_pl_interaction)



rm_lines <- c("16145", "34370", "37630", "47134", "90132", "94461", "21343", "94998", "95010")
d3 <- d3[!(row.names(d3) %in% rm_lines),]
d3 <- d3[row.names(d3) != "46707",]


m0.0 <- ulam(
  alist(
    stress ~ dbern(prob),
    logit(prob) <- b[pros_pl],
    b[pros_pl] ~ dnorm(0, 1)
  ), 
  data = list(stress = d3$next_stress,
              pros_pl = d3$pros_pl_interaction
  ), 
  chains = 4, cores = 4, iter = 4000#, log_lik = T
)

post0.0 <- extract.samples(m0.0)

diffs <- list(
  "Boundary" = post0.0$b[,1] - post0.0$b[,2],
  "No Boundary" = post0.0$b[,3] - post0.0$b[,4]
)

par(mar=c(5,6,4,2) + 0.1)
plot(NULL, xlim=c(min(diffs$`No Boundary`), max(diffs$Boundary)), ylim=c(-0.25,2),yaxt="n",ylab="",xlab="Log-odds difference", main = "Effect of -s plural type")
axis(2, at = c(0.5, 1.5), labels = c("No Boundary", "Boundary"), las=2)
abline(v=0, lty="dashed")
abline(h=0.1, lty="dotted")
abline(h=1.1, lty="dotted")
boundary_dens <- density(diffs$Boundary)
boundary_dens$ynorm <- boundary_dens$y / (max(boundary_dens$y)*1.2)
noboundary_dens <- density(diffs$`No Boundary`)
noboundary_dens$ynorm <- noboundary_dens$y / (max(noboundary_dens$y)*1.2)

arrows(x0 = -0.05, y0 = -0.15, x1 = -0.35, y1 = -0.15, col = "darkgrey", lwd=3)
text(x=-0.17,y=-0.06,labels = "fewer stress", col = "darkgrey", lwd=2)
arrows(x0 = 0.05, y0 = -0.15, x1 = 0.5, y1 = -0.15, col = "darkgrey", lwd=3)
text(x=0.17,y=-0.06,labels = "more stress", col = "darkgrey", lwd=2)

polygon(x = c(boundary_dens$x, max(boundary_dens$x)), y = c(boundary_dens$ynorm + 1.1, 1.1), col = adjustcolor("grey",alpha.f=0.5), border = adjustcolor("grey",alpha.f=0.5))
polygon(x = c(noboundary_dens$x, max(noboundary_dens$x)), y = c(noboundary_dens$ynorm + 0.1, 0.1), col = adjustcolor("grey",alpha.f=0.5), border = adjustcolor("grey",alpha.f=0.5))

par(mar=c(5,4,4,2) + 0.1)

# plus random intercepts

m0 <- ulam(
  alist(
    stress ~ dbern(prob),
    logit(prob) <- z[item]*sigma + b[pros_pl],
    b[pros_pl] ~ dnorm(0, 1),
    z[item] ~ dnorm(0, 1),
    sigma ~ dexp(1),
    gq> vector[item]:a <<- z*sigma
  ), 
  data = list(stress = d3$next_stress,
              item = d3$item_index,
              pros_pl = d3$pros_pl_interaction
  ), 
  chains = 4, cores = 4, iter = 4000, control = list(adapt_delta = 0.98)#, log_lik = T
)

post <- extract.samples(m0)

diffs <- list(
  "Boundary" = post$b[,1] - post$b[,2],
  "No Boundary" = post$b[,3] - post$b[,4]
)

par(mar=c(5,6,4,2) + 0.1)
plot(NULL, xlim=c(min(diffs$`No Boundary`), max(diffs$Boundary)), ylim=c(-0.25,2),yaxt="n",ylab="",xlab="Log-odds difference", main = "Effect of -s plural type")
axis(2, at = c(0.5, 1.5), labels = c("No Boundary", "Boundary"), las=2)
abline(v=0, lty="dashed")
abline(h=0.1, lty="dotted")
abline(h=1.1, lty="dotted")
boundary_dens <- density(diffs$Boundary)
boundary_dens$ynorm <- boundary_dens$y / (max(boundary_dens$y)*1.2)
noboundary_dens <- density(diffs$`No Boundary`)
noboundary_dens$ynorm <- noboundary_dens$y / (max(noboundary_dens$y)*1.2)

arrows(x0 = -0.05, y0 = -0.15, x1 = -0.4, y1 = -0.15, col = "darkgrey", lwd=3, length=0.15)
text(x=-0.2,y=-0.06,labels = "fewer stress", col = "darkgrey", lwd=2)
arrows(x0 = 0.05, y0 = -0.15, x1 = 0.38, y1 = -0.15, col = "darkgrey", lwd=3, length=0.15)
text(x=0.2,y=-0.06,labels = "more stress", col = "darkgrey", lwd=2)

polygon(x = c(boundary_dens$x, max(boundary_dens$x)), y = c(boundary_dens$ynorm + 1.1, 1.1), col = adjustcolor("grey",alpha.f=0.5), border = adjustcolor("grey",alpha.f=0.5))
polygon(x = c(noboundary_dens$x, max(noboundary_dens$x)), y = c(noboundary_dens$ynorm + 0.1, 0.1), col = adjustcolor("grey",alpha.f=0.5), border = adjustcolor("grey",alpha.f=0.5))

par(mar=c(5,4,4,2) + 0.1)

# We find the effect clearly when we include d_dist3[d_dist3$prop > 0.05 & d_dist3$prop < 0.95,]
# but not very clear yet at d_dist3[d_dist3$prop > 0.1 & d_dist3$prop < 0.9,]
# anyway we now need a invariable plural dataset, and create a 3-way interaction