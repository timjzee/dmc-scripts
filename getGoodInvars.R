if (Sys.info()[1] == "Darwin"){
  f_path = "/Volumes/tensusers/timzee/"
} else {
  f_path = "/vol/tensusers/timzee/"
}

library(rethinking)

d <- read.csv(paste(f_path, "other/SonarVar.csv", sep = ""), header = T, row.names = NULL)

d_dist <- table(d$item, d$s_plural)
d_dist2 <- d_dist[d_dist[,"0"] != 0 & d_dist[,"1"] != 0,]
d_dist3 <- data.frame(item=row.names(d_dist2), en=d_dist2[,"0"], s=d_dist2[,"1"])
d_dist3$prop <- d_dist3$s / (d_dist3$en + d_dist3$s)
d_dist4 <- d_dist3[d_dist3$prop > 0.05 & d_dist3$prop < 0.95,]

d2 <- d[d$item %in% d_dist4$item,]


d3 <- na.omit(d2)
table(list(prosodic_break=d3$prosodic_break, next_stress=d3$next_stress, s_plural=d3$s_plural))

d3$item_index <- coerce_index(d3$item)

d3$pros_pl_interaction <- ifelse(d3$prosodic_break == "1" & d3$s_plural == "1", 1,
                                 ifelse(d3$prosodic_break == "1" & d3$s_plural == "0", 2,
                                        ifelse(d3$prosodic_break == "0" & d3$s_plural == "1", 3, 4)))
d3$pros_pl_interaction <- as.integer(d3$pros_pl_interaction)

rm_lines <- c("16145", "34370", "37630", "47134", "90132", "94461", "21343", "94998", "95010")
d3 <- d3[!(row.names(d3) %in% rm_lines),]
d3 <- d3[row.names(d3) != "46707",]

var <- read.csv("/Users/tim/Library/Mobile Documents/com~apple~CloudDocs/od/PhD_Nijmegen/Lab_Rotation/var.csv")
var_sub <- var[var$word %in% names(table(d3$item)),]
var_sub$syl_n <- c(3, 2, 2, 2, 3, 3, 2, 3, 1, 2, 3, 2, 3, 2, 2, 2, 3, 3, 3, 3, 2)

hist(var_sub$f_pl, breaks = 20)
hist(var_sub$prop_pl, breaks = 20)
plot(var_sub$f_pl, var_sub$prop_pl)

hist(var_sub$syl_n, breaks = 20)

invar <- read.csv("/Volumes/tensusers/timzee/other/invar_probs2.csv")
invar <- invar[invar$final_stress == "+" & invar$penult_stress == "-" & 
                 invar$compound == "-" & ((invar$pl_type == "S" & invar$p_s > 0.9) | (invar$pl_type == "EN" & invar$p_en > 0.9)),]

invar <- na.omit(invar)

sum(var_sub$f_s)
sum(var_sub$f_en)


# let's match the invar nouns to var nouns in terms of n_syl, f_pl, prop_pl
invar_sub <- data.frame()
while (sum(invar_sub[invar_sub$pl_type == "S",]$f_pl) < 500 | sum(invar_sub[invar_sub$pl_type == "EN",]$f_pl) < 500){
  # get the only plural with 1 syllable
  #var_sub[var_sub$syl_n == 1,]
  # majority EN
  
  invar_sub <- invar[invar$word == sample(invar[invar$n_syl == 1 & (invar$prop_pl > 0.25 & invar$prop_pl < 0.45 ) & (invar$f_pl > 30 & invar$f_pl < 60),]$word, 1),]
  
  # now the 3 nouns with a f_pl over 150 and n_syl 2
  #var_sub[var_sub$f_pl > 150,]
  # 1 majority S around f_pl = 300
  invar_sub <- rbind(invar_sub, invar[invar$word == sample(invar[invar$n_syl == 2 & (invar$prop_pl > 0.15 & invar$prop_pl < 0.6 ) & (invar$f_pl > 100 & invar$f_pl < 500) & invar$pl_type == "S",]$word, 1),])
  # 2 majority EN around f_pl 150 and 300
  invar_sub <- rbind(invar_sub, invar[invar$word %in% sample(invar[invar$n_syl == 2 & (invar$prop_pl > 0.15 & invar$prop_pl < 0.6 ) & (invar$f_pl > 100 & invar$f_pl < 500) & invar$pl_type == "EN" & !(invar$word %in% c("bestuur", "gebed", "verdrag", "verslag")),]$word, 2),])
  
  # now the 2 nouns with a prop_pl over .75
  #var_sub[var_sub$prop_pl > .75,]
  # 1 majority S and syl_n 2 
  invar_sub <- rbind(invar_sub, invar[invar$prop_pl > .8 & invar$f_pl > 20 & invar$n_syl == 2 & invar$pl_type == "S",])
  # and 1 majority EN and syl_n 3
  invar_sub <- rbind(invar_sub, invar[invar$word == sample(invar[invar$prop_pl > .8 & invar$f_pl > 20 & invar$f_pl < 70 & invar$n_syl == 3 & invar$pl_type == "EN",]$word, 1),])
  
  
  # 6 remaining n_syl==2 nouns
  #var_sub[var_sub$prop_pl < .3 & var_sub$f_pl > 1 & var_sub$f_pl < 70 & var_sub$syl_n == 2,]
  # 2 majority S, 2 majority EN, 2 50-50
  invar_sub <- rbind(invar_sub, invar[invar$word %in% sample(invar[invar$prop_pl < .3 & invar$f_pl > 5 & invar$f_pl < 70 & invar$n_syl == 2 & invar$pl_type == "S" & !(invar$word %in% c("wagon", "tournee", "soiree", "salon", "record", "essay", "diner", "collier", "chanteur")),]$word, 3),])
  invar_sub <- rbind(invar_sub, invar[invar$word %in% sample(invar[invar$prop_pl < .3 & invar$f_pl > 5 & invar$f_pl < 70 & invar$n_syl == 2 & invar$pl_type == "EN" & !(invar$word %in% c("dieet", "duet", "gebruik", "habijt", "houweel", "japon", "ontslag", "metaal", "ontwerp", "serpent", "transport", "vennoot", "verband", "verlof", "vertrek")),]$word, 3),])
  
  # and 9 remaining n_syl==3 nouns
  #var_sub[var_sub$prop_pl < .3 & var_sub$f_pl > 1 & var_sub$f_pl < 70 & var_sub$syl_n == 3,]
  # 3 majority S, 4 majority EN, 2 50-50
  invar_sub <- rbind(invar_sub, invar[invar$word %in% sample(invar[invar$prop_pl < .3 & invar$f_pl > 1 & invar$f_pl < 70 & invar$n_syl == 3 & invar$pl_type == "S" & !(invar$word %in% c("corridor", "kolonel", "peloton", "presse-papier")),]$word, 4),])
  invar_sub <- rbind(invar_sub, invar[invar$word %in% sample(invar[invar$prop_pl < .3 & invar$f_pl > 5 & invar$f_pl < 70 & invar$n_syl == 3 & invar$pl_type == "EN" & !(invar$word %in% c("galerij", "garnizoen", "karavaan", "papegaai", "plattegrond", "tribunaal")),]$word, 5),])
}

#write.csv(invar_sub, paste(f_path, "other/invars_for_sonar.csv", sep = ""), quote = F, row.names = F)
invar_sub <- read.csv(paste(f_path, "other/invars_for_sonar.csv", sep = ""), header = T, row.names = NULL)

sum(invar_sub[invar_sub$pl_type == "S",]$f_pl)
sum(invar_sub[invar_sub$pl_type == "EN",]$f_pl)

sum(var_sub$f_s)
sum(var_sub$f_en)

plot(invar_sub$f_pl, invar_sub$prop_pl, ylim=c(0,1), xlim=c(0,400))
plot(var_sub$f_pl, var_sub$prop_pl, xlim=c(0,400))

invar_sub$prop_s <- ifelse(invar_sub$pl_type == "S", 1, 0)
par(mfrow=c(1,2))
plot(var_sub$f_pl, var_sub$prop_pl, cex = .5 + var_sub$syl_n/2, col = rgb(0,0,0, var_sub$prop_s), pch = 19, ylim=c(0,1), xlim=c(0,400), xlab="Plural Frequency", ylab="Proportion Plural", main="Variable")
points(var_sub$f_pl, var_sub$prop_pl, cex = .5 + var_sub$syl_n/2, col = rgb(0,0,0, 1), pch = 1, ylim=c(0,1), xlim=c(0,400))

plot(invar_sub$f_pl, invar_sub$prop_pl, cex = .5 + invar_sub$n_syl/2, col = rgb(0,0,0, invar_sub$prop_s), pch = 19, ylim=c(0,1), xlim=c(0,400), xlab="Plural Frequency", ylab="Proportion Plural", main="Invariable")
points(invar_sub$f_pl, invar_sub$prop_pl, cex = .5 + invar_sub$n_syl/2, col = rgb(0,0,0, 1), pch = 1, ylim=c(0,1), xlim=c(0,400))
par(mfrow=c(1,1))

names(var_sub)[names(var_sub)=="syl_n"] <- "Syllables"
names(var_sub)[names(var_sub)=="prop_s"] <- "-s Proportion"

library(ggplot2)
var_plot <- ggplot(var_sub, aes(x = f_pl, y = prop_pl, fill = `-s Proportion`, size = Syllables, xmin=0, xmax=400, ymin=0, ymax=1)) +
  xlab("Plural Frequency") + ylab("Plural Proportion") +
  geom_point(shape=21, color = "black") +
  ggtitle("A. Variable plurals") +
  scale_size(breaks = c(1,2,3)) +
  scale_fill_gradient(low = "black", high = "white")

legend <- get_legend(var_plot)

var_plot <- ggplot(var_sub, aes(x = f_pl, y = prop_pl, fill = `-s Proportion`, size = Syllables, ymin=0, ymax=1)) +
  xlab("Plural Frequency") + ylab("Plural Proportion") +
  geom_point(shape=21, color = "black", show.legend = FALSE) +
  ggtitle("A. Variable plurals") +
  scale_size(breaks = c(1,2,3)) +
  scale_x_continuous(trans='log10', limits = c(1,1000)) +
  scale_fill_gradient(low = "black", high = "white")

invar_plot <- ggplot(invar_sub, aes(x = f_pl, y = prop_pl, fill = prop_s, size = n_syl, ymin=0, ymax=1)) +
  xlab("Plural Frequency") + ylab("Plural Proportion") +
  geom_point(shape=21, color = "black", show.legend = FALSE) +
  ggtitle("B. Invariable plurals") +
  scale_size(breaks = c(1,2,3)) +
  scale_x_continuous(trans='log10', limits = c(1,1000)) +
  scale_fill_gradient(low = "black", high = "white")

plot_grid(var_plot, invar_plot, legend, ncol = 3, rel_widths = c(2/5, 2/5, 1/5))


hist(invar_sub$n_syl, breaks = 20)

invar_sub[, c("word", "pl_type")]

pl_ending <- c("vliezen", "cadeaus", "recepten", "talenten", "bonbons", "hypocrieten", "auteurs", 
               "beha's", "regimes", "debatten", "rabbijnen", "woestijnen", "abonnees", "ateliers", 
               "bataljons", "matadors", "dialecten", "horoscopen", "kapitalen", "paragrafen", "pathologen")

# load the data
d_invar <- read.csv(paste(f_path, "other/SonarInvar.csv", sep = ""), header = T, row.names = NULL)

d_dist_i <- table(d_invar$item, d_invar$s_plural)

d_invar2 <- na.omit(d_invar)

# d3 still needs to be cleaned manually of a few French sentences
check_french <- d_invar2[d_invar2$item %in% c("cadeau", "bonbon", "auteur", "abonnee", "atelier", "bataljon", "matador") & d_invar2$s_plural == "1",]

# 13476: En bij een bruiloft horen natuurlijk cadeaus .   (duplicate)
# 24137: Auteurs die teveel onleesbare zinnen na elkaar schrijven  beginnen opnieuw .    (duplicate)
# 25970: Ateliers de lat Grosne  Cormatin .     (frans)

# automatically get rid of ALL duplicates:
dups <- rle(d_invar2$ort)
dups_i <- cumsum(c(1, dups$lengths[-length(dups$lengths)]))
d_invar3 <- d_invar2[dups_i,]

# get rid of the french sentence
d_invar3 <- d_invar3[!(row.names(d_invar3) %in% c("25970")),]

d_invar3$stress_interaction <- ifelse(d_invar3$prosodic_break == "1" & d_invar3$next_stress == "1", 1,
                                ifelse(d_invar3$prosodic_break == "1" & d_invar3$next_stress == "0", 2,
                                       ifelse(d_invar3$prosodic_break == "0" & d_invar3$next_stress == "1", 3, 4)))
d_invar3$stress_interaction <- as.integer(d_invar3$stress_interaction)
d_invar3$item_index <- coerce_index(d_invar3$item)

d_invar3$pros_pl_interaction <- ifelse(d_invar3$prosodic_break == "1" & d_invar3$s_plural == "1", 1,
                                 ifelse(d_invar3$prosodic_break == "1" & d_invar3$s_plural == "0", 2,
                                        ifelse(d_invar3$prosodic_break == "0" & d_invar3$s_plural == "1", 3, 4)))
d_invar3$pros_pl_interaction <- as.integer(d_invar3$pros_pl_interaction)

# make next sound dependent variable
d_invar3$next_sound_dep <- ifelse(d_invar3$next_sound %in% c("s", "z"), 1, 0)
d_invar3$next_sound2 <- ifelse(d_invar3$next_sound %in% c("s", "z"), 1, 
                               ifelse(d_invar3$next_sound %in% c("@", "a", "A", "e", "E", "i", "I", "K", "L", "M", "o", "O", "y"), 2, 3))

# and interaction with prosody
d_invar3$sound_interaction <- ifelse(d_invar3$prosodic_break == "1" & d_invar3$next_sound2 == "1", 1,
                                     ifelse(d_invar3$prosodic_break == "1" & d_invar3$next_sound2 == "2", 2,
                                            ifelse(d_invar3$prosodic_break == "1" & d_invar3$next_sound2 == "3", 3,
                                                   ifelse(d_invar3$prosodic_break == "0" & d_invar3$next_sound2 == "1", 4, 
                                                          ifelse(d_invar3$prosodic_break == "0" & d_invar3$next_sound2 == "2", 5, 6)))))

d_invar3 <- na.omit(d_invar3)

m0 <- ulam(
  alist(
    stress ~ dbern(prob),
    logit(prob) <- z[item]*sigma + b[pros_pl],
    b[pros_pl] ~ dnorm(0, 1),
    z[item] ~ dnorm(0, 1),
    sigma ~ dexp(1),
    gq> vector[item]:a <<- z*sigma
  ), 
  data = list(stress = d_invar3$next_stress,
              item = d_invar3$item_index,
              pros_pl = d_invar3$pros_pl_interaction
  ), 
  chains = 4, cores = 4, iter = 4000, control = list(adapt_delta = 0.98)#, log_lik = T
)

precis(m0, depth = 2)
# way more stress if no preceding prosodic break
# makes sense because after a punctuation mark often an article is used
# same 'effect' in variable data

# but let's check whether -s plural causes fewer following stress
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

arrows(x0 = -0.05, y0 = -0.15, x1 = -0.5, y1 = -0.15, col = "darkgrey", lwd=3, length=0.15)
text(x=-0.2,y=-0.06,labels = "fewer stress", col = "darkgrey", lwd=2)
arrows(x0 = 0.05, y0 = -0.15, x1 = 0.5, y1 = -0.15, col = "darkgrey", lwd=3, length=0.15)
text(x=0.2,y=-0.06,labels = "more stress", col = "darkgrey", lwd=2)

polygon(x = c(boundary_dens$x, max(boundary_dens$x)), y = c(boundary_dens$ynorm + 1.1, 1.1), col = adjustcolor("grey",alpha.f=0.5), border = adjustcolor("grey",alpha.f=0.5))
polygon(x = c(noboundary_dens$x, max(noboundary_dens$x)), y = c(noboundary_dens$ynorm + 0.1, 0.1), col = adjustcolor("grey",alpha.f=0.5), border = adjustcolor("grey",alpha.f=0.5))

par(mar=c(5,4,4,2) + 0.1)

# what happens without random intercepts
m0.0 <- ulam(
  alist(
    stress ~ dbern(prob),
    logit(prob) <- b[pros_pl],
    b[pros_pl] ~ dnorm(0, 1)
  ), 
  data = list(stress = d_invar3$next_stress,
              pros_pl = d_invar3$pros_pl_interaction
  ), 
  chains = 4, cores = 4, iter = 3000, control = list(adapt_delta = 0.98)#, log_lik = T
)

post <- extract.samples(m0.0)

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

arrows(x0 = -0.05, y0 = -0.15, x1 = -0.2, y1 = -0.15, col = "darkgrey", lwd=3, length=0.15)
text(x=-0.1,y=-0.06,labels = "fewer stress", col = "darkgrey", lwd=2)
arrows(x0 = 0.05, y0 = -0.15, x1 = 0.2, y1 = -0.15, col = "darkgrey", lwd=3, length=0.15)
text(x=0.1,y=-0.06,labels = "more stress", col = "darkgrey", lwd=2)

polygon(x = c(boundary_dens$x, max(boundary_dens$x)), y = c(boundary_dens$ynorm + 1.1, 1.1), col = adjustcolor("grey",alpha.f=0.5), border = adjustcolor("grey",alpha.f=0.5))
polygon(x = c(noboundary_dens$x, max(noboundary_dens$x)), y = c(noboundary_dens$ynorm + 0.1, 0.1), col = adjustcolor("grey",alpha.f=0.5), border = adjustcolor("grey",alpha.f=0.5))

par(mar=c(5,4,4,2) + 0.1)

# nothing here, point estimates are almost identical to model that includes random interepts,
# seemingly less uncertainty but probably because the 2 contrast distributions in ranint model are more correlated
cor(diffs$`Boundary`, diffs$`No Boundary`)
# -0.01490468 for fixed model, 0.7764578 for ranint model

# what happens if we do the original causation direction

m0.5 <- ulam(
  alist(
    S ~ dbern(prob),
    logit(prob) <- z[item]*sigma + b[stress],
    b[stress] ~ dnorm(0, 1),
    z[item] ~ dnorm(0, 1),
    sigma ~ dexp(1),
    gq> vector[item]:a <<- z*sigma
  ), 
  data = list(S = d_invar3$s_plural, 
              item = d_invar3$item_index, 
              stress = d_invar3$stress_interaction
  ), 
  chains = 4, cores = 4, iter = 4000
)

post <- extract.samples(m0.5)

diffs <- list(
  "Boundary" = post$b[,1] - post$b[,2],
  "No Boundary" = post$b[,3] - post$b[,4]
)

par(mar=c(5,6,4,2) + 0.1)
plot(NULL, xlim=c(min(diffs$`No Boundary`), max(diffs$Boundary)), ylim=c(-0.25,2),yaxt="n",ylab="",xlab="Log-odds difference", main = "Effect of following stress")
axis(2, at = c(0.5, 1.5), labels = c("No Boundary", "Boundary"), las=2)
abline(v=0, lty="dashed")
abline(h=0.1, lty="dotted")
abline(h=1.1, lty="dotted")
boundary_dens <- density(diffs$Boundary)
boundary_dens$ynorm <- boundary_dens$y / (max(boundary_dens$y)*1.2)
noboundary_dens <- density(diffs$`No Boundary`)
noboundary_dens$ynorm <- noboundary_dens$y / (max(noboundary_dens$y)*1.2)

arrows(x0 = -0.05, y0 = -0.15, x1 = -4, y1 = -0.15, col = "darkgrey", lwd=3, length=0.15)
text(x=-2,y=-0.06,labels = "fewer -s", col = "darkgrey", lwd=2)
arrows(x0 = 0.05, y0 = -0.15, x1 = 4, y1 = -0.15, col = "darkgrey", lwd=3, length=0.15)
text(x=2,y=-0.06,labels = "more -s", col = "darkgrey", lwd=2)

polygon(x = c(boundary_dens$x, max(boundary_dens$x)), y = c(boundary_dens$ynorm + 1.1, 1.1), col = adjustcolor("grey",alpha.f=0.5), border = adjustcolor("grey",alpha.f=0.5))
polygon(x = c(noboundary_dens$x, max(noboundary_dens$x)), y = c(noboundary_dens$ynorm + 0.1, 0.1), col = adjustcolor("grey",alpha.f=0.5), border = adjustcolor("grey",alpha.f=0.5))

par(mar=c(5,4,4,2) + 0.1)

# nothing happens!
