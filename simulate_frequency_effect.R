library(zipfR)
library(betareg)
library(aods3)

if (Sys.info()[1] == "Darwin"){
  f_path = "/Volumes/tensusers/timzee/timbl_files/"
  f_path2 = "/Volumes/tensusers/timzee/other/"
} else {
  f_path = "/vol/tensusers/timzee/timbl_files/"
  f_path2 = "/vol/tensusers/timzee/other/"
}

var = read.csv(paste(f_path, "p_f_type_O_2syl_k4_ID_invar.csv", sep = ""))

var = var[var$f_s != 0,]
var$f_nons = var$f_en + var$f_other
var$s_prop = var$f_s/(var$f_s + var$f_en + var$f_other)
var$s_prop_trans = (var$s_prop*(nrow(var)-1) + 0.5) / nrow(var)
var$log_f_mv = log(var$f_s + var$f_en + var$f_other)
var$mv_relfreq = (var$f_s + var$f_en + var$f_other) / (var$f_s + var$f_en + var$f_other + var$f_ev)

var$f_mv = var$f_s + var$f_nons
N2 = nrow(var)
y2 = 1:N2
var_sorted = var[order(-var$f_mv),]
proby2 = var_sorted$f_mv / sum(var_sorted$f_mv)
plot(proby2 ~ y2, type = "h", col = "blue", ylab = "Probability", xlim=c(0,10))

# use all nouns, because var does not have f_mv = 1 
# assumption is that invar and var are not distributed differently
# or I can actually include invar nouns in estimation of the beta distribution of s_prop
# though we must transform because betareg can't deal with 0 or 1, which might overestimate the number of simulated variable plurals
# on the other hand, treating invar nouns as 0 or 1 might actually underestimate the actual number of variable plurals, as real s_prop is unknown (sinaasappel is treated as invar)

# or use only var plural nouns but use lemma freq and derive mv_freq later using mv_relfreq;
# if we use lemma freq, could we estimate total amount of lemmas that have variable plurals, following https://cran.r-project.org/web/packages/zipfR/vignettes/zipfr-tutorial.pdf

invar = read.csv(paste(f_path2, "invar_freqs.csv", sep = ""))

nouns = rbind(invar, var[,c("word","f_mv", "s_prop")])
nouns$s_prop_trans = (nouns$s_prop*(nrow(nouns)-1) + 0.5) / nrow(nouns)

# based on Baayen pp. 222-236; https://www.r-bloggers.com/the-zipf-and-zipf-mandelbrot-distributions/
nouns.tab = table(nouns$f_mv)
nouns.spc = spc(m = as.numeric(names(nouns.tab)), Vm = as.numeric(nouns.tab))
nouns.lnre.fzm = lnre("fzm", nouns.spc)
plot(nouns.spc, lnre.spc(nouns.lnre.fzm, sum(nouns$f_mv)))

lnre.sample = rlnre(nouns.lnre.fzm,n=1000)
lnre.sample.num = as.numeric(as.character(lnre.sample))
lnre.sample.num.sort = sort(lnre.sample.num, decreasing = T)

indexn = 1:1000
plot(lnre.sample.num.sort ~ indexn, type = "h", col = "blue")


s_prop.beta = betareg(s_prop ~ 1, data = var)
mu = plogis(s_prop.beta$coefficients$mean)
phi = s_prop.beta$coefficients$precision
alph = mu * phi
bet = (1 - mu) * phi

hist(var$s_prop, freq = F)
p = seq(0, 1, length=nrow(var))
lines(p, dbeta(p, alph, bet), type = "l")

beta.sample = rbeta(1000, alph, bet)

sim_data = data.frame(plural = 1:1000, frequency = lnre.sample.num, s_prop = beta.sample)

sim_plur.list = mapply(function(x,y) sample(c("s", "nons"), x, TRUE, c(y, 1 - y)), sim_data$frequency, sim_data$s_prop)
sim_plur_i.list = mapply(function(x,y) rep(x,y), sim_data$plural, sim_data$frequency)
sim_data_long = unlist(sim_plur.list)
names(sim_data_long) = as.character(unlist(sim_plur_i.list))

sim_data_sample = sample(sim_data_long, 10000, replace = FALSE)
sim_data_sample.df = data.frame(plural = names(sim_data_sample), suffix = sim_data_sample)
sim_data_sample.tab.wide = table(sim_data_sample.df$plural, sim_data_sample.df$suffix)
sim_data_sample.df.wide = data.frame(plural = as.integer(row.names(sim_data_sample.tab.wide)), 
                                     nons = sim_data_sample.tab.wide[,1],
                                     s = sim_data_sample.tab.wide[,2])
sim_data_sample_var.df.wide = sim_data_sample.df.wide[!(sim_data_sample.df.wide$nons == 0 | sim_data_sample.df.wide$s == 0),]
sim_data_sample_var.df.wide$true_s_prop = sapply(sim_data_sample_var.df.wide$plural, function(x) sim_data[sim_data$plural == x,]$s_prop)
sim_data_sample_var.df.wide$mv_freq = sim_data_sample_var.df.wide$s + sim_data_sample_var.df.wide$nons
sim_data_sample_var.df.wide$log_mv_freq = log(sim_data_sample_var.df.wide$mv_freq)
sim_data_sample_var.df.wide$observed_s_prop = sim_data_sample_var.df.wide$s / sim_data_sample_var.df.wide$mv_freq

sim_data.mod = aodml(cbind(s, nons) ~ true_s_prop * log_mv_freq, family = "bb", data = sim_data_sample_var.df.wide)
summary(sim_data.mod)


library(dplyr)
library(ggplot2)

pred_sim_data = sim_data_sample_var.df.wide
pred_sim_data$log_mv_freq = max(sim_data_sample_var.df.wide$log_mv_freq)

betabin_pred = predict(sim_data.mod, se.fit = T, newdata = pred_sim_data)
s_prop_pred_max = plogis(betabin_pred$fit)
s_prop_lo_max = plogis(betabin_pred$fit - qnorm(.975) * betabin_pred$se.fit)
s_prop_hi_max = plogis(betabin_pred$fit + qnorm(.975) * betabin_pred$se.fit)

pred_sim_data$log_mv_freq = median(sim_data_sample_var.df.wide$log_mv_freq)

betabin_pred = predict(sim_data.mod, se.fit = T, newdata = pred_sim_data)
s_prop_pred_med = plogis(betabin_pred$fit)
s_prop_lo_med = plogis(betabin_pred$fit - qnorm(.975) * betabin_pred$se.fit)
s_prop_hi_med = plogis(betabin_pred$fit + qnorm(.975) * betabin_pred$se.fit)

pred_sim_data$log_mv_freq = min(sim_data_sample_var.df.wide$log_mv_freq)

betabin_pred = predict(sim_data.mod, se.fit = T, newdata = pred_sim_data)
s_prop_pred_min = plogis(betabin_pred$fit)
s_prop_lo_min = plogis(betabin_pred$fit - qnorm(.975) * betabin_pred$se.fit)
s_prop_hi_min = plogis(betabin_pred$fit + qnorm(.975) * betabin_pred$se.fit)


sim_plot = cbind(sim_data_sample_var.df.wide, s_prop_pred_max, s_prop_lo_max, s_prop_hi_max, s_prop_pred_med, s_prop_lo_med, s_prop_hi_med, s_prop_pred_min, s_prop_lo_min, s_prop_hi_min)
sim_plot %>% ggplot() + 
  aes(x = true_s_prop, y = observed_s_prop) + 
  geom_point(color = "grey", alpha = .7) + 
  geom_line(aes(y=s_prop_pred_max, linetype = "Max")) +
  geom_ribbon( aes(ymin = s_prop_lo_max, ymax = s_prop_hi_max), alpha = .15) +
  geom_line(aes(y=s_prop_pred_med, linetype = "Median")) +
  geom_ribbon( aes(ymin = s_prop_lo_med, ymax = s_prop_hi_med), alpha = .15) +
  geom_line(aes(y=s_prop_pred_min, linetype = "Min")) +
  geom_ribbon( aes(ymin = s_prop_lo_min, ymax = s_prop_hi_min), alpha = .15) +
  labs(linetype='log_f_mv') 

