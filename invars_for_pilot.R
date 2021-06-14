setwd("/Volumes/tensusers/timzee/other/")
invar = read.csv("invar_probs.csv")

invar2 = invar[order(invar$f_pl, decreasing = T),]
invar2 = invar2[invar2$p_en == 1 | invar2$p_s == 1,]
invar2 = invar2[substr(as.character(invar2$word), nchar(as.character(invar2$word))-1, nchar(as.character(invar2$word))) != "je",]
invar2 = invar2[substr(as.character(invar2$word), nchar(as.character(invar2$word)), nchar(as.character(invar2$word))) != "s",]
invar2 = invar2[substr(as.character(invar2$word), nchar(as.character(invar2$word)), nchar(as.character(invar2$word))) != "f",]


head(invar2[invar2$pl_type == "S",], 20)
head(invar2[invar2$pl_type == "EN",], 20)

write.csv(head(invar2[invar2$pl_type == "S",], 20), "s_invars.csv", quote = F, row.names = F)
write.csv(head(invar2[invar2$pl_type == "EN",], 20), "en_invars.csv", quote = F, row.names = F)

#####
invar = invar[substr(as.character(invar$word), nchar(as.character(invar$word))-1, nchar(as.character(invar$word))) != "je",]
invar = invar[!(substr(as.character(invar$word), nchar(as.character(invar$word)), nchar(as.character(invar$word))) == "s" & invar$pl_type == "S"),]


invar$pl_type2 = as.factor(ifelse(test = invar$pl_type == "OTHER", yes = "OTHER", no = "NONOTHER"))
invar$pl_type2 = relevel(invar$pl_type2, ref="NONOTHER")
invar$log_ratio_pl = log((invar$f_pl+1)/(invar$f_sg+1))

abc = glm(pl_type2 ~ p_other, data = invar, family = "binomial")
##
invar = invar[substr(as.character(invar$word), nchar(as.character(invar$word))-1, nchar(as.character(invar$word))) != "je",]
invar = invar[!(substr(as.character(invar$word), nchar(as.character(invar$word)), nchar(as.character(invar$word))) == "s" & invar$pl_type == "S"),]
invar = invar[invar$word != "een",]
invar = invar[invar$f_pl > 1,]
invar$p_correct = ifelse(test = invar$pl_type == "EN", yes = invar$p_en, no = ifelse(test = invar$pl_type == "S", yes = invar$p_s, no = invar$p_other))
#invar = invar[invar$p_correct > 0.00000 & invar$p_correct < 0.99999,]
invar_rejects = invar[invar$p_correct <= 0.00000 | invar$p_correct >= 0.99999,]
invar$p_correct2 = (invar$p_correct*(nrow(invar)-1) + 0.5)/nrow(invar)
invar$log_ratio_pl = log((invar$f_pl+1)/(invar$f_sg+1))
invar$log_f_lex = log(invar$f_sg + invar$f_pl)
invar$pred_ent = -1*(invar$p_correct2 * log2(invar$p_correct2) + (1 - invar$p_correct2) * log2(1 - invar$p_correct2))
invar$pred_ent2 = (invar$pred_ent*(nrow(invar)-1) + 0.5)/nrow(invar)

library(betareg)
library(effects)
abc = betareg(pred_ent2 ~ log_ratio_pl, data = invar)
summary(abc)
plot(effect("log_ratio_pl", abc))


library(qgam)
library(itsadug)
m = gam(formula = p_correct2 ~ s(log_ratio_pl), family = "betar", data = invar)
plot_smooth(m, view = "log_ratio_pl", transform = plogis)
# bij hele lage log_ratio_pl invar[invar$log_ratio_pl < -6,], zijn veel meervouden gewoon fout
# dus dat verklaart lage p_correct bij lage log_Ratio_pl

mb = gam(formula = p_correct2 ~ s(log_ratio_pl, log_f_lex), family = "betar", data = invar)
plot(mb,scheme = 2)
fvisgam(mb, view=c("log_ratio_pl", "log_f_lex"), too.far = 0.1)

quantile(invar$p_correct2, c(0.05, 0.1, 0.2, 0.4))
invar$p_correct3 = qlogis(invar$p_correct2)

m05 = qgam(p_correct3 ~ s(log_ratio_pl), data = invar, qu = 0.05)
plot_smooth(m05, view = "log_ratio_pl", transform = plogis, ylim = c(0,1))
#abline(h=plogis(quantile(invar$p_correct2, c(0.05))), col="blue", lty="dashed")
m07 = qgam(p_correct3 ~ s(log_ratio_pl), data = invar, qu = 0.07)
plot_smooth(m07, view = "log_ratio_pl", transform = plogis, ylim = c(0,1))
m1 = qgam(p_correct3 ~ s(log_ratio_pl), data = invar, qu = 0.1)
plot_smooth(m1, view = "log_ratio_pl", transform = plogis, ylim = c(0,1))
m2 = qgam(p_correct3 ~ s(log_ratio_pl), data = invar, qu = 0.2)
plot_smooth(m2, view = "log_ratio_pl", transform = plogis, ylim = c(0,1))
m4 = qgam(p_correct3 ~ s(log_ratio_pl), data = invar, qu = 0.4)
plot_smooth(m4, view = "log_ratio_pl", transform = plogis, ylim = c(0,1))

#