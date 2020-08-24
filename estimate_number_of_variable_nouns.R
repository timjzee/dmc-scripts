library(zipfR2)

set.seed(666)

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

var.tab = table(var$f_mv)
var.spc = spc(m = as.numeric(names(var.tab)), Vm = as.numeric(var.tab))
#var.spc2 = rbind(c(1, 1), var.spc)
par(mfrow=c(1,3))
var.lnre.fzm1 = lnre("fzm", var.spc, cost = "gof", m.min=2, m.max=20, debug = F)
plot(var.spc, lnre.spc(var.lnre.fzm1, sum(var$f_mv)), main = "Excl. hapax legomena", ylim = c(0,175), log = "x")
var.lnre.fzm2 = lnre("fzm", var.spc, cost = "gof", m.min=3, m.max=20, debug = F)
plot(var.spc, lnre.spc(var.lnre.fzm2, sum(var$f_mv)), main = "Excl. dis legomena", ylim = c(0,175), log = "x")
var.lnre.fzm3 = lnre("fzm", var.spc, cost = "gof", m.min=4, m.max=20, debug = F)
plot(var.spc, lnre.spc(var.lnre.fzm3, sum(var$f_mv)), main = "Excl. tris legomena", ylim = c(0,175), log = "x")
par(mfrow=c(1,1))

var.lnre.fzm3.vgc <- lnre.vgc(var.lnre.fzm3, (1:100) * 28e2)
var.lnre.fzm1.vgc <- lnre.vgc(var.lnre.fzm1, (1:100) * 28e2)
plot(var.lnre.fzm1.vgc, var.lnre.fzm3.vgc, N0=N(var.lnre.fzm3), legend=c("Excl. hapax legomena", "Excl. tris legomena"))

## we can see the result of sampling error here.
## Vm of low m is underestimated because plurals with only a few observations
## are less likely to show variation, especially if the true -s/-en proportion is close to 0 or 1
var$entropy = ifelse(var$s_prop == 1 | var$s_prop == 0, 0, -(var$s_prop*log2(var$s_prop) + (1-var$s_prop)*log2(1-var$s_prop)))
entr_hi = var[var$entropy >= median(var$entropy),]
entr_hi.tab = table(entr_hi$f_mv)
entr_hi.spc = spc(m = as.numeric(names(entr_hi.tab)), Vm = as.numeric(entr_hi.tab))




# number entropy
var$entropy = ifelse(var$mv_relfreq == 1, 0, -(var$mv_relfreq*log2(var$mv_relfreq) + (1-var$mv_relfreq)*log2(1-var$mv_relfreq)))
# is there a sampling bias by defining entropy this way?

entr_lo = var[var$entropy < .5,]
entr_hi = var[var$entropy >= .5,]

entr_lo.tab = table(entr_lo$f_mv)
entr_lo.spc = spc(m = as.numeric(names(entr_lo.tab)), Vm = as.numeric(entr_lo.tab))

entr_hi.tab = table(entr_hi$f_mv)
entr_hi.spc = spc(m = as.numeric(names(entr_hi.tab)), Vm = as.numeric(entr_hi.tab))

par(mfrow=c(1,2))
entr_lo.fzm1 = lnre("fzm", entr_lo.spc, cost = "gof", m.min=2, m.max=20, debug = F)
plot(entr_lo.spc, lnre.spc(entr_lo.fzm1, sum(entr_lo$f_mv)), main = "Low entropy", log = "x", ylim = c(0,50))

entr_hi.fzm1 = lnre("fzm", entr_hi.spc, cost = "gof", m.min=2, m.max=20, debug = F)
plot(entr_hi.spc, lnre.spc(entr_hi.fzm1, sum(entr_hi$f_mv)), main = "High entropy", log = "x", ylim = c(0,50))
par(mfrow=c(1,1))

entr_lo.fzm1.vgc <- lnre.vgc(entr_lo.fzm1, (1:100) * 28e2)
entr_hi.fzm1.vgc <- lnre.vgc(entr_hi.fzm1, (1:100) * 28e2)
plot(entr_lo.fzm1.vgc, entr_hi.fzm1.vgc, legend=c("Low entropy", "High entropy"))

## get trustworthy plural numbers to establish whether variable plurals are productive
# plurals counts from high frequency singulars underestimate the number of low frequency plurals to a lesser degree in a relative sense
# however it does underestimate them in an absolute sense, see:
var$log_f_ev = log(var$f_ev)
freqsing = var[var$log_f_ev > median(var$log_f_ev),]
freqsing.tab = table(freqsing$f_mv)
freqsing.spc = spc(m = as.numeric(names(freqsing.tab)), Vm = as.numeric(freqsing.tab))
plot(freqsing.spc)


