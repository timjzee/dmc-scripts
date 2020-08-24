library(sjPlot)
library(cocor)
library(betareg)
library(effects)
library(corrplot)
library(languageR)
library(lmtest)
library(aod)
library(VGAM)
library(aods3)

if (Sys.info()[1] == "Darwin"){
  f_path = "/Volumes/tensusers/timzee/timbl_files/"
  f_path2 = "/Volumes/tensusers/timzee/other/"
} else {
  f_path = "/vol/tensusers/timzee/timbl_files/"
  f_path2 = "/vol/tensusers/timzee/other/"
}

korte_vocalen = c("I", "E", "A", "O", "}")
lange_vocalen = c("i", "y", "e", "|", "a", "o", "u")
voor_vocalen = c("I", "E", "}", "i", "e", "|", "y")
achter_vocalen = c("A", "O", "o", "u", "a")
diftongen = c("K", "L", "M")
schwa = c("@")
leen_vocalen = c(")", "*", "<", "!", "(")
klinkers = c(korte_vocalen, lange_vocalen, diftongen, schwa, leen_vocalen)
occlusieven = c("p", "b", "t", "d", "k", "g")
fricatieven = c("f", "v", "s", "z", "S", "Z", "x", "G", "h")
obstruenten = c(occlusieven, fricatieven)
nasalen = c("m", "n", "N")
liquidae = c("l", "r")
halfvocalen = c("j", "w")
sonoranten = c(nasalen, liquidae, halfvocalen)
sonoranten_cluster = c()
for (s1 in sonoranten) {
  for (s2 in sonoranten) {
    sonoranten_cluster = c(sonoranten_cluster, paste(s1, s2, sep = ""))
  }
}
medeklinkers = c(occlusieven, fricatieven, nasalen, liquidae, halfvocalen)

get_phon_template = function(wrd, ns, n1, o2, n2, c2, fin_phon, s2) {
  if (fin_phon %in% obstruenten) {
    return("obstruent")
  } else if (o2 %in% c("j", "tj", "pj", "kj") & n2 == "@" & c2 == "=" & substr(wrd, nchar(wrd) - 1, nchar(wrd)) == "je" & !(substr(wrd, nchar(wrd) - 2, nchar(wrd) - 2) %in% c("j", "n"))) {
    return("tje")
  } else if ((n2 %in% diftongen & c2 == "=") | (n2 %in% c(lange_vocalen, diftongen) & c2 %in% halfvocalen)) {
    return("diph")
  } else if ((n2 %in% c(lange_vocalen, diftongen) & c2 %in% sonoranten) | (n2 %in% korte_vocalen & c2 %in% sonoranten_cluster)) {
    return("long_vowel_son")
  } else if ((((n2 %in% korte_vocalen & c2 %in% sonoranten) | (n2 %in% voor_vocalen & c2 == "=")) & n1 == "=") | (n2 %in% korte_vocalen & c2 %in% sonoranten & n1 != "=" & s2 == "+" & ns == 1)) {
    return("short_vowel_son")
  } else if (n2 %in% korte_vocalen & c2 == "N" & n2 == "I" & n1 != "=" & s2 == "-") {
    return("ing")
  } else if (n2 %in% voor_vocalen & c2 == "=") {
    return("front_vowel")
  } else if (n2 %in% achter_vocalen & c2 == "=") {
    return("back_vowel")
  } else if (c2 %in% sonoranten & n2 == "@") {
    return("schwa_son")
  } else if (n2 %in% korte_vocalen & c2 %in% sonoranten & n1 != "=" & s2 == "-") {
    return("unstressed_vowel_son")
  } else if (c2 == "=" & n2 == "@") {
    return("schwa")
  } else if (n2 %in% korte_vocalen & c2 %in% sonoranten & n1 != "=" & s2 == "+") {
    return("stressed_vowel_son")
  } else {
    return("other")
  }
}

get_default_suffix = function(temp) {
  if (temp == "obstruent") {
    return("en")
  } else if (temp == "diph") {
    return("en")
  } else if (temp == "long_vowel_son") {
    return("en")
  } else if (temp == "short_vowel_son") {
    return("en")
  } else if (temp == "ing") {
    return("en")
  } else if (temp == "tje") {
    return("s")
  } else if (temp == "front_vowel") {
    return("s")
  } else if (temp == "back_vowel") {
    return("s")
  } else if (temp == "schwa_son") {
    return("s")
  } else if (temp == "unstressed_vowel_son") {
    return("s")
  } else if (temp == "schwa") {
    return("s")
  } else if (temp == "stressed_vowel_son") {
    return("none")
  } else {
    return("none")
  }
}

get_incongruency = function(congr, en_f, other_f, s_f) {
  if (congr == "en") {
    return(((s_f + other_f) / (en_f + other_f + s_f)) * 100)
  } else if (congr == "s") {
    return(((en_f + other_f) / (en_f + other_f + s_f)) * 100)
  } else {
    return(NA)
  }
}

get_variable_percentage = function(var_f, en_f, other_f, s_f) {
  return((var_f / (en_f + other_f + s_f + var_f)) * 100)
}

calculate_cor = function(f_name, n) {
  print(n)
  if (file.exists(paste(f_path, f_name, sep = ""))) {
    results = read.csv(paste(f_path, f_name, sep = ""), header = T)
    results$s_prop = results$f_s/(results$f_s + results$f_en + results$f_other)
    pf_cor = cor(results$p_s, results$s_prop)
    return(pf_cor)
  } else {
    return(NA)
  }
}

calculate_cor_mid = function(f_name, n) {
  print(n)
  if (file.exists(paste(f_path, f_name, sep = ""))) {
    results = read.csv(paste(f_path, f_name, sep = ""), header = T)
    results$s_prop = results$f_s/(results$f_s + results$f_en + results$f_other)
    pf_cor = cor(results[results$s_prop > .05 & results$s_prop < .95,]$p_s, results[results$s_prop > .05 & results$s_prop < .95,]$s_prop)
    return(pf_cor)
  } else {
    return(NA)
  }
}

get_acc = function(f_name, n) {
  print(n)
  if (file.exists(paste(f_path, f_name, sep = ""))) {
    results = read.csv(paste(f_path, f_name, sep = ""), header = T)
    if ("accuracy" %in% colnames(results)) {
      return(results$accuracy[1])
    } else {
      return(NA)
    }
  } else {
    return(NA)
  }
}

get_beta_pars = function(f_name, n) {
  print(n)
  if (file.exists(paste(f_path, f_name, sep = ""))) {
    results = read.csv(paste(f_path, f_name, sep = ""), header = T)
    results$p_s_trans = (results$p_s*(nrow(results)-1) + 0.5) / nrow(results)
    p_s_mod = betareg(p_s_trans ~ 1, data = results)
    mu = plogis(p_s_mod$coefficients$mean)
    phi = p_s_mod$coefficients$precision
    alph = mu * phi
    bet = (1 - mu) * phi
    aic = AIC(p_s_mod)
    return(c(mu, phi, alph, bet, aic))
  } else {
    return(c(NA, NA, NA, NA, NA))
  }
}


get_mv_relfreq_sig = function(f_name, n, model_type) {
  print(n)
  if (file.exists(paste(f_path, f_name, sep = ""))) {
    results = read.csv(paste(f_path, f_name, sep = ""), header = T)
    results[results$word == "client",]$f_ev = 115   # this should all be moved to python script
    results[results$word == "big",]$f_ev = 801
    results[results$word == "orgie",]$f_s = 16
#    results[results$word == "grieve",]$f_ev = 5  # is een ww
#    results[results$word == "grieve",]$f_s = 40
    results = results[!(results$word %in% c("l", "pop", "portier", "net", "water", "god", "cent", "karaat", "punt", "post", "kajak", "grieve")),]
    results = results[results$f_s != 0,]
#    results = results[results$f_other == 0,]
    results$s_prop = results$f_s/(results$f_s + results$f_en + results$f_other)
    results$log_f_mv = log(results$f_s + results$f_en + results$f_other)
    results$mv_relfreq = (results$f_s + results$f_en + results$f_other) / (results$f_s + results$f_en + results$f_other + results$f_ev)
    results$f_nons = results$f_en + results$f_other
    if (model_type == "betareg"){
      results$s_prop2 = (results$s_prop*(nrow(results)-1) + 0.5) / nrow(results)
      prop = betareg(s_prop2 ~ p_s*mv_relfreq + p_s*log_f_mv, data = results)
      return(summary(prop)$coefficients$mean["p_s:mv_relfreq", 4])
    } else if (model_type == "betabin_aod"){
      prop = betabin(cbind(f_s, f_nons) ~ p_s * mv_relfreq + p_s * log_f_mv, ~ 1, data = results, control = list(maxit=5000))
      return(summary(prop)@Coef["p_s:mv_relfreq",]$`Pr(> |z|)`)
    } else if (model_type == "betabin_vgam"){
      prop = vglm(cbind(f_s, f_nons) ~ p_s * mv_relfreq + p_s * log_f_mv, betabinomial, results)
      return(summary(prop)@coef3["p_s:mv_relfreq", "Pr(>|z|)"])
    } else {
      prop = aodml(cbind(f_s, f_nons) ~ p_s * mv_relfreq + p_s * log_f_mv, family = "bb", data = results)
      return(summary(prop)$BCoef["p_s:mv_relfreq", "Pr(> |z|)"])
    }
  } else {
    return(NA)
  }
}

get_mv_freq_relfreq_sig = function(f_name, n, model_type) {
  print(n)
  if (file.exists(paste(f_path, f_name, sep = ""))) {
    results = read.csv(paste(f_path, f_name, sep = ""), header = T)
    results[results$word == "client",]$f_ev = 115
    results[results$word == "big",]$f_ev = 801
    results[results$word == "orgie",]$f_s = 16
#    results[results$word == "grieve",]$f_ev = 5  # is een ww
#    results[results$word == "grieve",]$f_s = 40
    results = results[!(results$word %in% c("l", "pop", "portier", "net", "water", "god", "cent", "karaat", "punt", "post", "kajak", "grieve")),]
    results = results[results$f_s != 0,]
#    results = results[results$f_other == 0,]
    results$s_prop = results$f_s/(results$f_s + results$f_en + results$f_other)
    results$log_f_mv = log(results$f_s + results$f_en + results$f_other)
    results$mv_relfreq = (results$f_s + results$f_en + results$f_other) / (results$f_s + results$f_en + results$f_other + results$f_ev)
    results$f_nons = results$f_en + results$f_other
    if (model_type == "betareg"){
      results$s_prop2 = (results$s_prop*(nrow(results)-1) + 0.5) / nrow(results)
      prop = betareg(s_prop2 ~ p_s*mv_relfreq*log_f_mv, data = results)
      return(summary(prop)$coefficients$mean["p_s:mv_relfreq:log_f_mv", 4])
    } else if (model_type == "betabin_aod"){
      prop = betabin(cbind(f_s, f_nons) ~ p_s * mv_relfreq * log_f_mv, ~ 1, data = results, control = list(maxit=10000))
      return(summary(prop)@Coef["p_s:mv_relfreq:log_f_mv",]$`Pr(> |z|)`)
    } else if (model_type == "betabin_vgam"){
      prop = vglm(cbind(f_s, f_nons) ~ p_s * mv_relfreq * log_f_mv, betabinomial, results)
      return(summary(prop)@coef3["p_s:mv_relfreq:log_f_mv", "Pr(>|z|)"])
    } else {
      prop = aodml(cbind(f_s, f_nons) ~ p_s * mv_relfreq * log_f_mv, family = "bb", data = results)
      return(summary(prop)$BCoef["p_s:mv_relfreq", "Pr(> |z|)"])
    }
  } else {
    return(NA)
  }
}

gs_index = read.csv(paste(f_path, "gs_index.csv", sep = ""), header = T)

gs_index = gs_index[gs_index$instncs == "type" 
                    & gs_index$ad_invar == "True"
                    & gs_index$mtrc == "O"
#                    & gs_index$ad_var == "False" 
#                    & gs_index$ad_verb == "False" 
                    & gs_index$nneigh_k < 6
                    & gs_index$shrd_lems == "True"
                    ,]

gs_index$cor = mapply(calculate_cor, gs_index$file_n, gs_index$num)
gs_index_best = gs_index[gs_index$cor == max(gs_index$cor, na.rm = T),]
gs_index_best = gs_index_best[rowSums(is.na(gs_index_best))<length(gs_index_best),]
plot(gs_index$num, gs_index$cor)

gs_index$acc = mapply(get_acc, gs_index$file_n, gs_index$num)
gs_index_best_acc = gs_index[gs_index$acc == max(gs_index$acc, na.rm = T),]
gs_index_best_acc = gs_index_best_acc[rowSums(is.na(gs_index_best_acc))<length(gs_index_best_acc),]
plot(gs_index$num, gs_index$acc)

p_s_pars = mapply(get_beta_pars, gs_index$file_n, gs_index$num)

gs_index$p_s_mu = p_s_pars[1,]
gs_index$p_s_phi = p_s_pars[2,]
gs_index$p_s_alpha = p_s_pars[3,]
gs_index$p_s_beta = p_s_pars[4,]
gs_index$p_s_aic = p_s_pars[5,]

gs_index$mv_relfreq_sig = mapply(get_mv_relfreq_sig, gs_index$file_n, gs_index$num, "betabin_aods3")
#gs_index$mv_freq_relfreq_sig = mapply(get_mv_freq_relfreq_sig, gs_index$file_n, gs_index$num, "betabin_aods3")

gs_index_sig = gs_index[gs_index$mv_relfreq_sig < .05 
#                        & gs_index$p_s_beta > .22
                        & gs_index$nneigh_k > 1
                        & gs_index$n_syll > 1 
                        & gs_index$ad_verb == "False"
#                        & gs_index$ad_var == "False"
,]
gs_index_nonsig = gs_index[gs_index$mv_relfreq_sig >= .05 
#                          & gs_index$p_s_beta > .22 
                          & gs_index$nneigh_k > 1
                           & gs_index$n_syll > 1 
                           & gs_index$ad_verb == "False"
#                          & gs_index$ad_var == "False"
,]

nrow(gs_index_sig)
nrow(gs_index_nonsig)

nrow(gs_index_sig[gs_index_sig$p_s_alpha > .22 & gs_index_sig$p_s_beta > .22,])
nrow(gs_index_nonsig[gs_index_nonsig$p_s_alpha > .22 & gs_index_nonsig$p_s_beta > .22,])

nrow(gs_index_sig[gs_index_sig$merge == "False" & gs_index_sig$n_syll > 1 & gs_index_sig$nneigh_k < 4 & gs_index_sig$d_weight == "ID" & gs_index_sig$nneigh_k > 1,])
nrow(gs_index_nonsig[gs_index_nonsig$merge == "False" & gs_index_nonsig$n_syll > 1 & gs_index_nonsig$nneigh_k < 4 & gs_index_nonsig$d_weight == "ID" & gs_index_sig$nneigh_k > 1,])


nrow(gs_index_nonsig[gs_index_nonsig$n_syll == 1 | gs_index_nonsig$nneigh_k == 1,])

get_mv_relfreq_sig("p_f_type_O_merge_2syl_k4_ID_invar.csv", 1, "betabin_aods3")

#gs_index$cor_mid = mapply(calculate_cor_mid, gs_index$file_n, gs_index$num)
#gs_index_best_mid = gs_index[gs_index$cor_mid == max(gs_index$cor_mid, na.rm = T),]
#gs_index_best_mid = gs_index_best_mid[rowSums(is.na(gs_index_best_mid))<length(gs_index_best_mid),]
#plot(gs_index$num, gs_index$cor_mid)

par_names = c()
means = c()
stand_devs = c()
difference_significance = c()
difference_significance2 = c()

acc_means = c()
acc_sds = c()
acc_sig = c()
acc_sig2 = c()

# instances
# m_type = mean(gs_index[gs_index$instncs == "type" & gs_index$merge == "False",]$cor)
# sd_type = sd(gs_index[gs_index$instncs == "type" & gs_index$merge == "False",]$cor)
# m_token = mean(gs_index[gs_index$instncs == "token",]$cor, na.rm = T)
# sd_token = sd(gs_index[gs_index$instncs == "token",]$cor, na.rm = T)
# boxplot(cor ~ instncs, data = gs_index[gs_index$merge == "False",])
# instances_p = t.test(gs_index[gs_index$instncs == "type" & gs_index$merge == "False",]$cor, gs_index[gs_index$instncs == "token",]$cor, paired = TRUE, alternative = "two.sided")$p.value
# instances_p2 = ifelse(instances_p < .001, "***", ifelse(instances_p < .01, "**", ifelse(instances_p < .05, "*", "")))
# 
# acc_m_type = mean(gs_index[gs_index$instncs == "type" & gs_index$merge == "False",]$acc, na.rm = T)
# acc_sd_type = sd(gs_index[gs_index$instncs == "type" & gs_index$merge == "False",]$acc, na.rm = T)
# acc_m_token = mean(gs_index[gs_index$instncs == "token",]$acc, na.rm = T)
# acc_sd_token = sd(gs_index[gs_index$instncs == "token",]$acc, na.rm = T)
# 
# par_names = c(par_names, "instances", "type", "token")
# means = c(means, "", round(m_type, 3), round(m_token, 3))
# stand_devs = c(stand_devs, "", round(sd_type, 3), round(sd_token, 3))
# difference_significance = c(difference_significance, "", "", round(instances_p, 3))
# difference_significance2 = c(difference_significance2, "", "", instances_p2)
# 
# acc_means = c(acc_means, "", round(acc_m_type, 3), round(acc_m_token, 3))
# acc_sds = c(acc_sds, "", round(acc_sd_type, 3), round(acc_sd_token, 3))

# merge
m_unmerged = mean(gs_index[gs_index$instncs == "type" & gs_index$merge == "False",]$cor, na.rm = T)
sd_unmerged = sd(gs_index[gs_index$instncs == "type" & gs_index$merge == "False",]$cor, na.rm = T)
m_merged = mean(gs_index[gs_index$instncs == "type" & gs_index$merge == "True",]$cor, na.rm = T)
sd_merged = sd(gs_index[gs_index$instncs == "type" & gs_index$merge == "True",]$cor, na.rm = T)
boxplot(cor ~ merge, data = gs_index[gs_index$instncs == "type",])
merge_p = t.test(gs_index[gs_index$instncs == "type" & gs_index$merge == "False",]$cor, gs_index[gs_index$instncs == "type" & gs_index$merge == "True",]$cor, paired = TRUE, alternative = "two.sided")$p.value
merge_p2 = ifelse(merge_p < .001, "***", ifelse(merge_p < .01, "**", ifelse(merge_p < .05, "*", "")))

acc_m_unmerged = mean(gs_index[gs_index$instncs == "type" & gs_index$merge == "False",]$acc, na.rm = T)
acc_sd_unmerged = sd(gs_index[gs_index$instncs == "type" & gs_index$merge == "False",]$acc, na.rm = T)
acc_m_merged = mean(gs_index[gs_index$instncs == "type" & gs_index$merge == "True",]$acc, na.rm = T)
acc_sd_merged = sd(gs_index[gs_index$instncs == "type" & gs_index$merge == "True",]$acc, na.rm = T)
acc_merge_p = t.test(gs_index[gs_index$instncs == "type" & gs_index$merge == "False",]$acc, gs_index[gs_index$instncs == "type" & gs_index$merge == "True",]$acc, paired = TRUE, alternative = "two.sided")$p.value
acc_merge_p2 = ifelse(acc_merge_p < .001, "***", ifelse(acc_merge_p < .01, "**", ifelse(acc_merge_p < .05, "*", "")))


par_names = c(par_names, "Type Merging", "No", "Yes")
means = c(means, "", round(m_unmerged, 3), round(m_merged, 3))
stand_devs = c(stand_devs, "", round(sd_unmerged, 3), round(sd_merged, 3))
difference_significance = c(difference_significance, "", "", round(merge_p, 3))
difference_significance2 = c(difference_significance2, "", "", merge_p2)

acc_means = c(acc_means, "", round(acc_m_unmerged, 3), round(acc_m_merged, 3))
acc_sds = c(acc_sds, "", round(acc_sd_unmerged, 3), round(acc_sd_merged, 3))
acc_sig = c(acc_sig, "", "", round(acc_merge_p, 3))
acc_sig2 = c(acc_sig2, "", "", acc_merge_p2)

# num syll
m_1syl = mean(gs_index[gs_index$n_syll == 1,]$cor, na.rm = T)
sd_1syl = sd(gs_index[gs_index$n_syll == 1,]$cor, na.rm = T)
m_2syl = mean(gs_index[gs_index$n_syll == 2,]$cor, na.rm = T)
sd_2syl = sd(gs_index[gs_index$n_syll == 2,]$cor, na.rm = T)
m_3syl = mean(gs_index[gs_index$n_syll == 3,]$cor, na.rm = T)
sd_3syl = sd(gs_index[gs_index$n_syll == 3,]$cor, na.rm = T)
m_4syl = mean(gs_index[gs_index$n_syll == 4,]$cor, na.rm = T)
sd_4syl = sd(gs_index[gs_index$n_syll == 4,]$cor, na.rm = T)
boxplot(cor ~ n_syll, data = gs_index)
p_1_2syl = t.test(gs_index[gs_index$n_syll == 1,]$cor, gs_index[gs_index$n_syll == 2,]$cor, paired = TRUE, alternative = "two.sided")$p.value
p_2_3syl = t.test(gs_index[gs_index$n_syll == 2,]$cor, gs_index[gs_index$n_syll == 3,]$cor, paired = TRUE, alternative = "two.sided")$p.value
p_3_4syl = t.test(gs_index[gs_index$n_syll == 3,]$cor, gs_index[gs_index$n_syll == 4,]$cor, paired = TRUE, alternative = "two.sided")$p.value
p_1_2syl2 = ifelse(p_1_2syl < .001, "***", ifelse(p_1_2syl < .01, "**", ifelse(p_1_2syl < .05, "*", "")))
p_2_3syl2 = ifelse(p_2_3syl < .001, "***", ifelse(p_2_3syl < .01, "**", ifelse(p_2_3syl < .05, "*", "")))
p_3_4syl2 = ifelse(p_3_4syl < .001, "***", ifelse(p_3_4syl < .01, "**", ifelse(p_3_4syl < .05, "*", "")))

acc_m_1syl = mean(gs_index[gs_index$n_syll == 1,]$acc, na.rm = T)
acc_sd_1syl = sd(gs_index[gs_index$n_syll == 1,]$acc, na.rm = T)
acc_m_2syl = mean(gs_index[gs_index$n_syll == 2,]$acc, na.rm = T)
acc_sd_2syl = sd(gs_index[gs_index$n_syll == 2,]$acc, na.rm = T)
acc_m_3syl = mean(gs_index[gs_index$n_syll == 3,]$acc, na.rm = T)
acc_sd_3syl = sd(gs_index[gs_index$n_syll == 3,]$acc, na.rm = T)
acc_m_4syl = mean(gs_index[gs_index$n_syll == 4,]$acc, na.rm = T)
acc_sd_4syl = sd(gs_index[gs_index$n_syll == 4,]$acc, na.rm = T)
acc_p_1_2syl = t.test(gs_index[gs_index$n_syll == 1,]$acc, gs_index[gs_index$n_syll == 2,]$acc, paired = TRUE, alternative = "two.sided")$p.value
acc_p_2_3syl = t.test(gs_index[gs_index$n_syll == 2,]$acc, gs_index[gs_index$n_syll == 3,]$acc, paired = TRUE, alternative = "two.sided")$p.value
acc_p_3_4syl = t.test(gs_index[gs_index$n_syll == 3,]$acc, gs_index[gs_index$n_syll == 4,]$acc, paired = TRUE, alternative = "two.sided")$p.value
acc_p_1_2syl2 = ifelse(acc_p_1_2syl < .001, "***", ifelse(acc_p_1_2syl < .01, "**", ifelse(acc_p_1_2syl < .05, "*", "")))
acc_p_2_3syl2 = ifelse(acc_p_2_3syl < .001, "***", ifelse(acc_p_2_3syl < .01, "**", ifelse(acc_p_2_3syl < .05, "*", "")))
acc_p_3_4syl2 = ifelse(acc_p_3_4syl < .001, "***", ifelse(acc_p_3_4syl < .01, "**", ifelse(acc_p_3_4syl < .05, "*", "")))


par_names = c(par_names, "Number of Syllables", "1", "2", "3", "4")
means = c(means, "", round(m_1syl, 3), round(m_2syl, 3), round(m_3syl, 3), round(m_4syl, 3))
stand_devs = c(stand_devs, "", round(sd_1syl, 3), round(sd_2syl, 3), round(sd_3syl, 3), round(sd_4syl, 3))
difference_significance = c(difference_significance, "", "", round(p_1_2syl, 3), round(p_2_3syl, 3), round(p_3_4syl, 3))
difference_significance2 = c(difference_significance2, "", "", p_1_2syl2, p_2_3syl2, p_3_4syl2)

acc_means = c(acc_means, "", round(acc_m_1syl, 3), round(acc_m_2syl, 3), round(acc_m_3syl, 3), round(acc_m_4syl, 3))
acc_sds = c(acc_sds, "", round(acc_sd_1syl, 3), round(acc_sd_2syl, 3), round(acc_sd_3syl, 3), round(acc_sd_4syl, 3))
acc_sig = c(acc_sig, "", "", round(acc_p_1_2syl, 3), round(acc_p_2_3syl, 3), round(acc_p_3_4syl, 3))
acc_sig2 = c(acc_sig2, "", "", acc_p_1_2syl2, acc_p_2_3syl2, acc_p_3_4syl2)


# k num
m_k1 = mean(gs_index[gs_index$nneigh_k == 1,]$cor, na.rm = T)
sd_k1 = sd(gs_index[gs_index$nneigh_k == 1,]$cor, na.rm = T)
m_k2 = mean(gs_index[gs_index$nneigh_k == 2,]$cor, na.rm = T)
sd_k2 = sd(gs_index[gs_index$nneigh_k == 2,]$cor, na.rm = T)
m_k3 = mean(gs_index[gs_index$nneigh_k == 3,]$cor, na.rm = T)
sd_k3 = sd(gs_index[gs_index$nneigh_k == 3,]$cor, na.rm = T)
m_k4 = mean(gs_index[gs_index$nneigh_k == 4,]$cor, na.rm = T)
sd_k4 = sd(gs_index[gs_index$nneigh_k == 4,]$cor, na.rm = T)
m_k5 = mean(gs_index[gs_index$nneigh_k == 5,]$cor, na.rm = T)
sd_k5 = sd(gs_index[gs_index$nneigh_k == 5,]$cor, na.rm = T)

boxplot(cor ~ nneigh_k, data = gs_index)
p_k1_2 = t.test(gs_index[gs_index$nneigh_k == 1,]$cor, gs_index[gs_index$nneigh_k == 2,]$cor, paired = TRUE, alternative = "two.sided")$p.value
p_k2_3 = t.test(gs_index[gs_index$nneigh_k == 2,]$cor, gs_index[gs_index$nneigh_k == 3,]$cor, paired = TRUE, alternative = "two.sided")$p.value
p_k3_4 = t.test(gs_index[gs_index$nneigh_k == 3,]$cor, gs_index[gs_index$nneigh_k == 4,]$cor, paired = TRUE, alternative = "two.sided")$p.value
p_k4_5 = t.test(gs_index[gs_index$nneigh_k == 4,]$cor, gs_index[gs_index$nneigh_k == 5,]$cor, paired = TRUE, alternative = "two.sided")$p.value
p_k1_2b = ifelse(p_k1_2 < .001, "***", ifelse(p_k1_2 < .01, "**", ifelse(p_k1_2 < .05, "*", "")))
p_k2_3b = ifelse(p_k2_3 < .001, "***", ifelse(p_k2_3 < .01, "**", ifelse(p_k2_3 < .05, "*", "")))
p_k3_4b = ifelse(p_k3_4 < .001, "***", ifelse(p_k3_4 < .01, "**", ifelse(p_k3_4 < .05, "*", "")))
p_k4_5b = ifelse(p_k4_5 < .001, "***", ifelse(p_k4_5 < .01, "**", ifelse(p_k4_5 < .05, "*", "")))

acc_m_k1 = mean(gs_index[gs_index$nneigh_k == 1,]$acc, na.rm = T)
acc_sd_k1 = sd(gs_index[gs_index$nneigh_k == 1,]$acc, na.rm = T)
acc_m_k2 = mean(gs_index[gs_index$nneigh_k == 2,]$acc, na.rm = T)
acc_sd_k2 = sd(gs_index[gs_index$nneigh_k == 2,]$acc, na.rm = T)
acc_m_k3 = mean(gs_index[gs_index$nneigh_k == 3,]$acc, na.rm = T)
acc_sd_k3 = sd(gs_index[gs_index$nneigh_k == 3,]$acc, na.rm = T)
acc_m_k4 = mean(gs_index[gs_index$nneigh_k == 4,]$acc, na.rm = T)
acc_sd_k4 = sd(gs_index[gs_index$nneigh_k == 4,]$acc, na.rm = T)
acc_m_k5 = mean(gs_index[gs_index$nneigh_k == 5,]$acc, na.rm = T)
acc_sd_k5 = sd(gs_index[gs_index$nneigh_k == 5,]$acc, na.rm = T)

acc_p_k1_2 = t.test(gs_index[gs_index$nneigh_k == 1,]$acc, gs_index[gs_index$nneigh_k == 2,]$acc, paired = TRUE, alternative = "two.sided")$p.value
acc_p_k2_3 = t.test(gs_index[gs_index$nneigh_k == 2,]$acc, gs_index[gs_index$nneigh_k == 3,]$acc, paired = TRUE, alternative = "two.sided")$p.value
acc_p_k3_4 = t.test(gs_index[gs_index$nneigh_k == 3,]$acc, gs_index[gs_index$nneigh_k == 4,]$acc, paired = TRUE, alternative = "two.sided")$p.value
acc_p_k4_5 = t.test(gs_index[gs_index$nneigh_k == 4,]$acc, gs_index[gs_index$nneigh_k == 5,]$acc, paired = TRUE, alternative = "two.sided")$p.value
acc_p_k1_2b = ifelse(acc_p_k1_2 < .001, "***", ifelse(acc_p_k1_2 < .01, "**", ifelse(acc_p_k1_2 < .05, "*", "")))
acc_p_k2_3b = ifelse(acc_p_k2_3 < .001, "***", ifelse(acc_p_k2_3 < .01, "**", ifelse(acc_p_k2_3 < .05, "*", "")))
acc_p_k3_4b = ifelse(acc_p_k3_4 < .001, "***", ifelse(acc_p_k3_4 < .01, "**", ifelse(acc_p_k3_4 < .05, "*", "")))
acc_p_k4_5b = ifelse(acc_p_k4_5 < .001, "***", ifelse(acc_p_k4_5 < .01, "**", ifelse(acc_p_k4_5 < .05, "*", "")))


par_names = c(par_names, "k", "1", "2", "3", "4", "5")
means = c(means, "", round(m_k1, 3), round(m_k2, 3), round(m_k3, 3), round(m_k4, 3), round(m_k5, 3))
stand_devs = c(stand_devs, "", round(sd_k1, 3), round(sd_k2, 3), round(sd_k3, 3), round(sd_k4, 3), round(sd_k5, 3))
difference_significance = c(difference_significance, "", "", round(p_k1_2, 3), round(p_k2_3, 3), round(p_k3_4, 3), round(p_k4_5, 3))
difference_significance2 = c(difference_significance2, "", "", p_k1_2b, p_k2_3b, p_k3_4b, p_k4_5b)

acc_means = c(acc_means, "", round(acc_m_k1, 4), round(acc_m_k2, 4), round(acc_m_k3, 4), round(acc_m_k4, 4), round(acc_m_k5, 4))
acc_sds = c(acc_sds, "", round(acc_sd_k1, 4), round(acc_sd_k2, 4), round(acc_sd_k3, 4), round(acc_sd_k4, 4), round(acc_sd_k5, 4))
acc_sig = c(acc_sig, "", "", round(acc_p_k1_2, 3), round(acc_p_k2_3, 3), round(acc_p_k3_4, 3), round(acc_p_k4_5, 3))
acc_sig2 = c(acc_sig2, "", "", acc_p_k1_2b, acc_p_k2_3b, acc_p_k3_4b, acc_p_k4_5b)


# Distance Weighting
m_zero = mean(gs_index[gs_index$d_weight == "Z",]$cor, na.rm = T)
sd_zero = sd(gs_index[gs_index$d_weight == "Z",]$cor, na.rm = T)
m_inv = mean(gs_index[gs_index$d_weight == "ID",]$cor, na.rm = T)
sd_inv = sd(gs_index[gs_index$d_weight == "ID",]$cor, na.rm = T)
boxplot(cor ~ d_weight, data = gs_index)
d_weight_p = t.test(gs_index[gs_index$d_weight == "Z",]$cor, gs_index[gs_index$d_weight == "ID",]$cor, paired = TRUE, alternative = "two.sided")$p.value
d_weight_p2 = ifelse(d_weight_p < .001, "***", ifelse(d_weight_p < .01, "**", ifelse(d_weight_p < .05, "*", "")))

acc_m_zero = mean(gs_index[gs_index$d_weight == "Z",]$acc, na.rm = T)
acc_sd_zero = sd(gs_index[gs_index$d_weight == "Z",]$acc, na.rm = T)
acc_m_inv = mean(gs_index[gs_index$d_weight == "ID",]$acc, na.rm = T)
acc_sd_inv = sd(gs_index[gs_index$d_weight == "ID",]$acc, na.rm = T)
acc_d_weight_p = t.test(gs_index[gs_index$d_weight == "Z",]$acc, gs_index[gs_index$d_weight == "ID",]$acc, paired = TRUE, alternative = "two.sided")$p.value
acc_d_weight_p2 = ifelse(acc_d_weight_p < .001, "***", ifelse(acc_d_weight_p < .01, "**", ifelse(acc_d_weight_p < .05, "*", "")))


par_names = c(par_names, "Distance Weighting", "Zero Decay", "Inv. Distance Decay")
means = c(means, "", round(m_zero, 3), round(m_inv, 3))
stand_devs = c(stand_devs, "", round(sd_zero, 3), round(sd_inv, 3))
difference_significance = c(difference_significance, "", "", round(d_weight_p, 3))
difference_significance2 = c(difference_significance2, "", "", d_weight_p2)

acc_means = c(acc_means, "", round(acc_m_zero, 3), round(acc_m_inv, 3))
acc_sds = c(acc_sds, "", round(acc_sd_zero, 3), round(acc_sd_inv, 3))
acc_sig = c(acc_sig, "", "", round(acc_d_weight_p, 3))
acc_sig2 = c(acc_sig2, "", "", acc_d_weight_p2)


# training materials
# both = gs_index[gs_index$ad_var == "True" & gs_index$ad_invar == "True",]
# both$var_invar = "both"
# invar = gs_index[gs_index$ad_var == "False" & gs_index$ad_invar == "True",]
# invar$var_invar = "invar"
# var = gs_index[gs_index$ad_var == "True" & gs_index$ad_invar == "False",]
# var$var_invar = "var"
# gs_index2 = rbind(both, invar, var)
# gs_index2$var_invar = as.factor(gs_index2$var_invar)
# m_both = mean(gs_index2[gs_index2$var_invar == "both",]$cor, na.rm = T)
# sd_both = sd(gs_index2[gs_index2$var_invar == "both",]$cor, na.rm = T)
# m_invar = mean(gs_index2[gs_index2$var_invar == "invar",]$cor, na.rm = T)
# sd_invar = sd(gs_index2[gs_index2$var_invar == "invar",]$cor, na.rm = T)
# m_var = mean(gs_index2[gs_index2$var_invar == "var",]$cor, na.rm = T)
# sd_var = sd(gs_index2[gs_index2$var_invar == "var",]$cor, na.rm = T)
# boxplot(cor ~ var_invar, data = gs_index2)
# both_invar_p = t.test(gs_index2[gs_index2$var_invar == "both",]$cor, gs_index2[gs_index2$var_invar == "invar",]$cor, paired = TRUE, alternative = "two.sided")$p.value
# invar_var_p = t.test(gs_index2[gs_index2$var_invar == "invar",]$cor, gs_index2[gs_index2$var_invar == "var",]$cor, paired = TRUE, alternative = "two.sided")$p.value
# both_invar_p2 = ifelse(both_invar_p < .001, "***", ifelse(both_invar_p < .01, "**", ifelse(both_invar_p < .05, "*", "")))
# invar_var_p2 = ifelse(invar_var_p < .001, "***", ifelse(invar_var_p < .01, "**", ifelse(invar_var_p < .05, "*", "")))
# 
# acc_m_both = mean(gs_index2[gs_index2$var_invar == "both",]$acc, na.rm = T)
# acc_sd_both = sd(gs_index2[gs_index2$var_invar == "both",]$acc, na.rm = T)
# acc_m_invar = mean(gs_index2[gs_index2$var_invar == "invar",]$acc, na.rm = T)
# acc_sd_invar = sd(gs_index2[gs_index2$var_invar == "invar",]$acc, na.rm = T)
# acc_m_var = mean(gs_index2[gs_index2$var_invar == "var",]$acc, na.rm = T)
# acc_sd_var = sd(gs_index2[gs_index2$var_invar == "var",]$acc, na.rm = T)
# 
# par_names = c(par_names, "Plural Type", "Both", "Invariable", "Variable")
# means = c(means, "", round(m_both, 3), round(m_invar, 3), round(m_var, 3))
# stand_devs = c(stand_devs, "", round(sd_both, 3), round(sd_invar, 3), round(sd_var, 3))
# difference_significance = c(difference_significance, "", "", round(both_invar_p, 3), round(invar_var_p, 3))
# difference_significance2 = c(difference_significance2, "", "", both_invar_p2, invar_var_p2)
# 
# acc_means = c(acc_means, "", round(acc_m_both, 3), round(acc_m_invar, 3), round(acc_m_var, 3))
# acc_sds = c(acc_sds, "", round(acc_sd_both, 3), round(acc_sd_invar, 3), round(acc_sd_var, 3))
# 
# 
# # Adding verbs
# verbs_none = gs_index[gs_index$ad_verb == "False",]
# verbs_none$verbs = "none"
# verbs_EN = gs_index[gs_index$ad_verb == "True" & gs_index$verb_cl == "EN",]
# verbs_EN$verbs = "EN"
# verbs_VERB = gs_index[gs_index$ad_verb == "True" & gs_index$verb_cl == "VERB",]
# verbs_VERB$verbs = "VERB"
# gs_index3 = rbind(verbs_none, verbs_EN, verbs_VERB)
# gs_index3$verbs = as.factor(gs_index3$verbs)
# m_none = mean(gs_index3[gs_index3$verbs == "none",]$cor, na.rm = T)
# sd_none = sd(gs_index3[gs_index3$verbs == "none",]$cor, na.rm = T)
# m_EN = mean(gs_index3[gs_index3$verbs == "EN",]$cor, na.rm = T)
# sd_EN = sd(gs_index3[gs_index3$verbs == "EN",]$cor, na.rm = T)
# m_VERB = mean(gs_index3[gs_index3$verbs == "VERB",]$cor, na.rm = T)
# sd_VERB = sd(gs_index3[gs_index3$verbs == "VERB",]$cor, na.rm = T)
# boxplot(cor ~ verbs, data = gs_index3)
# none_EN_p = t.test(gs_index3[gs_index3$verbs == "none",]$cor, gs_index3[gs_index3$verbs == "EN",]$cor, paired = TRUE, alternative = "two.sided")$p.value
# EN_VERB_p = t.test(gs_index3[gs_index3$verbs == "EN",]$cor, gs_index3[gs_index3$verbs == "VERB",]$cor, paired = TRUE, alternative = "two.sided")$p.value
# none_EN_p2 = ifelse(none_EN_p < .001, "***", ifelse(none_EN_p < .01, "**", ifelse(none_EN_p < .05, "*", "")))
# EN_VERB_p2 = ifelse(EN_VERB_p < .001, "***", ifelse(EN_VERB_p < .01, "**", ifelse(EN_VERB_p < .05, "*", "")))
# 
# acc_m_none = mean(gs_index3[gs_index3$verbs == "none",]$acc, na.rm = T)
# acc_sd_none = sd(gs_index3[gs_index3$verbs == "none",]$acc, na.rm = T)
# acc_m_EN = mean(gs_index3[gs_index3$verbs == "EN",]$acc, na.rm = T)
# acc_sd_EN = sd(gs_index3[gs_index3$verbs == "EN",]$acc, na.rm = T)
# acc_m_VERB = mean(gs_index3[gs_index3$verbs == "VERB",]$acc, na.rm = T)
# acc_sd_VERB = sd(gs_index3[gs_index3$verbs == "VERB",]$acc, na.rm = T)
# 
# 
# par_names = c(par_names, "Add verbs", "No", "Yes (as plurals)", "Yes (as verbs)")
# means = c(means, "", round(m_none, 3), round(m_EN, 3), round(m_VERB, 3))
# stand_devs = c(stand_devs, "", round(sd_none, 3), round(sd_EN, 3), round(sd_VERB, 3))
# difference_significance = c(difference_significance, "", "", round(none_EN_p, 3), round(EN_VERB_p, 3))
# difference_significance2 = c(difference_significance2, "", "", none_EN_p2, EN_VERB_p2)
# 
# acc_means = c(acc_means, "", round(acc_m_none, 3), round(acc_m_EN, 3), round(acc_m_VERB, 3))
# acc_sds = c(acc_sds, "", round(acc_sd_none, 3), round(acc_sd_EN, 3), round(acc_sd_VERB, 3))


gs1 = data.frame(par_names, means, stand_devs, difference_significance2, acc_means, acc_sds, acc_sig2)
names(gs1) = c("Parameters", "Correlations", "SD", "Difference significance", "Accuracies", "SD", "Difference significance")
tab_df(gs1)


# p_f_type_1syl = read.csv(paste(f_path, "p_f_type_1syl_k3_ID_both_EN.csv", sep = ""), header = T)
# p_f_type_2syl = read.csv(paste(f_path, "p_f_type_2syl_k3_ID_both_EN.csv", sep = ""), header = T)
# p_f_type_3syl = read.csv(paste(f_path, "p_f_type_3syl_k3_ID_both_EN.csv", sep = ""), header = T)
# p_f_type_4syl = read.csv(paste(f_path, "p_f_type_4syl_k3_ID_both_EN.csv", sep = ""), header = T)
# 
# c_syl1 = gs_index[gs_index$file_n == "p_f_type_1syl_k3_ID_both_EN.csv",]$cor
# c_syl2 = gs_index[gs_index$file_n == "p_f_type_2syl_k3_ID_both_EN.csv",]$cor
# c_syl3 = gs_index[gs_index$file_n == "p_f_type_3syl_k3_ID_both_EN.csv",]$cor
# c_syl4 = gs_index[gs_index$file_n == "p_f_type_4syl_k3_ID_both_EN.csv",]$cor
# 
# par_names = c("1syl", "2syl", "3syl", "4syl")
# correlations = c(c_syl1, c_syl2, c_syl3, c_syl4)
# difference_significance = c(
#   NA, 
#   cocor.dep.groups.overlap(c_syl1, c_syl2, cor(p_f_type_1syl$p_s, p_f_type_2syl$p_s), nrow(p_f_type_1syl), test = c("hittner2003"))@hittner2003$p.value,
#   cocor.dep.groups.overlap(c_syl2, c_syl3, cor(p_f_type_2syl$p_s, p_f_type_3syl$p_s), nrow(p_f_type_1syl), test = c("hittner2003"))@hittner2003$p.value,
#   cocor.dep.groups.overlap(c_syl3, c_syl4, cor(p_f_type_3syl$p_s, p_f_type_4syl$p_s), nrow(p_f_type_1syl), test = c("hittner2003"))@hittner2003$p.value
# )
# #leave_one_out_accuracy = c(0.943743, 0.939386, 0.931907, 0.926054, 0.920721)
# gs1 = data.frame(par_names, round(correlations, 3), round(difference_significance, 3))
# names(gs1) = c("Parameters", "Correlations", "Difference significance")
# tab_df(gs1)
# 
# 
# 
# p_f_type_both = read.csv(paste(f_path, "p_f_type_1syl_k3_ID_both.csv", sep = ""), header = T)
# p_f_type_invar = read.csv(paste(f_path, "p_f_type_1syl_k3_ID_invar.csv", sep = ""), header = T)
# p_f_type_var = read.csv(paste(f_path, "p_f_type_1syl_k3_ID_var.csv", sep = ""), header = T)
# 
# c_both = gs_index[gs_index$file_n == "p_f_type_1syl_k3_ID_both.csv",]$cor
# c_invar = gs_index[gs_index$file_n == "p_f_type_1syl_k3_ID_invar.csv",]$cor
# c_var = gs_index[gs_index$file_n == "p_f_type_1syl_k3_ID_var.csv",]$cor
# 
# par_names = c("both", "invar", "var")
# correlations = c(c_both, c_invar, c_var)
# difference_significance = c(
#   NA, 
#   cocor.dep.groups.overlap(c_both, c_invar, cor(p_f_type_both$p_s, p_f_type_invar$p_s), nrow(p_f_type_both), test = c("hittner2003"))@hittner2003$p.value,
#   cocor.dep.groups.overlap(c_invar, c_var, cor(p_f_type_invar$p_s, p_f_type_var$p_s), nrow(p_f_type_both), test = c("hittner2003"))@hittner2003$p.value
# )
# #leave_one_out_accuracy = c(0.943743, 0.939386, 0.931907, 0.926054, 0.920721)
# gs2 = data.frame(par_names, round(correlations, 3), round(difference_significance, 3))
# names(gs2) = c("Parameters", "Correlations", "Difference significance")
# tab_df(gs2)
# 
# 
# 
# p_f_type_EN = read.csv(paste(f_path, "p_f_type_1syl_k3_ID_both_EN.csv", sep = ""), header = T)
# p_f_type = read.csv(paste(f_path, "p_f_type_1syl_k3_ID_both.csv", sep = ""), header = T)
# p_f_type_VERB = read.csv(paste(f_path, "p_f_type_1syl_k3_ID_both_VERB.csv", sep = ""), header = T)
# 
# c_EN = gs_index[gs_index$file_n == "p_f_type_1syl_k3_ID_both_EN.csv",]$cor
# c_NONE = gs_index[gs_index$file_n == "p_f_type_1syl_k3_ID_both.csv",]$cor
# c_VERB = gs_index[gs_index$file_n == "p_f_type_1syl_k3_ID_both_VERB.csv",]$cor
# 
# par_names = c("EN", "NONE", "VERB")
# correlations = c(c_EN, c_NONE, c_VERB)
# difference_significance = c(
#   NA, 
#   cocor.dep.groups.overlap(c_EN, c_NONE, cor(p_f_type_EN$p_s, p_f_type$p_s), nrow(p_f_type_EN), test = c("hittner2003"))@hittner2003$p.value,
#   cocor.dep.groups.overlap(c_NONE, c_VERB, cor(p_f_type$p_s, p_f_type_VERB$p_s), nrow(p_f_type_EN), test = c("hittner2003"))@hittner2003$p.value
# )
# #leave_one_out_accuracy = c(0.943743, 0.939386, 0.931907, 0.926054, 0.920721)
# gs3 = data.frame(par_names, round(correlations, 3), round(difference_significance, 3))
# names(gs3) = c("Parameters", "Correlations", "Difference significance")
# tab_df(gs3)
# 
#
# 
# p_f_type_1syl = read.csv(paste(f_path, "p_f_type_1syl_k1_Z_both_VERB.csv", sep = ""), header = T)
# p_f_type_1syl$s_prop = p_f_type_1syl$f_s/(p_f_type_1syl$f_s + p_f_type_1syl$f_en)
# p_f_type_1syl = p_f_type_1syl[p_f_type_1syl$s_prop > .05 & p_f_type_1syl$s_prop < .95,]
# p_f_type_2syl = read.csv(paste(f_path, "p_f_type_2syl_k1_Z_both_VERB.csv", sep = ""), header = T)
# p_f_type_2syl$s_prop = p_f_type_2syl$f_s/(p_f_type_2syl$f_s + p_f_type_2syl$f_en)
# p_f_type_2syl = p_f_type_2syl[p_f_type_2syl$s_prop > .05 & p_f_type_2syl$s_prop < .95,]
# p_f_type_3syl = read.csv(paste(f_path, "p_f_type_3syl_k1_Z_both_VERB.csv", sep = ""), header = T)
# p_f_type_3syl$s_prop = p_f_type_3syl$f_s/(p_f_type_3syl$f_s + p_f_type_3syl$f_en)
# p_f_type_3syl = p_f_type_3syl[p_f_type_3syl$s_prop > .05 & p_f_type_3syl$s_prop < .95,]
# p_f_type_4syl = read.csv(paste(f_path, "p_f_type_4syl_k1_Z_both_VERB.csv", sep = ""), header = T)
# p_f_type_4syl$s_prop = p_f_type_4syl$f_s/(p_f_type_4syl$f_s + p_f_type_4syl$f_en)
# p_f_type_4syl = p_f_type_4syl[p_f_type_4syl$s_prop > .05 & p_f_type_4syl$s_prop < .95,]
# 
# c_syl1 = gs_index[gs_index$file_n == "p_f_type_1syl_k1_Z_both_VERB.csv",]$cor_mid
# c_syl2 = gs_index[gs_index$file_n == "p_f_type_2syl_k1_Z_both_VERB.csv",]$cor_mid
# c_syl3 = gs_index[gs_index$file_n == "p_f_type_3syl_k1_Z_both_VERB.csv",]$cor_mid
# c_syl4 = gs_index[gs_index$file_n == "p_f_type_4syl_k1_Z_both_VERB.csv",]$cor_mid
# 
# par_names = c("1syl", "2syl", "3syl", "4syl")
# correlations = c(c_syl1, c_syl2, c_syl3, c_syl4)
# difference_significance = c(
#   NA, 
#   cocor.dep.groups.overlap(c_syl1, c_syl2, cor(p_f_type_1syl$p_s, p_f_type_2syl$p_s), nrow(p_f_type_1syl), test = c("hittner2003"))@hittner2003$p.value,
#   cocor.dep.groups.overlap(c_syl2, c_syl3, cor(p_f_type_2syl$p_s, p_f_type_3syl$p_s), nrow(p_f_type_1syl), test = c("hittner2003"))@hittner2003$p.value,
#   cocor.dep.groups.overlap(c_syl3, c_syl4, cor(p_f_type_3syl$p_s, p_f_type_4syl$p_s), nrow(p_f_type_1syl), test = c("hittner2003"))@hittner2003$p.value
# )
# #leave_one_out_accuracy = c(0.943743, 0.939386, 0.931907, 0.926054, 0.920721)
# gs1 = data.frame(par_names, round(correlations, 3), round(difference_significance, 3))
# names(gs1) = c("Parameters", "Correlations", "Difference significance")
# tab_df(gs1)
# 
# 
# p_f_type_both = read.csv(paste(f_path, "p_f_type_2syl_k1_Z_both.csv", sep = ""), header = T)
# p_f_type_both$s_prop = p_f_type_both$f_s/(p_f_type_both$f_s + p_f_type_both$f_en)
# p_f_type_both = p_f_type_both[p_f_type_both$s_prop > .05 & p_f_type_both$s_prop < .95,]
# p_f_type_invar = read.csv(paste(f_path, "p_f_type_2syl_k1_Z_invar.csv", sep = ""), header = T)
# p_f_type_invar$s_prop = p_f_type_invar$f_s/(p_f_type_invar$f_s + p_f_type_invar$f_en)
# p_f_type_invar = p_f_type_invar[p_f_type_invar$s_prop > .05 & p_f_type_invar$s_prop < .95,]
# p_f_type_var = read.csv(paste(f_path, "p_f_type_2syl_k1_Z_var.csv", sep = ""), header = T)
# p_f_type_var$s_prop = p_f_type_var$f_s/(p_f_type_var$f_s + p_f_type_var$f_en)
# p_f_type_var = p_f_type_var[p_f_type_var$s_prop > .05 & p_f_type_var$s_prop < .95,]
# 
# c_both = gs_index[gs_index$file_n == "p_f_type_2syl_k1_Z_both.csv",]$cor_mid
# c_invar = gs_index[gs_index$file_n == "p_f_type_2syl_k1_Z_invar.csv",]$cor_mid
# c_var = gs_index[gs_index$file_n == "p_f_type_2syl_k1_Z_var.csv",]$cor_mid
# 
# par_names = c("both", "invar", "var")
# correlations = c(c_both, c_invar, c_var)
# difference_significance = c(
#   NA, 
#   cocor.dep.groups.overlap(c_both, c_invar, cor(p_f_type_both$p_s, p_f_type_invar$p_s), nrow(p_f_type_both), test = c("hittner2003"))@hittner2003$p.value,
#   cocor.dep.groups.overlap(c_invar, c_var, cor(p_f_type_invar$p_s, p_f_type_var$p_s), nrow(p_f_type_both), test = c("hittner2003"))@hittner2003$p.value
# )
# #leave_one_out_accuracy = c(0.943743, 0.939386, 0.931907, 0.926054, 0.920721)
# gs1 = data.frame(par_names, round(correlations, 3), round(difference_significance, 3))
# names(gs1) = c("Parameters", "Correlations", "Difference significance")
# tab_df(gs1)
# 
# 
# p_f_type_verb = read.csv(paste(f_path, "p_f_type_2syl_k1_Z_both_VERB.csv", sep = ""), header = T)
# p_f_type_verb$s_prop = p_f_type_verb$f_s/(p_f_type_verb$f_s + p_f_type_verb$f_en)
# p_f_type_verb = p_f_type_verb[p_f_type_verb$s_prop > .05 & p_f_type_verb$s_prop < .95,]
# p_f_type = read.csv(paste(f_path, "p_f_type_2syl_k1_Z_both.csv", sep = ""), header = T)
# p_f_type$s_prop = p_f_type$f_s/(p_f_type$f_s + p_f_type$f_en)
# p_f_type = p_f_type[p_f_type$s_prop > .05 & p_f_type$s_prop < .95,]
# p_f_type_en = read.csv(paste(f_path, "p_f_type_2syl_k1_Z_both_EN.csv", sep = ""), header = T)
# p_f_type_en$s_prop = p_f_type_en$f_s/(p_f_type_en$f_s + p_f_type_en$f_en)
# p_f_type_en = p_f_type_en[p_f_type_en$s_prop > .05 & p_f_type_en$s_prop < .95,]
# 
# c_verb = gs_index[gs_index$file_n == "p_f_type_2syl_k1_Z_both_VERB.csv",]$cor_mid
# c_none = gs_index[gs_index$file_n == "p_f_type_2syl_k1_Z_both.csv",]$cor_mid
# c_en = gs_index[gs_index$file_n == "p_f_type_2syl_k1_Z_both_EN.csv",]$cor_mid
# 
# par_names = c("VERB", "NONE", "EN")
# correlations = c(c_verb, c_none, c_en)
# difference_significance = c(
#   NA, 
#   cocor.dep.groups.overlap(c_verb, c_none, cor(p_f_type_verb$p_s, p_f_type$p_s), nrow(p_f_type_verb), test = c("hittner2003"))@hittner2003$p.value,
#   cocor.dep.groups.overlap(c_none, c_en, cor(p_f_type$p_s, p_f_type_en$p_s), nrow(p_f_type_verb), test = c("hittner2003"))@hittner2003$p.value
# )
# #leave_one_out_accuracy = c(0.943743, 0.939386, 0.931907, 0.926054, 0.920721)
# gs1 = data.frame(par_names, round(correlations, 3), round(difference_significance, 3))
# names(gs1) = c("Parameters", "Correlations", "Difference significance")
# tab_df(gs1)
# 



invar = read.csv(paste(f_path2, "pl_type.master", sep = ""), header = F)
names(invar) = c("word", "right_compound_syls", "penult_onset", "penult_nucleus", "penult_coda", "final_onset", "final_nucleus", "final_coda", "penult_stress", "final_stress", "final_letter", "plural")
invar$final_coda = as.character(invar$final_coda)
invar$template = mapply(get_phon_template, as.character(invar$word), invar$right_compound_syls, invar$penult_nucleus, invar$final_onset, invar$final_nucleus, invar$final_coda, substr(invar$final_coda, nchar(invar$final_coda), nchar(invar$final_coda)), invar$final_stress)
invar$template = as.factor(invar$template)
invar$default = sapply(invar$template, get_default_suffix)

abc = table(invar$template, invar$plural)
templates = names(abc[,1])
en = abc[,1]
other = abc[,2]
s = abc[,3]
default = c("s", "en", "s", "en", "en", "en", "none", "s", "s", "en", "none", "s", "s")
table_order = c("obstruent", "diph", "long_vowel_son", "short_vowel_son", "ing", "front_vowel", "back_vowel", "schwa_son", "unstressed_vowel_son", "schwa", "tje", "stressed_vowel_son", "other")
temp_table = data.frame(default, en, other, s)
temp_table$percentage_incongruent = mapply(get_incongruency, temp_table$default, temp_table$en, temp_table$other, temp_table$s)
tab_df(temp_table[table_order,])

var = read.csv(paste(f_path, "p_f_type_O_merge_2syl_k4_ID_invar.csv", sep = ""))


# remove
# cent?,
# identiteit?, illegaal?,
# kantoorpik, kop, kwaad, kwaliteit, 
# laat, leven, penny,
# snee, varen?
#  wei?, zij?,

# exclude/fix client, big
# in Subtlex most singulars are tagged as 'vreemd' --> not a representative relative frequency
# in Subtlex some of 'orgies' are under 'georgies' lemma
# 5 grief is not included as singular for grieven/grieves
# 35 grieves in separate grieves lemma
# maybe exclude words that do not have -s as a variant
# 
#look at 'l'
# manually adjust final nucleus of 'enveloppe'
# god?
# kajak? kajakken is a verb
# cents, karaats, punts, kwadraats?
# nets, netten 2 verschillende lemma's?
# pop, portier 2 verschillende lemma's?
# post? verschillende lemma's --> ev frequentie niet betrouwbaar
# remove final letter, and all loanwords (like shilling, screentest) or keep loanwords and add all letters
# waters wateren verschillende lemmas? plus frequentie van enkelvoud is niet betrouwbaar

var$final_coda = as.character(var$final_coda)
var$template = mapply(get_phon_template, as.character(var$word), var$right_compound_syls, var$penult_nucleus, var$final_onset, var$final_nucleus, var$final_coda, substr(var$final_coda, nchar(var$final_coda), nchar(var$final_coda)), var$final_stress)
var$template = as.factor(var$template)
var$default = sapply(var$template, get_default_suffix)


tje = 0
names(tje) = c('tje')
temp_table$variable_types = c(table(var$template)[1:11], tje, table(var$template)[12])
temp_table$percentage_variable = mapply(get_variable_percentage, temp_table$variable_types, temp_table$en, temp_table$other, temp_table$s)
tab_df(temp_table[table_order,])

temp_table2 = temp_table
names(temp_table2) = c("default", "en", "other", "s", "% incongruent", "variable", "% variable")
temp_table2["% incongruent"] = round(temp_table2["% incongruent"], 2)
temp_table2["% variable"] = round(temp_table2["% variable"], 2)
tab_df(temp_table2[table_order,])

plot(temp_table[temp_table$default != "none",]$percentage_incongruent, temp_table[temp_table$default != "none",]$percentage_variable, type = "n", xlab = "Percentage incongruent", ylab = "Percentage variable")
text(temp_table[temp_table$default != "none",]$percentage_incongruent, temp_table[temp_table$default != "none",]$percentage_variable, row.names(temp_table[temp_table$default != "none",]))
cor(temp_table[temp_table$default != "none",]$percentage_incongruent, temp_table[temp_table$default != "none",]$percentage_variable)

var[var$word == "client",]$f_ev = 115
var[var$word == "big",]$f_ev = 801
var[var$word == "orgie",]$f_s = 16
var[var$word == "grieve",]$f_ev = 5
var[var$word == "grieve",]$f_s = 40
var = var[!(var$word %in% c("l", "pop", "portier", "net", "water", "god", "cent", "karaat", "punt", "post", "kajak", "grieve")),]
loanwords = c("")
var = var[var$f_s != 0,]
#var = var[var$f_other == 0,]
var$f_nons = var$f_en + var$f_other
var$s_prop = var$f_s/(var$f_s + var$f_en + var$f_other)
var$s_prop2 = (var$s_prop*(nrow(var)-1) + 0.5) / nrow(var)
var$log_f_mv = log(var$f_s + var$f_en + var$f_other)
# var$log_f_lem = log(var$f_s + var$f_en + var$f_other + var$f_ev)
var$mv_relfreq = (var$f_s + var$f_en + var$f_other) / (var$f_s + var$f_en + var$f_other + var$f_ev)
# var$plural_dominant = as.factor(var$mv_relfreq > 0.5)
# var$s_rel = var$f_s/(var$f_s + var$f_en + var$f_other)
# var$en_rel = var$f_en/(var$f_s + var$f_en + var$f_other)
# var$other_rel = var$f_other/(var$f_s + var$f_en + var$f_other)
# var$ev_rel = var$f_ev/(var$f_s + var$f_en + var$f_other)
# var$p_s2 = var$p_s
# var[var$p_s >= 1,]$p_s2 = 0.99999999999
# var[var$p_s <= 0,]$p_s2 = 0.00000000001
# var$p_en2 = var$p_en
# var[var$p_en >= 1,]$p_en2 = 0.99999999999
# var[var$p_en <= 0,]$p_en2 = 0.00000000001
# var$p_other2 = var$p_other
# var[var$p_other >= 1,]$p_other2 = 0.99999999999
# var[var$p_other <= 0,]$p_other2 = 0.00000000001
# get_entropy = function(s, en, other, pred_s, pred_en, pred_other) {
#   return(-(s*log2(pred_s) + en*log2(pred_en) + other*log2(pred_other)))
# }
# var$cross_entropy = mapply(get_entropy, var$s_rel, var$en_rel, var$other_rel, var$p_s2, var$p_en2, var$p_other2)
# var$log_cross_entropy = log(var$cross_entropy)


# perhaps use Baayen's rules to assign this variable
#var$deterministic = var$template %in% c("obstruent", "diph", "short_vowel_son", "ing", "front_vowel", "back_vowel", "schwa_son", "tje", "other")
#var$deterministic = as.factor(var$deterministic)

#hist(var[var$deterministic == "TRUE",]$p_s)
#hist(var[var$deterministic == "FALSE",]$p_s)

#plot(var$p_s, var$s_prop, type = "n")
#text(var$p_s, var$s_prop, var$word)
#c_k5 = cor(var$p_s, var$s_prop)
#c_k5

# collinearity check
corrplot(cor(var[, c("p_s", "mv_relfreq", "log_f_mv")], use = "complete.obs"), method = "number")
collin.fnc(var[, c("p_s", "mv_relfreq", "log_f_mv")])

var$s_prop2 = var$s_prop
var[var$s_prop >= 1,]$s_prop2 = 0.99999999999
var[var$s_prop <= 0,]$s_prop2 = 0.00000000001
prop2 = betareg(s_prop2 ~ p_s*mv_relfreq + p_s*log_f_mv, data = var)
prop = betareg(s_prop2 ~ p_s*mv_relfreq*log_f_mv, data = var)
summary(prop)
plot(effect("p_s:mv_relfreq:log_f_mv", prop))

lrtest(prop2, prop)

# louis: use logistic regression instead!
binom = glm(cbind(f_s, f_nons) ~ p_s * mv_relfreq + p_s * log_f_mv, family = "binomial", data = var)

quasibinom = glm(cbind(f_s, f_nons) ~ p_s * mv_relfreq + p_s * log_f_mv, family = "quasibinomial", data = var)


# however data is overdispersed, so we should beta-binomial instead!
betabin_prop = betabin(cbind(f_s, f_en) ~ p_s * mv_relfreq + p_s * log_f_mv, ~ 1, data = var, control = list(maxit=5000))
betabin_prop2 = betabin(cbind(f_s, f_en) ~ mv_relfreq + p_s * log_f_mv, ~ 1, data = var, control = list(maxit=5000))
anova(betabin_prop2, betabin_prop)

library(ggeffects)
plot(ggpredict(betabin_prop, c("p_s", "mv_relfreq [0.0004, 0.3, 0.5, 0.8, 1]", "log_f_mv [0.7, 3, 5, 7, 9]")))
plot(ggpredict(betabin_prop, c("p_s", "log_f_mv")))

# weird interaction effects in p_f_type_4syl_k3_Z_invar.csv, overfitting?
# try different package
library(VGAM)
betabin_vgam = vglm(cbind(f_s, f_en) ~ p_s * mv_relfreq * log_f_mv, betabinomial, var)
betabin_aod = betabin(cbind(f_s, f_en) ~ p_s * mv_relfreq * log_f_mv, ~ 1, data = var, control = list(maxit=5000))

summary(betabin_vgam)
summary(betabin_aod)

betabin_vgam = vglm(cbind(f_s, f_nons) ~ p_s * mv_relfreq + p_s * log_f_mv, betabinomial, var)

library(aods3)
betabin_aods3 = aodml(cbind(f_s, f_nons) ~ p_s * mv_relfreq + p_s * log_f_mv, family = "bb", data = var)
wald.test(b = coef(betabin_aods3), varb = vcov(betabin_aods3), Terms = 5)

library(dplyr)
library(ggplot2)

# following https://aosmith.rbind.io/2018/11/16/plot-fitted-lines/

var2 = var
var2$mv_relfreq = max(var$mv_relfreq)
var2$log_f_mv = median(var$log_f_mv)

betabin_pred = predict(betabin_vgam, se.fit = T, newdata = var2)
s_prop_pred_max = logitlink(betabin_pred$fitted.values[,1], inverse = T)
s_prop_lo_max = logitlink(betabin_pred$fitted.values[,1] - qnorm(.975) * betabin_pred$se.fit[,1], inverse = T)
s_prop_hi_max = logitlink(betabin_pred$fitted.values[,1] + qnorm(.975) * betabin_pred$se.fit[,1], inverse = T)

var2$mv_relfreq = median(var$mv_relfreq)

betabin_pred = predict(betabin_vgam, se.fit = T, newdata = var2)
s_prop_pred_med = logitlink(betabin_pred$fitted.values[,1], inverse = T)
s_prop_lo_med = logitlink(betabin_pred$fitted.values[,1] - qnorm(.975) * betabin_pred$se.fit[,1], inverse = T)
s_prop_hi_med = logitlink(betabin_pred$fitted.values[,1] + qnorm(.975) * betabin_pred$se.fit[,1], inverse = T)

var2$mv_relfreq = min(var$mv_relfreq)

betabin_pred = predict(betabin_vgam, se.fit = T, newdata = var2)
s_prop_pred_min = logitlink(betabin_pred$fitted.values[,1], inverse = T)
s_prop_lo_min = logitlink(betabin_pred$fitted.values[,1] - qnorm(.975) * betabin_pred$se.fit[,1], inverse = T)
s_prop_hi_min = logitlink(betabin_pred$fitted.values[,1] + qnorm(.975) * betabin_pred$se.fit[,1], inverse = T)


var_plot = cbind(var, s_prop_pred_max, s_prop_lo_max, s_prop_hi_max, s_prop_pred_med, s_prop_lo_med, s_prop_hi_med, s_prop_pred_min, s_prop_lo_min, s_prop_hi_min)
var_plot %>% ggplot() + 
  aes(x = p_s, y = s_prop) + 
  geom_point(color = "grey", alpha = .7) + 
  geom_line(aes(y=s_prop_pred_max, linetype = "Max")) +
  geom_ribbon( aes(ymin = s_prop_lo_max, ymax = s_prop_hi_max), alpha = .15) +
  geom_line(aes(y=s_prop_pred_med, linetype = "Median")) +
  geom_ribbon( aes(ymin = s_prop_lo_med, ymax = s_prop_hi_med), alpha = .15) +
  geom_line(aes(y=s_prop_pred_min, linetype = "Min")) +
  geom_ribbon( aes(ymin = s_prop_lo_min, ymax = s_prop_hi_min), alpha = .15) +
  labs(linetype='mv_relfreq') 


var2 = var
var2$mv_relfreq = median(var$mv_relfreq)
var2$log_f_mv = max(var$log_f_mv)

betabin_pred = predict(betabin_vgam, se.fit = T, newdata = var2)
s_prop_pred_max = logitlink(betabin_pred$fitted.values[,1], inverse = T)
s_prop_lo_max = logitlink(betabin_pred$fitted.values[,1] - qnorm(.975) * betabin_pred$se.fit[,1], inverse = T)
s_prop_hi_max = logitlink(betabin_pred$fitted.values[,1] + qnorm(.975) * betabin_pred$se.fit[,1], inverse = T)

var2$log_f_mv = median(var$log_f_mv)

betabin_pred = predict(betabin_vgam, se.fit = T, newdata = var2)
s_prop_pred_med = logitlink(betabin_pred$fitted.values[,1], inverse = T)
s_prop_lo_med = logitlink(betabin_pred$fitted.values[,1] - qnorm(.975) * betabin_pred$se.fit[,1], inverse = T)
s_prop_hi_med = logitlink(betabin_pred$fitted.values[,1] + qnorm(.975) * betabin_pred$se.fit[,1], inverse = T)

var2$log_f_mv = min(var$log_f_mv)

betabin_pred = predict(betabin_vgam, se.fit = T, newdata = var2)
s_prop_pred_min = logitlink(betabin_pred$fitted.values[,1], inverse = T)
s_prop_lo_min = logitlink(betabin_pred$fitted.values[,1] - qnorm(.975) * betabin_pred$se.fit[,1], inverse = T)
s_prop_hi_min = logitlink(betabin_pred$fitted.values[,1] + qnorm(.975) * betabin_pred$se.fit[,1], inverse = T)


var_plot = cbind(var, s_prop_pred_max, s_prop_lo_max, s_prop_hi_max, s_prop_pred_med, s_prop_lo_med, s_prop_hi_med, s_prop_pred_min, s_prop_lo_min, s_prop_hi_min)
var_plot %>% ggplot() + 
  aes(x = p_s, y = s_prop) + 
  geom_point(color = "grey", alpha = .7) + 
  geom_line(aes(y=s_prop_pred_max, linetype = "Max")) +
  geom_ribbon( aes(ymin = s_prop_lo_max, ymax = s_prop_hi_max), alpha = .15) +
  geom_line(aes(y=s_prop_pred_med, linetype = "Median")) +
  geom_ribbon( aes(ymin = s_prop_lo_med, ymax = s_prop_hi_med), alpha = .15) +
  geom_line(aes(y=s_prop_pred_min, linetype = "Min")) +
  geom_ribbon( aes(ymin = s_prop_lo_min, ymax = s_prop_hi_min), alpha = .15) +
  labs(linetype='log_f_mv') 



# check beta parameters of p_s
# https://stephens999.github.io/fiveMinuteStats/beta.html

library(betareg)
var$p_s_trans = (var$p_s*(nrow(var)-1) + 0.5) / nrow(var)
p_s_mod = betareg(p_s_trans ~ 1, data = var)
mu = plogis(p_s_mod$coefficients$mean)
phi = p_s_mod$coefficients$precision
alph = mu * phi
bet = (1 - mu) * phi

hist(var$p_s_trans, freq = F)
p = seq(0, 1, length=nrow(var))
lines(p, dbeta(p, alph, bet), type = "l")


# check half normal plot:
# https://www.rdocumentation.org/packages/hnp/versions/1.2-6/topics/hnp
library(hnp)
hnp(binom)
hnp(quasibinom)
hnp(betabin_aods3)
hnp(betabin_vgam)

# check qqplot
# https://intellinexus.wordpress.com/2010/11/29/creating-a-q-q-plot/
# https://dcgerard.github.io/updog/reference/betabinom.html

library(updog)
p <- ppoints(nrow(var))
pred_q = qbetabinom(p, weights(betabin_vgam, type = "prior"), logitlink(predictvglm(betabin_vgam)[,1], inverse = T), logitlink(predictvglm(betabin_vgam)[,2], inverse = T))
plot(log(var$f_s), log(pred_q))
abline(0,1)








var2 = var
var2$mv_relfreq = max(var$mv_relfreq)
var2$log_f_mv = median(var$log_f_mv)

betabin_pred = predict(betabin_aods3, se.fit = T, newdata = var2)
s_prop_pred_max = logitlink(betabin_pred$fit, inverse = T)
s_prop_lo_max = logitlink(betabin_pred$fit - qnorm(.975) * betabin_pred$se.fit, inverse = T)
s_prop_hi_max = logitlink(betabin_pred$fit + qnorm(.975) * betabin_pred$se.fit, inverse = T)

var2$mv_relfreq = median(var$mv_relfreq)

betabin_pred = predict(betabin_aods3, se.fit = T, newdata = var2)
s_prop_pred_med = logitlink(betabin_pred$fit, inverse = T)
s_prop_lo_med = logitlink(betabin_pred$fit - qnorm(.975) * betabin_pred$se.fit, inverse = T)
s_prop_hi_med = logitlink(betabin_pred$fit + qnorm(.975) * betabin_pred$se.fit, inverse = T)

var2$mv_relfreq = min(var$mv_relfreq)

betabin_pred = predict(betabin_aods3, se.fit = T, newdata = var2)
s_prop_pred_min = logitlink(betabin_pred$fit, inverse = T)
s_prop_lo_min = logitlink(betabin_pred$fit - qnorm(.975) * betabin_pred$se.fit, inverse = T)
s_prop_hi_min = logitlink(betabin_pred$fit + qnorm(.975) * betabin_pred$se.fit, inverse = T)


var_plot = cbind(var, s_prop_pred_max, s_prop_lo_max, s_prop_hi_max, s_prop_pred_med, s_prop_lo_med, s_prop_hi_med, s_prop_pred_min, s_prop_lo_min, s_prop_hi_min)
var_plot %>% ggplot() + 
  aes(x = p_s, y = s_prop) + 
  geom_point(color = "grey", alpha = .7) + 
  geom_line(aes(y=s_prop_pred_max, linetype = "Max")) +
  geom_ribbon( aes(ymin = s_prop_lo_max, ymax = s_prop_hi_max), alpha = .15) +
  geom_line(aes(y=s_prop_pred_med, linetype = "Median")) +
  geom_ribbon( aes(ymin = s_prop_lo_med, ymax = s_prop_hi_med), alpha = .15) +
  geom_line(aes(y=s_prop_pred_min, linetype = "Min")) +
  geom_ribbon( aes(ymin = s_prop_lo_min, ymax = s_prop_hi_min), alpha = .15) +
  labs(linetype='Proportion(PL)', x="Probability(-s)", y="Proportion(-s)") 


var2 = var
var2$mv_relfreq = median(var$mv_relfreq)
var2$log_f_mv = max(var$log_f_mv)

betabin_pred = predict(betabin_aods3, se.fit = T, newdata = var2)
s_prop_pred_max = logitlink(betabin_pred$fit, inverse = T)
s_prop_lo_max = logitlink(betabin_pred$fit - qnorm(.975) * betabin_pred$se.fit, inverse = T)
s_prop_hi_max = logitlink(betabin_pred$fit + qnorm(.975) * betabin_pred$se.fit, inverse = T)

var2$log_f_mv = median(var$log_f_mv)

betabin_pred = predict(betabin_aods3, se.fit = T, newdata = var2)
s_prop_pred_med = logitlink(betabin_pred$fit, inverse = T)
s_prop_lo_med = logitlink(betabin_pred$fit - qnorm(.975) * betabin_pred$se.fit, inverse = T)
s_prop_hi_med = logitlink(betabin_pred$fit + qnorm(.975) * betabin_pred$se.fit, inverse = T)

var2$log_f_mv = min(var$log_f_mv)

betabin_pred = predict(betabin_aods3, se.fit = T, newdata = var2)
s_prop_pred_min = logitlink(betabin_pred$fit, inverse = T)
s_prop_lo_min = logitlink(betabin_pred$fit - qnorm(.975) * betabin_pred$se.fit, inverse = T)
s_prop_hi_min = logitlink(betabin_pred$fit + qnorm(.975) * betabin_pred$se.fit, inverse = T)


var_plot = cbind(var, s_prop_pred_max, s_prop_lo_max, s_prop_hi_max, s_prop_pred_med, s_prop_lo_med, s_prop_hi_med, s_prop_pred_min, s_prop_lo_min, s_prop_hi_min)
var_plot %>% ggplot() + 
  aes(x = p_s, y = s_prop) + 
  geom_point(color = "grey", alpha = .7) + 
  geom_line(aes(y=s_prop_pred_max, linetype = "Max")) +
  geom_ribbon( aes(ymin = s_prop_lo_max, ymax = s_prop_hi_max), alpha = .15) +
  geom_line(aes(y=s_prop_pred_med, linetype = "Median")) +
  geom_ribbon( aes(ymin = s_prop_lo_med, ymax = s_prop_hi_med), alpha = .15) +
  geom_line(aes(y=s_prop_pred_min, linetype = "Min")) +
  geom_ribbon( aes(ymin = s_prop_lo_min, ymax = s_prop_hi_min), alpha = .15) +
  labs(linetype='log(freq(PL))', x="Probability(-s)", y="Proportion(-s)") 


# max log_f_mv predictions are not linear because of logit transformation, https://en.wikipedia.org/wiki/Logit
# which is bounded by 0 and 1
# but approaches linearity between 0.3 > s_prop < 0.7
# hence median and min look linear but max does not

