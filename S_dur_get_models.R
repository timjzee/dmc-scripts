library(lmerTest)
#library(afex)

if (Sys.info()[1] == "Darwin"){
  f_path = "/Volumes/tensusers/timzee/s_dur_models/"
  cgn_path = "/Volumes/tensusers/timzee/cgn/"
  ifadv_path = "/Volumes/tensusers/timzee/IFADVcorpus/"
  ecsd_path = "/Volumes/tensusers/timzee/ECSD/"
} else {
  f_path = "/vol/tensusers/timzee/s_dur_models/"
  cgn_path = "/vol/tensusers/timzee/cgn/"
  ifadv_path = "/vol/tensusers/timzee/IFADVcorpus/"
  ecsd_path = "/vol/tensusers/timzee/ECSD/"
}

print("Loading comp-a")
s_dur_a = read.csv(paste(cgn_path, "comp-a_s_static_final.csv", sep = ""))
s_dur_a$corpus = as.factor("cgn-a")
print("Loading comp-c")
s_dur_c = read.csv(paste(cgn_path, "comp-c_s_static_final.csv", sep = ""))
s_dur_c$corpus = as.factor("cgn-c")
s_dur_c$birth_year = as.integer(s_dur_c$birth_year)
print("Loading comp-d")
s_dur_d = read.csv(paste(cgn_path, "comp-d_s_static_final.csv", sep = ""))
s_dur_d$corpus = as.factor("cgn-d")
print("Loading ifadv")
s_dur_ifadv = read.csv(paste(ifadv_path, "ifadv_s_static_final.csv", sep = ""))
s_dur_ifadv$corpus = as.factor("ifadv")
s_dur_ifadv$mean_hnr = as.factor(s_dur_ifadv$mean_hnr)
levels(s_dur_ifadv$speaker_sex) = c("sex2", "sex1")
print("Loading ecsd")
s_dur_ecsd = read.csv(paste(ecsd_path, "ecsd_s_static_final.csv", sep = ""))
s_dur_ecsd$corpus = as.factor("ecsd")

print("Prepping data")

s_dur = rbind(s_dur_a, s_dur_c, s_dur_d, s_dur_ifadv, s_dur_ecsd)
s_dur$prev_mention = as.factor(s_dur$prev_mention)
s_dur$phrase_final = as.factor(s_dur$phrase_final)

s_dur$stressed = s_dur$num_syl == s_dur$word_stress
s_dur$stressed = as.factor(s_dur$stressed)
s_dur$stress_dist = s_dur$num_syl - s_dur$word_stress

is.na(s_dur$num_syl_pron) = !s_dur$num_syl_pron

# get rid of unnecessary columns
drop = c("ptan", "ptaf", "mean_hnr", "next_phon_dur", "prev_phon_dur", "birth_year", "speaker_sex", 
         "proportion_voiced", "proportion_voiced2", "s_cog_full", "s_cog_window", "per_mil_wf", 
         "word_class", "word_pos", "next_phon", "prev_phon", "sent_i", "word_sent_i", "word_chunk_i", 
         "chan")
s_dur = s_dur[ , !(names(s_dur) %in% drop)]

# Get rid of GEN-TIME
s_dur = s_dur[s_dur$type_of_s != "GEN-TIME",]
s_dur$type_of_s = as.factor(as.character(s_dur$type_of_s))
s_dur$type_of_s = relevel(s_dur$type_of_s, ref="S")

# don't create unnnecessary NAs
#is.na(s_dur$bigram_f) = !s_dur$bigram_f
s_dur$log_bigf = log10(s_dur$bigram_f + 1)


vowels = c("@", "A", "AU", "E", "E2", "EI", "EU", "I", "O", "U", "UI", "a", "e", "i", "o", "u", "y")
liquids = c("r", "l")
approximants = c("j", "w")
nasals = c("n", "m", "N")
fricatives = c("G", "S", "Z", "f", "h", "s", "v", "x", "z")
plosives = c("b", "d", "g", "k", "p", "t")

get_phon_class = function(x) {
  if (x %in% vowels){
    return("V")
  } else if (x %in% liquids){
    return("L")
  } else if (x %in% approximants){
    return("APP")
  } else if (x %in% nasals){
    return("N")
  } else if (x %in% fricatives){
    return("F")
  } else if (x %in% plosives){
    return("P")
  } else if (!is.na(x) & x == "SIL"){
    return("SIL")
  } else {
    return(NA)
  }
}

s_dur$next_phon_class = as.factor(sapply(s_dur$next_phon_pron, get_phon_class))
s_dur$prev_phon_class = as.factor(sapply(s_dur$prev_phon_pron, get_phon_class))

# see https://statisticalhorizons.com/multicollinearity
# V has most cases by far for the prev_phon_class
s_dur$prev_phon_class = relevel(s_dur$prev_phon_class, ref="V")
s_dur$next_phon_class = relevel(s_dur$next_phon_class, ref="V")

# remove lines for which measurements failed
s_dur = s_dur[!is.na(s_dur$s_dur), ]
# remove NA lines
s_dur = s_dur[rowSums(is.na(s_dur))<length(s_dur),]

# remove unrepresentative outliers (Baayen, 2008, p. 243)
s_dur = s_dur[s_dur$s_dur < 0.4,]

# make new predictors
n_cow = 5052213
n_subtlex = 437504
s_dur$wf = 10^s_dur$log_wf
s_dur[is.na(s_dur$wf),]$wf = 1
s_dur$log_wf = log10(s_dur$wf)
s_dur$p_next_w = (s_dur$bigram_f / n_cow) / (s_dur$wf / n_subtlex)

# don't create unnecessary NAs
s_dur[is.na(s_dur$lex_neb_freq),]$lex_neb_freq = 0
#s_dur$lex_neb_freq[s_dur$lex_neb_freq == 0] = NA
#n_lexicon = 251563
s_dur$prop_lex_neb_freq = s_dur$lex_neb_freq / s_dur$wf

s_dur$mean_syl_dur = s_dur$base_dur / s_dur$num_syl_pron

# transform dependent variable
s_dur$log_s_dur = log10(s_dur$s_dur)

# Let's scale & centre predictors to increase chance of convergence
# speech_rate_pron, mean_syl_dur, num_cons_pron, log_wf, prop_lex_neb_freq, p_next_w
#s_dur.cs = transform(s_dur, speech_rate_pron=scale(speech_rate_pron), 
#                     mean_syl_dur=scale(mean_syl_dur), num_cons_pron=scale(num_cons_pron), 
#                     log_wf=scale(log_wf), prop_lex_neb_freq=scale(prop_lex_neb_freq), 
#                     p_next_w=scale(p_next_w))

drop = c("lex_neb", "prev_phon_pron", "next_phon_pron", "lex_neb_freq", "bigram_f", "num_syl", 
         "word_stress", "base_dur", "num_syl_pron", "log_bigf", "prev_phon_class",
         "wf", "stressed")
s_dur = s_dur[ , !(names(s_dur) %in% drop)]

# Get rid of GEN-POSS for IFADV and ECSD
#s_dur = s_dur[!(s_dur$corpus %in% c("ecsd", "ifadv") & s_dur$type_of_s == "GEN-POSS"),]

########## FIT MODELS #########
print("Fitting complete models")
##INTERCEPTS ONLY
# intercepts = lmer(log_s_dur ~ speech_rate_pron + mean_syl_dur + num_cons_pron
#                    + log_wf + prop_lex_neb_freq + p_next_w
#                    + stress_dist + next_phon_class + prev_mention + phrase_final
#                    + type_of_s*corpus
#                    + (1 | speaker) + (1 | word_ort),
#                    control = lmerControl(optCtrl = list(maxfun = 1e6)),
#                    data=s_dur)

# saveRDS(intercepts, file = paste(f_path, "intercepts.rds", sep = ""))
# 
# s_dur2 = s_dur[abs(scale(resid(intercepts))) < 2.5,]
# 
# intercepts_tr = lmer(log_s_dur ~ speech_rate_pron + mean_syl_dur + num_cons_pron
#                    + log_wf + prop_lex_neb_freq + p_next_w
#                    + stress_dist + next_phon_class + prev_mention + phrase_final
#                    + type_of_s*corpus
#                    + (1 | speaker) + (1 | word_ort),
#                    control = lmerControl(optCtrl = list(maxfun = 1e6)),
#                    data=s_dur2)
# 
# saveRDS(intercepts_tr, file = paste(f_path, "intercepts_trimmed.rds", sep = ""))
# 
# # WITHOUT CORPUS INTERACTION
# intercepts2 = lmer(log_s_dur ~ speech_rate_pron + mean_syl_dur + num_cons_pron
#                    + log_wf + prop_lex_neb_freq + p_next_w
#                    + stress_dist + next_phon_class + prev_mention + phrase_final
#                    + type_of_s + corpus
#                    + (1 | speaker) + (1 | word_ort),
#                    control = lmerControl(optCtrl = list(maxfun = 1e6)),
#                    data=s_dur)
# 
# saveRDS(intercepts2, file = paste(f_path, "intercepts_nocorpus.rds", sep = ""))
# 
# # ZERO CORRELATION PARAMETERS
# 
# dummies = dummy(s_dur$type_of_s)
# type_of_sGEN_POSS = dummies[,"GEN-POSS"]
# type_of_sPART = dummies[,"PART"]
# type_of_sPL = dummies[,"PL"]
# 
# zcp = lmer(log_s_dur ~ speech_rate_pron + mean_syl_dur + num_cons_pron
#            + log_wf + prop_lex_neb_freq + p_next_w
#            + stress_dist + next_phon_class + prev_mention + phrase_final
#            + type_of_s*corpus
#            + (1 + type_of_sGEN_POSS + type_of_sPART + type_of_sPL || speaker)
#            + (1 | word_ort),
#            control = lmerControl(optCtrl = list(maxfun = 1e6)),
#            data=s_dur)
# 
# saveRDS(zcp, file = paste(f_path, "zcp.rds", sep = ""))
# 
# # WITHOUT CORPUS INTERACTION
# zcp2 = lmer(log_s_dur ~ speech_rate_pron + mean_syl_dur + num_cons_pron
#             + log_wf + prop_lex_neb_freq + p_next_w
#             + stress_dist + next_phon_class + prev_mention + phrase_final
#             + type_of_s + corpus
#             + (1 + type_of_sGEN_POSS + type_of_sPART + type_of_sPL || speaker)
#             + (1 | word_ort),
#             control = lmerControl(optCtrl = list(maxfun = 1e6)),
#             data=s_dur)
# 
# saveRDS(zcp2, file = paste(f_path, "zcp_nocorpus.rds", sep = ""))
# 
# # MAX MODEL
# max = lmer(log_s_dur ~ speech_rate_pron + mean_syl_dur + num_cons_pron
#            + log_wf + prop_lex_neb_freq + p_next_w
#            + stress_dist + next_phon_class + prev_mention + phrase_final
#            + type_of_s*corpus
#            + (1 + type_of_s | speaker) + (1 | word_ort),
#            control = lmerControl(optCtrl = list(maxfun = 1e6)),
#            data=s_dur)
# 
# saveRDS(max, file = paste(f_path, "max.rds", sep = ""))
# 
# max2 = lmer(log_s_dur ~ speech_rate_pron + mean_syl_dur + num_cons_pron
#             + log_wf + prop_lex_neb_freq + p_next_w
#             + stress_dist + next_phon_class + prev_mention + phrase_final
#             + type_of_s + corpus
#             + (1 + type_of_s | speaker) + (1 | word_ort),
#             control = lmerControl(optCtrl = list(maxfun = 1e6)),
#             data=s_dur)
# 
# saveRDS(max2, file = paste(f_path, "max_nocorpus.rds", sep = ""))
# 
# print("Making ANOVA tables")
# anova_table = anova(max, zcp, intercepts)
# saveRDS(anova_table, file = paste(f_path, "RE_anova_table.rds", sep = ""))
# 
# intercepts_corpus_table = anova(intercepts2, intercepts)
# saveRDS(intercepts_corpus_table, file = paste(f_path, "FE_intercepts_table.rds", sep = ""))
# 
# zcp_corpus_table = anova(zcp2, zcp)
# saveRDS(zcp_corpus_table, file = paste(f_path, "FE_zcp_table.rds", sep = ""))
# 
# max_corpus_table = anova(max2, max)
# saveRDS(max_corpus_table, file = paste(f_path, "FE_max_table.rds", sep = ""))

######### GET SEPARATE MODELS
print("Fitting separate models")

s_dur_a = s_dur[s_dur$corpus == "cgn-a",]
s_dur_c = s_dur[s_dur$corpus == "cgn-c",]
s_dur_d = s_dur[s_dur$corpus == "cgn-d",]
s_dur_ifadv = s_dur[s_dur$corpus == "ifadv",]
s_dur_ecsd = s_dur[s_dur$corpus == "ecsd",]
# 
# ## intercepts
# print("Intercept models")
# # CGN-A
# intercepts_a = lmer(log_s_dur ~ speech_rate_pron + mean_syl_dur + num_cons_pron
#                     + log_wf + prop_lex_neb_freq + p_next_w
#                     + stress_dist + next_phon_class + prev_mention + phrase_final
#                     + type_of_s
#                     + (1 | speaker) + (1 | word_ort),
#                     control = lmerControl(optCtrl = list(maxfun = 1e6)),
#                     data=s_dur_a)
# 
# saveRDS(intercepts_a, file = paste(f_path, "intercepts_a.rds", sep = ""))
# 
# intercepts_a_ctrl = lmer(log_s_dur ~ speech_rate_pron + mean_syl_dur + num_cons_pron
#                          + log_wf + prop_lex_neb_freq + p_next_w
#                          + stress_dist + next_phon_class + prev_mention + phrase_final
#                          + (1 | speaker) + (1 | word_ort),
#                          control = lmerControl(optCtrl = list(maxfun = 1e6)),
#                          data=s_dur_a)
# 
# saveRDS(intercepts_a_ctrl, file = paste(f_path, "intercepts_a_ctrl.rds", sep = ""))
# 
# # CGN-C
# intercepts_c = lmer(log_s_dur ~ speech_rate_pron + mean_syl_dur + num_cons_pron
#                     + log_wf + prop_lex_neb_freq + p_next_w
#                     + stress_dist + next_phon_class + prev_mention + phrase_final
#                     + type_of_s
#                     + (1 | speaker) + (1 | word_ort),
#                     control = lmerControl(optCtrl = list(maxfun = 1e6)),
#                     data=s_dur_c)
# 
# saveRDS(intercepts_c, file = paste(f_path, "intercepts_c.rds", sep = ""))
# 
# intercepts_c_ctrl = lmer(log_s_dur ~ speech_rate_pron + mean_syl_dur + num_cons_pron
#                          + log_wf + prop_lex_neb_freq + p_next_w
#                          + stress_dist + next_phon_class + prev_mention + phrase_final
#                          + (1 | speaker) + (1 | word_ort),
#                          control = lmerControl(optCtrl = list(maxfun = 1e6)),
#                          data=s_dur_c)
# 
# saveRDS(intercepts_c_ctrl, file = paste(f_path, "intercepts_c_ctrl.rds", sep = ""))
# 
# # CGN-D
# intercepts_d = lmer(log_s_dur ~ speech_rate_pron + mean_syl_dur + num_cons_pron
#                     + log_wf + prop_lex_neb_freq + p_next_w
#                     + stress_dist + next_phon_class + prev_mention + phrase_final
#                     + type_of_s
#                     + (1 | speaker) + (1 | word_ort),
#                     control = lmerControl(optCtrl = list(maxfun = 1e6)),
#                     data=s_dur_d)
# 
# saveRDS(intercepts_d, file = paste(f_path, "intercepts_d.rds", sep = ""))
# 
# intercepts_d_ctrl = lmer(log_s_dur ~ speech_rate_pron + mean_syl_dur + num_cons_pron
#                          + log_wf + prop_lex_neb_freq + p_next_w
#                          + stress_dist + next_phon_class + prev_mention + phrase_final
#                          + (1 | speaker) + (1 | word_ort),
#                          control = lmerControl(optCtrl = list(maxfun = 1e6)),
#                          data=s_dur_d)
# 
# saveRDS(intercepts_d_ctrl, file = paste(f_path, "intercepts_d_ctrl.rds", sep = ""))
# 
# # IFADV
# intercepts_ifadv = lmer(log_s_dur ~ speech_rate_pron + mean_syl_dur + num_cons_pron
#                         + log_wf + prop_lex_neb_freq + p_next_w
#                         + stress_dist + next_phon_class + prev_mention + phrase_final
#                         + type_of_s
#                         + (1 | speaker) + (1 | word_ort),
#                         control = lmerControl(optCtrl = list(maxfun = 1e6)),
#                         data=s_dur_ifadv)
# 
# saveRDS(intercepts_ifadv, file = paste(f_path, "intercepts_ifadv.rds", sep = ""))
# 
# intercepts_ifadv_ctrl = lmer(log_s_dur ~ speech_rate_pron + mean_syl_dur + num_cons_pron
#                              + log_wf + prop_lex_neb_freq + p_next_w
#                              + stress_dist + next_phon_class + prev_mention + phrase_final
#                              + (1 | speaker) + (1 | word_ort),
#                              control = lmerControl(optCtrl = list(maxfun = 1e6)),
#                              data=s_dur_ifadv)
# 
# saveRDS(intercepts_ifadv_ctrl, file = paste(f_path, "intercepts_ifadv_ctrl.rds", sep = ""))
# 
# # ECSD
# intercepts_ecsd = lmer(log_s_dur ~ speech_rate_pron + mean_syl_dur + num_cons_pron
#                        + log_wf + prop_lex_neb_freq + p_next_w
#                        + stress_dist + next_phon_class + prev_mention + phrase_final
#                        + type_of_s
#                        + (1 | speaker) + (1 | word_ort),
#                        control = lmerControl(optCtrl = list(maxfun = 1e6)),
#                        data=s_dur_ecsd)
# 
# saveRDS(intercepts_ecsd, file = paste(f_path, "intercepts_ecsd.rds", sep = ""))
# 
# intercepts_ecsd_ctrl = lmer(log_s_dur ~ speech_rate_pron + mean_syl_dur + num_cons_pron
#                             + log_wf + prop_lex_neb_freq + p_next_w
#                             + stress_dist + next_phon_class + prev_mention + phrase_final
#                             + (1 | speaker) + (1 | word_ort),
#                             control = lmerControl(optCtrl = list(maxfun = 1e6)),
#                             data=s_dur_ecsd)
# 
# saveRDS(intercepts_ecsd_ctrl, file = paste(f_path, "intercepts_ecsd_ctrl.rds", sep = ""))
# 
# ## zcp individual models
# print("zcp models")
# # CGN-A
# dummies = dummy(s_dur_a$type_of_s)
# type_of_sGEN_POSS = dummies[,"GEN-POSS"]
# type_of_sPART = dummies[,"PART"]
# type_of_sPL = dummies[,"PL"]
# 
# zcp_a = lmer(log_s_dur ~ speech_rate_pron + mean_syl_dur + num_cons_pron
#              + log_wf + prop_lex_neb_freq + p_next_w
#              + stress_dist + next_phon_class + prev_mention + phrase_final
#              + type_of_s
#              + (1 + type_of_sGEN_POSS + type_of_sPART + type_of_sPL || speaker)
#              + (1 | word_ort),
#              control = lmerControl(optCtrl = list(maxfun = 1e6)),
#              data=s_dur_a)
# 
# saveRDS(zcp_a, file = paste(f_path, "zcp_a.rds", sep = ""))
# 
# zcp_a_ctrl = lmer(log_s_dur ~ speech_rate_pron + mean_syl_dur + num_cons_pron
#                   + log_wf + prop_lex_neb_freq + p_next_w
#                   + stress_dist + next_phon_class + prev_mention + phrase_final
#                   + (1 + type_of_sGEN_POSS + type_of_sPART + type_of_sPL || speaker)
#                   + (1 | word_ort),
#                   control = lmerControl(optCtrl = list(maxfun = 1e6)),
#                   data=s_dur_a)
# 
# saveRDS(zcp_a_ctrl, file = paste(f_path, "zcp_a_ctrl.rds", sep = ""))
# 
# # CGN-C
# dummies = dummy(s_dur_c$type_of_s)
# type_of_sGEN_POSS = dummies[,"GEN-POSS"]
# type_of_sPART = dummies[,"PART"]
# type_of_sPL = dummies[,"PL"]
# 
# zcp_c = lmer(log_s_dur ~ speech_rate_pron + mean_syl_dur + num_cons_pron
#              + log_wf + prop_lex_neb_freq + p_next_w
#              + stress_dist + next_phon_class + prev_mention + phrase_final
#              + type_of_s
#              + (1 + type_of_sGEN_POSS + type_of_sPART + type_of_sPL || speaker)
#              + (1 | word_ort),
#              control = lmerControl(optCtrl = list(maxfun = 1e6)),
#              data=s_dur_c)
# 
# saveRDS(zcp_c, file = paste(f_path, "zcp_c.rds", sep = ""))
# 
# zcp_c_ctrl = lmer(log_s_dur ~ speech_rate_pron + mean_syl_dur + num_cons_pron
#                   + log_wf + prop_lex_neb_freq + p_next_w
#                   + stress_dist + next_phon_class + prev_mention + phrase_final
#                   + (1 + type_of_sGEN_POSS + type_of_sPART + type_of_sPL || speaker)
#                   + (1 | word_ort),
#                   control = lmerControl(optCtrl = list(maxfun = 1e6)),
#                   data=s_dur_c)
# 
# saveRDS(zcp_c_ctrl, file = paste(f_path, "zcp_c_ctrl.rds", sep = ""))
# 
# # CGN-D
# dummies = dummy(s_dur_d$type_of_s)
# type_of_sGEN_POSS = dummies[,"GEN-POSS"]
# type_of_sPART = dummies[,"PART"]
# type_of_sPL = dummies[,"PL"]
# 
# zcp_d = lmer(log_s_dur ~ speech_rate_pron + mean_syl_dur + num_cons_pron
#              + log_wf + prop_lex_neb_freq + p_next_w
#              + stress_dist + next_phon_class + prev_mention + phrase_final
#              + type_of_s
#              + (1 + type_of_sGEN_POSS + type_of_sPART + type_of_sPL || speaker)
#              + (1 | word_ort),
#              control = lmerControl(optCtrl = list(maxfun = 1e6)),
#              data=s_dur_d)
# 
# saveRDS(zcp_d, file = paste(f_path, "zcp_d.rds", sep = ""))
# 
# zcp_d_ctrl = lmer(log_s_dur ~ speech_rate_pron + mean_syl_dur + num_cons_pron
#                   + log_wf + prop_lex_neb_freq + p_next_w
#                   + stress_dist + next_phon_class + prev_mention + phrase_final
#                   + (1 + type_of_sGEN_POSS + type_of_sPART + type_of_sPL || speaker)
#                   + (1 | word_ort),
#                   control = lmerControl(optCtrl = list(maxfun = 1e6)),
#                   data=s_dur_d)
# 
# saveRDS(zcp_d_ctrl, file = paste(f_path, "zcp_d_ctrl.rds", sep = ""))
# 
# # IFADV
# dummies = dummy(s_dur_ifadv$type_of_s)
# type_of_sGEN_POSS = dummies[,"GEN-POSS"]
# type_of_sPART = dummies[,"PART"]
# type_of_sPL = dummies[,"PL"]
# 
# zcp_ifadv = lmer(log_s_dur ~ speech_rate_pron + mean_syl_dur + num_cons_pron
#                  + log_wf + prop_lex_neb_freq + p_next_w
#                  + stress_dist + next_phon_class + prev_mention + phrase_final
#                  + type_of_s
#                  + (1 + type_of_sGEN_POSS + type_of_sPART + type_of_sPL || speaker)
#                  + (1 | word_ort),
#                  control = lmerControl(optCtrl = list(maxfun = 1e6)),
#                  data=s_dur_ifadv)
# 
# saveRDS(zcp_ifadv, file = paste(f_path, "zcp_ifadv.rds", sep = ""))
# 
# zcp_ifadv_ctrl = lmer(log_s_dur ~ speech_rate_pron + mean_syl_dur + num_cons_pron
#                       + log_wf + prop_lex_neb_freq + p_next_w
#                       + stress_dist + next_phon_class + prev_mention + phrase_final
#                       + (1 + type_of_sGEN_POSS + type_of_sPART + type_of_sPL || speaker)
#                       + (1 | word_ort),
#                       control = lmerControl(optCtrl = list(maxfun = 1e6)),
#                       data=s_dur_ifadv)
# 
# saveRDS(zcp_ifadv_ctrl, file = paste(f_path, "zcp_ifadv_ctrl.rds", sep = ""))
# 
# # ECSD
# dummies = dummy(s_dur_ecsd$type_of_s)
# type_of_sGEN_POSS = dummies[,"GEN-POSS"]
# type_of_sPART = dummies[,"PART"]
# type_of_sPL = dummies[,"PL"]
# 
# zcp_ecsd = lmer(log_s_dur ~ speech_rate_pron + mean_syl_dur + num_cons_pron
#                 + log_wf + prop_lex_neb_freq + p_next_w
#                 + stress_dist + next_phon_class + prev_mention + phrase_final
#                 + type_of_s
#                 + (1 + type_of_sGEN_POSS + type_of_sPART + type_of_sPL || speaker)
#                 + (1 | word_ort),
#                 control = lmerControl(optCtrl = list(maxfun = 1e6)),
#                 data=s_dur_ecsd)
# 
# saveRDS(zcp_ecsd, file = paste(f_path, "zcp_ecsd.rds", sep = ""))
# 
# zcp_ecsd_ctrl = lmer(log_s_dur ~ speech_rate_pron + mean_syl_dur + num_cons_pron
#                      + log_wf + prop_lex_neb_freq + p_next_w
#                      + stress_dist + next_phon_class + prev_mention + phrase_final
#                      + (1 + type_of_sGEN_POSS + type_of_sPART + type_of_sPL || speaker)
#                      + (1 | word_ort),
#                      control = lmerControl(optCtrl = list(maxfun = 1e6)),
#                      data=s_dur_ecsd)
# 
# saveRDS(zcp_ecsd_ctrl, file = paste(f_path, "zcp_ecsd_ctrl.rds", sep = ""))

## max individual models
print("max models")
# # CGN-A
# 
# max_a = lmer(log_s_dur ~ speech_rate_pron + mean_syl_dur + num_cons_pron
#              + log_wf + prop_lex_neb_freq + p_next_w
#              + stress_dist + next_phon_class + prev_mention + phrase_final
#              + type_of_s
#              + (1 + type_of_s | speaker)
#              + (1 | word_ort),
#              control = lmerControl(optCtrl = list(maxfun = 1e6)),
#              data=s_dur_a)
# 
# saveRDS(max_a, file = paste(f_path, "max_a.rds", sep = ""))
# 
# max_a_ctrl = lmer(log_s_dur ~ speech_rate_pron + mean_syl_dur + num_cons_pron
#                   + log_wf + prop_lex_neb_freq + p_next_w
#                   + stress_dist + next_phon_class + prev_mention + phrase_final
#                   + (1 + type_of_s | speaker)
#                   + (1 | word_ort),
#                   control = lmerControl(optCtrl = list(maxfun = 1e6)),
#                   data=s_dur_a)
# 
# saveRDS(max_a_ctrl, file = paste(f_path, "max_a_ctrl.rds", sep = ""))
# 
# # CGN-C
# 
# max_c = lmer(log_s_dur ~ speech_rate_pron + mean_syl_dur + num_cons_pron
#              + log_wf + prop_lex_neb_freq + p_next_w
#              + stress_dist + next_phon_class + prev_mention + phrase_final
#              + type_of_s
#              + (1 + type_of_s | speaker)
#              + (1 | word_ort),
#              control = lmerControl(optCtrl = list(maxfun = 1e6)),
#              data=s_dur_c)
# 
# saveRDS(max_c, file = paste(f_path, "max_c.rds", sep = ""))
# 
# max_c_ctrl = lmer(log_s_dur ~ speech_rate_pron + mean_syl_dur + num_cons_pron
#                   + log_wf + prop_lex_neb_freq + p_next_w
#                   + stress_dist + next_phon_class + prev_mention + phrase_final
#                   + (1 + type_of_s | speaker)
#                   + (1 | word_ort),
#                   control = lmerControl(optCtrl = list(maxfun = 1e6)),
#                   data=s_dur_c)
# 
# saveRDS(max_c_ctrl, file = paste(f_path, "max_c_ctrl.rds", sep = ""))
# 
# # CGN-D
# 
# max_d = lmer(log_s_dur ~ speech_rate_pron + mean_syl_dur + num_cons_pron
#              + log_wf + prop_lex_neb_freq + p_next_w
#              + stress_dist + next_phon_class + prev_mention + phrase_final
#              + type_of_s
#              + (1 + type_of_s | speaker)
#              + (1 | word_ort),
#              control = lmerControl(optCtrl = list(maxfun = 1e6)),
#              data=s_dur_d)
# 
# saveRDS(max_d, file = paste(f_path, "max_d.rds", sep = ""))
# 
# max_d_ctrl = lmer(log_s_dur ~ speech_rate_pron + mean_syl_dur + num_cons_pron
#                   + log_wf + prop_lex_neb_freq + p_next_w
#                   + stress_dist + next_phon_class + prev_mention + phrase_final
#                   + (1 + type_of_s | speaker)
#                   + (1 | word_ort),
#                   control = lmerControl(optCtrl = list(maxfun = 1e6)),
#                   data=s_dur_d)
# 
# saveRDS(max_d_ctrl, file = paste(f_path, "max_d_ctrl.rds", sep = ""))
# 
# # IFADV
# 
# max_ifadv = lmer(log_s_dur ~ speech_rate_pron + mean_syl_dur + num_cons_pron
#                  + log_wf + prop_lex_neb_freq + p_next_w
#                  + stress_dist + next_phon_class + prev_mention + phrase_final
#                  + type_of_s
#                  + (1 + type_of_s | speaker)
#                  + (1 | word_ort),
#                  control = lmerControl(optCtrl = list(maxfun = 1e6)),
#                  data=s_dur_ifadv)
# 
# saveRDS(max_ifadv, file = paste(f_path, "max_ifadv.rds", sep = ""))
# 
# max_ifadv_ctrl = lmer(log_s_dur ~ speech_rate_pron + mean_syl_dur + num_cons_pron
#                       + log_wf + prop_lex_neb_freq + p_next_w
#                       + stress_dist + next_phon_class + prev_mention + phrase_final
#                       + (1 + type_of_s | speaker)
#                       + (1 | word_ort),
#                       control = lmerControl(optCtrl = list(maxfun = 1e6)),
#                       data=s_dur_ifadv)
# 
# saveRDS(max_ifadv_ctrl, file = paste(f_path, "max_ifadv_ctrl.rds", sep = ""))
# 
# # ECSD
# 
# max_ecsd = lmer(log_s_dur ~ speech_rate_pron + mean_syl_dur + num_cons_pron
#                 + log_wf + prop_lex_neb_freq + p_next_w
#                 + stress_dist + next_phon_class + prev_mention + phrase_final
#                 + type_of_s
#                 + (1 + type_of_s | speaker)
#                 + (1 | word_ort),
#                 control = lmerControl(optCtrl = list(maxfun = 1e6)),
#                 data=s_dur_ecsd)
# 
# saveRDS(max_ecsd, file = paste(f_path, "max_ecsd.rds", sep = ""))
# 
# max_ecsd_ctrl = lmer(log_s_dur ~ speech_rate_pron + mean_syl_dur + num_cons_pron
#                      + log_wf + prop_lex_neb_freq + p_next_w
#                      + stress_dist + next_phon_class + prev_mention + phrase_final
#                      + (1 + type_of_s | speaker)
#                      + (1 | word_ort),
#                      control = lmerControl(optCtrl = list(maxfun = 1e6)),
#                      data=s_dur_ecsd)
# 
# saveRDS(max_ecsd_ctrl, file = paste(f_path, "max_ecsd_ctrl.rds", sep = ""))

## relevelled versions of intercept model
# keeps GEN-POSS observations for ECSD and IFADV
s_dur$corpus = relevel(s_dur$corpus, ref="cgn-a")

intercepts_rel_a = lmer(log_s_dur ~ speech_rate_pron + mean_syl_dur + num_cons_pron
                        + log_wf + prop_lex_neb_freq + p_next_w
                        + stress_dist + next_phon_class + prev_mention + phrase_final
                        + type_of_s*corpus
                        + (1 | speaker) + (1 | word_ort),
                        control = lmerControl(optCtrl = list(maxfun = 1e6)),
                        data=s_dur)

saveRDS(intercepts_rel_a, file = paste(f_path, "intercepts_rel_a.rds", sep = ""))

s_dur$corpus = relevel(s_dur$corpus, ref="cgn-c")

intercepts_rel_c = lmer(log_s_dur ~ speech_rate_pron + mean_syl_dur + num_cons_pron
                        + log_wf + prop_lex_neb_freq + p_next_w
                        + stress_dist + next_phon_class + prev_mention + phrase_final
                        + type_of_s*corpus
                        + (1 | speaker) + (1 | word_ort),
                        control = lmerControl(optCtrl = list(maxfun = 1e6)),
                        data=s_dur)

saveRDS(intercepts_rel_c, file = paste(f_path, "intercepts_rel_c.rds", sep = ""))

s_dur$corpus = relevel(s_dur$corpus, ref="cgn-d")

intercepts_rel_d = lmer(log_s_dur ~ speech_rate_pron + mean_syl_dur + num_cons_pron
                        + log_wf + prop_lex_neb_freq + p_next_w
                        + stress_dist + next_phon_class + prev_mention + phrase_final
                        + type_of_s*corpus
                        + (1 | speaker) + (1 | word_ort),
                        control = lmerControl(optCtrl = list(maxfun = 1e6)),
                        data=s_dur)

saveRDS(intercepts_rel_d, file = paste(f_path, "intercepts_rel_d.rds", sep = ""))

s_dur$corpus = relevel(s_dur$corpus, ref="ifadv")

intercepts_rel_ifadv = lmer(log_s_dur ~ speech_rate_pron + mean_syl_dur + num_cons_pron
                            + log_wf + prop_lex_neb_freq + p_next_w
                            + stress_dist + next_phon_class + prev_mention + phrase_final
                            + type_of_s*corpus
                            + (1 | speaker) + (1 | word_ort),
                            control = lmerControl(optCtrl = list(maxfun = 1e6)),
                            data=s_dur)

saveRDS(intercepts_rel_ifadv, file = paste(f_path, "intercepts_rel_ifadv.rds", sep = ""))

s_dur$corpus = relevel(s_dur$corpus, ref="ecsd")

intercepts_rel_ecsd = lmer(log_s_dur ~ speech_rate_pron + mean_syl_dur + num_cons_pron
                           + log_wf + prop_lex_neb_freq + p_next_w
                           + stress_dist + next_phon_class + prev_mention + phrase_final
                           + type_of_s*corpus
                           + (1 | speaker) + (1 | word_ort),
                           control = lmerControl(optCtrl = list(maxfun = 1e6)),
                           data=s_dur)

saveRDS(intercepts_rel_ecsd, file = paste(f_path, "intercepts_rel_ecsd.rds", sep = ""))

