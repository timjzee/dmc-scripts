library(lmerTest)
library(effects)
library(languageR)
library(corrplot)
library(car)
library(rcompanion)
library(lsmeans)
#library(polycor)

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

get_NDLOutcome = function(x) {
  if (x == "S") {
    return("NONMORPH")
  } else if (x == "PL") {
    return("PL")
  } else if (x == "GEN-POSS") {
    return("GENPOSS")
  } else {
    return("PART")
  }
}

get_ActFromRemainingCues = function(cues_str, outcome) {
  cues = strsplit(cues_str, "_")
  sum_act = 0
  for (cue in cues[[1]]) {
    sum_act = sum_act + ifelse(cue %in% row.names(ndl.w) & outcome %in% colnames(ndl.w), ndl.w[cue, outcome], 0)
  }
#  sum_act = sum(sapply(cues, function(z) ndl.w[z, outcome]))
  return(sum_act)
}

get_ActDivFromRemainingCues = function(cues_str) {
  cues = strsplit(cues_str, "_")
  sum_act = 0
  for (cue in cues[[1]]){
    cue_present = cue %in% row.names(ndl.w)
    sum_act = sum_act + ifelse(cue_present, sum(ndl.w[cue,]), 0)
  }
#  sum_act = sum(sapply(cues, function(z) ndl.w[z,]))
  return(sum_act)
}

s_dur_a = read.csv(paste(cgn_path, "comp-a_s_ndl_static_final.csv", sep = ""))
s_dur_a$corpus = as.factor("cgn-a")
s_dur_a$register = as.factor("conversation")
s_dur_c = read.csv(paste(cgn_path, "comp-c_s_ndl_static_final.csv", sep = ""))
s_dur_c$corpus = as.factor("cgn-c")
s_dur_c$register = as.factor("conversation")
s_dur_c$birth_year = as.integer(s_dur_c$birth_year)
s_dur_d = read.csv(paste(cgn_path, "comp-d_s_ndl_static_final.csv", sep = ""))
s_dur_d$corpus = as.factor("cgn-d")
s_dur_d$register = as.factor("conversation")
s_dur_ifadv = read.csv(paste(ifadv_path, "ifadv_s_ndl_static_final.csv", sep = ""))
s_dur_ifadv$corpus = as.factor("ifadv")
s_dur_ifadv$register = as.factor("conversation")
s_dur_ifadv$mean_hnr = as.factor(s_dur_ifadv$mean_hnr)
levels(s_dur_ifadv$speaker_sex) = c("sex2", "sex1")
s_dur_ecsd = read.csv(paste(ecsd_path, "ecsd_s_ndl_static.csv", sep = ""))
s_dur_ecsd$corpus = as.factor("ecsd")
s_dur_ecsd$register = as.factor("conversation")

s_dur_k = read.csv(paste(cgn_path, "comp-k_s_ndl_static_final.csv", sep = ""))
s_dur_k$corpus = as.factor("cgn-k")
s_dur_k$register = as.factor("news")
s_dur_k$mean_hnr = as.factor(s_dur_k$mean_hnr)
s_dur_o = read.csv(paste(cgn_path, "comp-o_s_ndl_static_final.csv", sep = ""))
s_dur_o$corpus = as.factor("cgn-o")
s_dur_o$register = as.factor("stories")
s_dur_o$mean_hnr = as.factor(s_dur_o$mean_hnr)

s_dur = rbind(s_dur_a, s_dur_c, s_dur_d, s_dur_ifadv, s_dur_ecsd, s_dur_o, s_dur_k)
s_dur$prev_mention = as.factor(s_dur$prev_mention)
s_dur$phrase_final = as.factor(s_dur$phrase_final)


# inspect numbers:
table(s_dur_a$type_of_s)
table(s_dur_c$type_of_s)
table(s_dur_d$type_of_s)
table(s_dur_ifadv$type_of_s)
table(s_dur_ecsd$type_of_s)
table(s_dur_k$type_of_s)
table(s_dur_o$type_of_s)


s_dur$stressed = s_dur$num_syl == s_dur$word_stress
s_dur$stressed = as.factor(s_dur$stressed)
s_dur$stress_dist = s_dur$num_syl - s_dur$word_stress
# fix mistake from add_info_script
#s_dur$stress_dist[s_dur$stress_dist < 0] = NA
#s_dur$stressed[s_dur$stress_dist < 0] = NA
#s_dur$num_syl[s_dur$stress_dist < 0] = NA
is.na(s_dur$num_syl_pron) = !s_dur$num_syl_pron
# combine ifadv and ecsd to get rid of rank deficiency
#levels(s_dur$corpus) = c("cgn-a", "cgn-c", "cgn-d", "ifadv-ecsd", "ifadv-ecsd")

# get rid of unnecessary columns
drop = c("ptan", "ptaf", "mean_hnr", "next_phon_dur", "prev_phon_dur", "birth_year", "speaker_sex", 
         "proportion_voiced2", "per_mil_wf", 
         "word_class", "word_pos", "next_phon", "prev_phon", "sent_i", "word_sent_i", "word_chunk_i", 
         "chan")
s_dur = s_dur[ , !(names(s_dur) %in% drop)]

# Get rid of GEN-TIME
s_dur = s_dur[s_dur$type_of_s != "GEN-TIME",]
# s_dur = s_dur[s_dur$type_of_s %in% c("S", "PL"),]
s_dur$type_of_s = as.factor(as.character(s_dur$type_of_s))
s_dur$type_of_s = relevel(s_dur$type_of_s, ref="S")

# get ndl measures
ndl.w = readRDS(paste(cgn_path, "ifadv_ndl_weights.rds", sep = ""))
s_dur$ndl_outcome = sapply(as.character(s_dur$type_of_s), get_NDLOutcome)
s_dur$priorMorph = sapply(s_dur$ndl_outcome, function(x) sum(ndl.w[,x]))
s_dur$ActFromBoundaryDiphone = mapply(function(x, y) ifelse(x %in% row.names(ndl.w) & y %in% colnames(ndl.w)
                                                            , ndl.w[x, y], 0), 
                                      as.character(s_dur$ndl_boundary_diph), 
                                      s_dur$ndl_outcome)
s_dur$ActFromRemainingCues = mapply(get_ActFromRemainingCues,
                                    as.character(s_dur$other_ndl_cues),
                                    s_dur$ndl_outcome)
s_dur$ActDivFromBoundaryDiphone = sapply(as.character(s_dur$ndl_boundary_diph),
                                         function(x) ifelse(x %in% row.names(ndl.w), sum(ndl.w[x,]), 0))
s_dur$ActDivFromRemainingCues = sapply(as.character(s_dur$other_ndl_cues),
                                       get_ActDivFromRemainingCues)

s_dur$ActFromCues = s_dur$ActDivFromBoundaryDiphone + s_dur$ActFromRemainingCues
s_dur$ActDivFromCues = s_dur$ActDivFromBoundaryDiphone + s_dur$ActDivFromRemainingCues


# inspect ndl matrix
library(sjPlot)
tab_df(as.data.frame(ndl.w[c(".iets.", "EUk", "ks", "d@"), c(".leuks.", ".bos.", "PART", "PL")]))


# don't turn 0 into NA
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
s_dur = s_dur[!(is.na(s_dur$s_dur) | is.na(s_dur$s_cog_window)), ]
# remove NA lines
s_dur = s_dur[rowSums(is.na(s_dur))<length(s_dur),]

# inspect histogram
hist(s_dur$s_dur)
hist(s_dur$s_cog_window)
#hist(s_dur[s_dur$corpus == "ifadv",]$s_cog_window, xlim = c(0,4000))
#hist(s_dur[s_dur$corpus == "cgn-c",]$s_cog_window, xlim = c(0,4000))
#hist(s_dur[s_dur$corpus == "cgn-d",]$s_cog_window, xlim = c(0,4000))
#hist(s_dur[s_dur$corpus == "cgn-a",]$s_cog_window, xlim = c(0,4000))
hist(s_dur$proportion_voiced, breaks = 50)

# remove unrepresentative outliers (Baayen, 2008, p. 243)
s_dur = s_dur[s_dur$s_dur < 0.4,]

# make new predictors and get rid of unnecessary NAs
#n_cow = 5052213
#n_subtlex = 437504
#s_dur$wf = 10^s_dur$log_wf
#s_dur[is.na(s_dur$cow_wf),]$cow_wf = 1
s_dur[s_dur$cow_wf == 0,]$cow_wf = 1
s_dur$log_wf = log10(s_dur$cow_wf)
#s_dur$p_next_w = (s_dur$bigram_f / n_cow) / (s_dur$wf / n_subtlex)

s_dur[is.na(s_dur$num_syl_pron),]$num_syl_pron = 0

s_dur[is.na(s_dur$lex_neb),]$lex_neb = 0
s_dur[is.na(s_dur$lex_neb_freq),]$lex_neb_freq = 0
#n_lexicon = 251563
#s_dur$prop_lex_neb_freq = s_dur$lex_neb_freq / s_dur$wf

#s_dur$mean_syl_dur = s_dur$base_dur / s_dur$num_syl_pron

# transform dependent variable
s_dur$log_s_dur = log10(s_dur$s_dur)


# remove lines for which continuous predictors are NA
nrow(s_dur)
s_dur = s_dur[!(is.na(s_dur$speech_rate_pron) | is.na(s_dur$base_dur) 
                | is.na(s_dur$num_syl_pron) | is.na(s_dur$num_cons_pron)
                | is.na(s_dur$log_wf) | is.na(s_dur$lex_neb) | is.na(s_dur$log_bigf)
                | is.na(s_dur$stress_dist)), ]
nrow(s_dur)

# Inspect collinearity

continuous = c("speech_rate_pron", "base_dur", "num_syl_pron", 
               "num_cons_pron", "log_wf", "lex_neb", "log_bigf", "stress_dist")

corrplot(cor(s_dur[, continuous], use = "complete.obs"), method = "number")
vif(lm(log_s_dur ~ speech_rate_pron + base_dur + num_syl_pron 
       + num_cons_pron + log_wf + lex_neb + log_bigf , data=s_dur))
collin.fnc(na.omit(s_dur[, continuous]))$cnumber

############
# principle components
col_pred = s_dur[, c("speech_rate_pron", "base_dur", "num_syl_pron", "num_cons_pron", "log_wf", "lex_neb", "log_bigf", "stress_dist")]
col_pred_pca = prcomp(col_pred, center = T, scale. = T)
summary(col_pred_pca)

rotation_df = as.data.frame(col_pred_pca$rotation)
rotation_df_ordered = rotation_df[order(-abs(rotation_df$PC1)),]
structure(rotation_df_ordered[, "PC1"],
          names=rownames(rotation_df_ordered))

s_dur$PC1 = col_pred_pca$x[,1]
s_dur$PC2 = col_pred_pca$x[,2]
s_dur$PC3 = col_pred_pca$x[,3]
s_dur$PC4 = col_pred_pca$x[,4]
s_dur$PC5 = col_pred_pca$x[,5]
s_dur$PC6 = col_pred_pca$x[,6]

continuous = c("PC1", "PC2", "PC3", "PC4", "PC5", "PC6")
corrplot(cor(s_dur[, continuous], use = "complete.obs"), method = "number")

categorical = c("type_of_s", "register", "next_phon_class", 
                "prev_mention", "phrase_final")

cat_ass = matrix(c(cramerV(table(s_dur[,c("type_of_s", "type_of_s")]), bias.correct = TRUE),
                   cramerV(table(s_dur[,c("type_of_s", "register")]), bias.correct = TRUE),
                   cramerV(table(s_dur[,c("type_of_s", "next_phon_class")]), bias.correct = TRUE),
                   cramerV(table(s_dur[,c("type_of_s", "prev_mention")]), bias.correct = TRUE),
                   cramerV(table(s_dur[,c("type_of_s", "phrase_final")]), bias.correct = TRUE),
                   cramerV(table(s_dur[,c("register", "type_of_s")]), bias.correct = TRUE),
                   cramerV(table(s_dur[,c("register", "register")]), bias.correct = TRUE),
                   cramerV(table(s_dur[,c("register", "next_phon_class")]), bias.correct = TRUE),
                   cramerV(table(s_dur[,c("register", "prev_mention")]), bias.correct = TRUE),
                   cramerV(table(s_dur[,c("register", "phrase_final")]), bias.correct = TRUE),
                   cramerV(table(s_dur[,c("next_phon_class", "type_of_s")]), bias.correct = TRUE),
                   cramerV(table(s_dur[,c("next_phon_class", "register")]), bias.correct = TRUE),
                   cramerV(table(s_dur[,c("next_phon_class", "next_phon_class")]), bias.correct = TRUE),
                   cramerV(table(s_dur[,c("next_phon_class", "prev_mention")]), bias.correct = TRUE),
                   cramerV(table(s_dur[,c("next_phon_class", "phrase_final")]), bias.correct = TRUE),
                   cramerV(table(s_dur[,c("prev_mention", "type_of_s")]), bias.correct = TRUE),
                   cramerV(table(s_dur[,c("prev_mention", "register")]), bias.correct = TRUE),
                   cramerV(table(s_dur[,c("prev_mention", "next_phon_class")]), bias.correct = TRUE),
                   cramerV(table(s_dur[,c("prev_mention", "prev_mention")]), bias.correct = TRUE),
                   cramerV(table(s_dur[,c("prev_mention", "phrase_final")]), bias.correct = TRUE),
                   cramerV(table(s_dur[,c("phrase_final", "type_of_s")]), bias.correct = TRUE),
                   cramerV(table(s_dur[,c("phrase_final", "register")]), bias.correct = TRUE),
                   cramerV(table(s_dur[,c("phrase_final", "next_phon_class")]), bias.correct = TRUE),
                   cramerV(table(s_dur[,c("phrase_final", "prev_mention")]), bias.correct = TRUE),
                   cramerV(table(s_dur[,c("phrase_final", "phrase_final")]), bias.correct = TRUE)),
                 nrow = 5, ncol = 5, byrow = T, dimnames = list(
                   categorical,
                   categorical))

corrplot(cat_ass, method = "number")

cat_con = matrix(c(sqrt(summary(lm(PC1 ~ type_of_s, data = s_dur))$r.squared),
                   sqrt(summary(lm(PC2 ~ type_of_s, data = s_dur))$r.squared),
                   sqrt(summary(lm(PC3 ~ type_of_s, data = s_dur))$r.squared),
                   sqrt(summary(lm(PC4 ~ type_of_s, data = s_dur))$r.squared),
#                   sqrt(summary(lm(PC5 ~ type_of_s, data = s_dur))$r.squared),
                   sqrt(summary(lm(PC1 ~ register, data = s_dur))$r.squared),
                   sqrt(summary(lm(PC2 ~ register, data = s_dur))$r.squared),
                   sqrt(summary(lm(PC3 ~ register, data = s_dur))$r.squared),
                   sqrt(summary(lm(PC4 ~ register, data = s_dur))$r.squared),
#                   sqrt(summary(lm(PC5 ~ register, data = s_dur))$r.squared),
                   sqrt(summary(lm(PC1 ~ next_phon_class, data = s_dur))$r.squared),
                   sqrt(summary(lm(PC2 ~ next_phon_class, data = s_dur))$r.squared),
                   sqrt(summary(lm(PC3 ~ next_phon_class, data = s_dur))$r.squared),
                   sqrt(summary(lm(PC4 ~ next_phon_class, data = s_dur))$r.squared),
#                   sqrt(summary(lm(PC5 ~ next_phon_class, data = s_dur))$r.squared),
                   sqrt(summary(lm(PC1 ~ prev_mention, data = s_dur))$r.squared),
                   sqrt(summary(lm(PC2 ~ prev_mention, data = s_dur))$r.squared),
                   sqrt(summary(lm(PC3 ~ prev_mention, data = s_dur))$r.squared),
                   sqrt(summary(lm(PC4 ~ prev_mention, data = s_dur))$r.squared),
#                   sqrt(summary(lm(PC5 ~ prev_mention, data = s_dur))$r.squared),
                   sqrt(summary(lm(PC1 ~ phrase_final, data = s_dur))$r.squared),
                   sqrt(summary(lm(PC2 ~ phrase_final, data = s_dur))$r.squared),
                   sqrt(summary(lm(PC3 ~ phrase_final, data = s_dur))$r.squared),
                   sqrt(summary(lm(PC4 ~ phrase_final, data = s_dur))$r.squared)
#                   sqrt(summary(lm(PC5 ~ phrase_final, data = s_dur))$r.squared)
), 
nrow = 5, ncol = 4, byrow = T, dimnames = list(
  categorical,
  continuous))

corrplot(cat_con, method = "number")

### try with this correlation first
s_dur = na.omit(s_dur)

intercepts = lmer(log_s_dur ~ PC1 + PC2 + PC3 + PC4 
#                  + PC5
                  + next_phon_class + prev_mention 
#                  + phrase_final
                  + type_of_s*register
                  + (1 | speaker) + (1 | word_ort),
                  control = lmerControl(optCtrl = list(maxfun = 1e6)),
                  data=s_dur)

intercepts2 = lmer(log_s_dur ~ PC1 + PC2 + PC3 + PC4 
#                   + PC5
                   + next_phon_class + prev_mention 
#                   + phrase_final
                   + type_of_s + register
                   + (1 | speaker) + (1 | word_ort),
                   control = lmerControl(optCtrl = list(maxfun = 1e6)),
                   data=s_dur)

anova(intercepts, intercepts2)

plot(effect("type_of_s:register", intercepts, x.var = "type_of_s"), multiline = F, 
     layout = c(3,1), alternating = F, rotx = 45, ci.style = "bars", main = "")

intercepts_conv = lmer(log_s_dur ~ PC1 + PC2 + PC3 + PC4 
#                       + PC5
                  + next_phon_class + prev_mention 
#                  + phrase_final
                  + type_of_s
                  + (1 | speaker) + (1 | word_ort),
                  control = lmerControl(optCtrl = list(maxfun = 1e6)),
                  data=s_dur[s_dur$register == "conversation",])

lsmeans(intercepts_conv, "pairwise"~type_of_s)

intercepts_news = lmer(log_s_dur ~ PC1 + PC2 + PC3 + PC4 
#                       + PC5
                       + next_phon_class + prev_mention 
#                       + phrase_final
                       + type_of_s
                       + (1 | speaker) + (1 | word_ort),
                       control = lmerControl(optCtrl = list(maxfun = 1e6)),
                       data=s_dur[s_dur$register == "news",])

lsmeans(intercepts_news, "pairwise"~type_of_s)

intercepts_stor = lmer(log_s_dur ~ PC1 + PC2 + PC3 + PC4 
#                       + PC5
                       + next_phon_class + prev_mention 
#                       + phrase_final
                       + type_of_s
                       + (1 | speaker) + (1 | word_ort),
                       control = lmerControl(optCtrl = list(maxfun = 1e6)),
                       data=s_dur[s_dur$register == "stories",])

lsmeans(intercepts_stor, "pairwise"~type_of_s)

## try resideuals method
#s_dur$residPC1 = resid(lm(PC1 ~ type_of_s, data = s_dur))

#intercepts_res = lmer(log_s_dur ~ residPC1 + PC2 + PC3 + PC4 + PC5
#                  + next_phon_class + prev_mention + phrase_final
#                  + type_of_s*register
#                  + (1 | speaker) + (1 | word_ort),
#                  control = lmerControl(optCtrl = list(maxfun = 1e6)),
#                  data=s_dur)

#intercepts_res2 = lmer(log_s_dur ~ residPC1 + PC2 + PC3 + PC4 + PC5
#                   + next_phon_class + prev_mention + phrase_final
#                   + type_of_s + register
#                   + (1 | speaker) + (1 | word_ort),
#                   control = lmerControl(optCtrl = list(maxfun = 1e6)),
#                   data=s_dur)

#summary(intercepts2)
#summary(intercepts_res2)
# this actually boosts type_of_s, PC1 stays the same


### try Mirjam's residuals method

control = lmer(log_s_dur ~ PC1 + PC2 + PC3 + PC4 
#               + PC5
                  + next_phon_class + prev_mention 
#               + phrase_final
                  + (1 | speaker) + (1 | word_ort),
                  control = lmerControl(optCtrl = list(maxfun = 1e6)),
                  data=s_dur)

tab_model(control, show.ci = F, show.se = T, digits = 3)

s_dur$dur_resid = resid(control)

interest = lm(dur_resid ~ type_of_s*register,
                data=s_dur)

#interest2 = lm(dur_resid ~ type_of_s + register,
#               data=s_dur)

#anova(interest, interest2)
anova(interest)
tab_df(anova(interest))

plot(effect("type_of_s:register", interest, x.var = "type_of_s"), multiline = F, 
     layout = c(3,1), alternating = F, rotx = 45, ci.style = "bars", main = "")

interest_conv = lm(dur_resid ~ type_of_s,
                   data=s_dur[s_dur$register == "conversation",])

anova(interest_conv)
lsmeans(interest_conv, "pairwise"~type_of_s)

interest_news = lm(dur_resid ~ type_of_s,
                   data=s_dur[s_dur$register == "news",])

anova(interest_news)
lsmeans(interest_news, "pairwise"~type_of_s)

interest_stor = lm(dur_resid ~ type_of_s,
                   data=s_dur[s_dur$register == "stories",])

anova(interest_stor)
lsmeans(interest_stor, "pairwise"~type_of_s)


### try limitting type/token ratio to get rid of collinearity with type_of_s
table(s_dur$type_of_s, s_dur$register)

type_token_ratio = 25

set.seed(40)
s_dur$word_ort = as.factor(as.character(s_dur$word_ort))

# should we distinguish between partitive 'anders' and other 'anders' 

s_dur_conv = s_dur[s_dur$register == "conversation",]
word_t_conv = table(s_dur_conv$word_ort)
s_dur_conv$ort_freq = as.integer(word_t_conv[s_dur_conv$word_ort])
hist(s_dur_conv$ort_freq, breaks = 500, xlim = c(0,5000))
s_dur_freq_conv = s_dur_conv[s_dur_conv$ort_freq > type_token_ratio,]
s_dur_freq_conv_samp = s_dur_freq_conv[unlist(tapply(1:nrow(s_dur_freq_conv), s_dur_freq_conv$word_ort, sample, type_token_ratio)),]
s_dur_lim_conv = rbind(s_dur_freq_conv_samp, s_dur_conv[s_dur_conv$ort_freq <= type_token_ratio,])

s_dur_stor = s_dur[s_dur$register == "stories",]
word_t_stor = table(s_dur_stor$word_ort)
s_dur_stor$ort_freq = as.integer(word_t_stor[s_dur_stor$word_ort])
hist(s_dur_stor$ort_freq, breaks = 500, xlim = c(0,5000))
s_dur_freq_stor = s_dur_stor[s_dur_stor$ort_freq > type_token_ratio,]
s_dur_freq_stor_samp = s_dur_freq_stor[unlist(tapply(1:nrow(s_dur_freq_stor), s_dur_freq_stor$word_ort, sample, type_token_ratio)),]
s_dur_lim_stor = rbind(s_dur_freq_stor_samp, s_dur_stor[s_dur_stor$ort_freq <= type_token_ratio,])

s_dur_news = s_dur[s_dur$register == "news",]
word_t_news = table(s_dur_news$word_ort)
s_dur_news$ort_freq = as.integer(word_t_news[s_dur_news$word_ort])
hist(s_dur_news$ort_freq, breaks = 500, xlim = c(0,5000))
s_dur_freq_news = s_dur_news[s_dur_news$ort_freq > type_token_ratio,]
s_dur_freq_news_samp = s_dur_freq_news[unlist(tapply(1:nrow(s_dur_freq_news), s_dur_freq_news$word_ort, sample, type_token_ratio)),]
s_dur_lim_news = rbind(s_dur_freq_news_samp, s_dur_news[s_dur_news$ort_freq <= type_token_ratio,])

s_dur_lim = rbind(s_dur_lim_conv, s_dur_lim_stor, s_dur_lim_news)

table(s_dur_lim$type_of_s, s_dur_lim$register)

cat_con = matrix(c(sqrt(summary(lm(PC5 ~ type_of_s, data = s_dur_lim))$r.squared),
                   sqrt(summary(lm(PC1 ~ type_of_s, data = s_dur_lim))$r.squared),
                   sqrt(summary(lm(PC2 ~ type_of_s, data = s_dur_lim))$r.squared),
                   sqrt(summary(lm(PC3 ~ type_of_s, data = s_dur_lim))$r.squared),
                   sqrt(summary(lm(PC4 ~ type_of_s, data = s_dur_lim))$r.squared),
                   sqrt(summary(lm(PC5 ~ register, data = s_dur_lim))$r.squared),
                   sqrt(summary(lm(PC1 ~ register, data = s_dur_lim))$r.squared),
                   sqrt(summary(lm(PC2 ~ register, data = s_dur_lim))$r.squared),
                   sqrt(summary(lm(PC3 ~ register, data = s_dur_lim))$r.squared),
                   sqrt(summary(lm(PC4 ~ register, data = s_dur_lim))$r.squared),
                   sqrt(summary(lm(PC5 ~ next_phon_class, data = s_dur_lim))$r.squared),
                   sqrt(summary(lm(PC1 ~ next_phon_class, data = s_dur_lim))$r.squared),
                   sqrt(summary(lm(PC2 ~ next_phon_class, data = s_dur_lim))$r.squared),
                   sqrt(summary(lm(PC3 ~ next_phon_class, data = s_dur_lim))$r.squared),
                   sqrt(summary(lm(PC4 ~ next_phon_class, data = s_dur_lim))$r.squared),
                   sqrt(summary(lm(PC5 ~ prev_mention, data = s_dur_lim))$r.squared),
                   sqrt(summary(lm(PC1 ~ prev_mention, data = s_dur_lim))$r.squared),
                   sqrt(summary(lm(PC2 ~ prev_mention, data = s_dur_lim))$r.squared),
                   sqrt(summary(lm(PC3 ~ prev_mention, data = s_dur_lim))$r.squared),
                   sqrt(summary(lm(PC4 ~ prev_mention, data = s_dur_lim))$r.squared),
                   sqrt(summary(lm(PC5 ~ phrase_final, data = s_dur_lim))$r.squared),
                   sqrt(summary(lm(PC1 ~ phrase_final, data = s_dur_lim))$r.squared),
                   sqrt(summary(lm(PC2 ~ phrase_final, data = s_dur_lim))$r.squared),
                   sqrt(summary(lm(PC3 ~ phrase_final, data = s_dur_lim))$r.squared),
                   sqrt(summary(lm(PC4 ~ phrase_final, data = s_dur_lim))$r.squared)
), 
nrow = 5, ncol = 5, byrow = T, dimnames = list(
  categorical,
  continuous))

corrplot(cat_con, method = "number")

####
s_dur_lim = s_dur_lim[, c("log_s_dur", "PC1", "PC2", "PC3", "PC4", "PC5", 
                          "next_phon_class", "prev_mention", "phrase_final", 
                          "type_of_s", "register", "speaker", "word_ort", "corpus")]
s_dur_lim = na.omit(s_dur_lim)
table(s_dur_lim$register, s_dur_lim$type_of_s)

intercepts = lmer(log_s_dur ~ PC1 + PC2 + PC3 + PC4 + PC5
                  + next_phon_class + prev_mention + phrase_final
                  + type_of_s*register
                  + (1 | speaker) + (1 | word_ort),
                  control = lmerControl(optCtrl = list(maxfun = 1e6)),
                  data=s_dur_lim)

intercepts2 = lmer(log_s_dur ~ PC1 + PC2 + PC3 + PC4 + PC5
                   + next_phon_class + prev_mention + phrase_final
                   + type_of_s + register
                   + (1 | speaker) + (1 | word_ort),
                   control = lmerControl(optCtrl = list(maxfun = 1e6)),
                   data=s_dur_lim)

anova(intercepts, intercepts2)

anova(intercepts, type = 2)

plot(effect("type_of_s:register", intercepts))


# Centre of Gravity
# normal model

intercepts_cog = lmer(s_cog_window ~ PC1 + PC2 + PC3 + PC4
                  + next_phon_class + prev_mention 
                  # + phrase_final
                  + type_of_s*register
                  + (1 | speaker) + (1 | word_ort),
                  control = lmerControl(optCtrl = list(maxfun = 1e6)),
                  data=s_dur)

intercepts_cog2 = lmer(s_cog_window ~ PC1 + PC2 + PC3 + PC4
                   + next_phon_class + prev_mention 
                   # + phrase_final
                   + type_of_s + register
                   + (1 | speaker) + (1 | word_ort),
                   control = lmerControl(optCtrl = list(maxfun = 1e6)),
                   data=s_dur)

anova(intercepts_cog, intercepts_cog2)
plot(effect("type_of_s:register", intercepts_cog, x.var = "type_of_s"), multiline = F, 
     layout = c(3,1), alternating = F, rotx = 45, ci.style = "bars", main = "")

intercepts_cog_conv = lmer(s_cog_window ~ PC1 + PC2 + PC3 + PC4
                       + next_phon_class + prev_mention 
                       #+ phrase_final
                       + type_of_s
                       + (1 | speaker) + (1 | word_ort),
                       control = lmerControl(optCtrl = list(maxfun = 1e6)),
                       data=s_dur[s_dur$register == "conversation",])
anova(intercepts_cog_conv)
plot(effect("type_of_s", intercepts_cog_conv), main = "Conversation")

lsmeans(intercepts_cog_conv, "pairwise"~type_of_s)

intercepts_cog_news = lmer(s_cog_window ~ PC1 + PC2 + PC3 + PC4
                       + next_phon_class + prev_mention 
                       #+ phrase_final
                       + type_of_s
                       + (1 | speaker) + (1 | word_ort),
                       control = lmerControl(optCtrl = list(maxfun = 1e6)),
                       data=s_dur[s_dur$register == "news",])
anova(intercepts_cog_news)
plot(effect("type_of_s", intercepts_cog_news), main = "News")

lsmeans(intercepts_cog_news, "pairwise"~type_of_s)

intercepts_cog_stor = lmer(s_cog_window ~ PC1 + PC2 + PC3 + PC4
                       + next_phon_class + prev_mention + phrase_final
                       + type_of_s
                       + (1 | speaker) + (1 | word_ort),
                       control = lmerControl(optCtrl = list(maxfun = 1e6)),
                       data=s_dur[s_dur$register == "stories",])
anova(intercepts_cog_stor)
plot(effect("type_of_s", intercepts_cog_stor), main = "Stories")

lsmeans(intercepts_cog_stor, "pairwise"~type_of_s)

# mirjams residual method
control_cog = lmer(s_cog_window ~ PC1 + PC2 + PC3 + PC4
               + next_phon_class + prev_mention + phrase_final
               + (1 | speaker) + (1 | word_ort),
               control = lmerControl(optCtrl = list(maxfun = 1e6)),
               data=s_dur)

s_dur$cog_resid = resid(control_cog)

interest_cog = lm(cog_resid ~ type_of_s*register,
              data=s_dur)

#interest_cog2 = lm(cog_resid ~ type_of_s,
#               data=s_dur)

#anova(interest_cog, interest_cog2)
anova(interest_cog)
plot(effect("type_of_s:register", interest_cog, x.var = "type_of_s"), multiline = F, 
     layout = c(3,1), alternating = F, rotx = 45, ci.style = "bars", main = "")

interest_cog_conv = lm(cog_resid ~ type_of_s,
                   data=s_dur[s_dur$register == "conversation",])
summary(interest_cog_conv)

lsmeans(interest_cog_conv, "pairwise"~type_of_s)

interest_cog_news = lm(cog_resid ~ type_of_s,
                   data=s_dur[s_dur$register == "news",])
summary(interest_cog_news)

lsmeans(interest_cog_news, "pairwise"~type_of_s)

interest_cog_stor = lm(cog_resid ~ type_of_s,
                   data=s_dur[s_dur$register == "stories",])
summary(interest_cog_stor)

lsmeans(interest_cog_stor, "pairwise"~type_of_s)

## Voicing
s_dur$voiced = s_dur$proportion_voiced > 0.5
# mirjams residual method
control_voi = glmer(proportion_voiced ~ PC1 + PC2 + PC3 + PC4
                   + next_phon_class + prev_mention + phrase_final
                   + (1 | speaker) + (1 | word_ort),
#                   control = glmerControl(optCtrl = list(maxfun = 1e6)),
                   data=s_dur, family = binomial)

s_dur$voi_resid = resid(control_voi)

interest_voi = lm(voi_resid ~ type_of_s*register,
                  data=s_dur)
anova(interest_voi)
plot(effect("type_of_s:register", interest_voi, x.var = "type_of_s"), multiline = F, 
     layout = c(3,1), alternating = F, rotx = 45, ci.style = "bars", main = "")

## ndl models

continuous_plus = c("PC1", "PC2", "PC3", "PC4",
               "priorMorph", "ActFromCues",
               "ActDivFromCues")

corrplot(cor(s_dur[, continuous_plus], use = "complete.obs"), method = "number")

intercepts3 = lmer(log_s_dur ~ PC1 + PC2 + PC3 + PC4 + PC5
                   + next_phon_class + prev_mention + phrase_final
                   + priorMorph + ActFromBoundaryDiphone + ActFromRemainingCues 
                   + ActDivFromBoundaryDiphone + ActDivFromRemainingCues + register
                   + (1 | speaker) + (1 | word_ort),
                   control = lmerControl(optCtrl = list(maxfun = 1e6)),
                   data=s_dur)

anova(intercepts3, intercepts2)


##
interest3 = lm(dur_resid ~ ActDivFromRemainingCues,
               data=s_dur)

interest_conv_ndl = lm(dur_resid ~ priorMorph + ActFromBoundaryDiphone + ActFromRemainingCues 
                       + ActDivFromBoundaryDiphone + ActDivFromRemainingCues,
                   data=s_dur[s_dur$register == "conversation",])

interest_conv_ndl2 = lm(dur_resid ~ priorMorph + ActFromBoundaryDiphone + ActFromRemainingCues 
                       + ActDivFromBoundaryDiphone + ActDivFromRemainingCues + type_of_s,
                       data=s_dur[s_dur$register == "conversation",])

s_dur_conv = s_dur[s_dur$register == "conversation",]
control2 = lmer(log_s_dur ~ priorMorph + ActFromCues + ActDivFromCues
               + (1 | speaker) + (1 | word_ort),
               control = lmerControl(optCtrl = list(maxfun = 1e6)),
               data=s_dur_conv)
#control3 = lmer(log_s_dur ~ priorMorph + ActFromCues + ActDivFromCues + type_of_s
#                + (1 | speaker) + (1 | word_ort),
#                control = lmerControl(optCtrl = list(maxfun = 1e6)),
#                data=s_dur_conv)

#plot(effect("type_of_s", control3))

tab_model(control2, show.ci = F, show.se = T, digits = 3)

plot(effect("priorMorph", control2))

s_dur_conv$dur_resid2 = resid(control2)

interest_conv_ndl = lm(dur_resid2 ~ type_of_s,
                   data=s_dur_conv)
anova(interest_conv_ndl)
plot(effect("type_of_s", interest_conv_ndl), rotx = 45, main = "")
lsmeans(interest_conv_ndl, "pairwise"~type_of_s)

## type of s only has additional value for PART (maybe due to lack of training data for ndl)

ndl1 = lm(priorMorph ~ type_of_s, data=s_dur_conv)
plot(effect("type_of_s", ndl1))
ndl2 = lm(ActFromCues ~ type_of_s, data=s_dur_conv)
plot(effect("type_of_s", ndl2))
ndl3 = lm(ActDivFromCues ~ type_of_s, data=s_dur_conv)
plot(effect("type_of_s", ndl3))
