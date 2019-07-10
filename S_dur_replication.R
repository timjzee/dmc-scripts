library(lme4)
library(effects)
library(languageR)
library(corrplot)
library(car)
library(rcompanion)
library(RePsychLing)
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

s_dur_a = read.csv(paste(cgn_path, "comp-a_s_static_final.csv", sep = ""))
s_dur_a$corpus = as.factor("cgn-a")
s_dur_c = read.csv(paste(cgn_path, "comp-c_s_static_final.csv", sep = ""))
s_dur_c$corpus = as.factor("cgn-c")
s_dur_c$birth_year = as.integer(s_dur_c$birth_year)
s_dur_d = read.csv(paste(cgn_path, "comp-d_s_static_final.csv", sep = ""))
s_dur_d$corpus = as.factor("cgn-d")
s_dur_ifadv = read.csv(paste(ifadv_path, "ifadv_s_static_final.csv", sep = ""))
s_dur_ifadv$corpus = as.factor("ifadv")
s_dur_ifadv$mean_hnr = as.factor(s_dur_ifadv$mean_hnr)
levels(s_dur_ifadv$speaker_sex) = c("sex2", "sex1")
s_dur_ecsd = read.csv(paste(ecsd_path, "ecsd_s_static_final.csv", sep = ""))
s_dur_ecsd$corpus = as.factor("ecsd")

# inspect numbers:
table(s_dur_a$type_of_s)
table(s_dur_c$type_of_s)
table(s_dur_d$type_of_s)
table(s_dur_ifadv$type_of_s)
table(s_dur_ecsd$type_of_s)

s_dur = rbind(s_dur_a, s_dur_c, s_dur_d, s_dur_ifadv, s_dur_ecsd)
s_dur$prev_mention = as.factor(s_dur$prev_mention)
s_dur$phrase_final = as.factor(s_dur$phrase_final)

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
         "proportion_voiced", "proportion_voiced2", "s_cog_full", "s_cog_window", "per_mil_wf", 
         "word_class", "word_pos", "next_phon", "prev_phon", "sent_i", "word_sent_i", "word_chunk_i", 
         "chan")
s_dur = s_dur[ , !(names(s_dur) %in% drop)]

# Get rid of GEN-TIME
s_dur = s_dur[s_dur$type_of_s != "GEN-TIME",]
s_dur$type_of_s = as.factor(as.character(s_dur$type_of_s))
s_dur$type_of_s = relevel(s_dur$type_of_s, ref="S")

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
s_dur = s_dur[!is.na(s_dur$s_dur), ]
# remove NA lines
s_dur = s_dur[rowSums(is.na(s_dur))<length(s_dur),]

# inspect histogram
hist(s_dur$s_dur)

# remove unrepresentative outliers (Baayen, 2008, p. 243)
s_dur = s_dur[s_dur$s_dur < 0.4,]

# make new predictors and get rid of unnecessary NAs
n_cow = 5052213
n_subtlex = 437504
s_dur$wf = 10^s_dur$log_wf
s_dur[is.na(s_dur$wf),]$wf = 1
s_dur$log_wf = log10(s_dur$wf)
s_dur$p_next_w = (s_dur$bigram_f / n_cow) / (s_dur$wf / n_subtlex)

s_dur[is.na(s_dur$lex_neb_freq),]$lex_neb_freq = 0
#n_lexicon = 251563
s_dur$prop_lex_neb_freq = s_dur$lex_neb_freq / s_dur$wf

s_dur$mean_syl_dur = s_dur$base_dur / s_dur$num_syl_pron

# transform dependent variable
s_dur$log_s_dur = log10(s_dur$s_dur)

# Get rid of GEN-POSS for IFADV and ECSD
# s_dur = s_dur[!(s_dur$corpus %in% c("ecsd", "ifadv") & s_dur$type_of_s == "GEN-POSS"),]

# Inspect collinearity

continuous = c("speech_rate_pron", "base_dur", "num_syl", 
               "num_cons_pron", "log_wf", "lex_neb", "log_bigf", "stress_dist")

corrplot(cor(s_dur[, continuous], use = "complete.obs"), method = "number")
vif(lm(log_s_dur ~ speech_rate_pron + base_dur + num_syl_pron 
       + num_cons_pron + log_wf + lex_neb + log_bigf , data=s_dur))
collin.fnc(na.omit(s_dur[, continuous]))$cnumber

# vif shows no values above 5, 
# but condition number of 35 indicates strong collinearity in the dataset
# first of all let's get rid of num_syl (not used in Plag et al. and no clear predictions)

continuous = c("speech_rate_pron", "base_dur", "num_cons_pron", 
               "log_wf", "lex_neb", "log_bigf", "stress_dist")

corrplot(cor(s_dur[, continuous], use = "complete.obs"), method = "number")
collin.fnc(na.omit(s_dur[, continuous]))$cnumber

# let's consider pairwise correlations:

# log_bigf - log_wf
summary(lmer(log_s_dur ~ log_bigf + (1|speaker) + (1|word_ort), data=s_dur))$coefficients
summary(lmer(log_s_dur ~ log_wf + (1|speaker) + (1|word_ort), data=s_dur))$coefficients
summary(lmer(log_s_dur ~ log_bigf + log_wf + (1|speaker) + (1|word_ort), data=s_dur))$coefficients

# these effects can be disentangled by replacing log_bigf with p_next_w (Count (wp wn))/(Count (wp))
# i should probably use COW for word frequency
summary(lmer(log_s_dur ~ p_next_w + (1|speaker) + (1|word_ort), data=s_dur))$coefficients
summary(lmer(log_s_dur ~ p_next_w + log_wf + (1|speaker) + (1|word_ort), data=s_dur))$coefficients

continuous = c("speech_rate_pron", "base_dur", "num_cons_pron", 
               "log_wf", "lex_neb", "p_next_w", "stress_dist")

corrplot(cor(s_dur[, continuous], use = "complete.obs"), method = "number")
collin.fnc(na.omit(s_dur[, continuous]))$cnumber

# lex_neb - log_wf
summary(lmer(log_s_dur ~ lex_neb + (1|speaker) + (1|word_ort), data=s_dur))$coefficients
summary(lmer(log_s_dur ~ log_wf + (1|speaker) + (1|word_ort), data=s_dur))$coefficients
summary(lmer(log_s_dur ~ lex_neb + log_wf + (1|speaker) + (1|word_ort), data=s_dur))$coefficients
# some enhancement
# relation probably due to short words being more frequent and short words having more neighbours
# what happens if we look at it frequency instead, should be less related to word length
summary(lmer(log_s_dur ~ prop_lex_neb_freq + (1|speaker) + (1|word_ort), data=s_dur))$coefficients
summary(lmer(log_s_dur ~ prop_lex_neb_freq + log_wf + (1|speaker) + (1|word_ort), data=s_dur))$coefficients

continuous = c("speech_rate_pron", "base_dur", "num_cons_pron", 
               "log_wf", "prop_lex_neb_freq", "p_next_w", "stress_dist")

corrplot(cor(s_dur[, continuous], use = "complete.obs"), method = "number")
collin.fnc(na.omit(s_dur[, continuous]))$cnumber

# results in lower condition number (16.6)

# log_wf - base_dur
summary(lmer(log_s_dur ~ base_dur + (1|speaker) + (1|word_ort), data=s_dur))$coefficients
summary(lmer(log_s_dur ~ log_wf + (1|speaker) + (1|word_ort), data=s_dur))$coefficients
summary(lmer(log_s_dur ~ base_dur + log_wf + (1|speaker) + (1|word_ort), data=s_dur))$coefficients
# sign flip!
# relation probably due to short words being more frequent
# let's try using mean syllable duration
summary(lmer(log_s_dur ~ mean_syl_dur + (1|speaker) + (1|word_ort), data=s_dur))$coefficients
summary(lmer(log_s_dur ~ mean_syl_dur + log_wf + (1|speaker) + (1|word_ort), data=s_dur))$coefficients

# much better (using num_syl_pron)

continuous = c("speech_rate_pron", "mean_syl_dur", "num_cons_pron", 
               "log_wf", "prop_lex_neb_freq", "p_next_w", "stress_dist")

corrplot(cor(s_dur[, continuous], use = "complete.obs"), method = "number")
collin.fnc(na.omit(s_dur[, continuous]))$cnumber

# actually a small increase in condition number (due to different subset?)
# check log_wf - base_dur
summary(lmer(log_s_dur ~ stress_dist + (1|speaker) + (1|word_ort), data=s_dur))$coefficients
summary(lmer(log_s_dur ~ log_wf + (1|speaker) + (1|word_ort), data=s_dur))$coefficients
summary(lmer(log_s_dur ~ stress_dist + log_wf + (1|speaker) + (1|word_ort), data=s_dur))$coefficients
# a little suppression of log_wf, a little enhancement of stress_dist

# nice decrease in condition number (below 30 threshold)
# and now prop_lex_neb_freq instead of lex_neb also results in lower condition number
# perhaps mean phone duration of base would be even better (sensitive to differences between words with same amount of syllables)
summary(lmer(log_s_dur ~ speech_rate_pron + mean_syl_dur + num_cons_pron 
             + log_wf + prop_lex_neb_freq + p_next_w 
             + (1|speaker) + (1|word_ort), data=s_dur))$coefficients
# no sign flips compared to individual models
vif(lm(s_dur ~ speech_rate_pron + mean_syl_dur + num_cons_pron 
       + log_wf + prop_lex_neb_freq + p_next_w , data=s_dur))
# low vifs


# now let's take a look at the categorical predictors
categorical = c("type_of_s", "corpus", "next_phon_class", 
                "prev_phon_class", "prev_mention", "phrase_final")

#cramerV(table(s_dur[,c("type_of_s", "corpus")]), bias.correct = TRUE)

cat_ass = matrix(c(cramerV(table(s_dur[,c("type_of_s", "type_of_s")]), bias.correct = TRUE),
                   cramerV(table(s_dur[,c("type_of_s", "corpus")]), bias.correct = TRUE),
#                   cramerV(table(s_dur[,c("type_of_s", "stressed")]), bias.correct = TRUE),
                   cramerV(table(s_dur[,c("type_of_s", "next_phon_class")]), bias.correct = TRUE),
                   cramerV(table(s_dur[,c("type_of_s", "prev_phon_class")]), bias.correct = TRUE),
                   cramerV(table(s_dur[,c("type_of_s", "prev_mention")]), bias.correct = TRUE),
                   cramerV(table(s_dur[,c("type_of_s", "phrase_final")]), bias.correct = TRUE),
                   cramerV(table(s_dur[,c("corpus", "type_of_s")]), bias.correct = TRUE),
                   cramerV(table(s_dur[,c("corpus", "corpus")]), bias.correct = TRUE),
#                   cramerV(table(s_dur[,c("corpus", "stressed")]), bias.correct = TRUE),
                   cramerV(table(s_dur[,c("corpus", "next_phon_class")]), bias.correct = TRUE),
                   cramerV(table(s_dur[,c("corpus", "prev_phon_class")]), bias.correct = TRUE),
                   cramerV(table(s_dur[,c("corpus", "prev_mention")]), bias.correct = TRUE),
                   cramerV(table(s_dur[,c("corpus", "phrase_final")]), bias.correct = TRUE),
#                   cramerV(table(s_dur[,c("stressed", "type_of_s")]), bias.correct = TRUE),
#                   cramerV(table(s_dur[,c("stressed", "corpus")]), bias.correct = TRUE),
#                   cramerV(table(s_dur[,c("stressed", "stressed")]), bias.correct = TRUE),
#                   cramerV(table(s_dur[,c("stressed", "next_phon_class")]), bias.correct = TRUE),
#                   cramerV(table(s_dur[,c("stressed", "prev_phon_class")]), bias.correct = TRUE),
#                   cramerV(table(s_dur[,c("stressed", "prev_mention")]), bias.correct = TRUE),
#                   cramerV(table(s_dur[,c("stressed", "phrase_final")]), bias.correct = TRUE),
                   cramerV(table(s_dur[,c("next_phon_class", "type_of_s")]), bias.correct = TRUE),
                   cramerV(table(s_dur[,c("next_phon_class", "corpus")]), bias.correct = TRUE),
#                   cramerV(table(s_dur[,c("next_phon_class", "stressed")]), bias.correct = TRUE),
                   cramerV(table(s_dur[,c("next_phon_class", "next_phon_class")]), bias.correct = TRUE),
                   cramerV(table(s_dur[,c("next_phon_class", "prev_phon_class")]), bias.correct = TRUE),
                   cramerV(table(s_dur[,c("next_phon_class", "prev_mention")]), bias.correct = TRUE),
                   cramerV(table(s_dur[,c("next_phon_class", "phrase_final")]), bias.correct = TRUE),
                   cramerV(table(s_dur[,c("prev_phon_class", "type_of_s")]), bias.correct = TRUE),
                   cramerV(table(s_dur[,c("prev_phon_class", "corpus")]), bias.correct = TRUE),
#                   cramerV(table(s_dur[,c("prev_phon_class", "stressed")]), bias.correct = TRUE),
                   cramerV(table(s_dur[,c("prev_phon_class", "next_phon_class")]), bias.correct = TRUE),
                   cramerV(table(s_dur[,c("prev_phon_class", "prev_phon_class")]), bias.correct = TRUE),
                   cramerV(table(s_dur[,c("prev_phon_class", "prev_mention")]), bias.correct = TRUE),
                   cramerV(table(s_dur[,c("prev_phon_class", "phrase_final")]), bias.correct = TRUE),
                   cramerV(table(s_dur[,c("prev_mention", "type_of_s")]), bias.correct = TRUE),
                   cramerV(table(s_dur[,c("prev_mention", "corpus")]), bias.correct = TRUE),
#                   cramerV(table(s_dur[,c("prev_mention", "stressed")]), bias.correct = TRUE),
                   cramerV(table(s_dur[,c("prev_mention", "next_phon_class")]), bias.correct = TRUE),
                   cramerV(table(s_dur[,c("prev_mention", "prev_phon_class")]), bias.correct = TRUE),
                   cramerV(table(s_dur[,c("prev_mention", "prev_mention")]), bias.correct = TRUE),
                   cramerV(table(s_dur[,c("prev_mention", "phrase_final")]), bias.correct = TRUE),
                   cramerV(table(s_dur[,c("phrase_final", "type_of_s")]), bias.correct = TRUE),
                   cramerV(table(s_dur[,c("phrase_final", "corpus")]), bias.correct = TRUE),
#                   cramerV(table(s_dur[,c("phrase_final", "stressed")]), bias.correct = TRUE),
                   cramerV(table(s_dur[,c("phrase_final", "next_phon_class")]), bias.correct = TRUE),
                   cramerV(table(s_dur[,c("phrase_final", "prev_phon_class")]), bias.correct = TRUE),
                   cramerV(table(s_dur[,c("phrase_final", "prev_mention")]), bias.correct = TRUE),
                   cramerV(table(s_dur[,c("phrase_final", "phrase_final")]), bias.correct = TRUE)),
                 nrow = 6, ncol = 6, byrow = T, dimnames = list(
                   categorical,
                   categorical))

corrplot(cat_ass, method = "number")

# Note that as chi-squared values tend to increase with the number of cells, 
# the greater the difference between r (rows) and c (columns), the more 
# likely φc will tend to 1 without strong evidence of a meaningful correlation
# this might be the case for:
# stressed - type_of_s
summary(lmer(log_s_dur ~ stressed + (1|speaker) + (1|word_ort), data=s_dur))$coefficients
summary(lmer(log_s_dur ~ type_of_s + (1|speaker) + (1|word_ort), data=s_dur))$coefficients
summary(lmer(log_s_dur ~ stressed + type_of_s + (1|speaker) + (1|word_ort), data=s_dur))$coefficients
# Suppression, so at least it won't result in a type I error for our variable of interest

# phrase_final - next_phon_class association is due to phrases ending in SIL

# now let's look at categorical - continuous associations

cat_con = matrix(c(sqrt(summary(lm(speech_rate_pron ~ type_of_s, data = s_dur))$r.squared),
                   sqrt(summary(lm(mean_syl_dur ~ type_of_s, data = s_dur))$r.squared),
                   sqrt(summary(lm(num_cons_pron ~ type_of_s, data = s_dur))$r.squared),
                   sqrt(summary(lm(log_wf ~ type_of_s, data = s_dur))$r.squared),
                   sqrt(summary(lm(prop_lex_neb_freq ~ type_of_s, data = s_dur))$r.squared),
                   sqrt(summary(lm(p_next_w ~ type_of_s, data = s_dur))$r.squared),
                   sqrt(summary(lm(stress_dist ~ type_of_s, data = s_dur))$r.squared),
                   sqrt(summary(lm(speech_rate_pron ~ corpus, data = s_dur))$r.squared),
                   sqrt(summary(lm(mean_syl_dur ~ corpus, data = s_dur))$r.squared),
                   sqrt(summary(lm(num_cons_pron ~ corpus, data = s_dur))$r.squared),
                   sqrt(summary(lm(log_wf ~ corpus, data = s_dur))$r.squared),
                   sqrt(summary(lm(prop_lex_neb_freq ~ corpus, data = s_dur))$r.squared),
                   sqrt(summary(lm(p_next_w ~ corpus, data = s_dur))$r.squared),
                   sqrt(summary(lm(stress_dist ~ corpus, data = s_dur))$r.squared),
#                   sqrt(summary(lm(speech_rate_pron ~ stressed, data = s_dur))$r.squared),
#                   sqrt(summary(lm(mean_syl_dur ~ stressed, data = s_dur))$r.squared),
#                   sqrt(summary(lm(num_cons_pron ~ stressed, data = s_dur))$r.squared),
#                   sqrt(summary(lm(log_wf ~ stressed, data = s_dur))$r.squared),
#                   sqrt(summary(lm(prop_lex_neb_freq ~ stressed, data = s_dur))$r.squared),
#                   sqrt(summary(lm(p_next_w ~ stressed, data = s_dur))$r.squared),
                   sqrt(summary(lm(speech_rate_pron ~ next_phon_class, data = s_dur))$r.squared),
                   sqrt(summary(lm(mean_syl_dur ~ next_phon_class, data = s_dur))$r.squared),
                   sqrt(summary(lm(num_cons_pron ~ next_phon_class, data = s_dur))$r.squared),
                   sqrt(summary(lm(log_wf ~ next_phon_class, data = s_dur))$r.squared),
                   sqrt(summary(lm(prop_lex_neb_freq ~ next_phon_class, data = s_dur))$r.squared),
                   sqrt(summary(lm(p_next_w ~ next_phon_class, data = s_dur))$r.squared),
                   sqrt(summary(lm(stress_dist ~ next_phon_class, data = s_dur))$r.squared),
                   sqrt(summary(lm(speech_rate_pron ~ prev_phon_class, data = s_dur))$r.squared),
                   sqrt(summary(lm(mean_syl_dur ~ prev_phon_class, data = s_dur))$r.squared),
                   sqrt(summary(lm(num_cons_pron ~ prev_phon_class, data = s_dur))$r.squared),
                   sqrt(summary(lm(log_wf ~ prev_phon_class, data = s_dur))$r.squared),
                   sqrt(summary(lm(prop_lex_neb_freq ~ prev_phon_class, data = s_dur))$r.squared),
                   sqrt(summary(lm(p_next_w ~ prev_phon_class, data = s_dur))$r.squared),
                   sqrt(summary(lm(stress_dist ~ prev_phon_class, data = s_dur))$r.squared),
                   sqrt(summary(lm(speech_rate_pron ~ prev_mention, data = s_dur))$r.squared),
                   sqrt(summary(lm(mean_syl_dur ~ prev_mention, data = s_dur))$r.squared),
                   sqrt(summary(lm(num_cons_pron ~ prev_mention, data = s_dur))$r.squared),
                   sqrt(summary(lm(log_wf ~ prev_mention, data = s_dur))$r.squared),
                   sqrt(summary(lm(prop_lex_neb_freq ~ prev_mention, data = s_dur))$r.squared),
                   sqrt(summary(lm(p_next_w ~ prev_mention, data = s_dur))$r.squared),
                   sqrt(summary(lm(stress_dist ~ prev_mention, data = s_dur))$r.squared),
                   sqrt(summary(lm(speech_rate_pron ~ phrase_final, data = s_dur))$r.squared),
                   sqrt(summary(lm(mean_syl_dur ~ phrase_final, data = s_dur))$r.squared),
                   sqrt(summary(lm(num_cons_pron ~ phrase_final, data = s_dur))$r.squared),
                   sqrt(summary(lm(log_wf ~ phrase_final, data = s_dur))$r.squared),
                   sqrt(summary(lm(prop_lex_neb_freq ~ phrase_final, data = s_dur))$r.squared),
                   sqrt(summary(lm(p_next_w ~ phrase_final, data = s_dur))$r.squared),
                   sqrt(summary(lm(stress_dist ~ phrase_final, data = s_dur))$r.squared)
                   ), 
                 nrow = 6, ncol = 7, byrow = T, dimnames = list(
                   categorical,
                   continuous))

corrplot(cat_con, method = "number")

# Very strong (and understandable) association between 
# prev_phon_class - num_cons_pron
summary(lmer(log_s_dur ~ prev_phon_class + (1|speaker) + (1|word_ort), data=s_dur))$coefficients
summary(lmer(log_s_dur ~ num_cons_pron + (1|speaker) + (1|word_ort), data=s_dur))$coefficients
summary(lmer(log_s_dur ~ prev_phon_class + num_cons_pron + (1|speaker) + (1|word_ort), data=s_dur))$coefficients
# suppression, let's see which predictor performs better
cons0 = lmer(log_s_dur ~ 1 + (1|speaker) + (1|word_ort), data=na.omit(s_dur))
cons1 = lmer(log_s_dur ~ prev_phon_class + (1|speaker) + (1|word_ort), data=na.omit(s_dur))
cons2 = lmer(log_s_dur ~ num_cons_pron + (1|speaker) + (1|word_ort), data=na.omit(s_dur))
anova(cons0, cons1)
anova(cons0, cons2)
# lower AIC for prev_phon_class, but num_cons_pron also performs well and was also used by Plag et al
# let's remove prev_phon_class

# we should also look at
# type_of_s - log_wf
summary(lmer(log_s_dur ~ type_of_s + (1|speaker) + (1|word_ort), data=s_dur))$coefficients
summary(lmer(log_s_dur ~ log_wf + (1|speaker) + (1|word_ort), data=s_dur))$coefficients
summary(lmer(log_s_dur ~ type_of_s + log_wf + (1|speaker) + (1|word_ort), data=s_dur))$coefficients
# some levels of type_of_s change (effect for GEN-TIME gets a little larger, 
# PART barely changes, others get smaller) but no dramatic changes / changes in sign; 
# log_wf is enhanced a little

# and look at
# type_of_s - stress_dist
summary(lmer(log_s_dur ~ stress_dist + (1|speaker) + (1|word_ort), data=s_dur))$coefficients
summary(lmer(log_s_dur ~ type_of_s + (1|speaker) + (1|word_ort), data=s_dur))$coefficients
summary(lmer(log_s_dur ~ stress_dist + type_of_s + (1|speaker) + (1|word_ort), data=s_dur))$coefficients
# suppression of type_of_s


############ FINAL COLLINEARITY PLOTS ############

## CONTINUOUS
# BEFORE
continuous = c("speech_rate_pron", "base_dur", "num_cons_pron", 
               "log_wf", "lex_neb", "log_bigf", "stress_dist")

corrplot(cor(s_dur[, continuous], use = "complete.obs"), method = "number")
collin.fnc(na.omit(s_dur[, continuous]))$cnumber

drop = c("prev_phon_pron", "next_phon_pron", "lex_neb_freq", "bigram_f", "num_syl", 
         "word_stress", "num_syl_pron", "prev_phon_class",
         "wf", "stressed", "lex_neb", "base_dur", "log_bigf")
s_dur = s_dur[ , !(names(s_dur) %in% drop)]

s_dur = na.omit(s_dur)

# AFTER
continuous = c("speech_rate_pron", "mean_syl_dur", "num_cons_pron", 
               "log_wf", "prop_lex_neb_freq", "p_next_w", "stress_dist")

corrplot(cor(s_dur[, continuous], use = "complete.obs"), method = "number")
collin.fnc(na.omit(s_dur[, continuous]))$cnumber

## CATEGORICAL
categorical = c("type_of_s", "corpus", "next_phon_class", 
                "prev_mention", "phrase_final")

cat_ass = matrix(c(cramerV(table(s_dur[,c("type_of_s", "type_of_s")]), bias.correct = TRUE),
                   cramerV(table(s_dur[,c("type_of_s", "corpus")]), bias.correct = TRUE),
                   cramerV(table(s_dur[,c("type_of_s", "next_phon_class")]), bias.correct = TRUE),
                   cramerV(table(s_dur[,c("type_of_s", "prev_mention")]), bias.correct = TRUE),
                   cramerV(table(s_dur[,c("type_of_s", "phrase_final")]), bias.correct = TRUE),
                   cramerV(table(s_dur[,c("corpus", "type_of_s")]), bias.correct = TRUE),
                   cramerV(table(s_dur[,c("corpus", "corpus")]), bias.correct = TRUE),
                   cramerV(table(s_dur[,c("corpus", "next_phon_class")]), bias.correct = TRUE),
                   cramerV(table(s_dur[,c("corpus", "prev_mention")]), bias.correct = TRUE),
                   cramerV(table(s_dur[,c("corpus", "phrase_final")]), bias.correct = TRUE),
                   cramerV(table(s_dur[,c("next_phon_class", "type_of_s")]), bias.correct = TRUE),
                   cramerV(table(s_dur[,c("next_phon_class", "corpus")]), bias.correct = TRUE),
                   cramerV(table(s_dur[,c("next_phon_class", "next_phon_class")]), bias.correct = TRUE),
                   cramerV(table(s_dur[,c("next_phon_class", "prev_mention")]), bias.correct = TRUE),
                   cramerV(table(s_dur[,c("next_phon_class", "phrase_final")]), bias.correct = TRUE),
                   cramerV(table(s_dur[,c("prev_mention", "type_of_s")]), bias.correct = TRUE),
                   cramerV(table(s_dur[,c("prev_mention", "corpus")]), bias.correct = TRUE),
                   cramerV(table(s_dur[,c("prev_mention", "next_phon_class")]), bias.correct = TRUE),
                   cramerV(table(s_dur[,c("prev_mention", "prev_mention")]), bias.correct = TRUE),
                   cramerV(table(s_dur[,c("prev_mention", "phrase_final")]), bias.correct = TRUE),
                   cramerV(table(s_dur[,c("phrase_final", "type_of_s")]), bias.correct = TRUE),
                   cramerV(table(s_dur[,c("phrase_final", "corpus")]), bias.correct = TRUE),
                   cramerV(table(s_dur[,c("phrase_final", "next_phon_class")]), bias.correct = TRUE),
                   cramerV(table(s_dur[,c("phrase_final", "prev_mention")]), bias.correct = TRUE),
                   cramerV(table(s_dur[,c("phrase_final", "phrase_final")]), bias.correct = TRUE)),
                 nrow = 5, ncol = 5, byrow = T, dimnames = list(
                   categorical,
                   categorical))

corrplot(cat_ass, method = "number")

## CONTINUOUS - CATEGORICAL
cat_con = matrix(c(sqrt(summary(lm(speech_rate_pron ~ type_of_s, data = s_dur))$r.squared),
                   sqrt(summary(lm(mean_syl_dur ~ type_of_s, data = s_dur))$r.squared),
                   sqrt(summary(lm(num_cons_pron ~ type_of_s, data = s_dur))$r.squared),
                   sqrt(summary(lm(log_wf ~ type_of_s, data = s_dur))$r.squared),
                   sqrt(summary(lm(prop_lex_neb_freq ~ type_of_s, data = s_dur))$r.squared),
                   sqrt(summary(lm(p_next_w ~ type_of_s, data = s_dur))$r.squared),
                   sqrt(summary(lm(stress_dist ~ type_of_s, data = s_dur))$r.squared),
                   sqrt(summary(lm(speech_rate_pron ~ corpus, data = s_dur))$r.squared),
                   sqrt(summary(lm(mean_syl_dur ~ corpus, data = s_dur))$r.squared),
                   sqrt(summary(lm(num_cons_pron ~ corpus, data = s_dur))$r.squared),
                   sqrt(summary(lm(log_wf ~ corpus, data = s_dur))$r.squared),
                   sqrt(summary(lm(prop_lex_neb_freq ~ corpus, data = s_dur))$r.squared),
                   sqrt(summary(lm(p_next_w ~ corpus, data = s_dur))$r.squared),
                   sqrt(summary(lm(stress_dist ~ corpus, data = s_dur))$r.squared),
                   sqrt(summary(lm(speech_rate_pron ~ next_phon_class, data = s_dur))$r.squared),
                   sqrt(summary(lm(mean_syl_dur ~ next_phon_class, data = s_dur))$r.squared),
                   sqrt(summary(lm(num_cons_pron ~ next_phon_class, data = s_dur))$r.squared),
                   sqrt(summary(lm(log_wf ~ next_phon_class, data = s_dur))$r.squared),
                   sqrt(summary(lm(prop_lex_neb_freq ~ next_phon_class, data = s_dur))$r.squared),
                   sqrt(summary(lm(p_next_w ~ next_phon_class, data = s_dur))$r.squared),
                   sqrt(summary(lm(stress_dist ~ next_phon_class, data = s_dur))$r.squared),
                   sqrt(summary(lm(speech_rate_pron ~ prev_mention, data = s_dur))$r.squared),
                   sqrt(summary(lm(mean_syl_dur ~ prev_mention, data = s_dur))$r.squared),
                   sqrt(summary(lm(num_cons_pron ~ prev_mention, data = s_dur))$r.squared),
                   sqrt(summary(lm(log_wf ~ prev_mention, data = s_dur))$r.squared),
                   sqrt(summary(lm(prop_lex_neb_freq ~ prev_mention, data = s_dur))$r.squared),
                   sqrt(summary(lm(p_next_w ~ prev_mention, data = s_dur))$r.squared),
                   sqrt(summary(lm(stress_dist ~ prev_mention, data = s_dur))$r.squared),
                   sqrt(summary(lm(speech_rate_pron ~ phrase_final, data = s_dur))$r.squared),
                   sqrt(summary(lm(mean_syl_dur ~ phrase_final, data = s_dur))$r.squared),
                   sqrt(summary(lm(num_cons_pron ~ phrase_final, data = s_dur))$r.squared),
                   sqrt(summary(lm(log_wf ~ phrase_final, data = s_dur))$r.squared),
                   sqrt(summary(lm(prop_lex_neb_freq ~ phrase_final, data = s_dur))$r.squared),
                   sqrt(summary(lm(p_next_w ~ phrase_final, data = s_dur))$r.squared),
                   sqrt(summary(lm(stress_dist ~ phrase_final, data = s_dur))$r.squared)
), 
nrow = 5, ncol = 7, byrow = T, dimnames = list(
  categorical,
  continuous))

corrplot(cat_con, method = "number")

##################################################

drop = c("base_dur", "num_syl_pron", "log_bigf", "prev_phon_class",
         "wf", "stressed")
s_dur = s_dur[ , !(names(s_dur) %in% drop)]

############
# principle components
col_pred = s_dur[, c("speech_rate_pron", "mean_syl_dur", "num_cons_pron", "log_wf", "stress_dist")]
col_pred_pca = prcomp(col_pred, center = T, scale. = T)
summary(col_pred_pca)
s_dur$PC1 = col_pred_pca$x[,1]
s_dur$PC2 = col_pred_pca$x[,2]
s_dur$PC3 = col_pred_pca$x[,3]
s_dur$PC4 = col_pred_pca$x[,4]
s_dur$PC5 = col_pred_pca$x[,5]

continuous = c("PC5", "PC1", "PC2", 
               "PC3", "prop_lex_neb_freq", "p_next_w", "PC4")
corrplot(cor(s_dur[, continuous], use = "complete.obs"), method = "number")

categorical = c("type_of_s", "corpus", "next_phon_class", 
                "prev_mention", "phrase_final")

cat_con = matrix(c(sqrt(summary(lm(PC5 ~ type_of_s, data = s_dur))$r.squared),
                   sqrt(summary(lm(PC1 ~ type_of_s, data = s_dur))$r.squared),
                   sqrt(summary(lm(PC2 ~ type_of_s, data = s_dur))$r.squared),
                   sqrt(summary(lm(PC3 ~ type_of_s, data = s_dur))$r.squared),
                   sqrt(summary(lm(prop_lex_neb_freq ~ type_of_s, data = s_dur))$r.squared),
                   sqrt(summary(lm(p_next_w ~ type_of_s, data = s_dur))$r.squared),
                   sqrt(summary(lm(PC4 ~ type_of_s, data = s_dur))$r.squared),
                   sqrt(summary(lm(PC5 ~ corpus, data = s_dur))$r.squared),
                   sqrt(summary(lm(PC1 ~ corpus, data = s_dur))$r.squared),
                   sqrt(summary(lm(PC2 ~ corpus, data = s_dur))$r.squared),
                   sqrt(summary(lm(PC3 ~ corpus, data = s_dur))$r.squared),
                   sqrt(summary(lm(prop_lex_neb_freq ~ corpus, data = s_dur))$r.squared),
                   sqrt(summary(lm(p_next_w ~ corpus, data = s_dur))$r.squared),
                   sqrt(summary(lm(PC4 ~ corpus, data = s_dur))$r.squared),
                   sqrt(summary(lm(PC5 ~ next_phon_class, data = s_dur))$r.squared),
                   sqrt(summary(lm(PC1 ~ next_phon_class, data = s_dur))$r.squared),
                   sqrt(summary(lm(PC2 ~ next_phon_class, data = s_dur))$r.squared),
                   sqrt(summary(lm(PC3 ~ next_phon_class, data = s_dur))$r.squared),
                   sqrt(summary(lm(prop_lex_neb_freq ~ next_phon_class, data = s_dur))$r.squared),
                   sqrt(summary(lm(p_next_w ~ next_phon_class, data = s_dur))$r.squared),
                   sqrt(summary(lm(PC4 ~ next_phon_class, data = s_dur))$r.squared),
                   sqrt(summary(lm(PC5 ~ prev_mention, data = s_dur))$r.squared),
                   sqrt(summary(lm(PC1 ~ prev_mention, data = s_dur))$r.squared),
                   sqrt(summary(lm(PC2 ~ prev_mention, data = s_dur))$r.squared),
                   sqrt(summary(lm(PC3 ~ prev_mention, data = s_dur))$r.squared),
                   sqrt(summary(lm(prop_lex_neb_freq ~ prev_mention, data = s_dur))$r.squared),
                   sqrt(summary(lm(p_next_w ~ prev_mention, data = s_dur))$r.squared),
                   sqrt(summary(lm(PC4 ~ prev_mention, data = s_dur))$r.squared),
                   sqrt(summary(lm(PC5 ~ phrase_final, data = s_dur))$r.squared),
                   sqrt(summary(lm(PC1 ~ phrase_final, data = s_dur))$r.squared),
                   sqrt(summary(lm(PC2 ~ phrase_final, data = s_dur))$r.squared),
                   sqrt(summary(lm(PC3 ~ phrase_final, data = s_dur))$r.squared),
                   sqrt(summary(lm(prop_lex_neb_freq ~ phrase_final, data = s_dur))$r.squared),
                   sqrt(summary(lm(p_next_w ~ phrase_final, data = s_dur))$r.squared),
                   sqrt(summary(lm(PC4 ~ phrase_final, data = s_dur))$r.squared)
), 
nrow = 5, ncol = 7, byrow = T, dimnames = list(
  categorical,
  continuous))

corrplot(cat_con, method = "number")

summary(lmer(log_s_dur ~ PC1 + (1|speaker) + (1|word_ort), data=s_dur))$coefficients
summary(lmer(log_s_dur ~ type_of_s + (1|speaker) + (1|word_ort), data=s_dur))$coefficients
summary(lmer(log_s_dur ~ PC1 + type_of_s + (1|speaker) + (1|word_ort), data=s_dur))$coefficients

# what would happen to this association if we only take into account monosyllabic words above a certain frequency
s_dur_mono = s_dur[s_dur$num_syl == 1,]
# quick model shows it is significantly reduced by only including monosyllables (what would happen if we exclude function words instead)
s_dur = s_dur[!(s_dur$word_ort %in% c("is", "was", "als", "dus")),]
# still some association, but no enhancement anymore
# no significant interaction between corpus*type_of_s
# significant main effect of type_of_s


############
max = readRDS(paste(f_path, "max.rds", sep = ""))
summary(max)$optinfo$conv$lme4$messages
zcp = readRDS(paste(f_path, "zcp.rds", sep = ""))
summary(zcp)$optinfo$conv$lme4$messages
intercepts = readRDS(paste(f_path, "intercepts.rds", sep = ""))
summary(intercepts)$optinfo$conv$lme4$messages


REtable = readRDS(paste(f_path, "RE_anova_table.rds", sep = ""))
REtable
# LMMmax failed to converge and does not result in a significantly better fit (alpha = 0.4)
# LMMzcp converged but has singular covariance matrix
# let's check signifcance of corpus in both LMMzcp and LMMintercepts
zcp2 = readRDS(paste(f_path, "zcp_nocorpus.rds", sep = ""))
zcp_table = readRDS(paste(f_path, "FE_zcp_table.rds", sep = ""))
zcp_table

intercepts2 = readRDS(paste(f_path, "intercepts_nocorpus.rds", sep = ""))
intercepts_table = readRDS(paste(f_path, "FE_intercepts_table.rds", sep = ""))
intercepts_table
# both models agree there is an interaction
# both models seem fine and zcp explains more variance
# before we split into separate analyses for corpora, lets check residuals

par(mfrow=c(2,3))
plot(fitted(intercepts), residuals(intercepts))
cor(fitted(intercepts), residuals(intercepts))
qqnorm(residuals(intercepts))
qqline(residuals(intercepts))
plot(density(residuals(intercepts)))

s_dur2 = s_dur[abs(scale(resid(intercepts))) < 2.5,]
intercepts_tr = readRDS(paste(f_path, "intercepts_trimmed.rds", sep = ""))

plot(fitted(intercepts_tr), residuals(intercepts_tr))
cor(fitted(intercepts_tr), residuals(intercepts_tr))
qqnorm(residuals(intercepts_tr))
qqline(residuals(intercepts_tr))
plot(density(residuals(intercepts_tr)))

# doesn't really improve
par(mfrow=c(1,1))

plot(effect("type_of_s:corpus", intercepts, x.var = "type_of_s"), multiline = F, 
     layout = c(5,1), ylim = c(-1.4, -0.8), alternating = F, rotx = 45, ci.style = "bars")

# # check relevelled versions of intercepts model
#  
# s_dur$corpus = relevel(s_dur$corpus, ref="cgn-c")
# intercepts_rel_c = readRDS(paste(f_path, "intercepts_rel_c.rds", sep = ""))
# s_dur$corpus = relevel(s_dur$corpus, ref="cgn-d")
# intercepts_rel_d = readRDS(paste(f_path, "intercepts_rel_d.rds", sep = ""))
# s_dur$corpus = relevel(s_dur$corpus, ref="ifadv")
# intercepts_rel_ifadv = readRDS(paste(f_path, "intercepts_rel_ifadv.rds", sep = ""))
# s_dur$corpus = relevel(s_dur$corpus, ref="ecsd")
# intercepts_rel_ecsd = readRDS(paste(f_path, "intercepts_rel_ecsd.rds", sep = ""))
# s_dur$corpus = relevel(s_dur$corpus, ref="cgn-a")
# intercepts_rel_a = readRDS(paste(f_path, "intercepts_rel_a.rds", sep = ""))
# 
# pairwise = as.data.frame(ls_means(intercepts_rel_ecsd, pairwise = T, which = "type_of_s:corpus"))
# pairwise[c("type_of_sS:corpusecsd - type_of_sGEN-POSS:corpusecsd",
#            "type_of_sS:corpusecsd - type_of_sGEN-TIME:corpusecsd",
#            "type_of_sS:corpusecsd - type_of_sPART:corpusecsd",
#            "type_of_sS:corpusecsd - type_of_sPL:corpusecsd",
#            "type_of_sS:corpusecsd - type_of_sGEN-POSS:corpusifadv",
#            "type_of_sS:corpusecsd - type_of_sGEN-TIME:corpusifadv",
#            "type_of_sS:corpusecsd - type_of_sPART:corpusifadv",
#            "type_of_sS:corpusecsd - type_of_sPL:corpusifadv",
#            "type_of_sS:corpusecsd - type_of_sGEN-POSS:corpuscgn-d",
#            "type_of_sS:corpusecsd - type_of_sGEN-TIME:corpuscgn-d",
#            "type_of_sS:corpusecsd - type_of_sPART:corpuscgn-d",
#            "type_of_sS:corpusecsd - type_of_sPL:corpuscgn-d",
#            "type_of_sS:corpusecsd - type_of_sGEN-POSS:corpuscgn-c",
#            "type_of_sS:corpusecsd - type_of_sGEN-TIME:corpuscgn-c",
#            "type_of_sS:corpusecsd - type_of_sPART:corpuscgn-c",
#            "type_of_sS:corpusecsd - type_of_sPL:corpuscgn-c",
#            "type_of_sS:corpusecsd - type_of_sGEN-POSS:corpuscgn-a",
#            "type_of_sS:corpusecsd - type_of_sGEN-TIME:corpuscgn-a",
#            "type_of_sS:corpusecsd - type_of_sPART:corpuscgn-a",
#            "type_of_sS:corpusecsd - type_of_sPL:corpuscgn-a"
#            ), names(pairwise) %in% c("t value", "Pr(>|t|)")]

######### GET SEPARATE MODELS
s_dur_a = s_dur[s_dur$corpus == "cgn-a",]
s_dur_c = s_dur[s_dur$corpus == "cgn-c",]
s_dur_d = s_dur[s_dur$corpus == "cgn-d",]
s_dur_ifadv = s_dur[s_dur$corpus == "ifadv",]
s_dur_ifadv$type_of_s = as.factor(as.character(s_dur_ifadv$type_of_s))
s_dur_ifadv$type_of_s = relevel(s_dur_ifadv$type_of_s, ref="S")
s_dur_ecsd = s_dur[s_dur$corpus == "ecsd",]
s_dur_ecsd$type_of_s = as.factor(as.character(s_dur_ecsd$type_of_s))
s_dur_ecsd$type_of_s = relevel(s_dur_ecsd$type_of_s, ref="S")

intercepts_a = readRDS(paste(f_path, "intercepts_a.rds", sep = ""))
intercepts_c = readRDS(paste(f_path, "intercepts_c.rds", sep = ""))
intercepts_d = readRDS(paste(f_path, "intercepts_d.rds", sep = ""))
intercepts_ifadv = readRDS(paste(f_path, "intercepts_ifadv.rds", sep = ""))
intercepts_ecsd = readRDS(paste(f_path, "intercepts_ecsd.rds", sep = ""))

intercepts_a_ctrl = readRDS(paste(f_path, "intercepts_a_ctrl.rds", sep = ""))
intercepts_c_ctrl = readRDS(paste(f_path, "intercepts_c_ctrl.rds", sep = ""))
intercepts_d_ctrl = readRDS(paste(f_path, "intercepts_d_ctrl.rds", sep = ""))
intercepts_ifadv_ctrl = readRDS(paste(f_path, "intercepts_ifadv_ctrl.rds", sep = ""))
intercepts_ecsd_ctrl = readRDS(paste(f_path, "intercepts_ecsd_ctrl.rds", sep = ""))

summary(intercepts_a)
anova(intercepts_a_ctrl, intercepts_a)
plot(effect("type_of_s", intercepts_a), main = "CGN A", ylim = c(-1.40, -0.95),
     row = 1, col = 1, nrow = 1, ncol = 5, more = T, rotx = 45)
summary(intercepts_c)
anova(intercepts_c_ctrl, intercepts_c)
plot(effect("type_of_s", intercepts_c), main = "CGN C", ylim = c(-1.40, -0.95),
     row = 1, col = 2, nrow = 1, ncol = 5, more = T, rotx = 45)
summary(intercepts_d)
anova(intercepts_d_ctrl, intercepts_d)
plot(effect("type_of_s", intercepts_d), main = "CGN D", ylim = c(-1.40, -0.95),
     row = 1, col = 3, nrow = 1, ncol = 5, more = T, rotx = 45)
summary(intercepts_ifadv)
anova(intercepts_ifadv_ctrl, intercepts_ifadv)
plot(effect("type_of_s", intercepts_ifadv), main = "IFADV", ylim = c(-1.40, -0.95),
     row = 1, col = 4, nrow = 1, ncol = 5, more = T, rotx = 45)
summary(intercepts_ecsd)
anova(intercepts_ecsd_ctrl, intercepts_ecsd)
plot(effect("type_of_s", intercepts_ecsd), main = "ECSD", ylim = c(-1.40, -0.95),
     row = 1, col = 5, nrow = 1, ncol = 5, more = T, rotx = 45)

# type of s only seems to be significant in comp-a & ifadv

## now let's check in zcp model
zcp_a = readRDS(paste(f_path, "zcp_a.rds", sep = ""))
zcp_c = readRDS(paste(f_path, "zcp_c.rds", sep = ""))
zcp_d = readRDS(paste(f_path, "zcp_d.rds", sep = ""))
zcp_ifadv = readRDS(paste(f_path, "zcp_ifadv.rds", sep = ""))
zcp_ecsd = readRDS(paste(f_path, "zcp_ecsd.rds", sep = ""))

zcp_a_ctrl = readRDS(paste(f_path, "zcp_a_ctrl.rds", sep = ""))
zcp_c_ctrl = readRDS(paste(f_path, "zcp_c_ctrl.rds", sep = ""))
zcp_d_ctrl = readRDS(paste(f_path, "zcp_d_ctrl.rds", sep = ""))
zcp_ifadv_ctrl = readRDS(paste(f_path, "zcp_ifadv_ctrl.rds", sep = ""))
zcp_ecsd_ctrl = readRDS(paste(f_path, "zcp_ecsd_ctrl.rds", sep = ""))

summary(zcp_a)$optinfo$conv$lme4$messages
summary(zcp_a_ctrl)$optinfo$conv$lme4$messages
anova(zcp_a_ctrl, zcp_a)
plot(effect("type_of_s", zcp_a))
summary(zcp_c)$optinfo$conv$lme4$messages
summary(zcp_c_ctrl)$optinfo$conv$lme4$messages
anova(zcp_c_ctrl, zcp_c)
plot(effect("type_of_s", zcp_c))
summary(zcp_d)$optinfo$conv$lme4$messages
summary(zcp_d_ctrl)$optinfo$conv$lme4$messages
anova(zcp_d_ctrl, zcp_d)
plot(effect("type_of_s", zcp_d))
summary(zcp_ifadv)$optinfo$conv$lme4$messages
summary(zcp_ifadv_ctrl)$optinfo$conv$lme4$messages
anova(zcp_ifadv_ctrl, zcp_ifadv)
plot(effect("type_of_s", zcp_ifadv))
summary(zcp_ecsd)$optinfo$conv$lme4$messages
summary(zcp_ecsd_ctrl)$optinfo$conv$lme4$messages
anova(zcp_ecsd_ctrl, zcp_ecsd)
plot(effect("type_of_s", zcp_ecsd))

# some of these models failed to converge or are singular
