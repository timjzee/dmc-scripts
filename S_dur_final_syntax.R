library(languageR)
library(corrplot)
library(car)
library(rcompanion)
library(lme4)
library(effects)
library(gridExtra)

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

s_dur_a = read.csv(paste(cgn_path, "synvoirel_s_comb_comp-a2.csv", sep = ""))
s_dur_a$corpus = as.factor("cgn-a")
s_dur_a$register = as.factor("conversation")
s_dur_a$mean_hnr = as.factor(s_dur_a$mean_hnr)
s_dur_a$nn_end_score = as.factor(s_dur_a$nn_end_score)
s_dur_a = s_dur_a[s_dur_a$overlap == FALSE,]
s_dur_a = s_dur_a[ , !(names(s_dur_a) %in% c("overlap"))]
s_dur_c = read.csv(paste(cgn_path, "synvoirel_s_comb_comp-c2.csv", sep = ""))
s_dur_c$corpus = as.factor("cgn-c")
s_dur_c$register = as.factor("conversation")
s_dur_c$birth_year = as.integer(s_dur_c$birth_year)
s_dur_c$mean_hnr = as.factor(s_dur_c$mean_hnr)
s_dur_c$nn_end_score = as.factor(s_dur_c$nn_end_score)
s_dur_c = s_dur_c[s_dur_c$overlap == FALSE,]
s_dur_c = s_dur_c[ , !(names(s_dur_c) %in% c("overlap"))]
s_dur_d = read.csv(paste(cgn_path, "synvoirel_s_comb_comp-d2.csv", sep = ""))
s_dur_d$corpus = as.factor("cgn-d")
s_dur_d$register = as.factor("conversation")
s_dur_d$mean_hnr = as.factor(s_dur_d$mean_hnr)
s_dur_d$nn_end_score = as.factor(s_dur_d$nn_end_score)
s_dur_d = s_dur_d[s_dur_d$overlap == FALSE,]
s_dur_d = s_dur_d[ , !(names(s_dur_d) %in% c("overlap"))]
s_dur_ifadv = read.csv(paste(ifadv_path, "synvoirel_s_comb_ifadv.csv", sep = ""))
s_dur_ifadv$corpus = as.factor("ifadv")
s_dur_ifadv$register = as.factor("conversation")
s_dur_ifadv$mean_hnr = as.factor(s_dur_ifadv$mean_hnr)
s_dur_ifadv$nn_end_score = as.factor(s_dur_ifadv$nn_end_score)
levels(s_dur_ifadv$speaker_sex) = c("sex2", "sex1")
s_dur_ecsd = read.csv(paste(ecsd_path, "synvoirel_s_comb_ecsd.csv", sep = ""))
s_dur_ecsd$corpus = as.factor("ecsd")
s_dur_ecsd$register = as.factor("conversation")
s_dur_ecsd$mean_hnr = as.factor(s_dur_ecsd$mean_hnr)
s_dur_ecsd$nn_end_score = as.factor(s_dur_ecsd$nn_end_score)

s_dur_k = read.csv(paste(cgn_path, "synvoirel_s_comb_comp-k.csv", sep = ""))
s_dur_k$corpus = as.factor("cgn-k")
s_dur_k$register = as.factor("news")
s_dur_k$mean_hnr = as.factor(s_dur_k$mean_hnr)
s_dur_k$nn_end_score = as.factor(s_dur_k$nn_end_score)
s_dur_o = read.csv(paste(cgn_path, "synvoirel_s_comb_comp-o.csv", sep = ""))
s_dur_o$corpus = as.factor("cgn-o")
s_dur_o$register = as.factor("stories")
s_dur_o$mean_hnr = as.factor(s_dur_o$mean_hnr)
s_dur_o$nn_end_score = as.factor(s_dur_o$nn_end_score)

s_dur = rbind(s_dur_a, s_dur_c, s_dur_d, s_dur_ifadv, s_dur_ecsd, s_dur_o, s_dur_k)
s_dur$prev_mention = as.factor(s_dur$prev_mention)
s_dur$phrase_final = as.factor(s_dur$phrase_final)
#s_dur = s_dur[!(s_dur$mean_hnr == "--undefined--"),]
#s_dur$mean_hnr = as.numeric(s_dur$mean_hnr)
s_dur = s_dur[!(s_dur$nn_end_score == "--undefined--"),]
s_dur$nn_end_score = as.numeric(s_dur$nn_end_score) / 1000
s_dur = s_dur[rowSums(is.na(s_dur))<length(s_dur),]

s_dur$s_dur_kal = s_dur$kal_end - s_dur$kal_start
s_dur$s_dur_nn = s_dur$nn_end - s_dur$kal_start + 0.01
plot(density(s_dur$s_dur_kal), xlim = c(0, 0.5), lty = "dashed")
lines(density(s_dur$s_dur_nn))

# remove lines for which measurements cannot be trusted
s_dur = s_dur[!(is.na(s_dur$s_dur_nn) | is.na(s_dur$s_cog_window)), ]
s_dur = s_dur[!(s_dur$s_dur_nn < 0.005),]
is.na(s_dur$num_syl_pron) = !s_dur$num_syl_pron
# either limit based op preceding and following sounds
s_dur = s_dur[!(s_dur$prev_phon_pron %in% c("t", "d") | s_dur$next_phon_pron %in% c("j", "t", "d")),]
# or based on nn scores
plot(density(s_dur$nn_end_score))
s_dur = s_dur[s_dur$nn_start_score > 1.2 & s_dur$nn_end_score > 1.2,]
# remove underlyingly voiced final /s/ to check if it causes the effect
#s_dur = s_dur[s_dur$underlying_voice == "voiceless",]
s_dur = s_dur[s_dur$underlying_voice %in% c("voiced", "voiceless"),]
s_dur$underlying_voice = as.factor(as.character(s_dur$underlying_voice))
# remove NA lines
s_dur = s_dur[rowSums(is.na(s_dur))<length(s_dur),]

# remove unrepresentative outliers (Baayen, 2008, p. 243)
s_dur = s_dur[s_dur$s_dur_nn < 0.4,]

# make new predictors and get rid of unnecessary NAs
s_dur$stressed = s_dur$num_syl == s_dur$word_stress
s_dur$stressed = as.factor(s_dur$stressed)
s_dur$stress_dist = s_dur$num_syl - s_dur$word_stress

s_dur$log_bigf = log(s_dur$bigram_f + 1)

s_dur$next_phon_class = as.factor(sapply(s_dur$next_phon_pron, get_phon_class))
s_dur$prev_phon_class = as.factor(sapply(s_dur$prev_phon_pron, get_phon_class))
s_dur$prev_phon_class = relevel(s_dur$prev_phon_class, ref="V")
s_dur$next_phon_class = relevel(s_dur$next_phon_class, ref="V")

s_dur[s_dur$cow_wf == 0,]$cow_wf = 1
s_dur$log_wf = log(s_dur$cow_wf)
s_dur[is.na(s_dur$num_syl_pron),]$num_syl_pron = 0
s_dur[is.na(s_dur$lex_neb),]$lex_neb = 0
s_dur[is.na(s_dur$lex_neb_freq),]$lex_neb_freq = 0

# transform dependent variable
s_dur$log_s_dur = log(s_dur$s_dur_nn)
plot(density(s_dur$log_s_dur))

# remove lines for which continuous predictors are NA
nrow(s_dur)
s_dur = s_dur[!(is.na(s_dur$speech_rate_pron) | is.na(s_dur$base_dur) 
#                | is.na(s_dur$rel_freq1) 
#                | is.na(s_dur$rel_freq2)
                | is.na(s_dur$num_syl_pron) | is.na(s_dur$num_cons_pron)
                | is.na(s_dur$log_wf) | is.na(s_dur$lex_neb) | is.na(s_dur$log_bigf)
                | is.na(s_dur$stress_dist) | is.na(s_dur$syntax_f2)
                | is.na(s_dur$syntax_f3) | is.na(s_dur$syntax_f4)
                | is.na(s_dur$syntax_f5) | is.na(s_dur$syntax_f6)
                | is.na(s_dur$syntax_f7) | is.na(s_dur$syntax_f8)), ]
nrow(s_dur)

# convert syntax features to numeric
s_dur$syntax_f2 = as.numeric(s_dur$syntax_f2)
s_dur$syntax_f3 = as.numeric(s_dur$syntax_f3)
s_dur$syntax_f4 = as.numeric(s_dur$syntax_f4)
s_dur$syntax_f5 = as.numeric(s_dur$syntax_f5)
s_dur$syntax_f6 = as.numeric(s_dur$syntax_f6)
s_dur$syntax_f7 = as.numeric(s_dur$syntax_f7)
s_dur$syntax_f8 = as.numeric(s_dur$syntax_f8)

# Inspect collinearity
continuous = c("speech_rate_pron", "base_dur", "num_syl_pron", 
               "num_cons_pron", "log_wf", "lex_neb", "log_bigf", "stress_dist",
#               "rel_freq1", 
#               "rel_freq2",
               "syntax_f2", "syntax_f3", "syntax_f4", "syntax_f5",
               "syntax_f6", "syntax_f7", "syntax_f8")

corrplot(cor(s_dur[, continuous], use = "complete.obs"), method = "number")
vif(lm(log_s_dur ~ speech_rate_pron + base_dur + num_syl_pron 
       + num_cons_pron + log_wf + lex_neb + log_bigf , data=s_dur))
collin.fnc(na.omit(s_dur[, continuous]))$cnumber

# principle components
col_pred = s_dur[, c("speech_rate_pron", "base_dur", "num_syl_pron", "num_cons_pron", 
                     "log_wf", "lex_neb", "log_bigf", "stress_dist", "syntax_f2",
#                     "rel_freq1", 
#                     "rel_freq2",
                     "syntax_f3", "syntax_f4", "syntax_f5", "syntax_f6", "syntax_f7",
                     "syntax_f8")]
col_pred_pca = prcomp(col_pred, center = T, scale. = T)
summary(col_pred_pca)

rotation_df = as.data.frame(col_pred_pca$rotation)
rotation_df_ordered = rotation_df[order(-abs(rotation_df$PC3)),]
structure(rotation_df_ordered[, "PC3"],
          names=rownames(rotation_df_ordered))

# keep pc1 - pc9, threshold of 0.9 cumulative proportion reached

s_dur$PC1 = col_pred_pca$x[,1]
s_dur$PC2 = col_pred_pca$x[,2]
s_dur$PC3 = col_pred_pca$x[,3]
s_dur$PC4 = col_pred_pca$x[,4]
s_dur$PC5 = col_pred_pca$x[,5]
s_dur$PC6 = col_pred_pca$x[,6]
s_dur$PC7 = col_pred_pca$x[,7]
s_dur$PC8 = col_pred_pca$x[,8]
s_dur$PC9 = col_pred_pca$x[,9]
#s_dur$PC10 = col_pred_pca$x[,10]

continuous = c("PC1", "PC2", "PC3", "PC4", "PC5", "PC6", "PC7", "PC8", "PC9")
corrplot(cor(s_dur[, continuous], use = "complete.obs"), method = "number")

categorical = c("type_of_s", 
                "register", 
                "next_phon_class", 
                "prev_mention", "phrase_final", "underlying_voice")

# remove lines for which categorical predictors are NA
nrow(s_dur)
s_dur = s_dur[!(is.na(s_dur$type_of_s) | is.na(s_dur$next_phon_class) 
                | is.na(s_dur$prev_mention) | is.na(s_dur$register)
                | is.na(s_dur$phrase_final)), ]
nrow(s_dur)

s_dur = s_dur[s_dur$type_of_s %in% c("S", "PL"),]
s_dur$type_of_s = as.factor(as.character(s_dur$type_of_s))
s_dur$type_of_s = relevel(s_dur$type_of_s, ref="S")

cat_ass = matrix(c(cramerV(table(s_dur[,c("type_of_s", "type_of_s")]), bias.correct = TRUE),
                   cramerV(table(s_dur[,c("type_of_s", "register")]), bias.correct = TRUE),
                   cramerV(table(s_dur[,c("type_of_s", "next_phon_class")]), bias.correct = TRUE),
                   cramerV(table(s_dur[,c("type_of_s", "prev_mention")]), bias.correct = TRUE),
                   cramerV(table(s_dur[,c("type_of_s", "phrase_final")]), bias.correct = TRUE),
                   cramerV(table(s_dur[,c("type_of_s", "underlying_voice")]), bias.correct = TRUE),
                   cramerV(table(s_dur[,c("register", "type_of_s")]), bias.correct = TRUE),
                   cramerV(table(s_dur[,c("register", "register")]), bias.correct = TRUE),
                   cramerV(table(s_dur[,c("register", "next_phon_class")]), bias.correct = TRUE),
                   cramerV(table(s_dur[,c("register", "prev_mention")]), bias.correct = TRUE),
                   cramerV(table(s_dur[,c("register", "phrase_final")]), bias.correct = TRUE),
                   cramerV(table(s_dur[,c("register", "underlying_voice")]), bias.correct = TRUE),
                   cramerV(table(s_dur[,c("next_phon_class", "type_of_s")]), bias.correct = TRUE),
                   cramerV(table(s_dur[,c("next_phon_class", "register")]), bias.correct = TRUE),
                   cramerV(table(s_dur[,c("next_phon_class", "next_phon_class")]), bias.correct = TRUE),
                   cramerV(table(s_dur[,c("next_phon_class", "prev_mention")]), bias.correct = TRUE),
                   cramerV(table(s_dur[,c("next_phon_class", "phrase_final")]), bias.correct = TRUE),
                   cramerV(table(s_dur[,c("next_phon_class", "underlying_voice")]), bias.correct = TRUE),
                   cramerV(table(s_dur[,c("prev_mention", "type_of_s")]), bias.correct = TRUE),
                   cramerV(table(s_dur[,c("prev_mention", "register")]), bias.correct = TRUE),
                   cramerV(table(s_dur[,c("prev_mention", "next_phon_class")]), bias.correct = TRUE),
                   cramerV(table(s_dur[,c("prev_mention", "prev_mention")]), bias.correct = TRUE),
                   cramerV(table(s_dur[,c("prev_mention", "phrase_final")]), bias.correct = TRUE),
                   cramerV(table(s_dur[,c("prev_mention", "underlying_voice")]), bias.correct = TRUE),
                   cramerV(table(s_dur[,c("phrase_final", "type_of_s")]), bias.correct = TRUE),
                   cramerV(table(s_dur[,c("phrase_final", "register")]), bias.correct = TRUE),
                   cramerV(table(s_dur[,c("phrase_final", "next_phon_class")]), bias.correct = TRUE),
                   cramerV(table(s_dur[,c("phrase_final", "prev_mention")]), bias.correct = TRUE),
                   cramerV(table(s_dur[,c("phrase_final", "phrase_final")]), bias.correct = TRUE),
                   cramerV(table(s_dur[,c("phrase_final", "underlying_voice")]), bias.correct = TRUE),
                   cramerV(table(s_dur[,c("underlying_voice", "type_of_s")]), bias.correct = TRUE),
                   cramerV(table(s_dur[,c("underlying_voice", "register")]), bias.correct = TRUE),
                   cramerV(table(s_dur[,c("underlying_voice", "next_phon_class")]), bias.correct = TRUE),
                   cramerV(table(s_dur[,c("underlying_voice", "prev_mention")]), bias.correct = TRUE),
                   cramerV(table(s_dur[,c("underlying_voice", "phrase_final")]), bias.correct = TRUE),
                   cramerV(table(s_dur[,c("underlying_voice", "underlying_voice")]), bias.correct = TRUE)),
                 nrow = 6, ncol = 6, byrow = T, dimnames = list(
                   categorical,
                   categorical))

corrplot(cat_ass, method = "number")

cat_con = matrix(c(sqrt(summary(lm(PC1 ~ type_of_s, data = s_dur))$r.squared),
                   sqrt(summary(lm(PC2 ~ type_of_s, data = s_dur))$r.squared),
                   sqrt(summary(lm(PC3 ~ type_of_s, data = s_dur))$r.squared),
                   sqrt(summary(lm(PC4 ~ type_of_s, data = s_dur))$r.squared),
                   sqrt(summary(lm(PC5 ~ type_of_s, data = s_dur))$r.squared),
                   sqrt(summary(lm(PC6 ~ type_of_s, data = s_dur))$r.squared),
                   sqrt(summary(lm(PC7 ~ type_of_s, data = s_dur))$r.squared),
                   sqrt(summary(lm(PC8 ~ type_of_s, data = s_dur))$r.squared),
                   sqrt(summary(lm(PC9 ~ type_of_s, data = s_dur))$r.squared),
#                   sqrt(summary(lm(rel_freq1 ~ type_of_s, data = s_dur))$r.squared),
                   sqrt(summary(lm(PC1 ~ register, data = s_dur))$r.squared),
                   sqrt(summary(lm(PC2 ~ register, data = s_dur))$r.squared),
                   sqrt(summary(lm(PC3 ~ register, data = s_dur))$r.squared),
                   sqrt(summary(lm(PC4 ~ register, data = s_dur))$r.squared),
                   sqrt(summary(lm(PC5 ~ register, data = s_dur))$r.squared),
                   sqrt(summary(lm(PC6 ~ register, data = s_dur))$r.squared),
                   sqrt(summary(lm(PC7 ~ register, data = s_dur))$r.squared),
                   sqrt(summary(lm(PC8 ~ register, data = s_dur))$r.squared),
                   sqrt(summary(lm(PC9 ~ register, data = s_dur))$r.squared),
#                   sqrt(summary(lm(rel_freq1 ~ register, data = s_dur))$r.squared),
                   sqrt(summary(lm(PC1 ~ next_phon_class, data = s_dur))$r.squared),
                   sqrt(summary(lm(PC2 ~ next_phon_class, data = s_dur))$r.squared),
                   sqrt(summary(lm(PC3 ~ next_phon_class, data = s_dur))$r.squared),
                   sqrt(summary(lm(PC4 ~ next_phon_class, data = s_dur))$r.squared),
                   sqrt(summary(lm(PC5 ~ next_phon_class, data = s_dur))$r.squared),
                   sqrt(summary(lm(PC6 ~ next_phon_class, data = s_dur))$r.squared),
                   sqrt(summary(lm(PC7 ~ next_phon_class, data = s_dur))$r.squared),
                   sqrt(summary(lm(PC8 ~ next_phon_class, data = s_dur))$r.squared),
                   sqrt(summary(lm(PC9 ~ next_phon_class, data = s_dur))$r.squared),
#                   sqrt(summary(lm(rel_freq1 ~ next_phon_class, data = s_dur))$r.squared),
                   sqrt(summary(lm(PC1 ~ prev_mention, data = s_dur))$r.squared),
                   sqrt(summary(lm(PC2 ~ prev_mention, data = s_dur))$r.squared),
                   sqrt(summary(lm(PC3 ~ prev_mention, data = s_dur))$r.squared),
                   sqrt(summary(lm(PC4 ~ prev_mention, data = s_dur))$r.squared),
                   sqrt(summary(lm(PC5 ~ prev_mention, data = s_dur))$r.squared),
                   sqrt(summary(lm(PC6 ~ prev_mention, data = s_dur))$r.squared),
                   sqrt(summary(lm(PC7 ~ prev_mention, data = s_dur))$r.squared),
                   sqrt(summary(lm(PC8 ~ prev_mention, data = s_dur))$r.squared),
                   sqrt(summary(lm(PC9 ~ prev_mention, data = s_dur))$r.squared),
#                   sqrt(summary(lm(rel_freq1 ~ prev_mention, data = s_dur))$r.squared),
                   sqrt(summary(lm(PC1 ~ phrase_final, data = s_dur))$r.squared),
                   sqrt(summary(lm(PC2 ~ phrase_final, data = s_dur))$r.squared),
                   sqrt(summary(lm(PC3 ~ phrase_final, data = s_dur))$r.squared),
                   sqrt(summary(lm(PC4 ~ phrase_final, data = s_dur))$r.squared),
                   sqrt(summary(lm(PC5 ~ phrase_final, data = s_dur))$r.squared),
                   sqrt(summary(lm(PC6 ~ phrase_final, data = s_dur))$r.squared),
                   sqrt(summary(lm(PC7 ~ phrase_final, data = s_dur))$r.squared),
                   sqrt(summary(lm(PC8 ~ phrase_final, data = s_dur))$r.squared),
                   sqrt(summary(lm(PC9 ~ phrase_final, data = s_dur))$r.squared),
#                   sqrt(summary(lm(rel_freq1 ~ phrase_final, data = s_dur))$r.squared),
                   sqrt(summary(lm(PC1 ~ underlying_voice, data = s_dur))$r.squared),
                   sqrt(summary(lm(PC2 ~ underlying_voice, data = s_dur))$r.squared),
                   sqrt(summary(lm(PC3 ~ underlying_voice, data = s_dur))$r.squared),
                   sqrt(summary(lm(PC4 ~ underlying_voice, data = s_dur))$r.squared),
                   sqrt(summary(lm(PC5 ~ underlying_voice, data = s_dur))$r.squared),
                   sqrt(summary(lm(PC6 ~ underlying_voice, data = s_dur))$r.squared),
                   sqrt(summary(lm(PC7 ~ underlying_voice, data = s_dur))$r.squared),
                   sqrt(summary(lm(PC8 ~ underlying_voice, data = s_dur))$r.squared),
                   sqrt(summary(lm(PC9 ~ underlying_voice, data = s_dur))$r.squared)
#                   sqrt(summary(lm(rel_freq1 ~ underlying_voice, data = s_dur))$r.squared)
), 
nrow = 6, ncol = 9, byrow = T, dimnames = list(
  categorical,
  continuous))

corrplot(cat_con, method = "number")

### prepare dataset
# get rid of unnecessary columns
drop = c("ptan", "ptaf", "mean_hnr", "next_phon_dur", "prev_phon_dur", "birth_year", "speaker_sex", 
         "proportion_voiced2", "per_mil_wf", "prev_word",
         "word_class", "word_pos", "next_phon", "prev_phon", "sent_i", "word_sent_i", "word_chunk_i", 
         "chan")
s_dur = s_dur[ , !(names(s_dur) %in% drop)]

s_dur = na.omit(s_dur)

#s_dur = s_dur[s_dur$type_of_s %in% c("S", "PL"),]
#s_dur$type_of_s = as.factor(as.character(s_dur$type_of_s))
#s_dur$type_of_s = relevel(s_dur$type_of_s, ref="S")

### try Mirjam's residuals method

control = lmer(log_s_dur ~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9
#               + PC10
               + next_phon_class + prev_mention + underlying_voice
#               + rel_freq2
               + (1 | speaker) + (1 | word_ort),
               control = lmerControl(optCtrl = list(maxfun = 1e6, ftol_abs = 1e-8)),
               data=s_dur)

s_dur$dur_resid = resid(control)
s_dur_trim = s_dur[abs(scale(s_dur$dur_resid)) < 2.5,]

control_trim = lmer(log_s_dur ~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9
#                    + PC10
                    + next_phon_class + prev_mention + underlying_voice
#                    + rel_freq2
                    + (1 | speaker) + (1 | word_ort),
                    control = lmerControl(optCtrl = list(maxfun = 1e6, ftol_abs = 1e-8)),
                    data=s_dur_trim)

par(mfrow=c(2,2))
plot(predict(control), resid(control))
qqnorm(resid(control), main = "Original")
qqline(resid(control))
plot(predict(control_trim), resid(control_trim))
qqnorm(resid(control_trim), main = "Trimmed")
qqline(resid(control_trim))
par(mfrow=c(1,1))

s_dur_trim$dur_resid = resid(control_trim)

interest = lm(dur_resid ~ type_of_s*register,
              data=s_dur_trim)
anova(interest)

p1 = plot(effect("type_of_s:register", interest, x.var = "type_of_s"), multiline = T, 
#     layout = c(3,1), alternating = F, rotx = 45, 
     ci.style = "bars",
# key.args=list(space="right"),
#row = 1, nrow = 1, ncol = 2, more = TRUE,
     main = "Duration", xlab = "Morphological status", ylab = "Residuals in log(seconds)")

interest_conv = lm(dur_resid ~ type_of_s,
                   data=s_dur_trim[s_dur_trim$register == "conversation",])
summary(interest_conv)

interest_stor = lm(dur_resid ~ type_of_s,
                   data=s_dur_trim[s_dur_trim$register == "stories",])
summary(interest_stor)

interest_news = lm(dur_resid ~ type_of_s,
                   data=s_dur_trim[s_dur_trim$register == "news",])
summary(interest_news)

# back transform residuals:
s_dur_trim$dur_pred_lm = predict(interest)
s_dur_trim$dur_pred_lm_back = s_dur_trim$s_dur_nn - exp(s_dur_trim$log_s_dur - s_dur_trim$dur_pred_lm)
# Get means:
# note: first splitting dataset and using predictions from register-specific models yield same dur_pred_lm
mean_conv_s = mean(s_dur_trim[s_dur_trim$register == "conversation" & s_dur_trim$type_of_s == "S",]$dur_pred_lm_back)
mean_conv_pl = mean(s_dur_trim[s_dur_trim$register == "conversation" & s_dur_trim$type_of_s == "PL",]$dur_pred_lm_back)
mean_stor_s = mean(s_dur_trim[s_dur_trim$register == "stories" & s_dur_trim$type_of_s == "S",]$dur_pred_lm_back)
mean_stor_pl = mean(s_dur_trim[s_dur_trim$register == "stories" & s_dur_trim$type_of_s == "PL",]$dur_pred_lm_back)
mean_news_s = mean(s_dur_trim[s_dur_trim$register == "news" & s_dur_trim$type_of_s == "S",]$dur_pred_lm_back)
mean_news_pl = mean(s_dur_trim[s_dur_trim$register == "news" & s_dur_trim$type_of_s == "PL",]$dur_pred_lm_back)

mean_conv_s - mean_conv_pl
mean_news_s - mean_news_pl
# very small (2ms), but keep in mind that two-staged approach allows covariates to eat up as much variance as they can
# also having to exclude /s/s which were difficult to segment means that we can not analyse the complete range of reduction

# Centre of Gravity

control_cog = lmer(s_cog_window ~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9
                   + next_phon_class + prev_mention + underlying_voice
#                   + rel_freq2
                   + (1 | speaker) + (1 | word_ort),
                   control = lmerControl(optCtrl = list(maxfun = 1e6, ftol_abs = 1e-8)),
                   data=s_dur)

s_dur$cog_resid = resid(control_cog)
s_cog_trim = s_dur[abs(scale(s_dur$cog_resid)) < 2.5,]

control_cog_trim = lmer(s_cog_window ~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9
                        + next_phon_class + prev_mention + underlying_voice
#                        + rel_freq2
                        + (1 | speaker) + (1 | word_ort),
                        control = lmerControl(optCtrl = list(maxfun = 1e6, ftol_abs = 1e-8)),
                        data=s_cog_trim)

par(mfrow=c(2,2))
plot(predict(control_cog), resid(control_cog))
qqnorm(resid(control_cog), main = "Original")
qqline(resid(control_cog))
plot(predict(control_cog_trim), resid(control_cog_trim))
qqnorm(resid(control_cog_trim), main = "Trimmed")
qqline(resid(control_cog_trim))
par(mfrow=c(1,1))

s_cog_trim$cog_resid = resid(control_cog_trim)

interest_cog = lm(cog_resid ~ type_of_s*register,
                  data=s_cog_trim)

anova(interest_cog)

p2 = plot(effect("type_of_s:register", interest_cog, x.var = "type_of_s"), multiline = T, 
#     layout = c(3,1), alternating = F, rotx = 45, 
#     row = 1,nrow = 1,ncol = 2,
#     key.args = list(space = "right"),
     ci.style = "bars", main = "CoG", xlab = "Morphological status", ylab = "Residuals in Hz")

grid.arrange(p1, p2, ncol = 2)

interest_cog_conv = lm(cog_resid ~ type_of_s,
                       data=s_cog_trim[s_cog_trim$register == "conversation",])
summary(interest_cog_conv)

interest_cog_stor = lm(cog_resid ~ type_of_s,
                       data=s_cog_trim[s_cog_trim$register == "stories",])
summary(interest_cog_stor)

interest_cog_news = lm(cog_resid ~ type_of_s,
                       data=s_cog_trim[s_cog_trim$register == "news",])
summary(interest_cog_news)

# again differences are small (~25Hz), but two staged approach, and only 0-4000Hz range
s_cog_pred = effect("type_of_s:register", interest_cog, x.var = "type_of_s")$fit

# conversation
s_cog_pred[1] - s_cog_pred[2]

# stories
s_cog_pred[3] - s_cog_pred[4]

# news
s_cog_pred[5] - s_cog_pred[6]






# get_NDLOutcome = function(x) {
#   if (x == "S") {
#     return("NONMORPH")
#   } else if (x == "PL") {
#     return("PL")
#   } else if (x == "GEN-POSS") {
#     return("GENPOSS")
#   } else {
#     return("PART")
#   }
# }
# 
# get_ActFromRemainingCues = function(cues_str, outcome) {
#   cues = strsplit(cues_str, "_")
#   sum_act = 0
#   for (cue in cues[[1]]) {
#     sum_act = sum_act + ifelse(cue %in% row.names(ndl.w) & outcome %in% colnames(ndl.w), ndl.w[cue, outcome], 0)
#   }
#   #  sum_act = sum(sapply(cues, function(z) ndl.w[z, outcome]))
#   return(sum_act)
# }
# 
# get_ActDivFromRemainingCues = function(cues_str) {
#   cues = strsplit(cues_str, "_")
#   sum_act = 0
#   for (cue in cues[[1]]){
#     cue_present = cue %in% row.names(ndl.w)
#     sum_act = sum_act + ifelse(cue_present, sum(ndl.w[cue,]), 0)
#   }
#   #  sum_act = sum(sapply(cues, function(z) ndl.w[z,]))
#   return(sum_act)
# }
# 
# 
# # get ndl measures
# ndl.w = readRDS(paste(cgn_path, "ifadv_ndl_weights.rds", sep = ""))
# s_dur$ndl_outcome = sapply(as.character(s_dur$type_of_s), get_NDLOutcome)
# s_dur$priorMorph = sapply(s_dur$ndl_outcome, function(x) sum(ndl.w[,x]))
# s_dur$ActFromBoundaryDiphone = mapply(function(x, y) ifelse(x %in% row.names(ndl.w) & y %in% colnames(ndl.w)
#                                                             , ndl.w[x, y], 0), 
#                                       as.character(s_dur$ndl_boundary_diph), 
#                                       s_dur$ndl_outcome)
# s_dur$ActFromRemainingCues = mapply(get_ActFromRemainingCues,
#                                     as.character(s_dur$other_ndl_cues),
#                                     s_dur$ndl_outcome)
# s_dur$ActDivFromBoundaryDiphone = sapply(as.character(s_dur$ndl_boundary_diph),
#                                          function(x) ifelse(x %in% row.names(ndl.w), sum(ndl.w[x,]), 0))
# s_dur$ActDivFromRemainingCues = sapply(as.character(s_dur$other_ndl_cues),
#                                        get_ActDivFromRemainingCues)
# 
# 
# # principle components
# ndl_col_pred = s_dur[, c("priorMorph", "ActFromBoundaryDiphone", "ActFromRemainingCues", "ActDivFromBoundaryDiphone", "ActDivFromRemainingCues")]
# ndl_col_pred_pca = prcomp(ndl_col_pred, center = T, scale. = T)
# summary(ndl_col_pred_pca)
# 
# s_dur$NDL_PC1 = ndl_col_pred_pca$x[,1]
# s_dur$NDL_PC2 = ndl_col_pred_pca$x[,2]
# s_dur$NDL_PC3 = ndl_col_pred_pca$x[,3]
# s_dur$NDL_PC4 = ndl_col_pred_pca$x[,4]
# 
# #
# control_ndl = lmer(log_s_dur ~ NDL_PC1 + NDL_PC2 + NDL_PC3 + NDL_PC4 
#                + (1 | speaker) + (1 | word_ort),
#                control = lmerControl(optCtrl = list(maxfun = 1e6, ftol_abs = 1e-8)),
#                data=s_dur)
# 
# s_dur$ndl_dur_resid = resid(control_ndl)
# interest_ndl = lm(ndl_dur_resid ~ type_of_s*register,
#               data=s_dur)
# anova(interest)
# plot(effect("type_of_s:register", interest_ndl, x.var = "type_of_s"), multiline = T, 
#      ci.style = "bars", main = "Duration", xlab = "Morphological status", ylab = "Residuals in log(seconds)")
# 
