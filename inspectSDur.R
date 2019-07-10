library(lme4)
library(effects)
#library(languageR)
#library(corrplot)
#library(polycor)

if (Sys.info()[1] == "Darwin"){
  cgn_path = "/Volumes/tensusers/timzee/cgn/"
  ifadv_path = "/Volumes/tensusers/timzee/IFADVcorpus/"
  ecsd_path = "/Volumes/tensusers/timzee/ECSD/"
} else {
  cgn_path = "/vol/tensusers/timzee/cgn/"
  ifadv_path = "/vol/tensusers/timzee/IFADVcorpus/"
  ecsd_path = "/Volumes/tensusers/timzee/ECSD/"
}

s_dur_a = read.csv(paste(cgn_path, "comp-a_s_static.csv", sep = ""))
s_dur_a$corpus = as.factor("cgn-a")
s_dur_c = read.csv(paste(cgn_path, "comp-c_s_static.csv", sep = ""))
s_dur_c$corpus = as.factor("cgn-c")
s_dur_c$birth_year = as.integer(s_dur_c$birth_year)
s_dur_d = read.csv(paste(cgn_path, "comp-d_s_static.csv", sep = ""))
s_dur_d$corpus = as.factor("cgn-d")
s_dur_ifadv = read.csv(paste(ifadv_path, "ifadv_s_static.csv", sep = ""))
s_dur_ifadv$corpus = as.factor("ifadv")
s_dur_ifadv$mean_hnr = as.factor(s_dur_ifadv$mean_hnr)
levels(s_dur_ifadv$speaker_sex) = c("sex2", "sex1")
s_dur_ecsd = read.csv(paste(ecsd_path, "ecsd_s_static.csv", sep = ""))
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

# for now combine GEN levels:
levels(s_dur$type_of_s)
levels(s_dur$type_of_s) = c("GEN", "GEN", "PART", "PL", "S")
levels(s_dur$type_of_s)
# combine ifadv and ecsd to get rid of rank deficiency
#levels(s_dur$corpus) = c("cgn-a", "cgn-c", "cgn-d", "ifadv-ecsd", "ifadv-ecsd")

#s_dur$type_of_s = as.character(s_dur$type_of_s)
#s_dur = s_dur[s_dur$type_of_s != "OTHER",]
#s_dur$type_of_s = as.factor(s_dur$type_of_s)

# Add the following to selection script
# s_dur = s_dur[(s_dur$prev_phon_pron %in% c("[SPN]", "SIL") == FALSE) & (s_dur$next_phon_pron != "[SPN]"),]

s_dur$type_of_s = relevel(s_dur$type_of_s, ref="S")
#s_dur$num_cons_pron = as.numeric(s_dur$num_syl_pron)

is.na(s_dur$bigram_f) = !s_dur$bigram_f
s_dur$log_bigf = log10(s_dur$bigram_f)
#s_dur$num_cons_pron = as.factor(s_dur$num_cons_pron)

s_dur$stressed = s_dur$num_syl == s_dur$word_stress
s_dur$stressed = as.factor(s_dur$stressed)

vowels = c("@", "A", "AU", "E", "E2", "EI", "EU", "I", "O", "U", "UI", "a", "e", "i", "o", "u", "y")
liquids = c("r", "l")
approximants = c("j", "w")
nasals = c("n", "m", "N")
fricatives = c("G", "S", "Z", "f", "h", "s", "v", "x", "z")
plosives = c("b", "d", "g", "k", "p", "t")

#phon_classes = list("vowels"=vowels, "liquids"=liquids, "approximants"=approximants, "nasals"=nasals, "fricatives"=fricatives, "plosives"=plosives)
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

s_dur$next_phon_class = as.factor(sapply(s_dur$next_phon, get_phon_class))
s_dur$prev_phon_class = as.factor(sapply(s_dur$prev_phon_pron, get_phon_class))

# see https://statisticalhorizons.com/multicollinearity
# V has most cases by far for the prev_phon_class
s_dur$prev_phon_class = relevel(s_dur$prev_phon_class, ref="V")
s_dur$next_phon_class = relevel(s_dur$next_phon_class, ref="V")

# remove lines for which measurements failed
s_dur = s_dur[!is.na(s_dur$s_dur), ]

# inspect histogram
hist(s_dur$s_dur)

# remove unrepresentative outliers (Baayen, 2008, p. 243)
s_dur = s_dur[s_dur$s_dur < 0.4,]

# inspect qqplots for speaker
#qqmath(~s_dur|speaker, data = s_dur, layout = c(4,5,12))


# duration
#plot(s_dur$type_of_s, s_dur$s_dur)

# use m0 to spot collinearity
m0 = lmer(s_dur ~ type_of_s + speech_rate_pron + base_dur + num_syl_pron 
           + stressed + num_cons_pron + log_wf + lex_neb + log_bigf 
           + next_phon_class + prev_phon_class + prev_mention + phrase_final 
           + (1|speaker) + (1|word_ort), data=s_dur)
summary(m0)
#vcov(m0)

par(mfrow=c(2,3))
plot(fitted(m0), residuals(m0))
cor(fitted(m0), residuals(m0))
qqnorm(residuals(m0))
qqline(residuals(m0))
plot(density(residuals(m0)))

m1 = lmer(log10(s_dur) ~ type_of_s*corpus + speech_rate_pron + base_dur + num_syl_pron 
          + stressed + num_cons_pron + log_wf + lex_neb + log_bigf 
          + next_phon_class + prev_phon_class + prev_mention + phrase_final 
          + (1|speaker) + (1|word_ort), data=s_dur)
summary(m1)

plot(fitted(m1), residuals(m1))
cor(fitted(m1), residuals(m1))
qqnorm(residuals(m1))
qqline(residuals(m1))
plot(density(residuals(m1)))

plot(effect("type_of_s:corpus", m1, confidence.level=0.95))

m1b = lmer(log10(s_dur) ~ type_of_s + speech_rate_pron + base_dur + num_syl_pron 
          + stressed + num_cons_pron + log_wf + lex_neb + log_bigf 
          + next_phon_class + prev_phon_class + prev_mention + phrase_final 
          + (1+type_of_s|speaker) + (1|word_ort) + (1+type_of_s|corpus), data=s_dur)
summary(m1b)



s_dur_a = s_dur[s_dur$corpus == "cgn-a",]
s_dur_c = s_dur[s_dur$corpus == "cgn-c",]
s_dur_d = s_dur[s_dur$corpus == "cgn-d",]
s_dur_ifadv = s_dur[s_dur$corpus == "ifadv",]
s_dur_ecsd = s_dur[s_dur$corpus == "ecsd",]
s_dur_other = rbind(s_dur_ifadv, s_dur_ecsd)

m_a = lmer(log10(s_dur) ~ type_of_s + speech_rate_pron + base_dur + num_syl_pron 
              + stressed + num_cons_pron + log_wf + lex_neb + log_bigf 
              + next_phon_class + prev_phon_class + prev_mention + phrase_final 
              + (1|speaker) + (1|word_ort), data=s_dur_a)
summary(m_a)
plot(effect("type_of_s", m_a, confidence.level=0.95))


m_c = lmer(log10(s_dur) ~ type_of_s + speech_rate_pron + base_dur + num_syl_pron 
           + stressed + num_cons_pron + log_wf + lex_neb + log_bigf 
           + next_phon_class + prev_phon_class + prev_mention + phrase_final 
           + (1|speaker) + (1|word_ort), data=s_dur_c)
summary(m_c)
plot(effect("type_of_s", m_c, confidence.level=0.95))


m_d = lmer(log10(s_dur) ~ type_of_s + speech_rate_pron + base_dur + num_syl_pron 
           + stressed + num_cons_pron + log_wf + lex_neb + log_bigf 
           + next_phon_class + prev_phon_class + prev_mention + phrase_final 
           + (1|speaker) + (1|word_ort), data=s_dur_d)
summary(m_d)
plot(effect("type_of_s", m_d, confidence.level=0.95))


m_ifadv = lmer(log10(s_dur) ~ type_of_s + speech_rate_pron + base_dur + num_syl_pron 
           + stressed + num_cons_pron + log_wf + lex_neb + log_bigf 
           + next_phon_class + prev_phon_class + prev_mention + phrase_final 
           + (1|speaker) + (1|word_ort), data=s_dur_ifadv)
summary(m_ifadv)
plot(effect("type_of_s", m_ifadv, confidence.level=0.95))


m_ecsd = lmer(log10(s_dur) ~ type_of_s + speech_rate_pron + base_dur + num_syl_pron 
               + stressed + num_cons_pron + log_wf + lex_neb + log_bigf 
               + next_phon_class + prev_phon_class + prev_mention + phrase_final 
               + (1|speaker) + (1|word_ort), data=s_dur_ecsd)
summary(m_ecsd)
plot(effect("type_of_s", m_ecsd, confidence.level=0.95))

m_other = lmer(log10(s_dur) ~ type_of_s + speech_rate_pron + base_dur + num_syl_pron 
              + stressed + num_cons_pron + log_wf + lex_neb + log_bigf 
              + next_phon_class + prev_phon_class + prev_mention + phrase_final 
              + (1|speaker) + (1|word_ort), data=s_dur_other)
summary(m_other)
plot(effect("type_of_s", m_other, confidence.level=0.95))


#investigate genitives
#plot(s_dur[s_dur$type_of_s == "GEN-POSS",]$s_dur)
#text(s_dur[s_dur$type_of_s == "GEN-POSS",]$s_dur, labels = s_dur[s_dur$type_of_s == "GEN-POSS",]$word_ort, pos = 3, cex = 0.5)

# Creating a vif function that wroks with lmer
# function from https://github.com/aufrank/R-hacks/blob/master/mer-utils.R
vif.mer <- function (fit) {
  ## adapted from rms::vif
  v <- vcov(fit)
  nam <- names(fixef(fit))
  ## exclude intercepts
  ns <- sum(1 * (nam == "Intercept" | nam == "(Intercept)"))
  if (ns > 0) {
    v <- v[-(1:ns), -(1:ns), drop = FALSE]
    nam <- nam[-(1:ns)]
  }
  d <- diag(v)^0.5
  v <- diag(solve(v/(d %o% d)))
  names(v) <- nam
  v
}

vif.mer(m1)
# mainly num_cons_pron and prev_phon_classV
#cormat = hetcor(s_dur[,c("type_of_s", "speech_rate_pron", "base_dur",
#                         "num_syl_pron", "stressed", "num_cons_pron",
#                         "log_wf", "lex_neb", "log_bigf", "next_phon_class",
#                         "prev_phon_class", "proportion_voiced")])
#corrplot(cormat$correlations)


# Look at vignette from Bates et al 2018
#library(RePsychLing)

#vignette(package = "RePsychLing")



# now create maximal lmm

# max structure:

max_ran = matrix(c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
                   0, 1, 1, 0, 0, 0, 0, 0, 1, 1, 0, 1, 1), 
                 nrow = 2, ncol = 13, byrow = T, dimnames = list(
                   c("speaker","word_ort"),
                   c("type_of_s","speech_rate_pron","base_dur",
                     "num_syl", "stressed", "num_cons", "log_wf", 
                     "lex_neb", "log_bigf", "next_phon_class",
                     "prev_phon_class", "prev_mention", "phrase_final")))

maxLMM = lmer(s_dur ~ type_of_s + speech_rate_pron + base_dur + num_syl_pron 
              + stressed + num_cons_pron + log_wf + lex_neb + log_bigf 
              + next_phon_class + prev_phon_class 
              + (1 + type_of_s + speech_rate_pron + base_dur + num_syl_pron 
                 + stressed + num_cons_pron + log_wf + lex_neb + log_bigf 
                 + next_phon_class + prev_phon_class | speaker) 
              + (1 + speech_rate_pron + base_dur + log_bigf 
                 + next_phon_class | word_ort), data=s_dur)
#summary(maxLMM)
saveRDS(maxLMM, file = paste(f_path, "maxLMM.rds", sep = ""))

# we get a degenerate covariance matrix (see 1.00)

#Random effects:
# Groups    Name              Variance  Std.Dev. Corr                   
# word_ort  (Intercept)       2.528e-04 0.015900                        
# speaker   (Intercept)       5.290e-05 0.007273                        
#           type_of_sGEN-POSS 2.491e-03 0.049907 -0.17                  
#           type_of_sGEN-TIME 1.630e-05 0.004038 -0.05 -0.44            
#           type_of_sPART     8.626e-05 0.009288 -0.11 -0.47  1.00      
#           type_of_sPL       2.753e-05 0.005247 -0.08  0.98 -0.27 -0.32
# Residual                   1.428e-03 0.037785                        
#Number of obs: 58372, groups:  word_ort, 1443; speaker, 231


