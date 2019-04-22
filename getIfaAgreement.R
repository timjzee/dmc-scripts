library(effects)
library(MASS)
library(car)
library(rcompanion)
library(lme4)
library(boxcoxmix)

if (Sys.info()[1] == "Darwin"){
  f_path = "/Volumes/tensusers/timzee/IFAcorpus/"
  wd = "/Volumes/timzee/GitHub/dmc-scripts/"
} else {
  f_path = "/vol/tensusers/timzee/IFAcorpus/"
  wd = "/home/timzee/GitHub/dmc-scripts/"
}

source(paste(wd, "gwet/paired_t_test_for_agreement_coefficients.r", sep = ""))
source(paste(wd, "gwet/weights.gen.r", sep = ""))

vowels = c("@", "2", "9", "9+", "a", "A", "e", "E", "E+", "E~", "i", "I", "o", "O", "o+", "O+", "O~", "u", "y", "Y")
liquids = c("r", "l")
approximants = c("j", "w")
nasals = c("n", "m", "N", "J")
fricatives = c("f", "G", "h", "s", "S", "v", "x", "z", "Z")
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
  } else if (!is.na(x) & x == "*"){
    return("SIL")
  } else {
    return(NA)
  }
}

get_prev_phon_class = function(x) {
  prev_pc = NA
  xi = as.integer(x)
  if (!is.na(agr_man$phon_class[xi]) & xi > 1) {
    while(is.na(prev_pc)) {
      xi = xi - 1
      prev_pc = as.character(agr_man$phon_class[xi])
    }
  }
  return(prev_pc)
}

agr2 = read.csv(paste(f_path, "validation_data_small_pos.csv", sep = ""))
threshold = 0.02
agr2$ifa_kaldi_agr = agr2$ifa_kaldi_diff < threshold
agr2$ifa_tim_agr = agr2$ifa_tim_diff < threshold
agr2$tim_kaldi_agr = agr2$tim_kaldi_diff < threshold

#######

agr2$tim_lab_bool = !is.na(agr2$tim_labels)
num_tim_lab = table(agr2$sentence, agr2$tim_lab_bool)
num_l = num_tim_lab[,"TRUE"]
files = rownames(num_tim_lab)
tl_df = data.frame(files = rownames(num_tim_lab), num_l = num_tim_lab[,"TRUE"], stringsAsFactors = FALSE)
man_files = tl_df[tl_df$num_l > 0,]$files

agr_man = agr2[agr2$sentence %in% man_files,]
agr_man2 = agr_man
agr_man2$ifa_labels = as.character(agr_man2$ifa_labels)
agr_man2$ifa_labels[is.na(agr_man2$ifa_labels)] = "NA"
agr_man2$ifa_labels = as.factor(agr_man2$ifa_labels)
agr_man2$kaldi_labels = as.character(agr_man2$kaldi_labels)
agr_man2$kaldi_labels[is.na(agr_man2$kaldi_labels)] = "NA"
agr_man2$kaldi_labels = as.factor(agr_man2$kaldi_labels)
agr_man2$tim_labels = as.character(agr_man2$tim_labels)
agr_man2$tim_labels[is.na(agr_man2$tim_labels)] = "NA"
agr_man2$tim_labels = as.factor(agr_man2$tim_labels)

# let's get inter-annotator agreements and test the difference (including insertions)
ttest.fleiss(agr_man2[, c(2,4)], agr_man2[, c(2,3)])

# let's try without insertions
sub_agr = agr2[!is.na(agr2$ifa_labels) & !is.na(agr2$kaldi_labels) & !is.na(agr2$tim_labels),]
ttest.fleiss(sub_agr[, c(2,4)], sub_agr[, c(2,3)])

# we can make a confusion matrix
# first get respective phonetic classes
agr_man2$ifa_class = as.factor(sapply(agr_man2$ifa_labels, get_phon_class))
agr_man2$kaldi_class = as.factor(sapply(agr_man2$kaldi_labels, get_phon_class))
agr_man2$tim_class = as.factor(sapply(agr_man2$tim_labels, get_phon_class))
# turn NA into 'NA'
agr_man2$ifa_class = as.character(agr_man2$ifa_class)
agr_man2$ifa_class[is.na(agr_man2$ifa_class)] = "NA"
agr_man2$ifa_class = as.factor(agr_man2$ifa_class)
agr_man2$kaldi_class = as.character(agr_man2$kaldi_class)
agr_man2$kaldi_class[is.na(agr_man2$kaldi_class)] = "NA"
agr_man2$kaldi_class = as.factor(agr_man2$kaldi_class)
agr_man2$tim_class = as.character(agr_man2$tim_class)
agr_man2$tim_class[is.na(agr_man2$tim_class)] = "NA"
agr_man2$tim_class = as.factor(agr_man2$tim_class)

confusion_i_k = table(agr_man2$ifa_class, agr_man2$kaldi_class)
confusion_i_t = table(agr_man2$ifa_class, agr_man2$tim_class)
confusion_i_k
confusion_i_t

par(mfrow = c(1,3))
agr_tbl = table(agr_man[,c("ifa_kaldi_agr")])
prop_tbl = prop.table(agr_tbl)
num_i_k = nrow(agr_man[is.na(agr_man$ifa_kaldi_diff) == FALSE,])
barplot(prop_tbl, main = paste("ifa - kaldi N=", as.character(num_i_k), sep = ""), ylim = c(0, 1))
agr_tbl = table(agr_man[,c("ifa_tim_agr")])
prop_tbl = prop.table(agr_tbl)
num_i_t = nrow(agr_man[is.na(agr_man$ifa_tim_diff) == FALSE,])
barplot(prop_tbl, main = paste("ifa - tim N=", as.character(num_i_t), sep = ""), ylim = c(0, 1))
agr_tbl = table(agr_man[,c("tim_kaldi_agr")])
prop_tbl = prop.table(agr_tbl)
num_t_k = nrow(agr_man[is.na(agr_man$tim_kaldi_diff) == FALSE,])
barplot(prop_tbl, main = paste("tim - kaldi N=", as.character(num_t_k), sep = ""), ylim = c(0, 1))



agr_man$phon_class = as.factor(sapply(agr_man$ifa_labels, get_phon_class))
agr_man$phon_id = as.factor(1:nrow(agr_man))
# get indices of phones in sentence
agr_man$phon_num = unlist(sapply(unique(as.character(agr_man$sentence)), function(x) 1:nrow(agr_man[agr_man$sentence == x,])))
# get preceding ifa phon class
#agr_man$prev_phon_class = as.factor(c(NA, as.character(agr_man$phon_class[1:nrow(agr_man) - 1])))
agr_man$prev_phon_class = as.factor(sapply(agr_man$phon_id, get_prev_phon_class))
agr_man[agr_man$phon_num == 1,]$prev_phon_class = NA

agr_m_i_k = agr_man[, c(1,2,4,6,8,9,13,14,15,16)]
agr_m_i_k$annotator2 = "Kaldi"
names(agr_m_i_k) = c("sentence", "ifa_labels", "annot2_labels", "diff", "position", "agreement", "phon_class", "phon_id", "phon_num", "prev_phon_class", "annotator2")
agr_m_i_t = agr_man[, c(1,2,3,5,8,10,13,14,15,16)]
agr_m_i_t$annotator2 = "Tim"
names(agr_m_i_t) = c("sentence", "ifa_labels", "annot2_labels", "diff", "position", "agreement", "phon_class", "phon_id", "phon_num", "prev_phon_class", "annotator2")
agr_long = rbind(agr_m_i_k, agr_m_i_t)
agr_long$annotator2 = as.factor(agr_long$annotator2)
agr_long[agr_long$position == "SIL",]$position = NA
agr_long2 = agr_long[agr_long$diff < 1 & agr_long$phon_num != 1,]
agr_long2 = agr_long2[!is.na(agr_long2$sentence),]
agr_long2$speaker = as.factor(substr(agr_long2$sentence, 1, 4))

#mod1 = glm(agreement ~ annotator2*position + phon_class, data = agr_long, family = binomial)
#summary(mod1)

#par(mfrow = c(1,1))
#plot(effect("phon_class", mod1))
#plot(effect("annotator2:position", mod1))


#hist((agr_long2$diff^(1/3)))
#plot(density(agr_long2$diff^(1/3), na.rm = TRUE))
#agr_long2$diff_cube_rt = agr_long2$diff^(1/3)
#agr_long2 = agr_long2[rowSums(is.na(agr_long2)) != ncol(agr_long2),]
#mod2 = lm(diff_cube_rt ~ annotator2*position + phon_class, data = agr_long2)

#do Box-Cox test instead to normalize boundary difference
#Box-Cox only works with positive response variable
agr_long2$diff = agr_long2$diff + 0.0001
mod2 = lm(diff ~ annotator2*position + phon_class + prev_phon_class, data = agr_long2)
bc = boxcox(mod2)
lambda = bc$x[which.max(bc$y)]
mod3 = lm(diff^lambda ~ annotator2*position + phon_class + prev_phon_class, data = agr_long2)
qqnorm(mod2$residuals)
qqline(mod2$residuals)
qqnorm(mod3$residuals)
qqline(mod3$residuals)

summary(mod3)
# get collinearity estimates
vif(mod3)
cramerV(as.character(agr_long2$annotator2), as.character(agr_long2$position), bias.correct=TRUE)
cramerV(as.character(agr_long2$annotator2), as.character(agr_long2$phon_class), bias.correct=TRUE)
cramerV(as.character(agr_long2$annotator2), as.character(agr_long2$prev_phon_class), bias.correct=TRUE)
cramerV(as.character(agr_long2$position), as.character(agr_long2$phon_class), bias.correct=TRUE)
cramerV(as.character(agr_long2$position), as.character(agr_long2$prev_phon_class), bias.correct=TRUE)
cramerV(as.character(agr_long2$phon_class), as.character(agr_long2$prev_phon_class), bias.correct=TRUE)
# Collinearity due to phonotactics
# high vif of interaction term is due to its association with the main predictors that form it

par(mfrow = c(1,1))
plot(effect("phon_class", mod3))
# result shows us we should not consider sentence initial phones (mostly silences) as these have a difference of 0 by definition
plot(effect("prev_phon_class", mod3))
plot(effect("annotator2:position", mod3))

#######

i_k_ins = agr2[(is.na(agr2$ifa_labels) & !is.na(agr2$kaldi_labels)) | (!is.na(agr2$ifa_labels) & is.na(agr2$kaldi_labels)), ]
prop_k_ins = nrow(i_k_ins) / nrow(agr2[!is.na(agr2$kaldi_labels) | !is.na(agr2$ifa_labels),])
prop_k_ins
i_t_ins = agr2[(is.na(agr2$ifa_labels) & !is.na(agr2$tim_labels)) | (!is.na(agr2$ifa_labels) & is.na(agr2$tim_labels)), ]
prop_t_ins = nrow(i_t_ins) / nrow(agr2[!is.na(agr2$tim_labels) | !is.na(agr2$ifa_labels),])
prop_t_ins

par(mfrow = c(1,3))
agr_tbl = table(agr2[,agr2$ifa_kaldi_agr])
prop_tbl = prop.table(agr_tbl)
num_i_k = nrow(agr2[is.na(agr2$ifa_kaldi_diff) == FALSE,])
barplot(prop_tbl, main = paste("ifa - kaldi N = ", as.character(num_i_k), sep = ""), ylim = c(0, 1))
agr_tbl = table(agr2[,agr2$ifa_tim_agr])
prop_tbl = prop.table(agr_tbl)
num_i_t = nrow(agr2[is.na(agr2$ifa_tim_diff) == FALSE,])
barplot(prop_tbl, main = paste("ifa - tim N = ", as.character(num_i_t), sep = ""), ylim = c(0, 1))
agr_tbl = table(agr2[,agr2$tim_kaldi_agr])
prop_tbl = prop.table(agr_tbl)
num_t_k = nrow(agr2[is.na(agr2$tim_kaldi_diff) == FALSE,])
barplot(prop_tbl, main = paste("tim - kaldi N = ", as.character(num_t_k), sep = ""), ylim = c(0, 1))

s_agr2 = agr2[agr2$kaldi_labels == "s" & agr2$kaldi_pos == "E",]
s_agr2 = s_agr2[rowSums(is.na(s_agr2)) != ncol(s_agr2),]

par(mfrow = c(1,3))
agr_tbl = table(s_agr2[,c(9)])
prop_tbl = prop.table(agr_tbl)
num_i_k = nrow(s_agr2[is.na(s_agr2$ifa_kaldi_diff) == FALSE,])
barplot(prop_tbl, main = paste("ifa - kaldi N=", as.character(num_i_k), sep = ""), ylim = c(0, 1))
agr_tbl = table(s_agr2[,c(10)])
prop_tbl = prop.table(agr_tbl)
num_i_t = nrow(s_agr2[is.na(s_agr2$ifa_tim_diff) == FALSE,])
barplot(prop_tbl, main = paste("ifa - tim N=", as.character(num_i_t), sep = ""), ylim = c(0, 1))
agr_tbl = table(s_agr2[,c(11)])
prop_tbl = prop.table(agr_tbl)
num_t_k = nrow(s_agr2[is.na(s_agr2$tim_kaldi_diff) == FALSE,])
barplot(prop_tbl, main = paste("tim - kaldi N=", as.character(num_t_k), sep = ""), ylim = c(0, 1))

sub_agr = agr2[!is.na(agr2$ifa_labels) & !is.na(agr2$kaldi_labels) & !is.na(agr2$tim_labels),]

par(mfrow = c(1,3))
agr_tbl = table(sub_agr[,c(9)])
prop_tbl = prop.table(agr_tbl)
num_i_k = nrow(sub_agr[is.na(sub_agr$ifa_kaldi_diff) == FALSE,])
barplot(prop_tbl, main = paste("ifa - kaldi N=", as.character(num_i_k), sep = ""), ylim = c(0, 1))
agr_tbl = table(sub_agr[,c(10)])
prop_tbl = prop.table(agr_tbl)
num_i_t = nrow(sub_agr[is.na(sub_agr$ifa_tim_diff) == FALSE,])
barplot(prop_tbl, main = paste("ifa - tim N=", as.character(num_i_t), sep = ""), ylim = c(0, 1))
agr_tbl = table(sub_agr[,c(11)])
prop_tbl = prop.table(agr_tbl)
num_t_k = nrow(sub_agr[is.na(sub_agr$tim_kaldi_diff) == FALSE,])
barplot(prop_tbl, main = paste("tim - kaldi N=", as.character(num_t_k), sep = ""), ylim = c(0, 1))
