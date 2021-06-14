########
# Distribution Study

library(ggplot2)
library(rcompanion)

plot_aodml_effect = function(var_data, var_mod, predictor_var, moderator_var, constant_vars, dependent_var, predictor_lab=NULL, moderator_lab=NULL, dependent_lab=NULL, moderator_values=NULL, backtransform=NULL){
  if (is.null(backtransform)){
    backtransform = T
  }
  if (!backtransform){
    dependent_var_orig = dependent_var
    dependent_var = paste(dependent_var, "logit", sep = "_")
  }
  if (is.null(predictor_lab)){
    predictor_lab = predictor_var
  }
  if (is.null(moderator_lab)){
    moderator_lab = moderator_var
  }
  if (is.null(dependent_lab)){
    dependent_lab = dependent_var
  }
  if (is.null(moderator_values)){
    moderator_values = "minmedmax"
  }
  
  backtransform_vec = rep(backtransform, nrow(var_data))
  var2 = var_data
  if(!backtransform){
    var_data[[dependent_var]] = log(var_data[[dependent_var_orig]]/(1 - var_data[[dependent_var_orig]]))
  }
  for (v in constant_vars){
    var2[[v]] = median(var_data[[v]])
  }
  if (moderator_values == "minmedmax" | moderator_values == "minmeanmax"){
    var2[[moderator_var]] = max(var_data[[moderator_var]])
    mod_max_lab = "Max"
  } else if (moderator_values == "2_4_6"){
    var2[[moderator_var]] = 6
    mod_max_lab = "6"
  } else if (moderator_values == "-4_0_4"){
    var2[[moderator_var]] = 4
    mod_max_lab = "4"
  } else if (moderator_values == "meansd"){
    var2[[moderator_var]] = mean(var_data[[moderator_var]]) + sd(var_data[[moderator_var]])
    mod_max_lab = "+SD"
  }
  betabin_pred = predict(var_mod, se.fit = T, newdata = var2)
  s_prop_pred_max = ifelse(backtransform_vec, plogis(betabin_pred$fit), betabin_pred$fit)
  s_prop_lo_max = ifelse(backtransform_vec, plogis(betabin_pred$fit - qnorm(.975) * betabin_pred$se.fit), betabin_pred$fit - qnorm(.975) * betabin_pred$se.fit)
  s_prop_hi_max = ifelse(backtransform_vec, plogis(betabin_pred$fit + qnorm(.975) * betabin_pred$se.fit), betabin_pred$fit + qnorm(.975) * betabin_pred$se.fit)
  
  if (moderator_values == "minmedmax"){
    var2[[moderator_var]] = median(var_data[[moderator_var]])
    mod_med_lab = "Median"
  } else if (moderator_values == "minmeanmax"){
    var2[[moderator_var]] = mean(var_data[[moderator_var]])
    mod_med_lab = "Mean"
  } else if (moderator_values == "2_4_6"){
    var2[[moderator_var]] = 4
    mod_med_lab = "4"
  } else if (moderator_values == "-4_0_4"){
    var2[[moderator_var]] = 0
    mod_med_lab = "0"
  } else if (moderator_values == "meansd"){
    var2[[moderator_var]] = mean(var_data[[moderator_var]])
    mod_med_lab = "Mean"
  }
  
  betabin_pred = predict(var_mod, se.fit = T, newdata = var2)
  s_prop_pred_med = ifelse(backtransform_vec, plogis(betabin_pred$fit), betabin_pred$fit)
  s_prop_lo_med = ifelse(backtransform_vec, plogis(betabin_pred$fit - qnorm(.975) * betabin_pred$se.fit), betabin_pred$fit - qnorm(.975) * betabin_pred$se.fit)
  s_prop_hi_med = ifelse(backtransform_vec, plogis(betabin_pred$fit + qnorm(.975) * betabin_pred$se.fit), betabin_pred$fit + qnorm(.975) * betabin_pred$se.fit)
  
  if (moderator_values == "minmedmax" | moderator_values == "minmeanmax"){
    var2[[moderator_var]] = min(var_data[[moderator_var]])
    mod_min_lab = "Min"
  } else if (moderator_values == "2_4_6"){
    var2[[moderator_var]] = 2
    mod_min_lab = "2"
  } else if (moderator_values == "-4_0_4"){
    var2[[moderator_var]] = -4
    mod_min_lab = "-4"
  } else if (moderator_values == "meansd"){
    var2[[moderator_var]] = mean(var_data[[moderator_var]]) - sd(var_data[[moderator_var]])
    mod_min_lab = "-SD"
  }
  
  betabin_pred = predict(var_mod, se.fit = T, newdata = var2)
  s_prop_pred_min = ifelse(backtransform_vec, plogis(betabin_pred$fit), betabin_pred$fit)
  s_prop_lo_min = ifelse(backtransform_vec, plogis(betabin_pred$fit - qnorm(.975) * betabin_pred$se.fit), betabin_pred$fit - qnorm(.975) * betabin_pred$se.fit)
  s_prop_hi_min = ifelse(backtransform_vec, plogis(betabin_pred$fit + qnorm(.975) * betabin_pred$se.fit), betabin_pred$fit + qnorm(.975) * betabin_pred$se.fit)
  
  ggplot(var_data, aes(x = get(predictor_var), y = get(dependent_var))) +
    geom_ribbon( aes(ymin = s_prop_lo_max, ymax = s_prop_hi_max, color=mod_max_lab, fill=mod_max_lab), alpha = 1) +
    geom_ribbon( aes(ymin = s_prop_lo_med, ymax = s_prop_hi_med, color=mod_med_lab, fill=mod_med_lab), alpha = 1) +
    geom_ribbon( aes(ymin = s_prop_lo_min, ymax = s_prop_hi_min, color=mod_min_lab, fill=mod_min_lab), alpha = 1) +
    geom_ribbon( aes(ymin = s_prop_lo_min, ymax = s_prop_hi_min, color=mod_min_lab), alpha = 0) +
    geom_ribbon( aes(ymin = s_prop_lo_med, ymax = s_prop_hi_med, color=mod_med_lab), alpha = 0) +
    geom_ribbon( aes(ymin = s_prop_lo_max, ymax = s_prop_hi_max, color=mod_max_lab), alpha = 0) +
    geom_point(color = "grey", alpha = 1, size=0.5) +
    geom_line(aes(y=s_prop_pred_max, linetype = mod_max_lab)) +
    geom_line(aes(y=s_prop_pred_med, linetype = mod_med_lab)) +
    geom_line(aes(y=s_prop_pred_min, linetype = mod_min_lab)) +
    labs(x=predictor_lab, y=dependent_lab) +
    scale_colour_manual(name=moderator_lab, values=c("#009E73", "#0072B2", "#D55E00")) + 
    scale_linetype_manual(name=moderator_lab, values=c("solid", "dotted", "dashed")) +
    scale_fill_manual(name=moderator_lab, values=c("#009E73", "#0072B2", "#D55E00")) +
    theme(legend.position="top")
}

library(aods3)
library(hnp)
library(corrplot)

var = read.csv("s_dist.csv")

corrplot(cor(var[,c("p_s", "log_freq_pl", "log_ratio_pl")]), method = "number", type = "lower")

distribution_model = aodml(cbind(f_s, f_nons) ~ p_s*log_freq_pl + p_s*log_ratio_pl, var)
distribution_model_bin = glm(cbind(f_s, f_nons) ~ p_s*log_freq_pl + p_s*log_ratio_pl, family = "binomial", data = var)

lrt_X = -2 * (as.numeric(logLik(distribution_model_bin))-as.numeric(logLik(distribution_model)))
lrt_p = pchisq(lrt_X, df = 1, lower.tail = FALSE)
print(paste("X(1)=", as.character(lrt_X), ", p=", as.character(lrt_p), sep = ""))

par(mfrow=c(1,2))
hnp(distribution_model_bin, main="Binomial")
hnp(distribution_model, main="Beta-binomial")
par(mfrow=c(1,1))

summary(distribution_model)

plot_aodml_effect(var, distribution_model, predictor_var = "p_s", moderator_var = "log_ratio_pl", 
                  constant_vars = c("log_freq_pl"), dependent_var = "prop_s", 
                  predictor_lab = "-s Prediction", 
                  moderator_lab = "Plural Dominance", 
                  dependent_lab = "-s Bias", moderator_values = "-4_0_4",
                  backtransform = F)


plot_aodml_effect(var, distribution_model, predictor_var = "p_s", moderator_var = "log_freq_pl", 
                  constant_vars = c("log_ratio_pl"), dependent_var = "prop_s", 
                  predictor_lab = "-s Prediction", 
                  moderator_lab = "Plural Frequency", 
                  dependent_lab = "-s Bias", moderator_values = "2_4_6",
                  backtransform = F)

#########
# Duration Study

s_dur = read.csv("s_dur.csv")

col_pred = s_dur[, c("syntax_f2", "syntax_f3", "syntax_f4", "syntax_f5", "syntax_f6", "syntax_f7", "syntax_f8")]
col_pred_pca = prcomp(col_pred, center = T, scale. = T)
summary(col_pred_pca)
s_dur$PC1 = col_pred_pca$x[,1]
s_dur$PC2 = col_pred_pca$x[,2]
s_dur$PC3 = col_pred_pca$x[,3]
s_dur$PC4 = col_pred_pca$x[,4]
s_dur$PC5 = col_pred_pca$x[,5]

s_dur$speech_rate_pron_sc = scale(s_dur$speech_rate_pron)
s_dur$log_base_dur_sc = scale(s_dur$log_base_dur)
s_dur$num_syl_pron_sc = scale(s_dur$num_syl_pron)
s_dur$lex_neb_sc = scale(s_dur$lex_neb)
s_dur$probability_next_sc = scale(s_dur$probability_next)
s_dur$probability_prev_sc = scale(s_dur$probability_prev)
s_dur$PC1_sc = scale(s_dur$PC1)
s_dur$PC2_sc = scale(s_dur$PC2)
s_dur$PC3_sc = scale(s_dur$PC3)
s_dur$PC4_sc = scale(s_dur$PC4)
s_dur$PC5_sc = scale(s_dur$PC5)

pred_ass = matrix(c(cramerV(table(s_dur[,c("next_phon_class", "next_phon_class")]), bias.correct = TRUE),
                   cramerV(table(s_dur[,c("next_phon_class", "prev_phon_class")]), bias.correct = TRUE),
                   cramerV(table(s_dur[,c("next_phon_class", "stressed")]), bias.correct = TRUE),
                   cramerV(table(s_dur[,c("next_phon_class", "register")]), bias.correct = TRUE),
                   cramerV(table(s_dur[,c("next_phon_class", "recent_mention")]), bias.correct = TRUE),
                   sqrt(summary(lm(speech_rate_pron_sc ~ next_phon_class, data = s_dur))$r.squared),
                   sqrt(summary(lm(num_syl_pron_sc ~ next_phon_class, data = s_dur))$r.squared),
                   sqrt(summary(lm(log_base_dur_sc ~ next_phon_class, data = s_dur))$r.squared),
                   sqrt(summary(lm(PC1_sc ~ next_phon_class, data = s_dur))$r.squared),
                   sqrt(summary(lm(PC2_sc ~ next_phon_class, data = s_dur))$r.squared),
                   sqrt(summary(lm(PC3_sc ~ next_phon_class, data = s_dur))$r.squared),
                   sqrt(summary(lm(PC4_sc ~ next_phon_class, data = s_dur))$r.squared),
                   sqrt(summary(lm(PC5_sc ~ next_phon_class, data = s_dur))$r.squared),
                   sqrt(summary(lm(lex_neb_sc ~ next_phon_class, data = s_dur))$r.squared),
                   sqrt(summary(lm(probability_prev_sc ~ next_phon_class, data = s_dur))$r.squared),
                   sqrt(summary(lm(probability_next_sc ~ next_phon_class, data = s_dur))$r.squared),
                   sqrt(summary(lm(log_ratio_s ~ next_phon_class, data = s_dur))$r.squared),
                   sqrt(summary(lm(log_ratio_pl ~ next_phon_class, data = s_dur))$r.squared),
                   sqrt(summary(lm(log_f_s ~ next_phon_class, data = s_dur))$r.squared),
                   sqrt(summary(lm(log_rel_f_s ~ next_phon_class, data = s_dur))$r.squared),
                   cramerV(table(s_dur[,c("prev_phon_class", "next_phon_class")]), bias.correct = TRUE),
                   cramerV(table(s_dur[,c("prev_phon_class", "prev_phon_class")]), bias.correct = TRUE),
                   cramerV(table(s_dur[,c("prev_phon_class", "stressed")]), bias.correct = TRUE),
                   cramerV(table(s_dur[,c("prev_phon_class", "register")]), bias.correct = TRUE),
                   cramerV(table(s_dur[,c("prev_phon_class", "recent_mention")]), bias.correct = TRUE),
                   sqrt(summary(lm(speech_rate_pron_sc ~ prev_phon_class, data = s_dur))$r.squared),
                   sqrt(summary(lm(num_syl_pron_sc ~ prev_phon_class, data = s_dur))$r.squared),
                   sqrt(summary(lm(log_base_dur_sc ~ prev_phon_class, data = s_dur))$r.squared),
                   sqrt(summary(lm(PC1_sc ~ prev_phon_class, data = s_dur))$r.squared),
                   sqrt(summary(lm(PC2_sc ~ prev_phon_class, data = s_dur))$r.squared),
                   sqrt(summary(lm(PC3_sc ~ prev_phon_class, data = s_dur))$r.squared),
                   sqrt(summary(lm(PC4_sc ~ prev_phon_class, data = s_dur))$r.squared),
                   sqrt(summary(lm(PC5_sc ~ prev_phon_class, data = s_dur))$r.squared),
                   sqrt(summary(lm(lex_neb_sc ~ prev_phon_class, data = s_dur))$r.squared),
                   sqrt(summary(lm(probability_prev_sc ~ prev_phon_class, data = s_dur))$r.squared),
                   sqrt(summary(lm(probability_next_sc ~ prev_phon_class, data = s_dur))$r.squared),
                   sqrt(summary(lm(log_ratio_s ~ prev_phon_class, data = s_dur))$r.squared),
                   sqrt(summary(lm(log_ratio_pl ~ prev_phon_class, data = s_dur))$r.squared),
                   sqrt(summary(lm(log_f_s ~ prev_phon_class, data = s_dur))$r.squared),
                   sqrt(summary(lm(log_rel_f_s ~ prev_phon_class, data = s_dur))$r.squared),
                   cramerV(table(s_dur[,c("stressed", "next_phon_class")]), bias.correct = TRUE),
                   cramerV(table(s_dur[,c("stressed", "prev_phon_class")]), bias.correct = TRUE),
                   cramerV(table(s_dur[,c("stressed", "stressed")]), bias.correct = TRUE),
                   cramerV(table(s_dur[,c("stressed", "register")]), bias.correct = TRUE),
                   cramerV(table(s_dur[,c("stressed", "recent_mention")]), bias.correct = TRUE),
                   sqrt(summary(lm(speech_rate_pron_sc ~ stressed, data = s_dur))$r.squared),
                   sqrt(summary(lm(num_syl_pron_sc ~ stressed, data = s_dur))$r.squared),
                   sqrt(summary(lm(log_base_dur_sc ~ stressed, data = s_dur))$r.squared),
                   sqrt(summary(lm(PC1_sc ~ stressed, data = s_dur))$r.squared),
                   sqrt(summary(lm(PC2_sc ~ stressed, data = s_dur))$r.squared),
                   sqrt(summary(lm(PC3_sc ~ stressed, data = s_dur))$r.squared),
                   sqrt(summary(lm(PC4_sc ~ stressed, data = s_dur))$r.squared),
                   sqrt(summary(lm(PC5_sc ~ stressed, data = s_dur))$r.squared),
                   sqrt(summary(lm(lex_neb_sc ~ stressed, data = s_dur))$r.squared),
                   sqrt(summary(lm(probability_prev_sc ~ stressed, data = s_dur))$r.squared),
                   sqrt(summary(lm(probability_next_sc ~ stressed, data = s_dur))$r.squared),
                   sqrt(summary(lm(log_ratio_s ~ stressed, data = s_dur))$r.squared),
                   sqrt(summary(lm(log_ratio_pl ~ stressed, data = s_dur))$r.squared),
                   sqrt(summary(lm(log_f_s ~ stressed, data = s_dur))$r.squared),
                   sqrt(summary(lm(log_rel_f_s ~ stressed, data = s_dur))$r.squared),
                   cramerV(table(s_dur[,c("register", "next_phon_class")]), bias.correct = TRUE),
                   cramerV(table(s_dur[,c("register", "prev_phon_class")]), bias.correct = TRUE),
                   cramerV(table(s_dur[,c("register", "stressed")]), bias.correct = TRUE),
                   cramerV(table(s_dur[,c("register", "register")]), bias.correct = TRUE),
                   cramerV(table(s_dur[,c("register", "recent_mention")]), bias.correct = TRUE),
                   sqrt(summary(lm(speech_rate_pron_sc ~ register, data = s_dur))$r.squared),
                   sqrt(summary(lm(num_syl_pron_sc ~ register, data = s_dur))$r.squared),
                   sqrt(summary(lm(log_base_dur_sc ~ register, data = s_dur))$r.squared),
                   sqrt(summary(lm(PC1_sc ~ register, data = s_dur))$r.squared),
                   sqrt(summary(lm(PC2_sc ~ register, data = s_dur))$r.squared),
                   sqrt(summary(lm(PC3_sc ~ register, data = s_dur))$r.squared),
                   sqrt(summary(lm(PC4_sc ~ register, data = s_dur))$r.squared),
                   sqrt(summary(lm(PC5_sc ~ register, data = s_dur))$r.squared),
                   sqrt(summary(lm(lex_neb_sc ~ register, data = s_dur))$r.squared),
                   sqrt(summary(lm(probability_prev_sc ~ register, data = s_dur))$r.squared),
                   sqrt(summary(lm(probability_next_sc ~ register, data = s_dur))$r.squared),
                   sqrt(summary(lm(log_ratio_s ~ register, data = s_dur))$r.squared),
                   sqrt(summary(lm(log_ratio_pl ~ register, data = s_dur))$r.squared),
                   sqrt(summary(lm(log_f_s ~ register, data = s_dur))$r.squared),
                   sqrt(summary(lm(log_rel_f_s ~ register, data = s_dur))$r.squared),
                   cramerV(table(s_dur[,c("recent_mention", "next_phon_class")]), bias.correct = TRUE),
                   cramerV(table(s_dur[,c("recent_mention", "prev_phon_class")]), bias.correct = TRUE),
                   cramerV(table(s_dur[,c("recent_mention", "stressed")]), bias.correct = TRUE),
                   cramerV(table(s_dur[,c("recent_mention", "register")]), bias.correct = TRUE),
                   cramerV(table(s_dur[,c("recent_mention", "recent_mention")]), bias.correct = TRUE),
                   sqrt(summary(lm(speech_rate_pron_sc ~ recent_mention, data = s_dur))$r.squared),
                   sqrt(summary(lm(num_syl_pron_sc ~ recent_mention, data = s_dur))$r.squared),
                   sqrt(summary(lm(log_base_dur_sc ~ recent_mention, data = s_dur))$r.squared),
                   sqrt(summary(lm(PC1_sc ~ recent_mention, data = s_dur))$r.squared),
                   sqrt(summary(lm(PC2_sc ~ recent_mention, data = s_dur))$r.squared),
                   sqrt(summary(lm(PC3_sc ~ recent_mention, data = s_dur))$r.squared),
                   sqrt(summary(lm(PC4_sc ~ recent_mention, data = s_dur))$r.squared),
                   sqrt(summary(lm(PC5_sc ~ recent_mention, data = s_dur))$r.squared),
                   sqrt(summary(lm(lex_neb_sc ~ recent_mention, data = s_dur))$r.squared),
                   sqrt(summary(lm(probability_prev_sc ~ recent_mention, data = s_dur))$r.squared),
                   sqrt(summary(lm(probability_next_sc ~ recent_mention, data = s_dur))$r.squared),
                   sqrt(summary(lm(log_ratio_s ~ recent_mention, data = s_dur))$r.squared),
                   sqrt(summary(lm(log_ratio_pl ~ recent_mention, data = s_dur))$r.squared),
                   sqrt(summary(lm(log_f_s ~ recent_mention, data = s_dur))$r.squared),
                   sqrt(summary(lm(log_rel_f_s ~ recent_mention, data = s_dur))$r.squared),
                   sqrt(summary(lm(speech_rate_pron_sc ~ next_phon_class, data = s_dur))$r.squared),
                   sqrt(summary(lm(speech_rate_pron_sc ~ prev_phon_class, data = s_dur))$r.squared),
                   sqrt(summary(lm(speech_rate_pron_sc ~ stressed, data = s_dur))$r.squared),
                   sqrt(summary(lm(speech_rate_pron_sc ~ register, data = s_dur))$r.squared),
                   sqrt(summary(lm(speech_rate_pron_sc ~ recent_mention, data = s_dur))$r.squared),
                   cor(s_dur$speech_rate_pron_sc, s_dur$speech_rate_pron_sc),
                   cor(s_dur$speech_rate_pron_sc, s_dur$num_syl_pron_sc),
                   cor(s_dur$speech_rate_pron_sc, s_dur$log_base_dur_sc),
                   cor(s_dur$speech_rate_pron_sc, s_dur$PC1_sc),
                   cor(s_dur$speech_rate_pron_sc, s_dur$PC2_sc),
                   cor(s_dur$speech_rate_pron_sc, s_dur$PC3_sc),
                   cor(s_dur$speech_rate_pron_sc, s_dur$PC4_sc),
                   cor(s_dur$speech_rate_pron_sc, s_dur$PC5_sc),
                   cor(s_dur$speech_rate_pron_sc, s_dur$lex_neb_sc),
                   cor(s_dur$speech_rate_pron_sc, s_dur$probability_prev_sc),
                   cor(s_dur$speech_rate_pron_sc, s_dur$probability_next_sc),
                   cor(s_dur$speech_rate_pron_sc, s_dur$log_ratio_s),
                   cor(s_dur$speech_rate_pron_sc, s_dur$log_ratio_pl),
                   cor(s_dur$speech_rate_pron_sc, s_dur$log_f_s),
                   cor(s_dur$speech_rate_pron_sc, s_dur$log_rel_f_s),
                   sqrt(summary(lm(num_syl_pron_sc ~ next_phon_class, data = s_dur))$r.squared),
                   sqrt(summary(lm(num_syl_pron_sc ~ prev_phon_class, data = s_dur))$r.squared),
                   sqrt(summary(lm(num_syl_pron_sc ~ stressed, data = s_dur))$r.squared),
                   sqrt(summary(lm(num_syl_pron_sc ~ register, data = s_dur))$r.squared),
                   sqrt(summary(lm(num_syl_pron_sc ~ recent_mention, data = s_dur))$r.squared),
                   cor(s_dur$num_syl_pron_sc, s_dur$speech_rate_pron_sc),
                   cor(s_dur$num_syl_pron_sc, s_dur$num_syl_pron_sc),
                   cor(s_dur$num_syl_pron_sc, s_dur$log_base_dur_sc),
                   cor(s_dur$num_syl_pron_sc, s_dur$PC1_sc),
                   cor(s_dur$num_syl_pron_sc, s_dur$PC2_sc),
                   cor(s_dur$num_syl_pron_sc, s_dur$PC3_sc),
                   cor(s_dur$num_syl_pron_sc, s_dur$PC4_sc),
                   cor(s_dur$num_syl_pron_sc, s_dur$PC5_sc),
                   cor(s_dur$num_syl_pron_sc, s_dur$lex_neb_sc),
                   cor(s_dur$num_syl_pron_sc, s_dur$probability_prev_sc),
                   cor(s_dur$num_syl_pron_sc, s_dur$probability_next_sc),
                   cor(s_dur$num_syl_pron_sc, s_dur$log_ratio_s),
                   cor(s_dur$num_syl_pron_sc, s_dur$log_ratio_pl),
                   cor(s_dur$num_syl_pron_sc, s_dur$log_f_s),
                   cor(s_dur$num_syl_pron_sc, s_dur$log_rel_f_s),
                   sqrt(summary(lm(log_base_dur_sc ~ next_phon_class, data = s_dur))$r.squared),
                   sqrt(summary(lm(log_base_dur_sc ~ prev_phon_class, data = s_dur))$r.squared),
                   sqrt(summary(lm(log_base_dur_sc ~ stressed, data = s_dur))$r.squared),
                   sqrt(summary(lm(log_base_dur_sc ~ register, data = s_dur))$r.squared),
                   sqrt(summary(lm(log_base_dur_sc ~ recent_mention, data = s_dur))$r.squared),
                   cor(s_dur$log_base_dur_sc, s_dur$speech_rate_pron_sc),
                   cor(s_dur$log_base_dur_sc, s_dur$num_syl_pron_sc),
                   cor(s_dur$log_base_dur_sc, s_dur$log_base_dur_sc),
                   cor(s_dur$log_base_dur_sc, s_dur$PC1_sc),
                   cor(s_dur$log_base_dur_sc, s_dur$PC2_sc),
                   cor(s_dur$log_base_dur_sc, s_dur$PC3_sc),
                   cor(s_dur$log_base_dur_sc, s_dur$PC4_sc),
                   cor(s_dur$log_base_dur_sc, s_dur$PC5_sc),
                   cor(s_dur$log_base_dur_sc, s_dur$lex_neb_sc),
                   cor(s_dur$log_base_dur_sc, s_dur$probability_prev_sc),
                   cor(s_dur$log_base_dur_sc, s_dur$probability_next_sc),
                   cor(s_dur$log_base_dur_sc, s_dur$log_ratio_s),
                   cor(s_dur$log_base_dur_sc, s_dur$log_ratio_pl),
                   cor(s_dur$log_base_dur_sc, s_dur$log_f_s),
                   cor(s_dur$log_base_dur_sc, s_dur$log_rel_f_s),
                   sqrt(summary(lm(PC1_sc ~ next_phon_class, data = s_dur))$r.squared),
                   sqrt(summary(lm(PC1_sc ~ prev_phon_class, data = s_dur))$r.squared),
                   sqrt(summary(lm(PC1_sc ~ stressed, data = s_dur))$r.squared),
                   sqrt(summary(lm(PC1_sc ~ register, data = s_dur))$r.squared),
                   sqrt(summary(lm(PC1_sc ~ recent_mention, data = s_dur))$r.squared),
                   cor(s_dur$PC1_sc, s_dur$speech_rate_pron_sc),
                   cor(s_dur$PC1_sc, s_dur$num_syl_pron_sc),
                   cor(s_dur$PC1_sc, s_dur$log_base_dur_sc),
                   cor(s_dur$PC1_sc, s_dur$PC1_sc),
                   cor(s_dur$PC1_sc, s_dur$PC2_sc),
                   cor(s_dur$PC1_sc, s_dur$PC3_sc),
                   cor(s_dur$PC1_sc, s_dur$PC4_sc),
                   cor(s_dur$PC1_sc, s_dur$PC5_sc),
                   cor(s_dur$PC1_sc, s_dur$lex_neb_sc),
                   cor(s_dur$PC1_sc, s_dur$probability_prev_sc),
                   cor(s_dur$PC1_sc, s_dur$probability_next_sc),
                   cor(s_dur$PC1_sc, s_dur$log_ratio_s),
                   cor(s_dur$PC1_sc, s_dur$log_ratio_pl),
                   cor(s_dur$PC1_sc, s_dur$log_f_s),
                   cor(s_dur$PC1_sc, s_dur$log_rel_f_s),
                   sqrt(summary(lm(PC2_sc ~ next_phon_class, data = s_dur))$r.squared),
                   sqrt(summary(lm(PC2_sc ~ prev_phon_class, data = s_dur))$r.squared),
                   sqrt(summary(lm(PC2_sc ~ stressed, data = s_dur))$r.squared),
                   sqrt(summary(lm(PC2_sc ~ register, data = s_dur))$r.squared),
                   sqrt(summary(lm(PC2_sc ~ recent_mention, data = s_dur))$r.squared),
                   cor(s_dur$PC2_sc, s_dur$speech_rate_pron_sc),
                   cor(s_dur$PC2_sc, s_dur$num_syl_pron_sc),
                   cor(s_dur$PC2_sc, s_dur$log_base_dur_sc),
                   cor(s_dur$PC2_sc, s_dur$PC1_sc),
                   cor(s_dur$PC2_sc, s_dur$PC2_sc),
                   cor(s_dur$PC2_sc, s_dur$PC3_sc),
                   cor(s_dur$PC2_sc, s_dur$PC4_sc),
                   cor(s_dur$PC2_sc, s_dur$PC5_sc),
                   cor(s_dur$PC2_sc, s_dur$lex_neb_sc),
                   cor(s_dur$PC2_sc, s_dur$probability_prev_sc),
                   cor(s_dur$PC2_sc, s_dur$probability_next_sc),
                   cor(s_dur$PC2_sc, s_dur$log_ratio_s),
                   cor(s_dur$PC2_sc, s_dur$log_ratio_pl),
                   cor(s_dur$PC2_sc, s_dur$log_f_s),
                   cor(s_dur$PC2_sc, s_dur$log_rel_f_s),
                   sqrt(summary(lm(PC3_sc ~ next_phon_class, data = s_dur))$r.squared),
                   sqrt(summary(lm(PC3_sc ~ prev_phon_class, data = s_dur))$r.squared),
                   sqrt(summary(lm(PC3_sc ~ stressed, data = s_dur))$r.squared),
                   sqrt(summary(lm(PC3_sc ~ register, data = s_dur))$r.squared),
                   sqrt(summary(lm(PC3_sc ~ recent_mention, data = s_dur))$r.squared),
                   cor(s_dur$PC3_sc, s_dur$speech_rate_pron_sc),
                   cor(s_dur$PC3_sc, s_dur$num_syl_pron_sc),
                   cor(s_dur$PC3_sc, s_dur$log_base_dur_sc),
                   cor(s_dur$PC3_sc, s_dur$PC1_sc),
                   cor(s_dur$PC3_sc, s_dur$PC2_sc),
                   cor(s_dur$PC3_sc, s_dur$PC3_sc),
                   cor(s_dur$PC3_sc, s_dur$PC4_sc),
                   cor(s_dur$PC3_sc, s_dur$PC5_sc),
                   cor(s_dur$PC3_sc, s_dur$lex_neb_sc),
                   cor(s_dur$PC3_sc, s_dur$probability_prev_sc),
                   cor(s_dur$PC3_sc, s_dur$probability_next_sc),
                   cor(s_dur$PC3_sc, s_dur$log_ratio_s),
                   cor(s_dur$PC3_sc, s_dur$log_ratio_pl),
                   cor(s_dur$PC3_sc, s_dur$log_f_s),
                   cor(s_dur$PC3_sc, s_dur$log_rel_f_s),
                   sqrt(summary(lm(PC4_sc ~ next_phon_class, data = s_dur))$r.squared),
                   sqrt(summary(lm(PC4_sc ~ prev_phon_class, data = s_dur))$r.squared),
                   sqrt(summary(lm(PC4_sc ~ stressed, data = s_dur))$r.squared),
                   sqrt(summary(lm(PC4_sc ~ register, data = s_dur))$r.squared),
                   sqrt(summary(lm(PC4_sc ~ recent_mention, data = s_dur))$r.squared),
                   cor(s_dur$PC4_sc, s_dur$speech_rate_pron_sc),
                   cor(s_dur$PC4_sc, s_dur$num_syl_pron_sc),
                   cor(s_dur$PC4_sc, s_dur$log_base_dur_sc),
                   cor(s_dur$PC4_sc, s_dur$PC1_sc),
                   cor(s_dur$PC4_sc, s_dur$PC2_sc),
                   cor(s_dur$PC4_sc, s_dur$PC3_sc),
                   cor(s_dur$PC4_sc, s_dur$PC4_sc),
                   cor(s_dur$PC4_sc, s_dur$PC5_sc),
                   cor(s_dur$PC4_sc, s_dur$lex_neb_sc),
                   cor(s_dur$PC4_sc, s_dur$probability_prev_sc),
                   cor(s_dur$PC4_sc, s_dur$probability_next_sc),
                   cor(s_dur$PC4_sc, s_dur$log_ratio_s),
                   cor(s_dur$PC4_sc, s_dur$log_ratio_pl),
                   cor(s_dur$PC4_sc, s_dur$log_f_s),
                   cor(s_dur$PC4_sc, s_dur$log_rel_f_s),
                   sqrt(summary(lm(PC5_sc ~ next_phon_class, data = s_dur))$r.squared),
                   sqrt(summary(lm(PC5_sc ~ prev_phon_class, data = s_dur))$r.squared),
                   sqrt(summary(lm(PC5_sc ~ stressed, data = s_dur))$r.squared),
                   sqrt(summary(lm(PC5_sc ~ register, data = s_dur))$r.squared),
                   sqrt(summary(lm(PC5_sc ~ recent_mention, data = s_dur))$r.squared),
                   cor(s_dur$PC5_sc, s_dur$speech_rate_pron_sc),
                   cor(s_dur$PC5_sc, s_dur$num_syl_pron_sc),
                   cor(s_dur$PC5_sc, s_dur$log_base_dur_sc),
                   cor(s_dur$PC5_sc, s_dur$PC1_sc),
                   cor(s_dur$PC5_sc, s_dur$PC2_sc),
                   cor(s_dur$PC5_sc, s_dur$PC3_sc),
                   cor(s_dur$PC5_sc, s_dur$PC4_sc),
                   cor(s_dur$PC5_sc, s_dur$PC5_sc),
                   cor(s_dur$PC5_sc, s_dur$lex_neb_sc),
                   cor(s_dur$PC5_sc, s_dur$probability_prev_sc),
                   cor(s_dur$PC5_sc, s_dur$probability_next_sc),
                   cor(s_dur$PC5_sc, s_dur$log_ratio_s),
                   cor(s_dur$PC5_sc, s_dur$log_ratio_pl),
                   cor(s_dur$PC5_sc, s_dur$log_f_s),
                   cor(s_dur$PC5_sc, s_dur$log_rel_f_s),
                   sqrt(summary(lm(lex_neb_sc ~ next_phon_class, data = s_dur))$r.squared),
                   sqrt(summary(lm(lex_neb_sc ~ prev_phon_class, data = s_dur))$r.squared),
                   sqrt(summary(lm(lex_neb_sc ~ stressed, data = s_dur))$r.squared),
                   sqrt(summary(lm(lex_neb_sc ~ register, data = s_dur))$r.squared),
                   sqrt(summary(lm(lex_neb_sc ~ recent_mention, data = s_dur))$r.squared),
                   cor(s_dur$lex_neb_sc, s_dur$speech_rate_pron_sc),
                   cor(s_dur$lex_neb_sc, s_dur$num_syl_pron_sc),
                   cor(s_dur$lex_neb_sc, s_dur$log_base_dur_sc),
                   cor(s_dur$lex_neb_sc, s_dur$PC1_sc),
                   cor(s_dur$lex_neb_sc, s_dur$PC2_sc),
                   cor(s_dur$lex_neb_sc, s_dur$PC3_sc),
                   cor(s_dur$lex_neb_sc, s_dur$PC4_sc),
                   cor(s_dur$lex_neb_sc, s_dur$PC5_sc),
                   cor(s_dur$lex_neb_sc, s_dur$lex_neb_sc),
                   cor(s_dur$lex_neb_sc, s_dur$probability_prev_sc),
                   cor(s_dur$lex_neb_sc, s_dur$probability_next_sc),
                   cor(s_dur$lex_neb_sc, s_dur$log_ratio_s),
                   cor(s_dur$lex_neb_sc, s_dur$log_ratio_pl),
                   cor(s_dur$lex_neb_sc, s_dur$log_f_s),
                   cor(s_dur$lex_neb_sc, s_dur$log_rel_f_s),
                   sqrt(summary(lm(probability_prev_sc ~ next_phon_class, data = s_dur))$r.squared),
                   sqrt(summary(lm(probability_prev_sc ~ prev_phon_class, data = s_dur))$r.squared),
                   sqrt(summary(lm(probability_prev_sc ~ stressed, data = s_dur))$r.squared),
                   sqrt(summary(lm(probability_prev_sc ~ register, data = s_dur))$r.squared),
                   sqrt(summary(lm(probability_prev_sc ~ recent_mention, data = s_dur))$r.squared),
                   cor(s_dur$probability_prev_sc, s_dur$speech_rate_pron_sc),
                   cor(s_dur$probability_prev_sc, s_dur$num_syl_pron_sc),
                   cor(s_dur$probability_prev_sc, s_dur$log_base_dur_sc),
                   cor(s_dur$probability_prev_sc, s_dur$PC1_sc),
                   cor(s_dur$probability_prev_sc, s_dur$PC2_sc),
                   cor(s_dur$probability_prev_sc, s_dur$PC3_sc),
                   cor(s_dur$probability_prev_sc, s_dur$PC4_sc),
                   cor(s_dur$probability_prev_sc, s_dur$PC5_sc),
                   cor(s_dur$probability_prev_sc, s_dur$lex_neb_sc),
                   cor(s_dur$probability_prev_sc, s_dur$probability_prev_sc),
                   cor(s_dur$probability_prev_sc, s_dur$probability_next_sc),
                   cor(s_dur$probability_prev_sc, s_dur$log_ratio_s),
                   cor(s_dur$probability_prev_sc, s_dur$log_ratio_pl),
                   cor(s_dur$probability_prev_sc, s_dur$log_f_s),
                   cor(s_dur$probability_prev_sc, s_dur$log_rel_f_s),
                   sqrt(summary(lm(probability_next_sc ~ next_phon_class, data = s_dur))$r.squared),
                   sqrt(summary(lm(probability_next_sc ~ prev_phon_class, data = s_dur))$r.squared),
                   sqrt(summary(lm(probability_next_sc ~ stressed, data = s_dur))$r.squared),
                   sqrt(summary(lm(probability_next_sc ~ register, data = s_dur))$r.squared),
                   sqrt(summary(lm(probability_next_sc ~ recent_mention, data = s_dur))$r.squared),
                   cor(s_dur$probability_next_sc, s_dur$speech_rate_pron_sc),
                   cor(s_dur$probability_next_sc, s_dur$num_syl_pron_sc),
                   cor(s_dur$probability_next_sc, s_dur$log_base_dur_sc),
                   cor(s_dur$probability_next_sc, s_dur$PC1_sc),
                   cor(s_dur$probability_next_sc, s_dur$PC2_sc),
                   cor(s_dur$probability_next_sc, s_dur$PC3_sc),
                   cor(s_dur$probability_next_sc, s_dur$PC4_sc),
                   cor(s_dur$probability_next_sc, s_dur$PC5_sc),
                   cor(s_dur$probability_next_sc, s_dur$lex_neb_sc),
                   cor(s_dur$probability_next_sc, s_dur$probability_prev_sc),
                   cor(s_dur$probability_next_sc, s_dur$probability_next_sc),
                   cor(s_dur$probability_next_sc, s_dur$log_ratio_s),
                   cor(s_dur$probability_next_sc, s_dur$log_ratio_pl),
                   cor(s_dur$probability_next_sc, s_dur$log_f_s),
                   cor(s_dur$probability_next_sc, s_dur$log_rel_f_s),
                   sqrt(summary(lm(log_ratio_s ~ next_phon_class, data = s_dur))$r.squared),
                   sqrt(summary(lm(log_ratio_s ~ prev_phon_class, data = s_dur))$r.squared),
                   sqrt(summary(lm(log_ratio_s ~ stressed, data = s_dur))$r.squared),
                   sqrt(summary(lm(log_ratio_s ~ register, data = s_dur))$r.squared),
                   sqrt(summary(lm(log_ratio_s ~ recent_mention, data = s_dur))$r.squared),
                   cor(s_dur$log_ratio_s, s_dur$speech_rate_pron_sc),
                   cor(s_dur$log_ratio_s, s_dur$num_syl_pron_sc),
                   cor(s_dur$log_ratio_s, s_dur$log_base_dur_sc),
                   cor(s_dur$log_ratio_s, s_dur$PC1_sc),
                   cor(s_dur$log_ratio_s, s_dur$PC2_sc),
                   cor(s_dur$log_ratio_s, s_dur$PC3_sc),
                   cor(s_dur$log_ratio_s, s_dur$PC4_sc),
                   cor(s_dur$log_ratio_s, s_dur$PC5_sc),
                   cor(s_dur$log_ratio_s, s_dur$lex_neb_sc),
                   cor(s_dur$log_ratio_s, s_dur$probability_prev_sc),
                   cor(s_dur$log_ratio_s, s_dur$probability_next_sc),
                   cor(s_dur$log_ratio_s, s_dur$log_ratio_s),
                   cor(s_dur$log_ratio_s, s_dur$log_ratio_pl),
                   cor(s_dur$log_ratio_s, s_dur$log_f_s),
                   cor(s_dur$log_ratio_s, s_dur$log_rel_f_s),
                   sqrt(summary(lm(log_ratio_pl ~ next_phon_class, data = s_dur))$r.squared),
                   sqrt(summary(lm(log_ratio_pl ~ prev_phon_class, data = s_dur))$r.squared),
                   sqrt(summary(lm(log_ratio_pl ~ stressed, data = s_dur))$r.squared),
                   sqrt(summary(lm(log_ratio_pl ~ register, data = s_dur))$r.squared),
                   sqrt(summary(lm(log_ratio_pl ~ recent_mention, data = s_dur))$r.squared),
                   cor(s_dur$log_ratio_pl, s_dur$speech_rate_pron_sc),
                   cor(s_dur$log_ratio_pl, s_dur$num_syl_pron_sc),
                   cor(s_dur$log_ratio_pl, s_dur$log_base_dur_sc),
                   cor(s_dur$log_ratio_pl, s_dur$PC1_sc),
                   cor(s_dur$log_ratio_pl, s_dur$PC2_sc),
                   cor(s_dur$log_ratio_pl, s_dur$PC3_sc),
                   cor(s_dur$log_ratio_pl, s_dur$PC4_sc),
                   cor(s_dur$log_ratio_pl, s_dur$PC5_sc),
                   cor(s_dur$log_ratio_pl, s_dur$lex_neb_sc),
                   cor(s_dur$log_ratio_pl, s_dur$probability_prev_sc),
                   cor(s_dur$log_ratio_pl, s_dur$probability_next_sc),
                   cor(s_dur$log_ratio_pl, s_dur$log_ratio_s),
                   cor(s_dur$log_ratio_pl, s_dur$log_ratio_pl),
                   cor(s_dur$log_ratio_pl, s_dur$log_f_s),
                   cor(s_dur$log_ratio_pl, s_dur$log_rel_f_s),
                   sqrt(summary(lm(log_f_s ~ next_phon_class, data = s_dur))$r.squared),
                   sqrt(summary(lm(log_f_s ~ prev_phon_class, data = s_dur))$r.squared),
                   sqrt(summary(lm(log_f_s ~ stressed, data = s_dur))$r.squared),
                   sqrt(summary(lm(log_f_s ~ register, data = s_dur))$r.squared),
                   sqrt(summary(lm(log_f_s ~ recent_mention, data = s_dur))$r.squared),
                   cor(s_dur$log_f_s, s_dur$speech_rate_pron_sc),
                   cor(s_dur$log_f_s, s_dur$num_syl_pron_sc),
                   cor(s_dur$log_f_s, s_dur$log_base_dur_sc),
                   cor(s_dur$log_f_s, s_dur$PC1_sc),
                   cor(s_dur$log_f_s, s_dur$PC2_sc),
                   cor(s_dur$log_f_s, s_dur$PC3_sc),
                   cor(s_dur$log_f_s, s_dur$PC4_sc),
                   cor(s_dur$log_f_s, s_dur$PC5_sc),
                   cor(s_dur$log_f_s, s_dur$lex_neb_sc),
                   cor(s_dur$log_f_s, s_dur$probability_prev_sc),
                   cor(s_dur$log_f_s, s_dur$probability_next_sc),
                   cor(s_dur$log_f_s, s_dur$log_ratio_s),
                   cor(s_dur$log_f_s, s_dur$log_ratio_pl),
                   cor(s_dur$log_f_s, s_dur$log_f_s),
                   cor(s_dur$log_f_s, s_dur$log_rel_f_s),
                   sqrt(summary(lm(log_rel_f_s ~ next_phon_class, data = s_dur))$r.squared),
                   sqrt(summary(lm(log_rel_f_s ~ prev_phon_class, data = s_dur))$r.squared),
                   sqrt(summary(lm(log_rel_f_s ~ stressed, data = s_dur))$r.squared),
                   sqrt(summary(lm(log_rel_f_s ~ register, data = s_dur))$r.squared),
                   sqrt(summary(lm(log_rel_f_s ~ recent_mention, data = s_dur))$r.squared),
                   cor(s_dur$log_rel_f_s, s_dur$speech_rate_pron_sc),
                   cor(s_dur$log_rel_f_s, s_dur$num_syl_pron_sc),
                   cor(s_dur$log_rel_f_s, s_dur$log_base_dur_sc),
                   cor(s_dur$log_rel_f_s, s_dur$PC1_sc),
                   cor(s_dur$log_rel_f_s, s_dur$PC2_sc),
                   cor(s_dur$log_rel_f_s, s_dur$PC3_sc),
                   cor(s_dur$log_rel_f_s, s_dur$PC4_sc),
                   cor(s_dur$log_rel_f_s, s_dur$PC5_sc),
                   cor(s_dur$log_rel_f_s, s_dur$lex_neb_sc),
                   cor(s_dur$log_rel_f_s, s_dur$probability_prev_sc),
                   cor(s_dur$log_rel_f_s, s_dur$probability_next_sc),
                   cor(s_dur$log_rel_f_s, s_dur$log_ratio_s),
                   cor(s_dur$log_rel_f_s, s_dur$log_ratio_pl),
                   cor(s_dur$log_rel_f_s, s_dur$log_f_s),
                   cor(s_dur$log_rel_f_s, s_dur$log_rel_f_s)
                   ), 
                  nrow = 20, ncol = 20, byrow = T, dimnames = list(
                    c("Next segment", "Previous segment", "Word stress", "Register", "Recently mentioned", "Speech rate", "Number of syllables", "Base duration", ":Prosody[PC1]", ":Prosody[PC2]", ":Prosody[PC3]", ":Prosody[PC4]", ":Prosody[PC5]", "Phonological neighbours", "Probability from prev. word", "Probability from next word", "-s Bias", "Plural Dominance", "-s Frequency", "Relative -s Frequency"),
                    c("Next segment", "Previous segment", "Word stress", "Register", "Recently mentioned", "Speech rate", "Number of syllables", "Base duration", ":Prosody[PC1]", ":Prosody[PC2]", ":Prosody[PC3]", ":Prosody[PC4]", ":Prosody[PC5]", "Phonological neighbours", "Probability from prev. word", "Probability from next word", "-s Bias", "Plural Dominance", "-s Frequency", "Relative -s Frequency")
                    ))

corrplot(pred_ass, method = "number", type = "lower", tl.cex = 0.8, number.cex = 0.5, mar=c(0, 0, 0.8, 0))

library(lmerTest)

duration_model_cov = lmer(log_s_dur ~ 
                            speech_rate_pron_sc + 
                            log_base_dur_sc +
                            PC1_sc + PC2_sc + PC3_sc + PC4_sc + PC5_sc +
                            next_phon_class +
                            prev_phon_class + stressed + num_syl_pron_sc + lex_neb_sc +
                            probability_prev_sc + probability_next_sc +
                            recent_mention + 
                            register +
                            (1 | word) +
                            (1 | speaker), 
                          REML = T,
                          data = s_dur)

duration_model_abs = update(duration_model_cov, . ~ . + log_f_s)
duration_model_rel = update(duration_model_cov, . ~ . + log_rel_f_s)
duration_model_ratio = update(duration_model_cov, . ~ . + log_ratio_s*log_ratio_pl)

s_dur$dur_resid = resid(duration_model_abs)
s_dur_trim = s_dur[abs(scale(s_dur$dur_resid)) < 2.5,]
duration_model_abs_trim = lmer(duration_model_abs@call$formula, REML = T, data = s_dur_trim)                   
summary(duration_model_abs_trim)
s_dur$dur_resid = resid(duration_model_rel)
s_dur_trim = s_dur[abs(scale(s_dur$dur_resid)) < 2.5,]
duration_model_rel_trim = lmer(duration_model_rel@call$formula, REML = T, data = s_dur_trim)
summary(duration_model_rel_trim)
s_dur$dur_resid = resid(duration_model_ratio)
s_dur_trim = s_dur[abs(scale(s_dur$dur_resid)) < 2.5,]
duration_model_ratio_trim = lmer(duration_model_ratio@call$formula, REML = T, data = s_dur_trim)
summary(duration_model_ratio_trim)

AICs = c(AIC(refitML(duration_model_abs)), 
         AIC(refitML(duration_model_rel)),
         AIC(refitML(duration_model_ratio))
         )
mod_labs = c("log_f_s", "log_rel_f_s", "log_ratio_s * log_ratio_pl")

plot(1:length(AICs), AICs, xlab = "", ylab = "", xaxt='n', type = "h")
text(1:length(AICs), par("usr")[3], labels = mod_labs, srt = 45, adj = c(1.1,1.1), xpd = TRUE, cex=0.65)
abline(h = AIC(refitML(duration_model_cov)), lty = "dashed")

AIC(refitML(duration_model_ratio))
AIC(refitML(duration_model_abs))
exp((AIC(refitML(duration_model_ratio)) - AIC(refitML(duration_model_abs)))/2)

# covariates correlated with log_ratio_s: stressed, num_syl_pron
# which predicts s_dur better?
AIC(lm(log_s_dur ~ log_ratio_s*log_ratio_pl, data = s_dur))
AIC(lm(log_s_dur ~ stressed*log_ratio_pl, data = s_dur))
AIC(lm(log_s_dur ~ num_syl_pron_sc*log_ratio_pl, data = s_dur))

# backward elimination
duration_model_full = lmer(log_s_dur ~ 
                             speech_rate_pron_sc + 
                             log_base_dur_sc + 
                             PC1_sc + PC2_sc + PC3_sc + PC4_sc + PC5_sc + 
                             prev_phon_class + next_phon_class + lex_neb_sc +
                             probability_prev_sc + probability_next_sc +
                             recent_mention + register + 
                            log_ratio_s*log_ratio_pl +
                             (1 | word) + 
                             (1 | speaker), 
                           REML = T,
                           data = s_dur)

backward_elim = step(duration_model_full)
backward_elim

duration_model_ratio_small = lmer(log_s_dur ~ 
                            speech_rate_pron_sc + 
                            PC2_sc + 
                            next_phon_class + 
                            register +
                            log_ratio_s*log_ratio_pl +
                            (1 | speaker), 
                          REML = T,
                          data = s_dur)

qqnorm(resid(duration_model_ratio_small))
qqline(resid(duration_model_ratio_small), col="red")

s_dur$dur_resid = resid(duration_model_ratio_small)
s_dur_trim = s_dur[abs(scale(s_dur$dur_resid)) < 2.5,]

duration_model_ratio_small_trim = lmer(duration_model_ratio_small@call$formula, 
                                 REML = T,
                                 data = s_dur_trim)

qqnorm(resid(duration_model_ratio_small_trim))
qqline(resid(duration_model_ratio_small_trim), col="red")

summary(duration_model_ratio_small_trim)

library(sjPlot)

set_theme(legend.title.face = "plain", legend.pos="top", legend.size = 0.8, axis.title.size = 1, axis.title.color = "black", axis.textsize = 0.9)
pm = plot_model(duration_model_ratio_small_trim, type = "eff", terms = c("log_ratio_s", "log_ratio_pl[-4, 0, 4]"), color="bw", legend.title = "Plural Dominance", show.legend = T, title = "", axis.title = c("-s Bias", "-s Duration"))
pm$guides$colour = "legend"
pm$guides$fill = "legend"
pm$labels$title = NULL
pm + geom_ribbon(aes(ymin=pm$data$conf.low, ymax=pm$data$conf.high), alpha = 1, linetype=0, show.legend = T) + 
  geom_ribbon( aes(ymin=pm$data$conf.low, ymax=pm$data$conf.high), alpha = 0, linetype="solid", show.legend = T) + 
  geom_point(data = s_dur_trim, mapping = aes(x = log_ratio_s, y = log_s_dur), inherit.aes = FALSE, size=0.5, color="grey") +
  geom_line(aes(y=pm$data$predicted, x=pm$data$x), color="black", show.legend = T) +
  scale_colour_manual(name="Plural Dominance", values=c("#009E73", "#0072B2", "#D55E00")) + 
  scale_fill_manual(name="Plural Dominance", values=c("#009E73", "#0072B2", "#D55E00")) +
  scale_linetype_manual(name="Plural Dominance", values=c("solid", "dotted", "dashed"))
