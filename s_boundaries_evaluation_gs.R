if (Sys.info()[1] == "Darwin"){
  f_path = "/Volumes/tensusers/timzee/classifier_evaluation/s/grid_search_output/"
} else {
  f_path = "/vol/tensusers/timzee/classifier_evaluation/s/grid_search_output/"
}

network_type = c("FFNN", "BLSTM")
context_frames = c("5", "15")
kal_diff_weight = c("0", "0.5", "1")
apply_penalty = c("0", "1")
n_prec_values = c("0", "10")
n_subs_values = c("0", "10")
n_smooths = c("0", "1", "2", "3", "4")
take_sqrt = c("0", "1")

gs_names = c()
missing_NN = c()
missing_NN_startb = c()
missing_NN_starte = c()
missing_NN_endb = c()
missing_NN_ende = c()
perc20_TR_TS = c()
perc20_TR_TS_a = c()
perc20_TR_TS_c = c()
perc20_TR_TS_d = c()
perc20_TR_TS_k = c()
perc20_TR_TS_o = c()
perc20_TR_TS_ifadv = c()
perc20_TR_TS_ecsd = c()
perc20_TR_TS_startb = c()
perc20_TR_TS_startb_a = c()
perc20_TR_TS_startb_c = c()
perc20_TR_TS_startb_d = c()
perc20_TR_TS_startb_k = c()
perc20_TR_TS_startb_o = c()
perc20_TR_TS_startb_ifadv = c()
perc20_TR_TS_startb_ecsd = c()
perc20_TR_TS_starte = c()
perc20_TR_TS_starte_a = c()
perc20_TR_TS_starte_c = c()
perc20_TR_TS_starte_d = c()
perc20_TR_TS_starte_k = c()
perc20_TR_TS_starte_o = c()
perc20_TR_TS_starte_ifadv = c()
perc20_TR_TS_starte_ecsd = c()
perc20_TR_TS_endb = c()
perc20_TR_TS_endb_a = c()
perc20_TR_TS_endb_c = c()
perc20_TR_TS_endb_d = c()
perc20_TR_TS_endb_k = c()
perc20_TR_TS_endb_o = c()
perc20_TR_TS_endb_ifadv = c()
perc20_TR_TS_endb_ecsd = c()
perc20_TR_TS_ende = c()
perc20_TR_TS_ende_a = c()
perc20_TR_TS_ende_c = c()
perc20_TR_TS_ende_d = c()
perc20_TR_TS_ende_k = c()
perc20_TR_TS_ende_o = c()
perc20_TR_TS_ende_ifadv = c()
perc20_TR_TS_ende_ecsd = c()
perc20_TR_NN = c()
perc20_TR_NN_a = c()
perc20_TR_NN_c = c()
perc20_TR_NN_d = c()
perc20_TR_NN_k = c()
perc20_TR_NN_o = c()
perc20_TR_NN_ifadv = c()
perc20_TR_NN_ecsd = c()
perc20_TR_NN_startb = c()
perc20_TR_NN_startb_a = c()
perc20_TR_NN_startb_c = c()
perc20_TR_NN_startb_d = c()
perc20_TR_NN_startb_k = c()
perc20_TR_NN_startb_o = c()
perc20_TR_NN_startb_ifadv = c()
perc20_TR_NN_startb_ecsd = c()
perc20_TR_NN_starte = c()
perc20_TR_NN_starte_a = c()
perc20_TR_NN_starte_c = c()
perc20_TR_NN_starte_d = c()
perc20_TR_NN_starte_k = c()
perc20_TR_NN_starte_o = c()
perc20_TR_NN_starte_ifadv = c()
perc20_TR_NN_starte_ecsd = c()
perc20_TR_NN_endb = c()
perc20_TR_NN_endb_a = c()
perc20_TR_NN_endb_c = c()
perc20_TR_NN_endb_d = c()
perc20_TR_NN_endb_k = c()
perc20_TR_NN_endb_o = c()
perc20_TR_NN_endb_ifadv = c()
perc20_TR_NN_endb_ecsd = c()
perc20_TR_NN_ende = c()
perc20_TR_NN_ende_a = c()
perc20_TR_NN_ende_c = c()
perc20_TR_NN_ende_d = c()
perc20_TR_NN_ende_k = c()
perc20_TR_NN_ende_o = c()
perc20_TR_NN_ende_ifadv = c()
perc20_TR_NN_ende_ecsd = c()
perc20_SA_NN = c()
perc20_SA_NN_startb = c()
perc20_SA_NN_starte = c()
perc20_SA_NN_endb = c()
perc20_SA_NN_ende = c()
mean_TR_TS = c()
mean_TR_TS_a = c()
mean_TR_TS_c = c()
mean_TR_TS_d = c()
mean_TR_TS_k = c()
mean_TR_TS_o = c()
mean_TR_TS_ifadv = c()
mean_TR_TS_ecsd = c()
mean_TR_TS_startb = c()
mean_TR_TS_startb_a = c()
mean_TR_TS_startb_c = c()
mean_TR_TS_startb_d = c()
mean_TR_TS_startb_k = c()
mean_TR_TS_startb_o = c()
mean_TR_TS_startb_ifadv = c()
mean_TR_TS_startb_ecsd = c()
mean_TR_TS_starte = c()
mean_TR_TS_starte_a = c()
mean_TR_TS_starte_c = c()
mean_TR_TS_starte_d = c()
mean_TR_TS_starte_k = c()
mean_TR_TS_starte_o = c()
mean_TR_TS_starte_ifadv = c()
mean_TR_TS_starte_ecsd = c()
mean_TR_TS_endb = c()
mean_TR_TS_endb_a = c()
mean_TR_TS_endb_c = c()
mean_TR_TS_endb_d = c()
mean_TR_TS_endb_k = c()
mean_TR_TS_endb_o = c()
mean_TR_TS_endb_ifadv = c()
mean_TR_TS_endb_ecsd = c()
mean_TR_TS_ende = c()
mean_TR_TS_ende_a = c()
mean_TR_TS_ende_c = c()
mean_TR_TS_ende_d = c()
mean_TR_TS_ende_k = c()
mean_TR_TS_ende_o = c()
mean_TR_TS_ende_ifadv = c()
mean_TR_TS_ende_ecsd = c()
mean_TR_NN = c()
mean_TR_NN_a = c()
mean_TR_NN_c = c()
mean_TR_NN_d = c()
mean_TR_NN_k = c()
mean_TR_NN_o = c()
mean_TR_NN_ifadv = c()
mean_TR_NN_ecsd = c()
mean_TR_NN_startb = c()
mean_TR_NN_startb_a = c()
mean_TR_NN_startb_c = c()
mean_TR_NN_startb_d = c()
mean_TR_NN_startb_k = c()
mean_TR_NN_startb_o = c()
mean_TR_NN_startb_ifadv = c()
mean_TR_NN_startb_ecsd = c()
mean_TR_NN_starte = c()
mean_TR_NN_starte_a = c()
mean_TR_NN_starte_c = c()
mean_TR_NN_starte_d = c()
mean_TR_NN_starte_k = c()
mean_TR_NN_starte_o = c()
mean_TR_NN_starte_ifadv = c()
mean_TR_NN_starte_ecsd = c()
mean_TR_NN_endb = c()
mean_TR_NN_endb_a = c()
mean_TR_NN_endb_c = c()
mean_TR_NN_endb_d = c()
mean_TR_NN_endb_k = c()
mean_TR_NN_endb_o = c()
mean_TR_NN_endb_ifadv = c()
mean_TR_NN_endb_ecsd = c()
mean_TR_NN_ende = c()
mean_TR_NN_ende_a = c()
mean_TR_NN_ende_c = c()
mean_TR_NN_ende_d = c()
mean_TR_NN_ende_k = c()
mean_TR_NN_ende_o = c()
mean_TR_NN_ende_ifadv = c()
mean_TR_NN_ende_ecsd = c()
mean_SA_NN = c()
mean_SA_NN_startb = c()
mean_SA_NN_starte = c()
mean_SA_NN_endb = c()
mean_SA_NN_ende = c()
perc20_TR_KAL = c()
perc20_TR_KAL_a = c()
perc20_TR_KAL_c = c()
perc20_TR_KAL_d = c()
perc20_TR_KAL_k = c()
perc20_TR_KAL_o = c()
perc20_TR_KAL_ifadv = c()
perc20_TR_KAL_ecsd = c()
perc20_TR_KAL_startb = c()
perc20_TR_KAL_startb_a = c()
perc20_TR_KAL_startb_c = c()
perc20_TR_KAL_startb_d = c()
perc20_TR_KAL_startb_k = c()
perc20_TR_KAL_startb_o = c()
perc20_TR_KAL_startb_ifadv = c()
perc20_TR_KAL_startb_ecsd = c()
perc20_TR_KAL_ende = c()
perc20_TR_KAL_ende_a = c()
perc20_TR_KAL_ende_c = c()
perc20_TR_KAL_ende_d = c()
perc20_TR_KAL_ende_k = c()
perc20_TR_KAL_ende_o = c()
perc20_TR_KAL_ende_ifadv = c()
perc20_TR_KAL_ende_ecsd = c()
mean_TR_KAL = c()
mean_TR_KAL_a = c()
mean_TR_KAL_c = c()
mean_TR_KAL_d = c()
mean_TR_KAL_k = c()
mean_TR_KAL_o = c()
mean_TR_KAL_ifadv = c()
mean_TR_KAL_ecsd = c()
mean_TR_KAL_startb = c()
mean_TR_KAL_startb_a = c()
mean_TR_KAL_startb_c = c()
mean_TR_KAL_startb_d = c()
mean_TR_KAL_startb_k = c()
mean_TR_KAL_startb_o = c()
mean_TR_KAL_startb_ifadv = c()
mean_TR_KAL_startb_ecsd = c()
mean_TR_KAL_ende = c()
mean_TR_KAL_ende_a = c()
mean_TR_KAL_ende_c = c()
mean_TR_KAL_ende_d = c()
mean_TR_KAL_ende_k = c()
mean_TR_KAL_ende_o = c()
mean_TR_KAL_ende_ifadv = c()
mean_TR_KAL_ende_ecsd = c()

for (nt in network_type){
  for (cf in context_frames){
    for (kdw in kal_diff_weight){
      for (ap in apply_penalty){
        for (npv in n_prec_values){
          for (nsv in n_subs_values){
            for (ns in n_smooths){
              for (ts in take_sqrt){
                print(paste(nt, cf, kdw, ap, npv, nsv, ns, ts, sep = "_"))
                gs_name = paste(nt, cf, kdw, ap, npv, nsv, ns, ts, sep = "_")
                gs_names = c(gs_names, gs_name)
                bounds = read.csv(paste(f_path, gs_name, ".csv", sep = ""))
                bounds$corpus = as.factor(ifelse(substr(bounds$wav, 1, 1) %in% c("a", "c", "d", "k", "o"), paste("cgn-", substr(bounds$wav, 1, 1), sep = ""), ifelse(substr(bounds$wav, 1, 1) == "D", "ifadv", "ecsd")))
                missing_NN = c(missing_NN, sum(is.na(bounds$nn_start_b)) + sum(is.na(bounds$nn_start_e)) + sum(is.na(bounds$nn_end_b)) + sum(is.na(bounds$nn_end_e)))
                missing_NN_startb = c(missing_NN_startb, sum(is.na(bounds$nn_start_b)))
                missing_NN_starte = c(missing_NN_starte, sum(is.na(bounds$nn_start_e)))
                missing_NN_endb = c(missing_NN_endb, sum(is.na(bounds$nn_end_b)))
                missing_NN_ende = c(missing_NN_ende, sum(is.na(bounds$nn_end_e)))
                bounds$TR_TS_start_b = bounds$TR_start_b - bounds$TS_start_b
                bounds$TR_nn_start_b = bounds$TR_start_b - bounds$nn_start_b
                bounds$TR_kal_start_b = bounds$TR_start_b - bounds$kal_b
                bounds$TS_nn_start_b = bounds$TS_start_b - bounds$nn_start_b
                bounds$TS_TR_start_b = bounds$TS_start_b - bounds$TR_start_b
                bounds$TR_TS_start_e = bounds$TR_start_e - bounds$TS_start_e
                bounds$TR_nn_start_e = bounds$TR_start_e - bounds$nn_start_e
                bounds$TS_nn_start_e = bounds$TS_start_e - bounds$nn_start_e
                bounds$TS_TR_start_e = bounds$TS_start_e - bounds$TR_start_e
                bounds$TR_TS_end_b = bounds$TR_end_b - bounds$TS_end_b
                bounds$TR_nn_end_b = bounds$TR_end_b - bounds$nn_end_b
                bounds$TS_nn_end_b = bounds$TS_end_b - bounds$nn_end_b
                bounds$TS_TR_end_b = bounds$TS_end_b - bounds$TR_end_b
                bounds$TR_TS_end_e = bounds$TR_end_e - bounds$TS_end_e
                bounds$TR_nn_end_e = bounds$TR_end_e - bounds$nn_end_e
                bounds$TR_wrd_end_e = bounds$TR_end_e - bounds$wrd_e
                bounds$TR_kal_end_e = bounds$TR_end_e - bounds$kal_e
                bounds$TS_nn_end_e = bounds$TS_end_e - bounds$nn_end_e
                bounds$TS_TR_end_e = bounds$TS_end_e - bounds$TR_end_e
                bounds_TR_TS_long = c(bounds$TR_TS_start_b, bounds$TR_TS_start_e, bounds$TR_TS_end_b, bounds$TR_TS_end_e)
                bounds_TR_NN_long = c(bounds$TR_nn_start_b, bounds$TR_nn_start_e, bounds$TR_nn_end_b, bounds$TR_nn_end_e)
                bounds_SA_NN_long = c(bounds$TR_nn_start_b, bounds$TR_nn_start_e, bounds$TR_nn_end_b, bounds$TR_nn_end_e, bounds$TS_nn_start_b, bounds$TS_nn_start_e, bounds$TS_nn_end_b, bounds$TS_nn_end_e)
                bounds_TR_KAL_long = c(bounds$TR_kal_start_b, bounds$TR_kal_end_e)
                mean_TR_TS = c(mean_TR_TS, mean(abs(bounds_TR_TS_long), na.rm = TRUE))
                perc20_TR_TS = c(perc20_TR_TS, NROW(na.omit(bounds_TR_TS_long[abs(bounds_TR_TS_long) < 0.02])) / NROW(bounds_TR_TS_long))
                mean_TR_NN = c(mean_TR_NN, mean(abs(bounds_TR_NN_long), na.rm = TRUE))
                perc20_TR_NN = c(perc20_TR_NN, NROW(na.omit(bounds_TR_NN_long[abs(bounds_TR_NN_long) < 0.02])) / NROW(bounds_TR_NN_long))
                mean_SA_NN = c(mean_SA_NN, mean(abs(bounds_SA_NN_long), na.rm = TRUE))
                perc20_SA_NN = c(perc20_SA_NN, NROW(na.omit(bounds_SA_NN_long[abs(bounds_SA_NN_long) < 0.02])) / NROW(bounds_SA_NN_long))
                mean_TR_KAL = c(mean_TR_KAL, mean(abs(bounds_TR_KAL_long), na.rm = TRUE))
                perc20_TR_KAL = c(perc20_TR_KAL, NROW(na.omit(bounds_TR_KAL_long[abs(bounds_TR_KAL_long) < 0.02])) / NROW(bounds_TR_KAL_long))
                mean_TR_TS_startb = c(mean_TR_TS_startb, mean(abs(bounds$TR_TS_start_b), na.rm = TRUE))
                perc20_TR_TS_startb = c(perc20_TR_TS_startb, NROW(na.omit(bounds[abs(bounds$TR_TS_start_b) < 0.02,c("TR_TS_start_b")])) / NROW(bounds[,c("TR_TS_start_b")]))
                mean_TR_NN_startb = c(mean_TR_NN_startb, mean(abs(bounds$TR_nn_start_b), na.rm = TRUE))
                perc20_TR_NN_startb = c(perc20_TR_NN_startb, NROW(na.omit(bounds[abs(bounds$TR_nn_start_b) < 0.02,c("TR_nn_start_b")])) / NROW(bounds[,c("TR_nn_start_b")]))
                mean_SA_NN_startb = c(mean_SA_NN_startb, mean(abs(c(bounds$TR_nn_start_b, bounds$TS_nn_start_b)), na.rm = TRUE))
                perc20_SA_NN_startb = c(perc20_SA_NN_startb, (NROW(na.omit(bounds[abs(bounds$TR_nn_start_b) < 0.02,c("TR_nn_start_b")])) + NROW(na.omit(bounds[abs(bounds$TS_nn_start_b) < 0.02,c("TS_nn_start_b")]))) / (NROW(bounds[,c("TR_nn_start_b")]) + NROW(bounds[,c("TS_nn_start_b")])))
                mean_TR_KAL_startb = c(mean_TR_KAL_startb, mean(abs(bounds$TR_kal_start_b), na.rm = TRUE))
                perc20_TR_KAL_startb = c(perc20_TR_KAL_startb, NROW(na.omit(bounds[abs(bounds$TR_kal_start_b) < 0.02,c("TR_kal_start_b")])) / NROW(bounds[,c("TR_kal_start_b")]))
                mean_TR_TS_starte = c(mean_TR_TS_starte, mean(abs(bounds$TR_TS_start_e), na.rm = TRUE))
                perc20_TR_TS_starte = c(perc20_TR_TS_starte, NROW(na.omit(bounds[abs(bounds$TR_TS_start_e) < 0.02,c("TR_TS_start_e")])) / NROW(bounds[,c("TR_TS_start_e")]))
                mean_TR_NN_starte = c(mean_TR_NN_starte, mean(abs(bounds$TR_nn_start_e), na.rm = TRUE))
                perc20_TR_NN_starte = c(perc20_TR_NN_starte, NROW(na.omit(bounds[abs(bounds$TR_nn_start_e) < 0.02,c("TR_nn_start_e")])) / NROW(bounds[,c("TR_nn_start_e")]))
                mean_SA_NN_starte = c(mean_SA_NN_starte, mean(abs(c(bounds$TR_nn_start_e, bounds$TS_nn_start_e)), na.rm = TRUE))
                perc20_SA_NN_starte = c(perc20_SA_NN_starte, (NROW(na.omit(bounds[abs(bounds$TR_nn_start_e) < 0.02,c("TR_nn_start_e")])) + NROW(na.omit(bounds[abs(bounds$TS_nn_start_e) < 0.02,c("TS_nn_start_e")]))) / (NROW(bounds[,c("TR_nn_start_e")]) + NROW(bounds[,c("TS_nn_start_e")])))
		            mean_TR_TS_endb = c(mean_TR_TS_endb, mean(abs(bounds$TR_TS_end_b), na.rm = TRUE))
                perc20_TR_TS_endb = c(perc20_TR_TS_endb, NROW(na.omit(bounds[abs(bounds$TR_TS_end_b) < 0.02,c("TR_TS_end_b")])) / NROW(bounds[,c("TR_TS_end_b")]))
                mean_TR_NN_endb = c(mean_TR_NN_endb, mean(abs(bounds$TR_nn_end_b), na.rm = TRUE))
                perc20_TR_NN_endb = c(perc20_TR_NN_endb, NROW(na.omit(bounds[abs(bounds$TR_nn_end_b) < 0.02,c("TR_nn_end_b")])) / NROW(bounds[,c("TR_nn_end_b")]))
                mean_SA_NN_endb = c(mean_SA_NN_endb, mean(abs(c(bounds$TR_nn_end_b, bounds$TS_nn_end_b)), na.rm = TRUE))
                perc20_SA_NN_endb = c(perc20_SA_NN_endb, (NROW(na.omit(bounds[abs(bounds$TR_nn_end_b) < 0.02,c("TR_nn_end_b")])) + NROW(na.omit(bounds[abs(bounds$TS_nn_end_b) < 0.02,c("TS_nn_end_b")]))) / (NROW(bounds[,c("TR_nn_end_b")]) + NROW(bounds[,c("TS_nn_end_b")])))
		            mean_TR_TS_ende = c(mean_TR_TS_ende, mean(abs(bounds$TR_TS_end_e), na.rm = TRUE))
                perc20_TR_TS_ende = c(perc20_TR_TS_ende, NROW(na.omit(bounds[abs(bounds$TR_TS_end_e) < 0.02,c("TR_TS_end_e")])) / NROW(bounds[,c("TR_TS_end_e")]))
                mean_TR_NN_ende = c(mean_TR_NN_ende, mean(abs(bounds$TR_nn_end_e), na.rm = TRUE))
                perc20_TR_NN_ende = c(perc20_TR_NN_ende, NROW(na.omit(bounds[abs(bounds$TR_nn_end_e) < 0.02,c("TR_nn_end_e")])) / NROW(bounds[,c("TR_nn_end_e")]))
                mean_SA_NN_ende = c(mean_SA_NN_ende, mean(abs(c(bounds$TR_nn_end_e, bounds$TS_nn_end_e)), na.rm = TRUE))
                perc20_SA_NN_ende = c(perc20_SA_NN_ende, (NROW(na.omit(bounds[abs(bounds$TR_nn_end_e) < 0.02,c("TR_nn_end_e")])) + NROW(na.omit(bounds[abs(bounds$TS_nn_end_e) < 0.02,c("TS_nn_end_e")]))) / (NROW(bounds[,c("TR_nn_end_e")]) + NROW(bounds[,c("TS_nn_end_e")])))
                mean_TR_KAL_ende = c(mean_TR_KAL_ende, mean(abs(bounds$TR_kal_end_e), na.rm = TRUE))
                perc20_TR_KAL_ende = c(perc20_TR_KAL_ende, NROW(na.omit(bounds[abs(bounds$TR_kal_end_e) < 0.02,c("TR_kal_end_e")])) / NROW(bounds[,c("TR_kal_end_e")]))
                bounds_a = bounds[bounds$corpus == "cgn-a",]
                bounds_TR_TS_long = c(bounds_a$TR_TS_start_b, bounds_a$TR_TS_start_e, bounds_a$TR_TS_end_b, bounds_a$TR_TS_end_e)
                bounds_TR_NN_long = c(bounds_a$TR_nn_start_b, bounds_a$TR_nn_start_e, bounds_a$TR_nn_end_b, bounds_a$TR_nn_end_e)
                bounds_TR_KAL_long = c(bounds_a$TR_kal_start_b, bounds_a$TR_kal_end_e)
                mean_TR_TS_a = c(mean_TR_TS_a, mean(abs(bounds_TR_TS_long), na.rm = TRUE))
                perc20_TR_TS_a = c(perc20_TR_TS_a, NROW(na.omit(bounds_TR_TS_long[abs(bounds_TR_TS_long) < 0.02])) / NROW(bounds_TR_TS_long))
                mean_TR_NN_a = c(mean_TR_NN_a, mean(abs(bounds_TR_NN_long), na.rm = TRUE))
                perc20_TR_NN_a = c(perc20_TR_NN_a, NROW(na.omit(bounds_TR_NN_long[abs(bounds_TR_NN_long) < 0.02])) / NROW(bounds_TR_NN_long))
                mean_TR_KAL_a = c(mean_TR_KAL_a, mean(abs(bounds_TR_KAL_long), na.rm = TRUE))
                perc20_TR_KAL_a = c(perc20_TR_KAL_a, NROW(na.omit(bounds_TR_KAL_long[abs(bounds_TR_KAL_long) < 0.02])) / NROW(bounds_TR_KAL_long))
                mean_TR_TS_startb_a = c(mean_TR_TS_startb_a, mean(abs(bounds_a$TR_TS_start_b), na.rm = TRUE))
                perc20_TR_TS_startb_a = c(perc20_TR_TS_startb_a, NROW(na.omit(bounds_a[abs(bounds_a$TR_TS_start_b) < 0.02,c("TR_TS_start_b")])) / NROW(bounds_a[,c("TR_TS_start_b")]))
                mean_TR_NN_startb_a = c(mean_TR_NN_startb_a, mean(abs(bounds_a$TR_nn_start_b), na.rm = TRUE))
                perc20_TR_NN_startb_a = c(perc20_TR_NN_startb_a, NROW(na.omit(bounds_a[abs(bounds_a$TR_nn_start_b) < 0.02,c("TR_nn_start_b")])) / NROW(bounds_a[,c("TR_nn_start_b")]))
                mean_TR_KAL_startb_a = c(mean_TR_KAL_startb_a, mean(abs(bounds_a$TR_kal_start_b), na.rm = TRUE))
                perc20_TR_KAL_startb_a = c(perc20_TR_KAL_startb_a, NROW(na.omit(bounds_a[abs(bounds_a$TR_kal_start_b) < 0.02,c("TR_kal_start_b")])) / NROW(bounds_a[,c("TR_kal_start_b")]))
                mean_TR_TS_starte_a = c(mean_TR_TS_starte_a, mean(abs(bounds_a$TR_TS_start_e), na.rm = TRUE))
                perc20_TR_TS_starte_a = c(perc20_TR_TS_starte_a, NROW(na.omit(bounds_a[abs(bounds_a$TR_TS_start_e) < 0.02,c("TR_TS_start_e")])) / NROW(bounds_a[,c("TR_TS_start_e")]))
                mean_TR_NN_starte_a = c(mean_TR_NN_starte_a, mean(abs(bounds_a$TR_nn_start_e), na.rm = TRUE))
                perc20_TR_NN_starte_a = c(perc20_TR_NN_starte_a, NROW(na.omit(bounds_a[abs(bounds_a$TR_nn_start_e) < 0.02,c("TR_nn_start_e")])) / NROW(bounds_a[,c("TR_nn_start_e")]))
                mean_TR_TS_endb_a = c(mean_TR_TS_endb_a, mean(abs(bounds_a$TR_TS_end_b), na.rm = TRUE))
                perc20_TR_TS_endb_a = c(perc20_TR_TS_endb_a, NROW(na.omit(bounds_a[abs(bounds_a$TR_TS_end_b) < 0.02,c("TR_TS_end_b")])) / NROW(bounds_a[,c("TR_TS_end_b")]))
                mean_TR_NN_endb_a = c(mean_TR_NN_endb_a, mean(abs(bounds_a$TR_nn_end_b), na.rm = TRUE))
                perc20_TR_NN_endb_a = c(perc20_TR_NN_endb_a, NROW(na.omit(bounds_a[abs(bounds_a$TR_nn_end_b) < 0.02,c("TR_nn_end_b")])) / NROW(bounds_a[,c("TR_nn_end_b")]))
                mean_TR_TS_ende_a = c(mean_TR_TS_ende_a, mean(abs(bounds_a$TR_TS_end_e), na.rm = TRUE))
                perc20_TR_TS_ende_a = c(perc20_TR_TS_ende_a, NROW(na.omit(bounds_a[abs(bounds_a$TR_TS_end_e) < 0.02,c("TR_TS_end_e")])) / NROW(bounds_a[,c("TR_TS_end_e")]))
                mean_TR_NN_ende_a = c(mean_TR_NN_ende_a, mean(abs(bounds_a$TR_nn_end_e), na.rm = TRUE))
                perc20_TR_NN_ende_a = c(perc20_TR_NN_ende_a, NROW(na.omit(bounds_a[abs(bounds_a$TR_nn_end_e) < 0.02,c("TR_nn_end_e")])) / NROW(bounds_a[,c("TR_nn_end_e")]))
                mean_TR_KAL_ende_a = c(mean_TR_KAL_ende_a, mean(abs(bounds_a$TR_kal_end_e), na.rm = TRUE))
                perc20_TR_KAL_ende_a = c(perc20_TR_KAL_ende_a, NROW(na.omit(bounds_a[abs(bounds_a$TR_kal_end_e) < 0.02,c("TR_kal_end_e")])) / NROW(bounds_a[,c("TR_kal_end_e")]))
                
		            bounds_a = bounds[bounds$corpus == "cgn-c",]
                bounds_TR_TS_long = c(bounds_a$TR_TS_start_b, bounds_a$TR_TS_start_e, bounds_a$TR_TS_end_b, bounds_a$TR_TS_end_e)
                bounds_TR_NN_long = c(bounds_a$TR_nn_start_b, bounds_a$TR_nn_start_e, bounds_a$TR_nn_end_b, bounds_a$TR_nn_end_e)
                mean_TR_TS_c = c(mean_TR_TS_c, mean(abs(bounds_TR_TS_long), na.rm = TRUE))
                perc20_TR_TS_c = c(perc20_TR_TS_c, NROW(na.omit(bounds_TR_TS_long[abs(bounds_TR_TS_long) < 0.02])) / NROW(bounds_TR_TS_long))
                mean_TR_NN_c = c(mean_TR_NN_c, mean(abs(bounds_TR_NN_long), na.rm = TRUE))
                perc20_TR_NN_c = c(perc20_TR_NN_c, NROW(na.omit(bounds_TR_NN_long[abs(bounds_TR_NN_long) < 0.02])) / NROW(bounds_TR_NN_long))
                mean_TR_KAL_c = c(mean_TR_KAL_c, mean(abs(bounds_TR_KAL_long), na.rm = TRUE))
                perc20_TR_KAL_c = c(perc20_TR_KAL_c, NROW(na.omit(bounds_TR_KAL_long[abs(bounds_TR_KAL_long) < 0.02])) / NROW(bounds_TR_KAL_long))
                mean_TR_TS_startb_c = c(mean_TR_TS_startb_c, mean(abs(bounds_a$TR_TS_start_b), na.rm = TRUE))
                perc20_TR_TS_startb_c = c(perc20_TR_TS_startb_c, NROW(na.omit(bounds_a[abs(bounds_a$TR_TS_start_b) < 0.02,c("TR_TS_start_b")])) / NROW(bounds_a[,c("TR_TS_start_b")]))
                mean_TR_NN_startb_c = c(mean_TR_NN_startb_c, mean(abs(bounds_a$TR_nn_start_b), na.rm = TRUE))
                perc20_TR_NN_startb_c = c(perc20_TR_NN_startb_c, NROW(na.omit(bounds_a[abs(bounds_a$TR_nn_start_b) < 0.02,c("TR_nn_start_b")])) / NROW(bounds_a[,c("TR_nn_start_b")]))
                mean_TR_KAL_startb_c = c(mean_TR_KAL_startb_c, mean(abs(bounds_a$TR_kal_start_b), na.rm = TRUE))
                perc20_TR_KAL_startb_c = c(perc20_TR_KAL_startb_c, NROW(na.omit(bounds_a[abs(bounds_a$TR_kal_start_b) < 0.02,c("TR_kal_start_b")])) / NROW(bounds_a[,c("TR_kal_start_b")]))
                mean_TR_TS_starte_c = c(mean_TR_TS_starte_c, mean(abs(bounds_a$TR_TS_start_e), na.rm = TRUE))
                perc20_TR_TS_starte_c = c(perc20_TR_TS_starte_c, NROW(na.omit(bounds_a[abs(bounds_a$TR_TS_start_e) < 0.02,c("TR_TS_start_e")])) / NROW(bounds_a[,c("TR_TS_start_e")]))
                mean_TR_NN_starte_c = c(mean_TR_NN_starte_c, mean(abs(bounds_a$TR_nn_start_e), na.rm = TRUE))
                perc20_TR_NN_starte_c = c(perc20_TR_NN_starte_c, NROW(na.omit(bounds_a[abs(bounds_a$TR_nn_start_e) < 0.02,c("TR_nn_start_e")])) / NROW(bounds_a[,c("TR_nn_start_e")]))
                mean_TR_TS_endb_c = c(mean_TR_TS_endb_c, mean(abs(bounds_a$TR_TS_end_b), na.rm = TRUE))
                perc20_TR_TS_endb_c = c(perc20_TR_TS_endb_c, NROW(na.omit(bounds_a[abs(bounds_a$TR_TS_end_b) < 0.02,c("TR_TS_end_b")])) / NROW(bounds_a[,c("TR_TS_end_b")]))
                mean_TR_NN_endb_c = c(mean_TR_NN_endb_c, mean(abs(bounds_a$TR_nn_end_b), na.rm = TRUE))
                perc20_TR_NN_endb_c = c(perc20_TR_NN_endb_c, NROW(na.omit(bounds_a[abs(bounds_a$TR_nn_end_b) < 0.02,c("TR_nn_end_b")])) / NROW(bounds_a[,c("TR_nn_end_b")]))
                mean_TR_TS_ende_c = c(mean_TR_TS_ende_c, mean(abs(bounds_a$TR_TS_end_e), na.rm = TRUE))
                perc20_TR_TS_ende_c = c(perc20_TR_TS_ende_c, NROW(na.omit(bounds_a[abs(bounds_a$TR_TS_end_e) < 0.02,c("TR_TS_end_e")])) / NROW(bounds_a[,c("TR_TS_end_e")]))
                mean_TR_NN_ende_c = c(mean_TR_NN_ende_c, mean(abs(bounds_a$TR_nn_end_e), na.rm = TRUE))
                perc20_TR_NN_ende_c = c(perc20_TR_NN_ende_c, NROW(na.omit(bounds_a[abs(bounds_a$TR_nn_end_e) < 0.02,c("TR_nn_end_e")])) / NROW(bounds_a[,c("TR_nn_end_e")]))
                mean_TR_KAL_ende_c = c(mean_TR_KAL_ende_c, mean(abs(bounds_a$TR_kal_end_e), na.rm = TRUE))
                perc20_TR_KAL_ende_c = c(perc20_TR_KAL_ende_c, NROW(na.omit(bounds_a[abs(bounds_a$TR_kal_end_e) < 0.02,c("TR_kal_end_e")])) / NROW(bounds_a[,c("TR_kal_end_e")]))
                
		            bounds_a = bounds[bounds$corpus == "cgn-d",]
                bounds_TR_TS_long = c(bounds_a$TR_TS_start_b, bounds_a$TR_TS_start_e, bounds_a$TR_TS_end_b, bounds_a$TR_TS_end_e)
                bounds_TR_NN_long = c(bounds_a$TR_nn_start_b, bounds_a$TR_nn_start_e, bounds_a$TR_nn_end_b, bounds_a$TR_nn_end_e)
                mean_TR_TS_d = c(mean_TR_TS_d, mean(abs(bounds_TR_TS_long), na.rm = TRUE))
                perc20_TR_TS_d = c(perc20_TR_TS_d, NROW(na.omit(bounds_TR_TS_long[abs(bounds_TR_TS_long) < 0.02])) / NROW(bounds_TR_TS_long))
                mean_TR_NN_d = c(mean_TR_NN_d, mean(abs(bounds_TR_NN_long), na.rm = TRUE))
                perc20_TR_NN_d = c(perc20_TR_NN_d, NROW(na.omit(bounds_TR_NN_long[abs(bounds_TR_NN_long) < 0.02])) / NROW(bounds_TR_NN_long))
                mean_TR_KAL_d = c(mean_TR_KAL_d, mean(abs(bounds_TR_KAL_long), na.rm = TRUE))
                perc20_TR_KAL_d = c(perc20_TR_KAL_d, NROW(na.omit(bounds_TR_KAL_long[abs(bounds_TR_KAL_long) < 0.02])) / NROW(bounds_TR_KAL_long))
                mean_TR_TS_startb_d = c(mean_TR_TS_startb_d, mean(abs(bounds_a$TR_TS_start_b), na.rm = TRUE))
                perc20_TR_TS_startb_d = c(perc20_TR_TS_startb_d, NROW(na.omit(bounds_a[abs(bounds_a$TR_TS_start_b) < 0.02,c("TR_TS_start_b")])) / NROW(bounds_a[,c("TR_TS_start_b")]))
                mean_TR_NN_startb_d = c(mean_TR_NN_startb_d, mean(abs(bounds_a$TR_nn_start_b), na.rm = TRUE))
                perc20_TR_NN_startb_d = c(perc20_TR_NN_startb_d, NROW(na.omit(bounds_a[abs(bounds_a$TR_nn_start_b) < 0.02,c("TR_nn_start_b")])) / NROW(bounds_a[,c("TR_nn_start_b")]))
                mean_TR_KAL_startb_d = c(mean_TR_KAL_startb_d, mean(abs(bounds_a$TR_kal_start_b), na.rm = TRUE))
                perc20_TR_KAL_startb_d = c(perc20_TR_KAL_startb_d, NROW(na.omit(bounds_a[abs(bounds_a$TR_kal_start_b) < 0.02,c("TR_kal_start_b")])) / NROW(bounds_a[,c("TR_kal_start_b")]))
                mean_TR_TS_starte_d = c(mean_TR_TS_starte_d, mean(abs(bounds_a$TR_TS_start_e), na.rm = TRUE))
                perc20_TR_TS_starte_d = c(perc20_TR_TS_starte_d, NROW(na.omit(bounds_a[abs(bounds_a$TR_TS_start_e) < 0.02,c("TR_TS_start_e")])) / NROW(bounds_a[,c("TR_TS_start_e")]))
                mean_TR_NN_starte_d = c(mean_TR_NN_starte_d, mean(abs(bounds_a$TR_nn_start_e), na.rm = TRUE))
                perc20_TR_NN_starte_d = c(perc20_TR_NN_starte_d, NROW(na.omit(bounds_a[abs(bounds_a$TR_nn_start_e) < 0.02,c("TR_nn_start_e")])) / NROW(bounds_a[,c("TR_nn_start_e")]))
                mean_TR_TS_endb_d = c(mean_TR_TS_endb_d, mean(abs(bounds_a$TR_TS_end_b), na.rm = TRUE))
                perc20_TR_TS_endb_d = c(perc20_TR_TS_endb_d, NROW(na.omit(bounds_a[abs(bounds_a$TR_TS_end_b) < 0.02,c("TR_TS_end_b")])) / NROW(bounds_a[,c("TR_TS_end_b")]))
                mean_TR_NN_endb_d = c(mean_TR_NN_endb_d, mean(abs(bounds_a$TR_nn_end_b), na.rm = TRUE))
                perc20_TR_NN_endb_d = c(perc20_TR_NN_endb_d, NROW(na.omit(bounds_a[abs(bounds_a$TR_nn_end_b) < 0.02,c("TR_nn_end_b")])) / NROW(bounds_a[,c("TR_nn_end_b")]))
                mean_TR_TS_ende_d = c(mean_TR_TS_ende_d, mean(abs(bounds_a$TR_TS_end_e), na.rm = TRUE))
                perc20_TR_TS_ende_d = c(perc20_TR_TS_ende_d, NROW(na.omit(bounds_a[abs(bounds_a$TR_TS_end_e) < 0.02,c("TR_TS_end_e")])) / NROW(bounds_a[,c("TR_TS_end_e")]))
                mean_TR_NN_ende_d = c(mean_TR_NN_ende_d, mean(abs(bounds_a$TR_nn_end_e), na.rm = TRUE))
                perc20_TR_NN_ende_d = c(perc20_TR_NN_ende_d, NROW(na.omit(bounds_a[abs(bounds_a$TR_nn_end_e) < 0.02,c("TR_nn_end_e")])) / NROW(bounds_a[,c("TR_nn_end_e")]))
                mean_TR_KAL_ende_d = c(mean_TR_KAL_ende_d, mean(abs(bounds_a$TR_kal_end_e), na.rm = TRUE))
                perc20_TR_KAL_ende_d = c(perc20_TR_KAL_ende_d, NROW(na.omit(bounds_a[abs(bounds_a$TR_kal_end_e) < 0.02,c("TR_kal_end_e")])) / NROW(bounds_a[,c("TR_kal_end_e")]))
                
		            bounds_a = bounds[bounds$corpus == "cgn-k",]
                bounds_TR_TS_long = c(bounds_a$TR_TS_start_b, bounds_a$TR_TS_start_e, bounds_a$TR_TS_end_b, bounds_a$TR_TS_end_e)
                bounds_TR_NN_long = c(bounds_a$TR_nn_start_b, bounds_a$TR_nn_start_e, bounds_a$TR_nn_end_b, bounds_a$TR_nn_end_e)
                mean_TR_TS_k = c(mean_TR_TS_k, mean(abs(bounds_TR_TS_long), na.rm = TRUE))
                perc20_TR_TS_k = c(perc20_TR_TS_k, NROW(na.omit(bounds_TR_TS_long[abs(bounds_TR_TS_long) < 0.02])) / NROW(bounds_TR_TS_long))
                mean_TR_NN_k = c(mean_TR_NN_k, mean(abs(bounds_TR_NN_long), na.rm = TRUE))
                perc20_TR_NN_k = c(perc20_TR_NN_k, NROW(na.omit(bounds_TR_NN_long[abs(bounds_TR_NN_long) < 0.02])) / NROW(bounds_TR_NN_long))
                mean_TR_KAL_k = c(mean_TR_KAL_k, mean(abs(bounds_TR_KAL_long), na.rm = TRUE))
                perc20_TR_KAL_k = c(perc20_TR_KAL_k, NROW(na.omit(bounds_TR_KAL_long[abs(bounds_TR_KAL_long) < 0.02])) / NROW(bounds_TR_KAL_long))
                mean_TR_TS_startb_k = c(mean_TR_TS_startb_k, mean(abs(bounds_a$TR_TS_start_b), na.rm = TRUE))
                perc20_TR_TS_startb_k = c(perc20_TR_TS_startb_k, NROW(na.omit(bounds_a[abs(bounds_a$TR_TS_start_b) < 0.02,c("TR_TS_start_b")])) / NROW(bounds_a[,c("TR_TS_start_b")]))
                mean_TR_NN_startb_k = c(mean_TR_NN_startb_k, mean(abs(bounds_a$TR_nn_start_b), na.rm = TRUE))
                perc20_TR_NN_startb_k = c(perc20_TR_NN_startb_k, NROW(na.omit(bounds_a[abs(bounds_a$TR_nn_start_b) < 0.02,c("TR_nn_start_b")])) / NROW(bounds_a[,c("TR_nn_start_b")]))
                mean_TR_KAL_startb_k = c(mean_TR_KAL_startb_k, mean(abs(bounds_a$TR_kal_start_b), na.rm = TRUE))
                perc20_TR_KAL_startb_k = c(perc20_TR_KAL_startb_k, NROW(na.omit(bounds_a[abs(bounds_a$TR_kal_start_b) < 0.02,c("TR_kal_start_b")])) / NROW(bounds_a[,c("TR_kal_start_b")]))
                mean_TR_TS_starte_k = c(mean_TR_TS_starte_k, mean(abs(bounds_a$TR_TS_start_e), na.rm = TRUE))
                perc20_TR_TS_starte_k = c(perc20_TR_TS_starte_k, NROW(na.omit(bounds_a[abs(bounds_a$TR_TS_start_e) < 0.02,c("TR_TS_start_e")])) / NROW(bounds_a[,c("TR_TS_start_e")]))
                mean_TR_NN_starte_k = c(mean_TR_NN_starte_k, mean(abs(bounds_a$TR_nn_start_e), na.rm = TRUE))
                perc20_TR_NN_starte_k = c(perc20_TR_NN_starte_k, NROW(na.omit(bounds_a[abs(bounds_a$TR_nn_start_e) < 0.02,c("TR_nn_start_e")])) / NROW(bounds_a[,c("TR_nn_start_e")]))
                mean_TR_TS_endb_k = c(mean_TR_TS_endb_k, mean(abs(bounds_a$TR_TS_end_b), na.rm = TRUE))
                perc20_TR_TS_endb_k = c(perc20_TR_TS_endb_k, NROW(na.omit(bounds_a[abs(bounds_a$TR_TS_end_b) < 0.02,c("TR_TS_end_b")])) / NROW(bounds_a[,c("TR_TS_end_b")]))
                mean_TR_NN_endb_k = c(mean_TR_NN_endb_k, mean(abs(bounds_a$TR_nn_end_b), na.rm = TRUE))
                perc20_TR_NN_endb_k = c(perc20_TR_NN_endb_k, NROW(na.omit(bounds_a[abs(bounds_a$TR_nn_end_b) < 0.02,c("TR_nn_end_b")])) / NROW(bounds_a[,c("TR_nn_end_b")]))
                mean_TR_TS_ende_k = c(mean_TR_TS_ende_k, mean(abs(bounds_a$TR_TS_end_e), na.rm = TRUE))
                perc20_TR_TS_ende_k = c(perc20_TR_TS_ende_k, NROW(na.omit(bounds_a[abs(bounds_a$TR_TS_end_e) < 0.02,c("TR_TS_end_e")])) / NROW(bounds_a[,c("TR_TS_end_e")]))
                mean_TR_NN_ende_k = c(mean_TR_NN_ende_k, mean(abs(bounds_a$TR_nn_end_e), na.rm = TRUE))
                perc20_TR_NN_ende_k = c(perc20_TR_NN_ende_k, NROW(na.omit(bounds_a[abs(bounds_a$TR_nn_end_e) < 0.02,c("TR_nn_end_e")])) / NROW(bounds_a[,c("TR_nn_end_e")]))
                mean_TR_KAL_ende_k = c(mean_TR_KAL_ende_k, mean(abs(bounds_a$TR_kal_end_e), na.rm = TRUE))
                perc20_TR_KAL_ende_k = c(perc20_TR_KAL_ende_k, NROW(na.omit(bounds_a[abs(bounds_a$TR_kal_end_e) < 0.02,c("TR_kal_end_e")])) / NROW(bounds_a[,c("TR_kal_end_e")]))
                
                bounds_a = bounds[bounds$corpus == "cgn-o",]
                bounds_TR_TS_long = c(bounds_a$TR_TS_start_b, bounds_a$TR_TS_start_e, bounds_a$TR_TS_end_b, bounds_a$TR_TS_end_e)
                bounds_TR_NN_long = c(bounds_a$TR_nn_start_b, bounds_a$TR_nn_start_e, bounds_a$TR_nn_end_b, bounds_a$TR_nn_end_e)
                mean_TR_TS_o = c(mean_TR_TS_o, mean(abs(bounds_TR_TS_long), na.rm = TRUE))
                perc20_TR_TS_o = c(perc20_TR_TS_o, NROW(na.omit(bounds_TR_TS_long[abs(bounds_TR_TS_long) < 0.02])) / NROW(bounds_TR_TS_long))
                mean_TR_NN_o = c(mean_TR_NN_o, mean(abs(bounds_TR_NN_long), na.rm = TRUE))
                perc20_TR_NN_o = c(perc20_TR_NN_o, NROW(na.omit(bounds_TR_NN_long[abs(bounds_TR_NN_long) < 0.02])) / NROW(bounds_TR_NN_long))
                mean_TR_KAL_o = c(mean_TR_KAL_o, mean(abs(bounds_TR_KAL_long), na.rm = TRUE))
                perc20_TR_KAL_o = c(perc20_TR_KAL_o, NROW(na.omit(bounds_TR_KAL_long[abs(bounds_TR_KAL_long) < 0.02])) / NROW(bounds_TR_KAL_long))
                mean_TR_TS_startb_o = c(mean_TR_TS_startb_o, mean(abs(bounds_a$TR_TS_start_b), na.rm = TRUE))
                perc20_TR_TS_startb_o = c(perc20_TR_TS_startb_o, NROW(na.omit(bounds_a[abs(bounds_a$TR_TS_start_b) < 0.02,c("TR_TS_start_b")])) / NROW(bounds_a[,c("TR_TS_start_b")]))
                mean_TR_NN_startb_o = c(mean_TR_NN_startb_o, mean(abs(bounds_a$TR_nn_start_b), na.rm = TRUE))
                perc20_TR_NN_startb_o = c(perc20_TR_NN_startb_o, NROW(na.omit(bounds_a[abs(bounds_a$TR_nn_start_b) < 0.02,c("TR_nn_start_b")])) / NROW(bounds_a[,c("TR_nn_start_b")]))
                mean_TR_KAL_startb_o = c(mean_TR_KAL_startb_o, mean(abs(bounds_a$TR_kal_start_b), na.rm = TRUE))
                perc20_TR_KAL_startb_o = c(perc20_TR_KAL_startb_o, NROW(na.omit(bounds_a[abs(bounds_a$TR_kal_start_b) < 0.02,c("TR_kal_start_b")])) / NROW(bounds_a[,c("TR_kal_start_b")]))
                mean_TR_TS_starte_o = c(mean_TR_TS_starte_o, mean(abs(bounds_a$TR_TS_start_e), na.rm = TRUE))
                perc20_TR_TS_starte_o = c(perc20_TR_TS_starte_o, NROW(na.omit(bounds_a[abs(bounds_a$TR_TS_start_e) < 0.02,c("TR_TS_start_e")])) / NROW(bounds_a[,c("TR_TS_start_e")]))
                mean_TR_NN_starte_o = c(mean_TR_NN_starte_o, mean(abs(bounds_a$TR_nn_start_e), na.rm = TRUE))
                perc20_TR_NN_starte_o = c(perc20_TR_NN_starte_o, NROW(na.omit(bounds_a[abs(bounds_a$TR_nn_start_e) < 0.02,c("TR_nn_start_e")])) / NROW(bounds_a[,c("TR_nn_start_e")]))
                mean_TR_TS_endb_o = c(mean_TR_TS_endb_o, mean(abs(bounds_a$TR_TS_end_b), na.rm = TRUE))
                perc20_TR_TS_endb_o = c(perc20_TR_TS_endb_o, NROW(na.omit(bounds_a[abs(bounds_a$TR_TS_end_b) < 0.02,c("TR_TS_end_b")])) / NROW(bounds_a[,c("TR_TS_end_b")]))
                mean_TR_NN_endb_o = c(mean_TR_NN_endb_o, mean(abs(bounds_a$TR_nn_end_b), na.rm = TRUE))
                perc20_TR_NN_endb_o = c(perc20_TR_NN_endb_o, NROW(na.omit(bounds_a[abs(bounds_a$TR_nn_end_b) < 0.02,c("TR_nn_end_b")])) / NROW(bounds_a[,c("TR_nn_end_b")]))
                mean_TR_TS_ende_o = c(mean_TR_TS_ende_o, mean(abs(bounds_a$TR_TS_end_e), na.rm = TRUE))
                perc20_TR_TS_ende_o = c(perc20_TR_TS_ende_o, NROW(na.omit(bounds_a[abs(bounds_a$TR_TS_end_e) < 0.02,c("TR_TS_end_e")])) / NROW(bounds_a[,c("TR_TS_end_e")]))
                mean_TR_NN_ende_o = c(mean_TR_NN_ende_o, mean(abs(bounds_a$TR_nn_end_e), na.rm = TRUE))
                perc20_TR_NN_ende_o = c(perc20_TR_NN_ende_o, NROW(na.omit(bounds_a[abs(bounds_a$TR_nn_end_e) < 0.02,c("TR_nn_end_e")])) / NROW(bounds_a[,c("TR_nn_end_e")]))
                mean_TR_KAL_ende_o = c(mean_TR_KAL_ende_o, mean(abs(bounds_a$TR_kal_end_e), na.rm = TRUE))
                perc20_TR_KAL_ende_o = c(perc20_TR_KAL_ende_o, NROW(na.omit(bounds_a[abs(bounds_a$TR_kal_end_e) < 0.02,c("TR_kal_end_e")])) / NROW(bounds_a[,c("TR_kal_end_e")]))
                
                bounds_a = bounds[bounds$corpus == "ifadv",]
                bounds_TR_TS_long = c(bounds_a$TR_TS_start_b, bounds_a$TR_TS_start_e, bounds_a$TR_TS_end_b, bounds_a$TR_TS_end_e)
                bounds_TR_NN_long = c(bounds_a$TR_nn_start_b, bounds_a$TR_nn_start_e, bounds_a$TR_nn_end_b, bounds_a$TR_nn_end_e)
                mean_TR_TS_ifadv = c(mean_TR_TS_ifadv, mean(abs(bounds_TR_TS_long), na.rm = TRUE))
                perc20_TR_TS_ifadv = c(perc20_TR_TS_ifadv, NROW(na.omit(bounds_TR_TS_long[abs(bounds_TR_TS_long) < 0.02])) / NROW(bounds_TR_TS_long))
                mean_TR_NN_ifadv = c(mean_TR_NN_ifadv, mean(abs(bounds_TR_NN_long), na.rm = TRUE))
                perc20_TR_NN_ifadv = c(perc20_TR_NN_ifadv, NROW(na.omit(bounds_TR_NN_long[abs(bounds_TR_NN_long) < 0.02])) / NROW(bounds_TR_NN_long))
                mean_TR_KAL_ifadv = c(mean_TR_KAL_ifadv, mean(abs(bounds_TR_KAL_long), na.rm = TRUE))
                perc20_TR_KAL_ifadv = c(perc20_TR_KAL_ifadv, NROW(na.omit(bounds_TR_KAL_long[abs(bounds_TR_KAL_long) < 0.02])) / NROW(bounds_TR_KAL_long))
                mean_TR_TS_startb_ifadv = c(mean_TR_TS_startb_ifadv, mean(abs(bounds_a$TR_TS_start_b), na.rm = TRUE))
                perc20_TR_TS_startb_ifadv = c(perc20_TR_TS_startb_ifadv, NROW(na.omit(bounds_a[abs(bounds_a$TR_TS_start_b) < 0.02,c("TR_TS_start_b")])) / NROW(bounds_a[,c("TR_TS_start_b")]))
                mean_TR_NN_startb_ifadv = c(mean_TR_NN_startb_ifadv, mean(abs(bounds_a$TR_nn_start_b), na.rm = TRUE))
                perc20_TR_NN_startb_ifadv = c(perc20_TR_NN_startb_ifadv, NROW(na.omit(bounds_a[abs(bounds_a$TR_nn_start_b) < 0.02,c("TR_nn_start_b")])) / NROW(bounds_a[,c("TR_nn_start_b")]))
                mean_TR_KAL_startb_ifadv = c(mean_TR_KAL_startb_ifadv, mean(abs(bounds_a$TR_kal_start_b), na.rm = TRUE))
                perc20_TR_KAL_startb_ifadv = c(perc20_TR_KAL_startb_ifadv, NROW(na.omit(bounds_a[abs(bounds_a$TR_kal_start_b) < 0.02,c("TR_kal_start_b")])) / NROW(bounds_a[,c("TR_kal_start_b")]))
                mean_TR_TS_starte_ifadv = c(mean_TR_TS_starte_ifadv, mean(abs(bounds_a$TR_TS_start_e), na.rm = TRUE))
                perc20_TR_TS_starte_ifadv = c(perc20_TR_TS_starte_ifadv, NROW(na.omit(bounds_a[abs(bounds_a$TR_TS_start_e) < 0.02,c("TR_TS_start_e")])) / NROW(bounds_a[,c("TR_TS_start_e")]))
                mean_TR_NN_starte_ifadv = c(mean_TR_NN_starte_ifadv, mean(abs(bounds_a$TR_nn_start_e), na.rm = TRUE))
                perc20_TR_NN_starte_ifadv = c(perc20_TR_NN_starte_ifadv, NROW(na.omit(bounds_a[abs(bounds_a$TR_nn_start_e) < 0.02,c("TR_nn_start_e")])) / NROW(bounds_a[,c("TR_nn_start_e")]))
                mean_TR_TS_endb_ifadv = c(mean_TR_TS_endb_ifadv, mean(abs(bounds_a$TR_TS_end_b), na.rm = TRUE))
                perc20_TR_TS_endb_ifadv = c(perc20_TR_TS_endb_ifadv, NROW(na.omit(bounds_a[abs(bounds_a$TR_TS_end_b) < 0.02,c("TR_TS_end_b")])) / NROW(bounds_a[,c("TR_TS_end_b")]))
                mean_TR_NN_endb_ifadv = c(mean_TR_NN_endb_ifadv, mean(abs(bounds_a$TR_nn_end_b), na.rm = TRUE))
                perc20_TR_NN_endb_ifadv = c(perc20_TR_NN_endb_ifadv, NROW(na.omit(bounds_a[abs(bounds_a$TR_nn_end_b) < 0.02,c("TR_nn_end_b")])) / NROW(bounds_a[,c("TR_nn_end_b")]))
                mean_TR_TS_ende_ifadv = c(mean_TR_TS_ende_ifadv, mean(abs(bounds_a$TR_TS_end_e), na.rm = TRUE))
                perc20_TR_TS_ende_ifadv = c(perc20_TR_TS_ende_ifadv, NROW(na.omit(bounds_a[abs(bounds_a$TR_TS_end_e) < 0.02,c("TR_TS_end_e")])) / NROW(bounds_a[,c("TR_TS_end_e")]))
                mean_TR_NN_ende_ifadv = c(mean_TR_NN_ende_ifadv, mean(abs(bounds_a$TR_nn_end_e), na.rm = TRUE))
                perc20_TR_NN_ende_ifadv = c(perc20_TR_NN_ende_ifadv, NROW(na.omit(bounds_a[abs(bounds_a$TR_nn_end_e) < 0.02,c("TR_nn_end_e")])) / NROW(bounds_a[,c("TR_nn_end_e")]))
                mean_TR_KAL_ende_ifadv = c(mean_TR_KAL_ende_ifadv, mean(abs(bounds_a$TR_kal_end_e), na.rm = TRUE))
                perc20_TR_KAL_ende_ifadv = c(perc20_TR_KAL_ende_ifadv, NROW(na.omit(bounds_a[abs(bounds_a$TR_kal_end_e) < 0.02,c("TR_kal_end_e")])) / NROW(bounds_a[,c("TR_kal_end_e")]))
                
                bounds_a = bounds[bounds$corpus == "ecsd",]
                bounds_TR_TS_long = c(bounds_a$TR_TS_start_b, bounds_a$TR_TS_start_e, bounds_a$TR_TS_end_b, bounds_a$TR_TS_end_e)
                bounds_TR_NN_long = c(bounds_a$TR_nn_start_b, bounds_a$TR_nn_start_e, bounds_a$TR_nn_end_b, bounds_a$TR_nn_end_e)
                mean_TR_TS_ecsd = c(mean_TR_TS_ecsd, mean(abs(bounds_TR_TS_long), na.rm = TRUE))
                perc20_TR_TS_ecsd = c(perc20_TR_TS_ecsd, NROW(na.omit(bounds_TR_TS_long[abs(bounds_TR_TS_long) < 0.02])) / NROW(bounds_TR_TS_long))
                mean_TR_NN_ecsd = c(mean_TR_NN_ecsd, mean(abs(bounds_TR_NN_long), na.rm = TRUE))
                perc20_TR_NN_ecsd = c(perc20_TR_NN_ecsd, NROW(na.omit(bounds_TR_NN_long[abs(bounds_TR_NN_long) < 0.02])) / NROW(bounds_TR_NN_long))
                mean_TR_KAL_ecsd = c(mean_TR_KAL_ecsd, mean(abs(bounds_TR_KAL_long), na.rm = TRUE))
                perc20_TR_KAL_ecsd = c(perc20_TR_KAL_ecsd, NROW(na.omit(bounds_TR_KAL_long[abs(bounds_TR_KAL_long) < 0.02])) / NROW(bounds_TR_KAL_long))
                mean_TR_TS_startb_ecsd = c(mean_TR_TS_startb_ecsd, mean(abs(bounds_a$TR_TS_start_b), na.rm = TRUE))
                perc20_TR_TS_startb_ecsd = c(perc20_TR_TS_startb_ecsd, NROW(na.omit(bounds_a[abs(bounds_a$TR_TS_start_b) < 0.02,c("TR_TS_start_b")])) / NROW(bounds_a[,c("TR_TS_start_b")]))
                mean_TR_NN_startb_ecsd = c(mean_TR_NN_startb_ecsd, mean(abs(bounds_a$TR_nn_start_b), na.rm = TRUE))
                perc20_TR_NN_startb_ecsd = c(perc20_TR_NN_startb_ecsd, NROW(na.omit(bounds_a[abs(bounds_a$TR_nn_start_b) < 0.02,c("TR_nn_start_b")])) / NROW(bounds_a[,c("TR_nn_start_b")]))
                mean_TR_KAL_startb_ecsd = c(mean_TR_KAL_startb_ecsd, mean(abs(bounds_a$TR_kal_start_b), na.rm = TRUE))
                perc20_TR_KAL_startb_ecsd = c(perc20_TR_KAL_startb_ecsd, NROW(na.omit(bounds_a[abs(bounds_a$TR_kal_start_b) < 0.02,c("TR_kal_start_b")])) / NROW(bounds_a[,c("TR_kal_start_b")]))
                mean_TR_TS_starte_ecsd = c(mean_TR_TS_starte_ecsd, mean(abs(bounds_a$TR_TS_start_e), na.rm = TRUE))
                perc20_TR_TS_starte_ecsd = c(perc20_TR_TS_starte_ecsd, NROW(na.omit(bounds_a[abs(bounds_a$TR_TS_start_e) < 0.02,c("TR_TS_start_e")])) / NROW(bounds_a[,c("TR_TS_start_e")]))
                mean_TR_NN_starte_ecsd = c(mean_TR_NN_starte_ecsd, mean(abs(bounds_a$TR_nn_start_e), na.rm = TRUE))
                perc20_TR_NN_starte_ecsd = c(perc20_TR_NN_starte_ecsd, NROW(na.omit(bounds_a[abs(bounds_a$TR_nn_start_e) < 0.02,c("TR_nn_start_e")])) / NROW(bounds_a[,c("TR_nn_start_e")]))
                mean_TR_TS_endb_ecsd = c(mean_TR_TS_endb_ecsd, mean(abs(bounds_a$TR_TS_end_b), na.rm = TRUE))
                perc20_TR_TS_endb_ecsd = c(perc20_TR_TS_endb_ecsd, NROW(na.omit(bounds_a[abs(bounds_a$TR_TS_end_b) < 0.02,c("TR_TS_end_b")])) / NROW(bounds_a[,c("TR_TS_end_b")]))
                mean_TR_NN_endb_ecsd = c(mean_TR_NN_endb_ecsd, mean(abs(bounds_a$TR_nn_end_b), na.rm = TRUE))
                perc20_TR_NN_endb_ecsd = c(perc20_TR_NN_endb_ecsd, NROW(na.omit(bounds_a[abs(bounds_a$TR_nn_end_b) < 0.02,c("TR_nn_end_b")])) / NROW(bounds_a[,c("TR_nn_end_b")]))
                mean_TR_TS_ende_ecsd = c(mean_TR_TS_ende_ecsd, mean(abs(bounds_a$TR_TS_end_e), na.rm = TRUE))
                perc20_TR_TS_ende_ecsd = c(perc20_TR_TS_ende_ecsd, NROW(na.omit(bounds_a[abs(bounds_a$TR_TS_end_e) < 0.02,c("TR_TS_end_e")])) / NROW(bounds_a[,c("TR_TS_end_e")]))
                mean_TR_NN_ende_ecsd = c(mean_TR_NN_ende_ecsd, mean(abs(bounds_a$TR_nn_end_e), na.rm = TRUE))
                perc20_TR_NN_ende_ecsd = c(perc20_TR_NN_ende_ecsd, NROW(na.omit(bounds_a[abs(bounds_a$TR_nn_end_e) < 0.02,c("TR_nn_end_e")])) / NROW(bounds_a[,c("TR_nn_end_e")]))
                mean_TR_KAL_ende_ecsd = c(mean_TR_KAL_ende_ecsd, mean(abs(bounds_a$TR_kal_end_e), na.rm = TRUE))
                perc20_TR_KAL_ende_ecsd = c(perc20_TR_KAL_ende_ecsd, NROW(na.omit(bounds_a[abs(bounds_a$TR_kal_end_e) < 0.02,c("TR_kal_end_e")])) / NROW(bounds_a[,c("TR_kal_end_e")]))
                
              }
            }
          }
        }
      }
    }
  }
}

gs_df = data.frame(
  gs_names,
  missing_NN,
  missing_NN_startb,
  missing_NN_starte,
  missing_NN_endb,
  missing_NN_ende,
  perc20_TR_TS,
  perc20_TR_TS_a,
  perc20_TR_TS_c,
  perc20_TR_TS_d,
  perc20_TR_TS_k,
  perc20_TR_TS_o,
  perc20_TR_TS_ifadv,
  perc20_TR_TS_ecsd,
  perc20_TR_TS_startb,
  perc20_TR_TS_startb_a,
  perc20_TR_TS_startb_c,
  perc20_TR_TS_startb_d,
  perc20_TR_TS_startb_k,
  perc20_TR_TS_startb_o,
  perc20_TR_TS_startb_ifadv,
  perc20_TR_TS_startb_ecsd,
  perc20_TR_TS_starte,
  perc20_TR_TS_starte_a,
  perc20_TR_TS_starte_c,
  perc20_TR_TS_starte_d,
  perc20_TR_TS_starte_k,
  perc20_TR_TS_starte_o,
  perc20_TR_TS_starte_ifadv,
  perc20_TR_TS_starte_ecsd,
  perc20_TR_TS_endb,
  perc20_TR_TS_endb_a,
  perc20_TR_TS_endb_c,
  perc20_TR_TS_endb_d,
  perc20_TR_TS_endb_k,
  perc20_TR_TS_endb_o,
  perc20_TR_TS_endb_ifadv,
  perc20_TR_TS_endb_ecsd,
  perc20_TR_TS_ende,
  perc20_TR_TS_ende_a,
  perc20_TR_TS_ende_c,
  perc20_TR_TS_ende_d,
  perc20_TR_TS_ende_k,
  perc20_TR_TS_ende_o,
  perc20_TR_TS_ende_ifadv,
  perc20_TR_TS_ende_ecsd,
  perc20_TR_NN,
  perc20_TR_NN_a,
  perc20_TR_NN_c,
  perc20_TR_NN_d,
  perc20_TR_NN_k,
  perc20_TR_NN_o,
  perc20_TR_NN_ifadv,
  perc20_TR_NN_ecsd,
  perc20_TR_NN_startb,
  perc20_TR_NN_startb_a,
  perc20_TR_NN_startb_c,
  perc20_TR_NN_startb_d,
  perc20_TR_NN_startb_k,
  perc20_TR_NN_startb_o,
  perc20_TR_NN_startb_ifadv,
  perc20_TR_NN_startb_ecsd,
  perc20_TR_NN_starte,
  perc20_TR_NN_starte_a,
  perc20_TR_NN_starte_c,
  perc20_TR_NN_starte_d,
  perc20_TR_NN_starte_k,
  perc20_TR_NN_starte_o,
  perc20_TR_NN_starte_ifadv,
  perc20_TR_NN_starte_ecsd,
  perc20_TR_NN_endb,
  perc20_TR_NN_endb_a,
  perc20_TR_NN_endb_c,
  perc20_TR_NN_endb_d,
  perc20_TR_NN_endb_k,
  perc20_TR_NN_endb_o,
  perc20_TR_NN_endb_ifadv,
  perc20_TR_NN_endb_ecsd,
  perc20_TR_NN_ende,
  perc20_TR_NN_ende_a,
  perc20_TR_NN_ende_c,
  perc20_TR_NN_ende_d,
  perc20_TR_NN_ende_k,
  perc20_TR_NN_ende_o,
  perc20_TR_NN_ende_ifadv,
  perc20_TR_NN_ende_ecsd,
  perc20_SA_NN,
  perc20_SA_NN_startb,
  perc20_SA_NN_starte,
  perc20_SA_NN_endb,
  perc20_SA_NN_ende,
  mean_TR_TS,
  mean_TR_TS_a,
  mean_TR_TS_c,
  mean_TR_TS_d,
  mean_TR_TS_k,
  mean_TR_TS_o,
  mean_TR_TS_ifadv,
  mean_TR_TS_ecsd,
  mean_TR_TS_startb,
  mean_TR_TS_startb_a,
  mean_TR_TS_startb_c,
  mean_TR_TS_startb_d,
  mean_TR_TS_startb_k,
  mean_TR_TS_startb_o,
  mean_TR_TS_startb_ifadv,
  mean_TR_TS_startb_ecsd,
  mean_TR_TS_starte,
  mean_TR_TS_starte_a,
  mean_TR_TS_starte_c,
  mean_TR_TS_starte_d,
  mean_TR_TS_starte_k,
  mean_TR_TS_starte_o,
  mean_TR_TS_starte_ifadv,
  mean_TR_TS_starte_ecsd,
  mean_TR_TS_endb,
  mean_TR_TS_endb_a,
  mean_TR_TS_endb_c,
  mean_TR_TS_endb_d,
  mean_TR_TS_endb_k,
  mean_TR_TS_endb_o,
  mean_TR_TS_endb_ifadv,
  mean_TR_TS_endb_ecsd,
  mean_TR_TS_ende,
  mean_TR_TS_ende_a,
  mean_TR_TS_ende_c,
  mean_TR_TS_ende_d,
  mean_TR_TS_ende_k,
  mean_TR_TS_ende_o,
  mean_TR_TS_ende_ifadv,
  mean_TR_TS_ende_ecsd,
  mean_TR_NN,
  mean_TR_NN_a,
  mean_TR_NN_c,
  mean_TR_NN_d,
  mean_TR_NN_k,
  mean_TR_NN_o,
  mean_TR_NN_ifadv,
  mean_TR_NN_ecsd,
  mean_TR_NN_startb,
  mean_TR_NN_startb_a,
  mean_TR_NN_startb_c,
  mean_TR_NN_startb_d,
  mean_TR_NN_startb_k,
  mean_TR_NN_startb_o,
  mean_TR_NN_startb_ifadv,
  mean_TR_NN_startb_ecsd,
  mean_TR_NN_starte,
  mean_TR_NN_starte_a,
  mean_TR_NN_starte_c,
  mean_TR_NN_starte_d,
  mean_TR_NN_starte_k,
  mean_TR_NN_starte_o,
  mean_TR_NN_starte_ifadv,
  mean_TR_NN_starte_ecsd,
  mean_TR_NN_endb,
  mean_TR_NN_endb_a,
  mean_TR_NN_endb_c,
  mean_TR_NN_endb_d,
  mean_TR_NN_endb_k,
  mean_TR_NN_endb_o,
  mean_TR_NN_endb_ifadv,
  mean_TR_NN_endb_ecsd,
  mean_TR_NN_ende,
  mean_TR_NN_ende_a,
  mean_TR_NN_ende_c,
  mean_TR_NN_ende_d,
  mean_TR_NN_ende_k,
  mean_TR_NN_ende_o,
  mean_TR_NN_ende_ifadv,
  mean_TR_NN_ende_ecsd,
  mean_SA_NN,
  mean_SA_NN_startb,
  mean_SA_NN_starte,
  mean_SA_NN_endb,
  mean_SA_NN_ende,
  perc20_TR_KAL,
  perc20_TR_KAL_a,
  perc20_TR_KAL_c,
  perc20_TR_KAL_d,
  perc20_TR_KAL_k,
  perc20_TR_KAL_o,
  perc20_TR_KAL_ifadv,
  perc20_TR_KAL_ecsd,
  perc20_TR_KAL_startb,
  perc20_TR_KAL_startb_a,
  perc20_TR_KAL_startb_c,
  perc20_TR_KAL_startb_d,
  perc20_TR_KAL_startb_k,
  perc20_TR_KAL_startb_o,
  perc20_TR_KAL_startb_ifadv,
  perc20_TR_KAL_startb_ecsd,
  perc20_TR_KAL_ende,
  perc20_TR_KAL_ende_a,
  perc20_TR_KAL_ende_c,
  perc20_TR_KAL_ende_d,
  perc20_TR_KAL_ende_k,
  perc20_TR_KAL_ende_o,
  perc20_TR_KAL_ende_ifadv,
  perc20_TR_KAL_ende_ecsd,
  mean_TR_KAL,
  mean_TR_KAL_a,
  mean_TR_KAL_c,
  mean_TR_KAL_d,
  mean_TR_KAL_k,
  mean_TR_KAL_o,
  mean_TR_KAL_ifadv,
  mean_TR_KAL_ecsd,
  mean_TR_KAL_startb,
  mean_TR_KAL_startb_a,
  mean_TR_KAL_startb_c,
  mean_TR_KAL_startb_d,
  mean_TR_KAL_startb_k,
  mean_TR_KAL_startb_o,
  mean_TR_KAL_startb_ifadv,
  mean_TR_KAL_startb_ecsd,
  mean_TR_KAL_ende,
  mean_TR_KAL_ende_a,
  mean_TR_KAL_ende_c,
  mean_TR_KAL_ende_d,
  mean_TR_KAL_ende_k,
  mean_TR_KAL_ende_o,
  mean_TR_KAL_ende_ifadv,
  mean_TR_KAL_ende_ecsd
)

library(scales)
# compared to TR
par(mfrow = c(4,1), mar=c(6,2,1,2), oma=c(0,0,2,0))
# mtext("[Network Type]_[Context Frames]_[KALDI weight]_[Apply Penalty]_[Preceding Context]_[Subsequent Context]_[Smoothing]_[Take sqrt]", side = 3, outer = TRUE)

# p1 = plot(361:480, gs_df[order(gs_df$perc20_TR_NN),]$perc20_TR_NN[361:480], xlab = "", ylab = "", xaxt='n', type = "h") #, ylim = c(min(gs_df[order(gs_df$perc20_TR_NN),]$perc20_TR_NN[361:480])-0.002, gs_df$perc20_TR_TS[1]+0.002))
# text(361:480, par("usr")[3], labels = gs_df[order(gs_df$perc20_TR_NN),]$gs_names[361:480], srt = 45, adj = c(1.1,1.1), xpd = TRUE, cex=0.6)
# points(x = 361:480, y = rescale(gs_df[order(gs_df$perc20_TR_NN),]$mean_TR_NN[361:480], to = c(min(gs_df[order(gs_df$perc20_TR_NN),]$perc20_TR_NN[361:480]), max(gs_df[order(gs_df$perc20_TR_NN),]$perc20_TR_NN[361:480]))))
# axis(side = 4, at = c(gs_df[order(gs_df$perc20_TR_NN),]$perc20_TR_NN[361], mean(gs_df[order(gs_df$perc20_TR_NN),]$perc20_TR_NN[c(361,480)]), gs_df[order(gs_df$perc20_TR_NN),]$perc20_TR_NN[480]),
#      labels = round(c(min(gs_df[order(gs_df$perc20_TR_NN),]$mean_TR_NN[361:480]), mean(c(min(gs_df[order(gs_df$perc20_TR_NN),]$mean_TR_NN[361:480]), max(gs_df[order(gs_df$perc20_TR_NN),]$mean_TR_NN[361:480]))), max(gs_df[order(gs_df$perc20_TR_NN),]$mean_TR_NN[361:480])), digits = 4)) 
# p2 = plot(241:360, gs_df[order(gs_df$perc20_TR_NN),]$perc20_TR_NN[241:360], xlab = "", ylab = "", xaxt='n', type = "h")
# text(241:360, par("usr")[3], labels = gs_df[order(gs_df$perc20_TR_NN),]$gs_names[241:360], srt = 45, adj = c(1.1,1.1), xpd = TRUE, cex=0.6)
# points(x = 241:360, y = rescale(gs_df[order(gs_df$perc20_TR_NN),]$mean_TR_NN[241:360], to = c(min(gs_df[order(gs_df$perc20_TR_NN),]$perc20_TR_NN[241:360]), max(gs_df[order(gs_df$perc20_TR_NN),]$perc20_TR_NN[241:360]))))
# axis(side = 4, at = c(gs_df[order(gs_df$perc20_TR_NN),]$perc20_TR_NN[241], mean(gs_df[order(gs_df$perc20_TR_NN),]$perc20_TR_NN[c(241,360)]), gs_df[order(gs_df$perc20_TR_NN),]$perc20_TR_NN[360]),
#      labels = round(c(min(gs_df[order(gs_df$perc20_TR_NN),]$mean_TR_NN[241:360]), mean(c(min(gs_df[order(gs_df$perc20_TR_NN),]$mean_TR_NN[241:360]), max(gs_df[order(gs_df$perc20_TR_NN),]$mean_TR_NN[241:360]))), max(gs_df[order(gs_df$perc20_TR_NN),]$mean_TR_NN[241:360])), digits = 4)) 
# p3 = plot(121:240, gs_df[order(gs_df$perc20_TR_NN),]$perc20_TR_NN[121:240], xlab = "", ylab = "", xaxt='n', type = "h")
# text(121:240, par("usr")[3], labels = gs_df[order(gs_df$perc20_TR_NN),]$gs_names[121:240], srt = 45, adj = c(1.1,1.1), xpd = TRUE, cex=0.6)
# points(x = 121:240, y = rescale(gs_df[order(gs_df$perc20_TR_NN),]$mean_TR_NN[121:240], to = c(min(gs_df[order(gs_df$perc20_TR_NN),]$perc20_TR_NN[121:240]), max(gs_df[order(gs_df$perc20_TR_NN),]$perc20_TR_NN[121:240]))))
# axis(side = 4, at = c(gs_df[order(gs_df$perc20_TR_NN),]$perc20_TR_NN[121], mean(gs_df[order(gs_df$perc20_TR_NN),]$perc20_TR_NN[c(121,240)]), gs_df[order(gs_df$perc20_TR_NN),]$perc20_TR_NN[240]),
#      labels = round(c(min(gs_df[order(gs_df$perc20_TR_NN),]$mean_TR_NN[121:240]), mean(c(min(gs_df[order(gs_df$perc20_TR_NN),]$mean_TR_NN[121:240]), max(gs_df[order(gs_df$perc20_TR_NN),]$mean_TR_NN[121:240]))), max(gs_df[order(gs_df$perc20_TR_NN),]$mean_TR_NN[121:240])), digits = 4)) 
# p4 = plot(1:120, gs_df[order(gs_df$perc20_TR_NN),]$perc20_TR_NN[1:120], xlab = "", ylab = "", xaxt='n', type = "h")
# text(1:120, par("usr")[3], labels = gs_df[order(gs_df$perc20_TR_NN),]$gs_names[1:120], srt = 45, adj = c(1.1,1.1), xpd = TRUE, cex=0.6)
# points(x = 1:120, y = rescale(gs_df[order(gs_df$perc20_TR_NN),]$mean_TR_NN[1:120], to = c(min(gs_df[order(gs_df$perc20_TR_NN),]$perc20_TR_NN[1:120]), max(gs_df[order(gs_df$perc20_TR_NN),]$perc20_TR_NN[1:120]))))
# axis(side = 4, at = c(gs_df[order(gs_df$perc20_TR_NN),]$perc20_TR_NN[1], mean(gs_df[order(gs_df$perc20_TR_NN),]$perc20_TR_NN[c(1,120)]), gs_df[order(gs_df$perc20_TR_NN),]$perc20_TR_NN[120]),
#      labels = round(c(min(gs_df[order(gs_df$perc20_TR_NN),]$mean_TR_NN[1:120]), mean(c(min(gs_df[order(gs_df$perc20_TR_NN),]$mean_TR_NN[1:120]), max(gs_df[order(gs_df$perc20_TR_NN),]$mean_TR_NN[1:120]))), max(gs_df[order(gs_df$perc20_TR_NN),]$mean_TR_NN[1:120])), digits = 4)) 
# 
# mtext("All Boundaries (1/2) - TR Baseline", side = 3, outer = TRUE)
# 
# p1 = plot(841:960, gs_df[order(gs_df$perc20_TR_NN),]$perc20_TR_NN[841:960], xlab = "", ylab = "", xaxt='n', type = "h", ylim = c(gs_df[order(gs_df$perc20_TR_NN),]$perc20_TR_NN[841]-0.001, gs_df$perc20_TR_TS[1]+0.002))
# text(841:960, par("usr")[3], labels = gs_df[order(gs_df$perc20_TR_NN),]$gs_names[841:960], srt = 45, adj = c(1.1,1.1), xpd = TRUE, cex=0.6)
# abline(h = gs_df$perc20_TR_TS[1], lty = "dashed")
# text(841 + 0.5, gs_df$perc20_TR_TS[1] - 0.005, labels = c("Human Agreement"))
# points(x = 841:960, y = rescale(gs_df[order(gs_df$perc20_TR_NN),]$mean_TR_NN[841:960], to = c(gs_df[order(gs_df$perc20_TR_NN),]$perc20_TR_NN[841]-0.001, gs_df$perc20_TR_TS[1]-0.01)))
# axis(side = 4, at = c(gs_df[order(gs_df$perc20_TR_NN),]$perc20_TR_NN[841], mean(c(gs_df[order(gs_df$perc20_TR_NN),]$perc20_TR_NN[841], gs_df$perc20_TR_TS[1]-0.01)), gs_df$perc20_TR_TS[1]-0.01),
#      labels = round(c(min(gs_df[order(gs_df$perc20_TR_NN),]$mean_TR_NN[841:960]), mean(c(min(gs_df[order(gs_df$perc20_TR_NN),]$mean_TR_NN[841:960]), max(gs_df[order(gs_df$perc20_TR_NN),]$mean_TR_NN[841:960]))), max(gs_df[order(gs_df$perc20_TR_NN),]$mean_TR_NN[841:960])), digits = 4)) 
# abline(h = rescale(gs_df[order(gs_df$perc20_TR_NN),]$mean_TR_TS[841:960], to = c(gs_df[order(gs_df$perc20_TR_NN),]$perc20_TR_NN[841]-0.001, gs_df$perc20_TR_TS[1]-0.01))[1], lty = "dotted")
# p2 = plot(721:840, gs_df[order(gs_df$perc20_TR_NN),]$perc20_TR_NN[721:840], xlab = "", ylab = "", xaxt='n', type = "h")
# text(721:840, par("usr")[3], labels = gs_df[order(gs_df$perc20_TR_NN),]$gs_names[721:840], srt = 45, adj = c(1.1,1.1), xpd = TRUE, cex=0.6)
# points(x = 721:840, y = rescale(gs_df[order(gs_df$perc20_TR_NN),]$mean_TR_NN[721:840], to = c(min(gs_df[order(gs_df$perc20_TR_NN),]$perc20_TR_NN[721:840]), max(gs_df[order(gs_df$perc20_TR_NN),]$perc20_TR_NN[721:840]))))
# axis(side = 4, at = c(gs_df[order(gs_df$perc20_TR_NN),]$perc20_TR_NN[721], mean(gs_df[order(gs_df$perc20_TR_NN),]$perc20_TR_NN[c(721,840)]), gs_df[order(gs_df$perc20_TR_NN),]$perc20_TR_NN[840]),
#      labels = round(c(min(gs_df[order(gs_df$perc20_TR_NN),]$mean_TR_NN[721:840]), mean(c(min(gs_df[order(gs_df$perc20_TR_NN),]$mean_TR_NN[721:840]), max(gs_df[order(gs_df$perc20_TR_NN),]$mean_TR_NN[721:840]))), max(gs_df[order(gs_df$perc20_TR_NN),]$mean_TR_NN[721:840])), digits = 4)) 
# p3 = plot(601:720, gs_df[order(gs_df$perc20_TR_NN),]$perc20_TR_NN[601:720], xlab = "", ylab = "", xaxt='n', type = "h")
# text(601:720, par("usr")[3], labels = gs_df[order(gs_df$perc20_TR_NN),]$gs_names[601:720], srt = 45, adj = c(1.1,1.1), xpd = TRUE, cex=0.6)
# points(x = 601:720, y = rescale(gs_df[order(gs_df$perc20_TR_NN),]$mean_TR_NN[601:720], to = c(min(gs_df[order(gs_df$perc20_TR_NN),]$perc20_TR_NN[601:720]), max(gs_df[order(gs_df$perc20_TR_NN),]$perc20_TR_NN[601:720]))))
# axis(side = 4, at = c(gs_df[order(gs_df$perc20_TR_NN),]$perc20_TR_NN[601], mean(gs_df[order(gs_df$perc20_TR_NN),]$perc20_TR_NN[c(601,720)]), gs_df[order(gs_df$perc20_TR_NN),]$perc20_TR_NN[720]),
#      labels = round(c(min(gs_df[order(gs_df$perc20_TR_NN),]$mean_TR_NN[601:720]), mean(c(min(gs_df[order(gs_df$perc20_TR_NN),]$mean_TR_NN[601:720]), max(gs_df[order(gs_df$perc20_TR_NN),]$mean_TR_NN[601:720]))), max(gs_df[order(gs_df$perc20_TR_NN),]$mean_TR_NN[601:720])), digits = 4)) 
# p4 = plot(481:600, gs_df[order(gs_df$perc20_TR_NN),]$perc20_TR_NN[481:600], xlab = "", ylab = "", xaxt='n', type = "h")
# text(481:600, par("usr")[3], labels = gs_df[order(gs_df$perc20_TR_NN),]$gs_names[481:600], srt = 45, adj = c(1.1,1.1), xpd = TRUE, cex=0.6)
# points(x = 481:600, y = rescale(gs_df[order(gs_df$perc20_TR_NN),]$mean_TR_NN[481:600], to = c(min(gs_df[order(gs_df$perc20_TR_NN),]$perc20_TR_NN[481:600]), max(gs_df[order(gs_df$perc20_TR_NN),]$perc20_TR_NN[481:600]))))
# axis(side = 4, at = c(gs_df[order(gs_df$perc20_TR_NN),]$perc20_TR_NN[481], mean(gs_df[order(gs_df$perc20_TR_NN),]$perc20_TR_NN[c(481,600)]), gs_df[order(gs_df$perc20_TR_NN),]$perc20_TR_NN[600]),
#      labels = round(c(min(gs_df[order(gs_df$perc20_TR_NN),]$mean_TR_NN[481:600]), mean(c(min(gs_df[order(gs_df$perc20_TR_NN),]$mean_TR_NN[481:600]), max(gs_df[order(gs_df$perc20_TR_NN),]$mean_TR_NN[481:600]))), max(gs_df[order(gs_df$perc20_TR_NN),]$mean_TR_NN[481:600])), digits = 4)) 
# 
# mtext("All Boundaries (2/2) - TR Baseline", side = 3, outer = TRUE)
# 

# compared to both TR and TS
p1 = plot(361:480, gs_df[order(gs_df$perc20_SA_NN),]$perc20_SA_NN[361:480], xlab = "", ylab = "", xaxt='n', type = "h") #, ylim = c(min(gs_df[order(gs_df$perc20_SA_NN),]$perc20_SA_NN[361:480])-0.002, gs_df$perc20_TR_TS[1]+0.002))
text(361:480, par("usr")[3], labels = gs_df[order(gs_df$perc20_SA_NN),]$gs_names[361:480], srt = 45, adj = c(1.1,1.1), xpd = TRUE, cex=0.6)
points(x = 361:480, y = rescale(gs_df[order(gs_df$perc20_SA_NN),]$mean_SA_NN[361:480], to = c(min(gs_df[order(gs_df$perc20_SA_NN),]$perc20_SA_NN[361:480]), max(gs_df[order(gs_df$perc20_SA_NN),]$perc20_SA_NN[361:480]))))
axis(side = 4, at = c(gs_df[order(gs_df$perc20_SA_NN),]$perc20_SA_NN[361], mean(gs_df[order(gs_df$perc20_SA_NN),]$perc20_SA_NN[c(361,480)]), gs_df[order(gs_df$perc20_SA_NN),]$perc20_SA_NN[480]),
     labels = round(c(min(gs_df[order(gs_df$perc20_SA_NN),]$mean_SA_NN[361:480]), mean(c(min(gs_df[order(gs_df$perc20_SA_NN),]$mean_SA_NN[361:480]), max(gs_df[order(gs_df$perc20_SA_NN),]$mean_SA_NN[361:480]))), max(gs_df[order(gs_df$perc20_SA_NN),]$mean_SA_NN[361:480])), digits = 4)) 
p2 = plot(241:360, gs_df[order(gs_df$perc20_SA_NN),]$perc20_SA_NN[241:360], xlab = "", ylab = "", xaxt='n', type = "h")
text(241:360, par("usr")[3], labels = gs_df[order(gs_df$perc20_SA_NN),]$gs_names[241:360], srt = 45, adj = c(1.1,1.1), xpd = TRUE, cex=0.6)
points(x = 241:360, y = rescale(gs_df[order(gs_df$perc20_SA_NN),]$mean_SA_NN[241:360], to = c(min(gs_df[order(gs_df$perc20_SA_NN),]$perc20_SA_NN[241:360]), max(gs_df[order(gs_df$perc20_SA_NN),]$perc20_SA_NN[241:360]))))
axis(side = 4, at = c(gs_df[order(gs_df$perc20_SA_NN),]$perc20_SA_NN[241], mean(gs_df[order(gs_df$perc20_SA_NN),]$perc20_SA_NN[c(241,360)]), gs_df[order(gs_df$perc20_SA_NN),]$perc20_SA_NN[360]),
     labels = round(c(min(gs_df[order(gs_df$perc20_SA_NN),]$mean_SA_NN[241:360]), mean(c(min(gs_df[order(gs_df$perc20_SA_NN),]$mean_SA_NN[241:360]), max(gs_df[order(gs_df$perc20_SA_NN),]$mean_SA_NN[241:360]))), max(gs_df[order(gs_df$perc20_SA_NN),]$mean_SA_NN[241:360])), digits = 4)) 
p3 = plot(121:240, gs_df[order(gs_df$perc20_SA_NN),]$perc20_SA_NN[121:240], xlab = "", ylab = "", xaxt='n', type = "h")
text(121:240, par("usr")[3], labels = gs_df[order(gs_df$perc20_SA_NN),]$gs_names[121:240], srt = 45, adj = c(1.1,1.1), xpd = TRUE, cex=0.6)
points(x = 121:240, y = rescale(gs_df[order(gs_df$perc20_SA_NN),]$mean_SA_NN[121:240], to = c(min(gs_df[order(gs_df$perc20_SA_NN),]$perc20_SA_NN[121:240]), max(gs_df[order(gs_df$perc20_SA_NN),]$perc20_SA_NN[121:240]))))
axis(side = 4, at = c(gs_df[order(gs_df$perc20_SA_NN),]$perc20_SA_NN[121], mean(gs_df[order(gs_df$perc20_SA_NN),]$perc20_SA_NN[c(121,240)]), gs_df[order(gs_df$perc20_SA_NN),]$perc20_SA_NN[240]),
     labels = round(c(min(gs_df[order(gs_df$perc20_SA_NN),]$mean_SA_NN[121:240]), mean(c(min(gs_df[order(gs_df$perc20_SA_NN),]$mean_SA_NN[121:240]), max(gs_df[order(gs_df$perc20_SA_NN),]$mean_SA_NN[121:240]))), max(gs_df[order(gs_df$perc20_SA_NN),]$mean_SA_NN[121:240])), digits = 4)) 
p4 = plot(1:120, gs_df[order(gs_df$perc20_SA_NN),]$perc20_SA_NN[1:120], xlab = "", ylab = "", xaxt='n', type = "h")
text(1:120, par("usr")[3], labels = gs_df[order(gs_df$perc20_SA_NN),]$gs_names[1:120], srt = 45, adj = c(1.1,1.1), xpd = TRUE, cex=0.6)
points(x = 1:120, y = rescale(gs_df[order(gs_df$perc20_SA_NN),]$mean_SA_NN[1:120], to = c(min(gs_df[order(gs_df$perc20_SA_NN),]$perc20_SA_NN[1:120]), max(gs_df[order(gs_df$perc20_SA_NN),]$perc20_SA_NN[1:120]))))
axis(side = 4, at = c(gs_df[order(gs_df$perc20_SA_NN),]$perc20_SA_NN[1], mean(gs_df[order(gs_df$perc20_SA_NN),]$perc20_SA_NN[c(1,120)]), gs_df[order(gs_df$perc20_SA_NN),]$perc20_SA_NN[120]),
     labels = round(c(min(gs_df[order(gs_df$perc20_SA_NN),]$mean_SA_NN[1:120]), mean(c(min(gs_df[order(gs_df$perc20_SA_NN),]$mean_SA_NN[1:120]), max(gs_df[order(gs_df$perc20_SA_NN),]$mean_SA_NN[1:120]))), max(gs_df[order(gs_df$perc20_SA_NN),]$mean_SA_NN[1:120])), digits = 4)) 

mtext("All Boundaries (1/2) - SA Baseline", side = 3, outer = TRUE)

p1 = plot(841:960, gs_df[order(gs_df$perc20_SA_NN),]$perc20_SA_NN[841:960], xlab = "", ylab = "", xaxt='n', type = "h", ylim = c(gs_df[order(gs_df$perc20_SA_NN),]$perc20_SA_NN[841]-0.001, gs_df$perc20_TR_TS[1]+0.002))
text(841:960, par("usr")[3], labels = gs_df[order(gs_df$perc20_SA_NN),]$gs_names[841:960], srt = 45, adj = c(1.1,1.1), xpd = TRUE, cex=0.6)
abline(h = gs_df$perc20_TR_TS[1], lty = "dashed")
text(841 + 0.5, gs_df$perc20_TR_TS[1] - 0.008, labels = c("Human Agreement"))
abline(h = mean(c(max(gs_df$perc20_SA_NN_startb), max(gs_df$perc20_SA_NN_starte), max(gs_df$perc20_SA_NN_endb), max(gs_df$perc20_SA_NN_ende))), lty = "dashed")
text(960 - 6, mean(c(max(gs_df$perc20_SA_NN_startb), max(gs_df$perc20_SA_NN_starte), max(gs_df$perc20_SA_NN_endb), max(gs_df$perc20_SA_NN_ende))) - 0.008, labels = c("Separate Boundary Type Configurations"))
# points(x = 841:960, y = rescale(gs_df[order(gs_df$perc20_SA_NN),]$mean_SA_NN[841:960], to = c(gs_df[order(gs_df$perc20_SA_NN),]$perc20_SA_NN[841]-0.001, gs_df$perc20_TR_TS[1]-0.01)))
# axis(side = 4, at = c(gs_df[order(gs_df$perc20_SA_NN),]$perc20_SA_NN[841], mean(c(gs_df[order(gs_df$perc20_SA_NN),]$perc20_SA_NN[841], gs_df$perc20_TR_TS[1]-0.01)), gs_df$perc20_TR_TS[1]-0.01),
#      labels = round(c(min(gs_df[order(gs_df$perc20_SA_NN),]$mean_SA_NN[841:960]), mean(c(min(gs_df[order(gs_df$perc20_SA_NN),]$mean_SA_NN[841:960]), max(gs_df[order(gs_df$perc20_SA_NN),]$mean_SA_NN[841:960]))), max(gs_df[order(gs_df$perc20_SA_NN),]$mean_SA_NN[841:960])), digits = 4)) 
# abline(h = rescale(gs_df[order(gs_df$perc20_SA_NN),]$mean_TR_TS[841:960], to = c(gs_df[order(gs_df$perc20_SA_NN),]$perc20_SA_NN[841]-0.001, gs_df$perc20_TR_TS[1]-0.01))[1], lty = "dotted")
p2 = plot(721:840, gs_df[order(gs_df$perc20_SA_NN),]$perc20_SA_NN[721:840], xlab = "", ylab = "", xaxt='n', type = "h")
text(721:840, par("usr")[3], labels = gs_df[order(gs_df$perc20_SA_NN),]$gs_names[721:840], srt = 45, adj = c(1.1,1.1), xpd = TRUE, cex=0.6)
# points(x = 721:840, y = rescale(gs_df[order(gs_df$perc20_SA_NN),]$mean_SA_NN[721:840], to = c(min(gs_df[order(gs_df$perc20_SA_NN),]$perc20_SA_NN[721:840]), max(gs_df[order(gs_df$perc20_SA_NN),]$perc20_SA_NN[721:840]))))
# axis(side = 4, at = c(gs_df[order(gs_df$perc20_SA_NN),]$perc20_SA_NN[721], mean(gs_df[order(gs_df$perc20_SA_NN),]$perc20_SA_NN[c(721,840)]), gs_df[order(gs_df$perc20_SA_NN),]$perc20_SA_NN[840]),
#      labels = round(c(min(gs_df[order(gs_df$perc20_SA_NN),]$mean_SA_NN[721:840]), mean(c(min(gs_df[order(gs_df$perc20_SA_NN),]$mean_SA_NN[721:840]), max(gs_df[order(gs_df$perc20_SA_NN),]$mean_SA_NN[721:840]))), max(gs_df[order(gs_df$perc20_SA_NN),]$mean_SA_NN[721:840])), digits = 4)) 
p3 = plot(601:720, gs_df[order(gs_df$perc20_SA_NN),]$perc20_SA_NN[601:720], xlab = "", ylab = "", xaxt='n', type = "h")
text(601:720, par("usr")[3], labels = gs_df[order(gs_df$perc20_SA_NN),]$gs_names[601:720], srt = 45, adj = c(1.1,1.1), xpd = TRUE, cex=0.6)
# points(x = 601:720, y = rescale(gs_df[order(gs_df$perc20_SA_NN),]$mean_SA_NN[601:720], to = c(min(gs_df[order(gs_df$perc20_SA_NN),]$perc20_SA_NN[601:720]), max(gs_df[order(gs_df$perc20_SA_NN),]$perc20_SA_NN[601:720]))))
# axis(side = 4, at = c(gs_df[order(gs_df$perc20_SA_NN),]$perc20_SA_NN[601], mean(gs_df[order(gs_df$perc20_SA_NN),]$perc20_SA_NN[c(601,720)]), gs_df[order(gs_df$perc20_SA_NN),]$perc20_SA_NN[720]),
#      labels = round(c(min(gs_df[order(gs_df$perc20_SA_NN),]$mean_SA_NN[601:720]), mean(c(min(gs_df[order(gs_df$perc20_SA_NN),]$mean_SA_NN[601:720]), max(gs_df[order(gs_df$perc20_SA_NN),]$mean_SA_NN[601:720]))), max(gs_df[order(gs_df$perc20_SA_NN),]$mean_SA_NN[601:720])), digits = 4)) 
p4 = plot(481:600, gs_df[order(gs_df$perc20_SA_NN),]$perc20_SA_NN[481:600], xlab = "", ylab = "", xaxt='n', type = "h")
text(481:600, par("usr")[3], labels = gs_df[order(gs_df$perc20_SA_NN),]$gs_names[481:600], srt = 45, adj = c(1.1,1.1), xpd = TRUE, cex=0.6)
# points(x = 481:600, y = rescale(gs_df[order(gs_df$perc20_SA_NN),]$mean_SA_NN[481:600], to = c(min(gs_df[order(gs_df$perc20_SA_NN),]$perc20_SA_NN[481:600]), max(gs_df[order(gs_df$perc20_SA_NN),]$perc20_SA_NN[481:600]))))
# axis(side = 4, at = c(gs_df[order(gs_df$perc20_SA_NN),]$perc20_SA_NN[481], mean(gs_df[order(gs_df$perc20_SA_NN),]$perc20_SA_NN[c(481,600)]), gs_df[order(gs_df$perc20_SA_NN),]$perc20_SA_NN[600]),
#      labels = round(c(min(gs_df[order(gs_df$perc20_SA_NN),]$mean_SA_NN[481:600]), mean(c(min(gs_df[order(gs_df$perc20_SA_NN),]$mean_SA_NN[481:600]), max(gs_df[order(gs_df$perc20_SA_NN),]$mean_SA_NN[481:600]))), max(gs_df[order(gs_df$perc20_SA_NN),]$mean_SA_NN[481:600])), digits = 4)) 

mtext("All Boundaries (2/2) - SA Baseline", side = 3, outer = TRUE)

# best overall classifier(s):
max(gs_df$perc20_TR_TS)
max(gs_df$perc20_SA_NN)
max(gs_df$perc20_TR_KAL)
best_class_all = as.character(gs_df[gs_df$perc20_SA_NN == max(gs_df$perc20_SA_NN),]$gs_names)
# and see whether they outperform KALDI
gs_df[gs_df$gs_names %in% best_class_all, c("gs_names", "mean_SA_NN", "perc20_TR_KAL_startb", "perc20_SA_NN_startb", "perc20_TR_KAL_ende", "perc20_SA_NN_ende")]

# get corresponding agreement for respective boundaries
gs_df[as.character(gs_df$gs_names) %in% best_class_all,]$perc20_SA_NN_startb
max(gs_df$perc20_SA_NN_startb)
gs_df[as.character(gs_df$gs_names) %in% best_class_all,]$perc20_SA_NN_starte
max(gs_df$perc20_SA_NN_starte)
gs_df[as.character(gs_df$gs_names) %in% best_class_all,]$perc20_SA_NN_endb
max(gs_df$perc20_SA_NN_endb)
gs_df[as.character(gs_df$gs_names) %in% best_class_all,]$perc20_SA_NN_ende
max(gs_df$perc20_SA_NN_ende)

mean(c(max(gs_df$perc20_SA_NN_startb), max(gs_df$perc20_SA_NN_starte), max(gs_df$perc20_SA_NN_endb), max(gs_df$perc20_SA_NN_ende)))

# best classifiers for separate boundaries
# [Network Type]_[Context Frames]_[KALDI weight]_[Apply Penalty]_[Preceding Context]_[Subsequent Context]_[Smoothing]_[Take sqrt]
best_class_startb = as.character(gs_df[gs_df$perc20_SA_NN_startb == max(gs_df$perc20_SA_NN_startb),]$gs_names)
gs_df[gs_df$gs_names %in% best_class_startb, c("gs_names", "mean_SA_NN")]
best_class_starte = as.character(gs_df[gs_df$perc20_SA_NN_starte == max(gs_df$perc20_SA_NN_starte),]$gs_names)
gs_df[gs_df$gs_names %in% best_class_starte, c("gs_names", "mean_SA_NN")]
best_class_endb = as.character(gs_df[gs_df$perc20_SA_NN_endb == max(gs_df$perc20_SA_NN_endb),]$gs_names)
gs_df[gs_df$gs_names %in% best_class_endb, c("gs_names", "mean_SA_NN")]
best_class_ende = as.character(gs_df[gs_df$perc20_SA_NN_ende == max(gs_df$perc20_SA_NN_ende),]$gs_names)
gs_df[gs_df$gs_names %in% best_class_ende, c("gs_names", "mean_SA_NN")]

# p1 = plot(361:480, gs_df[order(gs_df$perc20_TR_NN_startb),]$perc20_TR_NN_startb[361:480], xlab = "", ylab = "", xaxt='n', type = "h")
# text(361:480, par("usr")[3], labels = gs_df[order(gs_df$perc20_TR_NN_startb),]$gs_names[361:480], srt = 45, adj = c(1.1,1.1), xpd = TRUE, cex=0.6)
# p2 = plot(241:360, gs_df[order(gs_df$perc20_TR_NN_startb),]$perc20_TR_NN_startb[241:360], xlab = "", ylab = "", xaxt='n', type = "h")
# text(241:360, par("usr")[3], labels = gs_df[order(gs_df$perc20_TR_NN_startb),]$gs_names[241:360], srt = 45, adj = c(1.1,1.1), xpd = TRUE, cex=0.6)
# abline(h = gs_df$perc20_TR_KAL_startb[1], lty = "dashed")
# text(241, gs_df$perc20_TR_KAL_startb[1] + 0.005, labels = c("Kaldi Proportion"))
# p3 = plot(121:240, gs_df[order(gs_df$perc20_TR_NN_startb),]$perc20_TR_NN_startb[121:240], xlab = "", ylab = "", xaxt='n', type = "h")
# text(121:240, par("usr")[3], labels = gs_df[order(gs_df$perc20_TR_NN_startb),]$gs_names[121:240], srt = 45, adj = c(1.1,1.1), xpd = TRUE, cex=0.6)
# p4 = plot(1:120, gs_df[order(gs_df$perc20_TR_NN_startb),]$perc20_TR_NN_startb[1:120], xlab = "", ylab = "", xaxt='n', type = "h")
# text(1:120, par("usr")[3], labels = gs_df[order(gs_df$perc20_TR_NN_startb),]$gs_names[1:120], srt = 45, adj = c(1.1,1.1), xpd = TRUE, cex=0.6)
# 
# mtext("1st Start Boundary (1/2) - TR Baseline", side = 3, outer = TRUE)
# 
# 
# p1 = plot(841:960, gs_df[order(gs_df$perc20_TR_NN_startb),]$perc20_TR_NN_startb[841:960], xlab = "", ylab = "", xaxt='n', type = "h", ylim = c(min(gs_df[order(gs_df$perc20_TR_NN_startb),]$perc20_TR_NN_startb[841:960])-0.001, gs_df$perc20_TR_TS_startb[1]+0.002))
# text(841:960, par("usr")[3], labels = gs_df[order(gs_df$perc20_TR_NN_startb),]$gs_names[841:960], srt = 45, adj = c(1.1,1.1), xpd = TRUE, cex=0.6)
# abline(h = gs_df$perc20_TR_TS_startb[1], lty = "dashed")
# text(841 + 0.5, gs_df$perc20_TR_TS_startb[1] - 0.005, labels = c("Human Agreement"))
# p2 = plot(721:840, gs_df[order(gs_df$perc20_TR_NN_startb),]$perc20_TR_NN_startb[721:840], xlab = "", ylab = "", xaxt='n', type = "h")
# text(721:840, par("usr")[3], labels = gs_df[order(gs_df$perc20_TR_NN_startb),]$gs_names[721:840], srt = 45, adj = c(1.1,1.1), xpd = TRUE, cex=0.6)
# p3 = plot(601:720, gs_df[order(gs_df$perc20_TR_NN_startb),]$perc20_TR_NN_startb[601:720], xlab = "", ylab = "", xaxt='n', type = "h")
# text(601:720, par("usr")[3], labels = gs_df[order(gs_df$perc20_TR_NN_startb),]$gs_names[601:720], srt = 45, adj = c(1.1,1.1), xpd = TRUE, cex=0.6)
# p4 = plot(481:600, gs_df[order(gs_df$perc20_TR_NN_startb),]$perc20_TR_NN_startb[481:600], xlab = "", ylab = "", xaxt='n', type = "h")
# text(481:600, par("usr")[3], labels = gs_df[order(gs_df$perc20_TR_NN_startb),]$gs_names[481:600], srt = 45, adj = c(1.1,1.1), xpd = TRUE, cex=0.6)
# 
# mtext("1st Start Boundary (2/2) - TR Baseline", side = 3, outer = TRUE)

p1 = plot(841:960, gs_df[order(gs_df$perc20_SA_NN_startb),]$perc20_SA_NN_startb[841:960], xlab = "", ylab = "", xaxt='n', type = "h", ylim = c(min(gs_df[order(gs_df$perc20_SA_NN_startb),]$perc20_SA_NN_startb[841:960])-0.001, gs_df$perc20_TR_TS_startb[1]+0.002))
text(841:960, par("usr")[3], labels = gs_df[order(gs_df$perc20_SA_NN_startb),]$gs_names[841:960], srt = 45, adj = c(1.1,1.1), xpd = TRUE, cex=0.6)
abline(h = gs_df$perc20_TR_TS_startb[1], lty = "dashed")
text(841 + 0.5, gs_df$perc20_TR_TS_startb[1] - 0.005, labels = c("Human Agreement"))
p2 = plot(721:840, gs_df[order(gs_df$perc20_SA_NN_startb),]$perc20_SA_NN_startb[721:840], xlab = "", ylab = "", xaxt='n', type = "h")
text(721:840, par("usr")[3], labels = gs_df[order(gs_df$perc20_SA_NN_startb),]$gs_names[721:840], srt = 45, adj = c(1.1,1.1), xpd = TRUE, cex=0.6)
p3 = plot(601:720, gs_df[order(gs_df$perc20_SA_NN_startb),]$perc20_SA_NN_startb[601:720], xlab = "", ylab = "", xaxt='n', type = "h")
text(601:720, par("usr")[3], labels = gs_df[order(gs_df$perc20_SA_NN_startb),]$gs_names[601:720], srt = 45, adj = c(1.1,1.1), xpd = TRUE, cex=0.6)
p4 = plot(481:600, gs_df[order(gs_df$perc20_SA_NN_startb),]$perc20_SA_NN_startb[481:600], xlab = "", ylab = "", xaxt='n', type = "h")
text(481:600, par("usr")[3], labels = gs_df[order(gs_df$perc20_SA_NN_startb),]$gs_names[481:600], srt = 45, adj = c(1.1,1.1), xpd = TRUE, cex=0.6)

mtext("1st Start Boundary (2/2) - SA Baseline", side = 3, outer = TRUE)


# p1 = plot(361:480, gs_df[order(gs_df$perc20_TR_NN_starte),]$perc20_TR_NN_starte[361:480], xlab = "", ylab = "", xaxt='n', type = "h")
# text(361:480, par("usr")[3], labels = gs_df[order(gs_df$perc20_TR_NN_starte),]$gs_names[361:480], srt = 45, adj = c(1.1,1.1), xpd = TRUE, cex=0.6)
# p2 = plot(241:360, gs_df[order(gs_df$perc20_TR_NN_starte),]$perc20_TR_NN_starte[241:360], xlab = "", ylab = "", xaxt='n', type = "h")
# text(241:360, par("usr")[3], labels = gs_df[order(gs_df$perc20_TR_NN_starte),]$gs_names[241:360], srt = 45, adj = c(1.1,1.1), xpd = TRUE, cex=0.6)
# p3 = plot(121:240, gs_df[order(gs_df$perc20_TR_NN_starte),]$perc20_TR_NN_starte[121:240], xlab = "", ylab = "", xaxt='n', type = "h")
# text(121:240, par("usr")[3], labels = gs_df[order(gs_df$perc20_TR_NN_starte),]$gs_names[121:240], srt = 45, adj = c(1.1,1.1), xpd = TRUE, cex=0.6)
# p4 = plot(1:120, gs_df[order(gs_df$perc20_TR_NN_starte),]$perc20_TR_NN_starte[1:120], xlab = "", ylab = "", xaxt='n', type = "h")
# text(1:120, par("usr")[3], labels = gs_df[order(gs_df$perc20_TR_NN_starte),]$gs_names[1:120], srt = 45, adj = c(1.1,1.1), xpd = TRUE, cex=0.6)
# 
# mtext("2nd Start Boundary (1/2) - TR Baseline", side = 3, outer = TRUE)
# 
# 
# p1 = plot(841:960, gs_df[order(gs_df$perc20_TR_NN_starte),]$perc20_TR_NN_starte[841:960], xlab = "", ylab = "", xaxt='n', type = "h", ylim = c(min(gs_df[order(gs_df$perc20_TR_NN_starte),]$perc20_TR_NN_starte[841:960])-0.001, gs_df$perc20_TR_TS_starte[1]+0.002))
# text(841:960, par("usr")[3], labels = gs_df[order(gs_df$perc20_TR_NN_starte),]$gs_names[841:960], srt = 45, adj = c(1.1,1.1), xpd = TRUE, cex=0.6)
# abline(h = gs_df$perc20_TR_TS_starte[1], lty = "dashed")
# text(841 + 0.5, gs_df$perc20_TR_TS_starte[1] - 0.003, labels = c("Human Agreement"))
# p2 = plot(721:840, gs_df[order(gs_df$perc20_TR_NN_starte),]$perc20_TR_NN_starte[721:840], xlab = "", ylab = "", xaxt='n', type = "h")
# text(721:840, par("usr")[3], labels = gs_df[order(gs_df$perc20_TR_NN_starte),]$gs_names[721:840], srt = 45, adj = c(1.1,1.1), xpd = TRUE, cex=0.6)
# p3 = plot(601:720, gs_df[order(gs_df$perc20_TR_NN_starte),]$perc20_TR_NN_starte[601:720], xlab = "", ylab = "", xaxt='n', type = "h")
# text(601:720, par("usr")[3], labels = gs_df[order(gs_df$perc20_TR_NN_starte),]$gs_names[601:720], srt = 45, adj = c(1.1,1.1), xpd = TRUE, cex=0.6)
# p4 = plot(481:600, gs_df[order(gs_df$perc20_TR_NN_starte),]$perc20_TR_NN_starte[481:600], xlab = "", ylab = "", xaxt='n', type = "h")
# text(481:600, par("usr")[3], labels = gs_df[order(gs_df$perc20_TR_NN_starte),]$gs_names[481:600], srt = 45, adj = c(1.1,1.1), xpd = TRUE, cex=0.6)
# 
# mtext("2nd Start Boundary (2/2) - TR Baseline", side = 3, outer = TRUE)

p1 = plot(841:960, gs_df[order(gs_df$perc20_SA_NN_starte),]$perc20_SA_NN_starte[841:960], xlab = "", ylab = "", xaxt='n', type = "h", ylim = c(min(gs_df[order(gs_df$perc20_SA_NN_starte),]$perc20_SA_NN_starte[841:960])-0.001, gs_df$perc20_TR_TS_starte[1]+0.002))
text(841:960, par("usr")[3], labels = gs_df[order(gs_df$perc20_SA_NN_starte),]$gs_names[841:960], srt = 45, adj = c(1.1,1.1), xpd = TRUE, cex=0.6)
abline(h = gs_df$perc20_TR_TS_starte[1], lty = "dashed")
text(841 + 0.5, gs_df$perc20_TR_TS_starte[1] - 0.003, labels = c("Human Agreement"))
p2 = plot(721:840, gs_df[order(gs_df$perc20_SA_NN_starte),]$perc20_SA_NN_starte[721:840], xlab = "", ylab = "", xaxt='n', type = "h")
text(721:840, par("usr")[3], labels = gs_df[order(gs_df$perc20_SA_NN_starte),]$gs_names[721:840], srt = 45, adj = c(1.1,1.1), xpd = TRUE, cex=0.6)
p3 = plot(601:720, gs_df[order(gs_df$perc20_SA_NN_starte),]$perc20_SA_NN_starte[601:720], xlab = "", ylab = "", xaxt='n', type = "h")
text(601:720, par("usr")[3], labels = gs_df[order(gs_df$perc20_SA_NN_starte),]$gs_names[601:720], srt = 45, adj = c(1.1,1.1), xpd = TRUE, cex=0.6)
p4 = plot(481:600, gs_df[order(gs_df$perc20_SA_NN_starte),]$perc20_SA_NN_starte[481:600], xlab = "", ylab = "", xaxt='n', type = "h")
text(481:600, par("usr")[3], labels = gs_df[order(gs_df$perc20_SA_NN_starte),]$gs_names[481:600], srt = 45, adj = c(1.1,1.1), xpd = TRUE, cex=0.6)

mtext("2nd Start Boundary (2/2) - SA Baseline", side = 3, outer = TRUE)



# p1 = plot(361:480, gs_df[order(gs_df$perc20_TR_NN_endb),]$perc20_TR_NN_endb[361:480], xlab = "", ylab = "", xaxt='n', type = "h")
# text(361:480, par("usr")[3], labels = gs_df[order(gs_df$perc20_TR_NN_endb),]$gs_names[361:480], srt = 45, adj = c(1.1,1.1), xpd = TRUE, cex=0.6)
# p2 = plot(241:360, gs_df[order(gs_df$perc20_TR_NN_endb),]$perc20_TR_NN_endb[241:360], xlab = "", ylab = "", xaxt='n', type = "h")
# text(241:360, par("usr")[3], labels = gs_df[order(gs_df$perc20_TR_NN_endb),]$gs_names[241:360], srt = 45, adj = c(1.1,1.1), xpd = TRUE, cex=0.6)
# p3 = plot(121:240, gs_df[order(gs_df$perc20_TR_NN_endb),]$perc20_TR_NN_endb[121:240], xlab = "", ylab = "", xaxt='n', type = "h")
# text(121:240, par("usr")[3], labels = gs_df[order(gs_df$perc20_TR_NN_endb),]$gs_names[121:240], srt = 45, adj = c(1.1,1.1), xpd = TRUE, cex=0.6)
# p4 = plot(1:120, gs_df[order(gs_df$perc20_TR_NN_endb),]$perc20_TR_NN_endb[1:120], xlab = "", ylab = "", xaxt='n', type = "h")
# text(1:120, par("usr")[3], labels = gs_df[order(gs_df$perc20_TR_NN_endb),]$gs_names[1:120], srt = 45, adj = c(1.1,1.1), xpd = TRUE, cex=0.6)
# 
# mtext("1st End Boundary (1/2) - TR Baseline", side = 3, outer = TRUE)
# 
# 
# p1 = plot(841:960, gs_df[order(gs_df$perc20_TR_NN_endb),]$perc20_TR_NN_endb[841:960], xlab = "", ylab = "", xaxt='n', type = "h", ylim = c(min(gs_df[order(gs_df$perc20_TR_NN_endb),]$perc20_TR_NN_endb[841:960])-0.001, gs_df$perc20_TR_TS_endb[1]+0.002))
# text(841:960, par("usr")[3], labels = gs_df[order(gs_df$perc20_TR_NN_endb),]$gs_names[841:960], srt = 45, adj = c(1.1,1.1), xpd = TRUE, cex=0.6)
# abline(h = gs_df$perc20_TR_TS_endb[1], lty = "dashed")
# text(841 + 0.5, gs_df$perc20_TR_TS_endb[1] - 0.01, labels = c("Human Agreement"))
# p2 = plot(721:840, gs_df[order(gs_df$perc20_TR_NN_endb),]$perc20_TR_NN_endb[721:840], xlab = "", ylab = "", xaxt='n', type = "h")
# text(721:840, par("usr")[3], labels = gs_df[order(gs_df$perc20_TR_NN_endb),]$gs_names[721:840], srt = 45, adj = c(1.1,1.1), xpd = TRUE, cex=0.6)
# p3 = plot(601:720, gs_df[order(gs_df$perc20_TR_NN_endb),]$perc20_TR_NN_endb[601:720], xlab = "", ylab = "", xaxt='n', type = "h")
# text(601:720, par("usr")[3], labels = gs_df[order(gs_df$perc20_TR_NN_endb),]$gs_names[601:720], srt = 45, adj = c(1.1,1.1), xpd = TRUE, cex=0.6)
# p4 = plot(481:600, gs_df[order(gs_df$perc20_TR_NN_endb),]$perc20_TR_NN_endb[481:600], xlab = "", ylab = "", xaxt='n', type = "h")
# text(481:600, par("usr")[3], labels = gs_df[order(gs_df$perc20_TR_NN_endb),]$gs_names[481:600], srt = 45, adj = c(1.1,1.1), xpd = TRUE, cex=0.6)
# 
# mtext("1st End Boundary (2/2) - TR Baseline", side = 3, outer = TRUE)

p1 = plot(841:960, gs_df[order(gs_df$perc20_SA_NN_endb),]$perc20_SA_NN_endb[841:960], xlab = "", ylab = "", xaxt='n', type = "h", ylim = c(min(gs_df[order(gs_df$perc20_SA_NN_endb),]$perc20_SA_NN_endb[841:960])-0.001, gs_df$perc20_TR_TS_endb[1]+0.002))
text(841:960, par("usr")[3], labels = gs_df[order(gs_df$perc20_SA_NN_endb),]$gs_names[841:960], srt = 45, adj = c(1.1,1.1), xpd = TRUE, cex=0.6)
abline(h = gs_df$perc20_TR_TS_endb[1], lty = "dashed")
text(841 + 0.5, gs_df$perc20_TR_TS_endb[1] - 0.01, labels = c("Human Agreement"))
p2 = plot(721:840, gs_df[order(gs_df$perc20_SA_NN_endb),]$perc20_SA_NN_endb[721:840], xlab = "", ylab = "", xaxt='n', type = "h")
text(721:840, par("usr")[3], labels = gs_df[order(gs_df$perc20_SA_NN_endb),]$gs_names[721:840], srt = 45, adj = c(1.1,1.1), xpd = TRUE, cex=0.6)
p3 = plot(601:720, gs_df[order(gs_df$perc20_SA_NN_endb),]$perc20_SA_NN_endb[601:720], xlab = "", ylab = "", xaxt='n', type = "h")
text(601:720, par("usr")[3], labels = gs_df[order(gs_df$perc20_SA_NN_endb),]$gs_names[601:720], srt = 45, adj = c(1.1,1.1), xpd = TRUE, cex=0.6)
p4 = plot(481:600, gs_df[order(gs_df$perc20_SA_NN_endb),]$perc20_SA_NN_endb[481:600], xlab = "", ylab = "", xaxt='n', type = "h")
text(481:600, par("usr")[3], labels = gs_df[order(gs_df$perc20_SA_NN_endb),]$gs_names[481:600], srt = 45, adj = c(1.1,1.1), xpd = TRUE, cex=0.6)

mtext("1st End Boundary (2/2) - SA Baseline", side = 3, outer = TRUE)




# p1 = plot(361:480, gs_df[order(gs_df$perc20_TR_NN_ende),]$perc20_TR_NN_ende[361:480], xlab = "", ylab = "", xaxt='n', type = "h")
# text(361:480, par("usr")[3], labels = gs_df[order(gs_df$perc20_TR_NN_ende),]$gs_names[361:480], srt = 45, adj = c(1.1,1.1), xpd = TRUE, cex=0.6)
# p2 = plot(241:360, gs_df[order(gs_df$perc20_TR_NN_ende),]$perc20_TR_NN_ende[241:360], xlab = "", ylab = "", xaxt='n', type = "h")
# text(241:360, par("usr")[3], labels = gs_df[order(gs_df$perc20_TR_NN_ende),]$gs_names[241:360], srt = 45, adj = c(1.1,1.1), xpd = TRUE, cex=0.6)
# p3 = plot(121:240, gs_df[order(gs_df$perc20_TR_NN_ende),]$perc20_TR_NN_ende[121:240], xlab = "", ylab = "", xaxt='n', type = "h")
# text(121:240, par("usr")[3], labels = gs_df[order(gs_df$perc20_TR_NN_ende),]$gs_names[121:240], srt = 45, adj = c(1.1,1.1), xpd = TRUE, cex=0.6)
# abline(h = gs_df$perc20_TR_KAL_ende[1], lty = "dashed")
# text(121, gs_df$perc20_TR_KAL_ende[1] - 0.003, labels = c("Kaldi Agreement"))
# p4 = plot(1:120, gs_df[order(gs_df$perc20_TR_NN_ende),]$perc20_TR_NN_ende[1:120], xlab = "", ylab = "", xaxt='n', type = "h")
# text(1:120, par("usr")[3], labels = gs_df[order(gs_df$perc20_TR_NN_ende),]$gs_names[1:120], srt = 45, adj = c(1.1,1.1), xpd = TRUE, cex=0.6)
# 
# mtext("2nd End Boundary (1/2) - TR Baseline", side = 3, outer = TRUE)
# 
# 
# p1 = plot(841:960, gs_df[order(gs_df$perc20_TR_NN_ende),]$perc20_TR_NN_ende[841:960], xlab = "", ylab = "", xaxt='n', type = "h", ylim = c(min(gs_df[order(gs_df$perc20_TR_NN_ende),]$perc20_TR_NN_ende[841:960])-0.001, gs_df$perc20_TR_TS_ende[1]+0.002))
# text(841:960, par("usr")[3], labels = gs_df[order(gs_df$perc20_TR_NN_ende),]$gs_names[841:960], srt = 45, adj = c(1.1,1.1), xpd = TRUE, cex=0.6)
# abline(h = gs_df$perc20_TR_TS_ende[1], lty = "dashed")
# text(841 + 0.5, gs_df$perc20_TR_TS_ende[1] - 0.01, labels = c("Human Agreement"))
# p2 = plot(721:840, gs_df[order(gs_df$perc20_TR_NN_ende),]$perc20_TR_NN_ende[721:840], xlab = "", ylab = "", xaxt='n', type = "h")
# text(721:840, par("usr")[3], labels = gs_df[order(gs_df$perc20_TR_NN_ende),]$gs_names[721:840], srt = 45, adj = c(1.1,1.1), xpd = TRUE, cex=0.6)
# p3 = plot(601:720, gs_df[order(gs_df$perc20_TR_NN_ende),]$perc20_TR_NN_ende[601:720], xlab = "", ylab = "", xaxt='n', type = "h")
# text(601:720, par("usr")[3], labels = gs_df[order(gs_df$perc20_TR_NN_ende),]$gs_names[601:720], srt = 45, adj = c(1.1,1.1), xpd = TRUE, cex=0.6)
# p4 = plot(481:600, gs_df[order(gs_df$perc20_TR_NN_ende),]$perc20_TR_NN_ende[481:600], xlab = "", ylab = "", xaxt='n', type = "h")
# text(481:600, par("usr")[3], labels = gs_df[order(gs_df$perc20_TR_NN_ende),]$gs_names[481:600], srt = 45, adj = c(1.1,1.1), xpd = TRUE, cex=0.6)
# 
# mtext("2nd End Boundary (2/2) - TR Baseline", side = 3, outer = TRUE)


p1 = plot(841:960, gs_df[order(gs_df$perc20_SA_NN_ende),]$perc20_SA_NN_ende[841:960], xlab = "", ylab = "", xaxt='n', type = "h", ylim = c(min(gs_df[order(gs_df$perc20_SA_NN_ende),]$perc20_SA_NN_ende[841:960]), gs_df$perc20_TR_TS_ende[1]+0.002))
text(841:960, par("usr")[3], labels = gs_df[order(gs_df$perc20_SA_NN_ende),]$gs_names[841:960], srt = 45, adj = c(1.1,1.1), xpd = TRUE, cex=0.6)
abline(h = gs_df$perc20_TR_TS_ende[1], lty = "dashed")
text(841 + 0.5, gs_df$perc20_TR_TS_ende[1] - 0.01, labels = c("Human Agreement"))
p2 = plot(721:840, gs_df[order(gs_df$perc20_SA_NN_ende),]$perc20_SA_NN_ende[721:840], xlab = "", ylab = "", xaxt='n', type = "h")
text(721:840, par("usr")[3], labels = gs_df[order(gs_df$perc20_SA_NN_ende),]$gs_names[721:840], srt = 45, adj = c(1.1,1.1), xpd = TRUE, cex=0.6)
p3 = plot(601:720, gs_df[order(gs_df$perc20_SA_NN_ende),]$perc20_SA_NN_ende[601:720], xlab = "", ylab = "", xaxt='n', type = "h")
text(601:720, par("usr")[3], labels = gs_df[order(gs_df$perc20_SA_NN_ende),]$gs_names[601:720], srt = 45, adj = c(1.1,1.1), xpd = TRUE, cex=0.6)
p4 = plot(481:600, gs_df[order(gs_df$perc20_SA_NN_ende),]$perc20_SA_NN_ende[481:600], xlab = "", ylab = "", xaxt='n', type = "h")
text(481:600, par("usr")[3], labels = gs_df[order(gs_df$perc20_SA_NN_ende),]$gs_names[481:600], srt = 45, adj = c(1.1,1.1), xpd = TRUE, cex=0.6)

mtext("2nd End Boundary (2/2) - SA Baseline", side = 3, outer = TRUE)
