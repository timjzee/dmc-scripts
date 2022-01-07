form Command line parameters
    boolean apply_penalty 1
    real kal_diff_weight 0.5
    integer n_smooths 1
    boolean take_sqrt 0
    integer n_prec_values 10
    integer n_subs_values 0
    word context_frames_schwa 5
    word context_frames_n 15
    word context_frames_N 5
    word network_type_schwa BLSTM
    word network_type_n BLSTM
    word network_type_N BLSTM
    real threshold_schwa 0.2
    real threshold_n 0.2
    real threshold_N 0.8
    boolean subtract_n 0
    boolean debug_mode 0
endform

Text writing preferences: "UTF-8"

if macintosh
    tensusers$ = "/Volumes/tensusers/timzee/"
else
    tensusers$ = "/vol/tensusers/timzee/"
endif

eval_folder$ = "eval2"

double_derivative = 1
frag_buffer = 0.2

segments$[1] = "e"
segments$[2] = "n"
segments$[3] = "N"
segments_full$[1] = "schwa"
segments_full$[2] = "nasal"
segments_full$[3] = "nasalization"
thresholds[1] = threshold_schwa
thresholds[2] = threshold_n
thresholds[3] = threshold_N
context_frames$[1] = context_frames_schwa$
context_frames$[2] = context_frames_n$
context_frames$[3] = context_frames_N$
network_types$[1] = network_type_schwa$
network_types$[2] = network_type_n$
network_types$[3] = network_type_N$

# Make sure input file has a header
Read Table from comma-separated file: tensusers$ + "classifier_evaluation/en/" + "nn_eval_en_o2_annotated.csv"
Rename: "chunks"

for s_i from 1 to 3
    s$ = segments$[s_i]
    Append column: "nn_" + s$ + "_start_b"
    Append column: "nn_" + s$ + "_start_e"
    Append column: "nn_" + s$ + "_end_b"
    Append column: "nn_" + s$ + "_end_e"
    Append column: "nn_" + s$ + "_trace"
    if s$ != "N"
        Append column: s$ + "_f0"
        Append column: s$ + "_fP1"
        Append column: s$ + "_fA1"
        Append column: s$ + "_fA2"
        Append column: s$ + "_A1minP1"
    endif
endfor


procedure inspectChunk: annotateChunk.id
    selectObject: "Table chunks"
    filepath$ = Get value: annotateChunk.id, "wav"
    c_channel$ = Get value: annotateChunk.id, "chan"
    name_length = length(filepath$)
    if left$(filepath$, 1) == "p"
        s_name$ = filepath$
        corpus$ = "ecsd"
        cgn = 0
    elsif left$(filepath$, 1) == "D"
        s_name$ = filepath$
        corpus$ = "ifadv"
        cgn = 0
    else
        s_name$ = right$(filepath$, name_length - 5)
        corpus$ = left$(filepath$, 1)
        cgn = 1
    endif

    # Get neural network boundaries
    for s_i from 1 to 3
        s$ = segments$[s_i]
        s_text$ = segments_full$[s_i]
        kal_start = Get value: annotateChunk.id, "kal_start"
        kal_end = Get value: annotateChunk.id, "kal_end"
        sound_end = Get value: annotateChunk.id, "sound_end"
        frag_start = kal_start - frag_buffer * 1.5
        frag_start$ = fixed$(frag_start, 3)
        frag_end = kal_end + frag_buffer
        if frag_start < 0
            frag_start = 0.000
            frag_start$ = "0.000"
        endif
        if frag_end > sound_end
            frag_end = sound_end
        endif
        frag_path$ = tensusers$ + "af_classification/pred_textgrids_keras_en/" + eval_folder$ + "/" + network_types$[s_i] + "_" + context_frames$[s_i] + "/"
        if corpus$ = "ecsd"
            frag_file$ = frag_path$ + s_name$ + "_S_" + c_channel$ + "_" + frag_start$ + "_" + fixed$(frag_end, 3) + "_" + s_text$ + ".IntensityTier"
        else
            frag_file$ = frag_path$ + s_name$ + "_" + c_channel$ + "_" + frag_start$ + "_" + fixed$(frag_end, 3) + "_" + s_text$ + ".IntensityTier"
        endif
        if fileReadable(frag_file$)
            Read from file: frag_file$
            p_name$ = selected$("IntensityTier")
            selectObject: "IntensityTier " + p_name$
            s_time = Get start time
            e_time = Get end time
            Create TextGrid: s_time, e_time, "starts ends", "starts ends"
            # flatten nasal trace before schwa
            if s$ == "n"
                selectObject: "Table chunks"
                schwa_start_b$ = Get value: annotateChunk.id, "nn_e_start_b"
                schwa_start_e$ = Get value: annotateChunk.id, "nn_e_start_e"
                schwa_end_b$ = Get value: annotateChunk.id, "nn_e_end_b"
                schwa_end_e$ = Get value: annotateChunk.id, "nn_e_end_e"
                schwa_present = not index(schwa_start_b$ + schwa_start_e$ + schwa_end_b$ + schwa_end_e$, "NA")
                if schwa_present
                    selectObject: "IntensityTier " + p_name$
                    schwa_start_i = Get low index from time: number(schwa_start_b$)
                    Formula: "if (col <= schwa_start_i) then 0 else self endif"
                endif
            elsif s$ == "N"
                selectObject: "Table chunks"
                schwa_start_b$ = Get value: annotateChunk.id, "nn_e_start_b"
                schwa_start_e$ = Get value: annotateChunk.id, "nn_e_start_e"
                schwa_end_b$ = Get value: annotateChunk.id, "nn_e_end_b"
                schwa_end_e$ = Get value: annotateChunk.id, "nn_e_end_e"
                schwa_present = not index(schwa_start_b$ + schwa_start_e$ + schwa_end_b$ + schwa_end_e$, "NA")
                nasal_start_b$ = Get value: annotateChunk.id, "nn_n_start_b"
                nasal_start_e$ = Get value: annotateChunk.id, "nn_n_start_e"
                nasal_end_b$ = Get value: annotateChunk.id, "nn_n_end_b"
                nasal_end_e$ = Get value: annotateChunk.id, "nn_n_end_e"
                nasal_present = not index(nasal_start_b$ + nasal_start_e$ + nasal_end_b$ + nasal_end_e$, "NA")
                selectObject: "IntensityTier " + p_name$
                if schwa_present
                    schwa_start_i = Get low index from time: number(schwa_start_b$)
                    if nasal_present
                        nasal_end_i = Get low index from time: number(nasal_end_e$)
                        Formula: "if (col <= schwa_start_i) or (col > nasal_end_i) then 0 else self endif"
                    else
                        schwa_end_i = Get low index from time: number(schwa_end_e$)
                        Formula: "if (col <= schwa_start_i) or (col > schwa_end_i) then 0 else self endif"
                    endif
                else
                    if nasal_present
                        nasal_start_i = Get low index from time: number(nasal_start_b$)
                        nasal_end_i = Get low index from time: number(nasal_end_e$)
                        Formula: "if (col < nasal_start_i) or (col > nasal_end_i) then 0 else self endif"
                    endif
                endif
            endif
            selectObject: "IntensityTier " + p_name$
            # add points to allow for n subtraction across different context sizes
            if context_frames$[s_i] == "15"
                z_t = Get time from index: 1
                z_v = Get value at index: 1
                for added_p from 1 to 10
                    Add point: z_t - 0.005*added_p, z_v
                endfor
                f_i = Get number of points
                f_t = Get time from index: f_i
                f_v = Get value at index: f_i
                for added_p from 1 to 10
                    Add point: f_t + 0.005*added_p, f_v
                endfor
            endif
            # square root step
            if take_sqrt
                Formula: "sqrt(self)"
            endif
            # subtract n step
            if subtract_n and s$ == "e"
                p_file$ = right$(frag_file$, length(frag_file$) - rindex(frag_file$, "/"))
                n_file$ = replace_regex$(p_file$, "_[a-z]+(?=\.IntensityTier)", "_nasal", 0)
                Read from file: tensusers$ + "af_classification/pred_textgrids_keras_en/" + eval_folder$ + "/" + network_types$[2] + "_" + context_frames$[2] + "/" + n_file$
                n_name$ = selected$("IntensityTier")
                if context_frames$[2] == "15"
                    z_t = Get time from index: 1
                    z_v = Get value at index: 1
                    for added_p from 1 to 10
                        Add point: z_t - 0.005*added_p, z_v
                    endfor
                    f_i = Get number of points
                    f_t = Get time from index: f_i
                    f_v = Get value at index: f_i
                    for added_p from 1 to 10
                        Add point: f_t + 0.005*added_p, f_v
                    endfor
                endif
                if take_sqrt
                    Formula: "sqrt(self)"
                endif
                for step from 1 to n_smooths
                    Copy: n_name$ + "_copy"
                    Formula: "if (col > 1) and (col <= (ncol - 1)) then (IntensityTier_'n_name$'[col - 1] + IntensityTier_'n_name$'[col] + IntensityTier_'n_name$'[col + 1]) / 3 else IntensityTier_'n_name$'[col] endif"
                    removeObject: "IntensityTier " + n_name$
                    Rename: n_name$
                endfor
                selectObject: "IntensityTier " + p_name$
                Copy: p_name$ + "_copy"
                Formula: "if (IntensityTier_'n_name$'[col] > IntensityTier_'p_name$'[col]) then 0 else IntensityTier_'p_name$'[col] - IntensityTier_'n_name$'[col] endif"
                removeObject: "IntensityTier " + p_name$
                Rename: p_name$
                removeObject: "IntensityTier " + n_name$
            endif
            # smoothing step
            for step from 1 to n_smooths
                Copy: p_name$ + "_copy"
                Formula: "if (col > 1) and (col <= (ncol - 1)) then (IntensityTier_'p_name$'[col - 1] + IntensityTier_'p_name$'[col] + IntensityTier_'p_name$'[col + 1]) / 3 else IntensityTier_'p_name$'[col] endif"
                removeObject: "IntensityTier " + p_name$
                Rename: p_name$
            endfor
            if double_derivative
                Copy: p_name$ + "_diff_diff"
                Formula: "if (col < (ncol - 1)) and (col > 1) then (IntensityTier_'p_name$'[col + 1] - 2 * IntensityTier_'p_name$'[col] + IntensityTier_'p_name$'[col - 1]) else 0 endif"
                Down to TableOfReal
                To Table: "rowLabel"
                p_max_diff2 = Get maximum: "Intensity (dB)"
                p_min_diff2 = Get minimum: "Intensity (dB)"
                removeObject: "TableOfReal " + p_name$ + "_diff_diff"
                removeObject: "Table " + p_name$ + "_diff_diff"
            endif
            selectObject: "IntensityTier " + p_name$
            Copy: p_name$ + "_diff"
            # time derivative measured in time steps (which corresponds to 5 ms)
            Formula: "if (col < (ncol - 1)) and (col > 1) then (IntensityTier_'p_name$'[col + 1] - IntensityTier_'p_name$'[col - 1]) else 0 endif"
            num_points = Get number of points
            Down to TableOfReal
            To Table: "rowLabel"
            p_max_diff = Get maximum: "Intensity (dB)"
            p_min_diff = Get minimum: "Intensity (dB)"
            removeObject: "TableOfReal " + p_name$ + "_diff"
            removeObject: "Table " + p_name$ + "_diff"
            # we also keep track of the preceding values in calculating strength of start boundary
            prec_values# = zero#(n_prec_values)
            # and the same for end boundary
            for point from 2 to (num_points - 1)
                selectObject: "IntensityTier " + p_name$ + "_diff"
                diff_point = Get value at index: point
                if diff_point > p_max_diff / 10
                    diff_prev = Get value at index: point - 1
                    diff_next = Get value at index: point + 1
                    if (diff_prev < diff_point) and (diff_next < diff_point)
                        t_maxpoint = Get time from index: point
                        # subtract average preceding strength
                        if n_prec_values > 0
                            t_maxstrength = diff_point - mean(prec_values#)
                        else
                            t_maxstrength = diff_point
                        endif
                        if t_maxstrength > p_max_diff
                            t_maxstrength = p_max_diff
                        endif
                        selectObject: "TextGrid starts_ends"
                        Insert point: 1, t_maxpoint, string$(t_maxstrength)
                    endif
                elif diff_point < p_min_diff / 10
                    diff_prev = Get value at index: point - 1
                    diff_next = Get value at index: point + 1
                    if (diff_prev > diff_point) and (diff_next > diff_point)
                        t_minpoint = Get time from index: point
                        if n_subs_values > 0
                            if (point + n_subs_values) > num_points
                                subs_max = num_points
                            else
                                subs_max = point + n_subs_values
                            endif
                            cumul_subs_diff = 0
                            for subs_point from (point + 1) to subs_max
                                subs_diff = Get value at index: subs_point
                                cumul_subs_diff = cumul_subs_diff + abs(subs_diff)
                            endfor
                            t_minstrength = abs(diff_point) - (cumul_subs_diff/(subs_max - point))
                        else
                            t_minstrength = abs(diff_point)
                        endif
                        if t_minstrength > abs(p_min_diff)
                            t_minstrength = abs(p_min_diff)
                        endif
                        selectObject: "TextGrid starts_ends"
                        Insert point: 2, t_minpoint, string$(t_minstrength)
                    endif
                endif
                for prec_v from 1 to (n_prec_values - 1)
                    prec_values#[prec_v] = prec_values#[prec_v+1]
                endfor
                if n_prec_values > 0
                    prec_values#[n_prec_values] = abs(diff_point)
                endif
            endfor
    #        removeObject: "IntensityTier " + p_name$ + "_diff"
            selectObject: "TextGrid starts_ends"
            Extract one tier: 1
            Down to Table: "no", 6, "yes", "no"
            Insert column: 1, "diff"
            Insert column: 1, "tmin2"
            Insert column: 1, "tmin2_i"
            Insert column: 1, "tmin3"
            n_starts = Get number of rows
            dd_search_window = 10
            for start_cand from 1 to n_starts
                start_t = Get value: start_cand, "tmin"
                if double_derivative
                    selectObject: "IntensityTier " + p_name$ + "_diff_diff"
                    dd_i = Get nearest index from time: start_t
                    dd_i = dd_i + 1
                    dd_i_stop = dd_i - dd_search_window
                    if dd_i_stop < 1
                        dd_i_stop = 1
                    endif
                    repeat
                        dd_i = dd_i - 1
                        dd_cur = Get value at index: dd_i
                        if dd_cur > p_max_diff2 / 10
                            dd_prev = Get value at index: dd_i - 1
                            dd_next = Get value at index: dd_i + 1
                            if (dd_prev < dd_cur) and (dd_next < dd_cur)
                                dd_time = Get time from index: dd_i
                                selectObject: "Table starts"
                                Set numeric value: start_cand, "tmin2", dd_time
                                Set numeric value: start_cand, "tmin2_i", dd_i
                                goto FOUND_start_b
                            endif
                        endif
                    until dd_i == dd_i_stop
                    label FOUND_start_b
                    selectObject: "IntensityTier " + p_name$ + "_diff_diff"
                    dd_i_start = Get nearest index from time: start_t
                    dd_i_stop = dd_i_start + dd_search_window
                    num_dd_i = Get number of points
                    if dd_i_stop > num_dd_i
                        dd_i_stop = num_dd_i
                    endif
                    for dd_i from dd_i_start to dd_i_stop
                        dd_cur = Get value at index: dd_i
                        if dd_cur < p_min_diff2 / 10
                            dd_prev = Get value at index: dd_i - 1
                            dd_next = Get value at index: dd_i + 1
                            if (dd_prev > dd_cur) and (dd_next > dd_cur)
                                dd_time = Get time from index: dd_i
                                selectObject: "Table starts"
                                Set numeric value: start_cand, "tmin3", dd_time
                                goto FOUND_start_e
                            endif
                        endif
                    endfor
                    label FOUND_start_e
                endif
                selectObject: "Table starts"
                start_diff = (1 - abs(kal_start - start_t) * 10) * kal_diff_weight
                Set numeric value: start_cand, "diff", start_diff
            endfor
            Append sum column: "diff", "text", "score"
            nn_start_score = Get maximum: "score"
            nn_start_score$ = fixed$(nn_start_score, 3)
            nn_start_i = Search column: "score", string$(nn_start_score)
            nn_start_index = undefined
            if nn_start_i == 0
                nn_start = undefined
                nn_start$ = "NA"
                nn_start_e$ = "NA"
            else
                if double_derivative
                    nn_start = Get value: nn_start_i, "tmin2"
                    if nn_start == undefined
                        nn_start$ = "NA"
                        appendInfoLine: "ERROR: could not find nn_start_b for " + filepath$ + "," + c_channel$
                    else
                        nn_start$ = fixed$(nn_start, 3)
                        nn_start_index = Get value: nn_start_i, "tmin2_i"
                    endif
                    nn_start_e = Get value: nn_start_i, "tmin3"
                    if nn_start_e == undefined
                        nn_start_e$ = "NA"
                        appendInfoLine: "ERROR: could not find nn_start_e for " + filepath$ + "," + c_channel$
    #                    exitScript()
                    else
                        nn_start_e$ = fixed$(nn_start_e, 3)
                    endif
                else
                    nn_start = Get value: nn_start_i, "tmin"
                    nn_start$ = fixed$(nn_start, 3)
                    nn_start_e$ = "NA"
                endif
            endif
            selectObject: "TextGrid starts_ends"
            Extract one tier: 2
            # it doesn't make much sense to ignore end candidates before
            # the chosen start time, because certain fragments may not have
            # a clear start time, but almost all should have an end time
            # if anything the end time should be used to narrow down start candidates
            # for this we would need to find end before we find the start
#            if nn_start$ != "NA"
#                wrong_end_points = Get low index from time: 1, nn_start
#                for end_point from 1 to wrong_end_points
#                    Remove point: 1, 1
#                endfor
#            endif
            Down to Table: "no", 6, "yes", "no"
            Insert column: 1, "mod_strength"
            Insert column: 1, "diff"
            Insert column: 1, "tmin2"
            Insert column: 1, "tmin2_i"
            Insert column: 1, "tmin3"
            n_ends = Get number of rows
            prev_strength = 0
            cum_strength = 0
            penalty = 0
            prev_end = 0
            for end_cand from 1 to n_ends
                end_t = Get value: end_cand, "tmin"
                end_diff = (1 - abs(kal_end - end_t) * 10) * kal_diff_weight
                Set numeric value: end_cand, "diff", end_diff
                cur_strength = Get value: end_cand, "text"
                selectObject: "TextGrid starts_ends"
                if prev_end and apply_penalty and prev_end > nn_start
                    closest_start_i = Get low index from time: 1, prev_end
                    closest_start = Get time of point: 1, closest_start_i
                    if closest_start == nn_start
                        penalty = cum_strength * (prev_end - closest_start) * 10
                    else
                        cum_strength = 0
                        penalty += prev_strength * (prev_end - closest_start) * 10
                    endif
                else
                    penalty = 0
                endif
                if double_derivative
                    selectObject: "IntensityTier " + p_name$ + "_diff_diff"
                    dd_i_start = Get nearest index from time: end_t
                    dd_i_stop = dd_i_start + dd_search_window
                    num_dd_i = Get number of points
                    if dd_i_stop > num_dd_i
                        dd_i_stop = num_dd_i
                    endif
                    for dd_i from dd_i_start to dd_i_stop
                        dd_cur = Get value at index: dd_i
                        if dd_cur > p_max_diff2 / 10
                            dd_prev = Get value at index: dd_i - 1
                            dd_next = Get value at index: dd_i + 1
                            if (dd_prev < dd_cur) and (dd_next < dd_cur)
                                dd_time = Get time from index: dd_i
                                selectObject: "Table ends"
                                Set numeric value: end_cand, "tmin2", dd_time
                                Set numeric value: end_cand, "tmin2_i", dd_i
                                goto FOUND_end_e
                            endif
                        endif
                    endfor
                    label FOUND_end_e
                    selectObject: "IntensityTier " + p_name$ + "_diff_diff"
                    dd_i = Get nearest index from time: end_t
                    dd_i = dd_i + 1
                    dd_i_stop = dd_i - dd_search_window
                    if dd_i_stop < 1
                        dd_i_stop = 1
                    endif
                    repeat
                        dd_i = dd_i - 1
                        dd_cur = Get value at index: dd_i
                        if dd_cur < p_min_diff2 / 10
                            dd_prev = Get value at index: dd_i - 1
                            dd_next = Get value at index: dd_i + 1
                            if (dd_prev > dd_cur) and (dd_next > dd_cur)
                                dd_time = Get time from index: dd_i
                                selectObject: "Table ends"
                                Set numeric value: end_cand, "tmin3", dd_time
                                goto FOUND_end_b
                            endif
                        endif
                    until dd_i == dd_i_stop
                    label FOUND_end_b
                endif
                selectObject: "Table ends"
                mod_strength = cur_strength - penalty
                Set numeric value: end_cand, "mod_strength", mod_strength
                prev_strength = cur_strength
                cum_strength += cur_strength
                prev_end = end_t
            endfor
            Append sum column: "mod_strength", "diff", "score"
            nn_end_score = Get maximum: "score"
            nn_end_score$ = fixed$(nn_end_score, 3)
            nn_end_i = Search column: "score", string$(nn_end_score)
            nn_end_index = undefined
            if nn_end_i == 0
                nn_end = undefined
                nn_end$ = "NA"
                nn_end_b$ = "NA"
            else
                if double_derivative
                    nn_end = Get value: nn_end_i, "tmin2"
                    if nn_end == undefined
                        nn_end$ = "NA"
                        appendInfoLine: "ERROR: could not find nn_end_e for " + filepath$ + "," + c_channel$
    #                    exitScript()
                    else
                        nn_end$ = fixed$(nn_end, 3)
                        nn_end_index = Get value: nn_end_i, "tmin2_i"
                    endif
                    nn_end_b = Get value: nn_end_i, "tmin3"
                    if nn_end_b == undefined
                        nn_end_b$ = "NA"
                        appendInfoLine: "ERROR: could not find nn_end_b for " + filepath$ + "," + c_channel$
    #                    exitScript()
                    else
                        nn_end_b$ = fixed$(nn_end_b, 3)
                    endif
                else
                    nn_end = Get value: nn_end_i, "tmin"
                    nn_end$ = fixed$(nn_end, 3)
                    nn_end_b$ = "NA"
                endif
            endif
            # Check if max meets threshold
            if nn_end_b$ != "NA" and nn_start_e$ != "NA"
                selectObject: "IntensityTier " + p_name$
                if nn_end_b$ == nn_start_e$
                    peak_val = Get value at time: nn_end_b
                else
                    start_e_i = Get nearest index from time: nn_start_e
                    end_b_i = Get nearest index from time: nn_end_b
                    Down to TableOfReal
                    Extract row ranges: string$(start_e_i) + ":" + string$(end_b_i)
                    peak_val = Get column mean (index): 2
                    removeObject: "TableOfReal " + p_name$
                    removeObject: "TableOfReal " + p_name$ + "_rows"
#                    peak_val = Get value at time: nn_start_e + (nn_end_b - nn_start_e) / 2
                endif
                if peak_val < thresholds[s_i]
                    nn_start$ = "NA"
                    nn_start_e$ = "NA"
                    nn_end_b$ = "NA"
                    nn_end$ = "NA"
                endif
            endif
            #
            if (nn_start_index != undefined) and (nn_end_index != undefined)
                trace_range$ = string$(nn_start_index) + ":" + string$(nn_end_index)
                selectObject: "IntensityTier " + p_name$
                Down to TableOfReal
                Extract row ranges: trace_range$
                To Table: "rowLabel"
                Remove column: "Time (s)"
                Remove column: "rowLabel"
                bla$ = List: "no"
                bla2$ = replace_regex$(bla$,"[^0-9]+\n", "c(", 1)
                bla3$ = replace_regex$(bla2$,"\n$", ")",0)
                nn_trace$ = replace_regex$(bla3$,"\n", ",",0)
                removeObject: "TableOfReal " + p_name$
                removeObject: "TableOfReal " + p_name$ + "_rows"
                removeObject: "Table " + p_name$ + "_rows"
            else
                nn_trace$ = "NA"
            endif
            Read from file: tensusers$ + "af_classification/pred_fragments_en/" + eval_folder$ + "/" + s_name$ + "_" + c_channel$ + "_" + frag_start$ + "_" + fixed$(frag_end, 3) + ".wav"
            w_name$ = selected$("Sound")
            Shift times to: "start time", frag_start
            # get A1-P1
            if s$ != "N"
                if index(nn_start$ + nn_start_e$ + nn_end$ + nn_end_b$, "NA")
                    a1_min_p1$ = "NA"
                    fa1$ = "NA"
                    fa2$ = "NA"
                    fp1$ = "NA"
                    f0$ = "NA"
                else
                    # find middle 80% of segment
                    s_dur = nn_end - nn_start
                    a1p1_s = nn_start + 0.1*s_dur
                    a1p1_e = nn_end - 0.1*s_dur
                    # do the thing
                    To Formant (burg): 0, 5, 5500, 0.025, 50
                    form1 = Get mean: 1, a1p1_s, a1p1_e, "hertz"
                    form2 = Get mean: 2, a1p1_s, a1p1_e, "hertz"
                    Remove
                    selectObject: "Sound " + w_name$
                    To Pitch: 0, 75, 600
                    f0 = Get mean: a1p1_s, a1p1_e, "Hertz"
                    Remove
                    if f0 == undefined
                        appendInfoLine: "Couldn't find f0"
                        a1_min_p1$ = "NA"
                        fa1$ = "NA"
                        fa2$ = "NA"
                        fp1$ = "NA"
                        f0$ = "NA"
                    else
                        selectObject: "Sound " + w_name$
                        Extract part: a1p1_s, a1p1_e, "Gaussian1", 1, "no"
                        To Spectrum: "no"
                        removeObject: "Sound " + w_name$ + "_part"
                        Cepstral smoothing: min(f0*3, 500)
                        Rename: w_name$ + "_part_smooth"
                        form1s = Get frequency of nearest maximum: form1
                        form2s = Get frequency of nearest maximum: form2
                        if form1s > 1000 or form2s < 1000 or form2s > 2000
                            a1_min_p1$ = "NA"
                            fa1$ = fixed$(form1s,3)
                            fa2$ = fixed$(form2s,3)
                            fp1$ = "NA"
                            f0$ = fixed$(f0,3)
                        else
                            nasalpeak = Get frequency of nearest maximum: (form1s + form2s) / 2
                            selectObject: "Spectrum " + w_name$ + "_part"
                            if (nasalpeak != form1s) and (nasalpeak != form2s)
                                fp1 = nasalpeak
                            else
                                fp1 = (form1s + form2s) / 2
                            endif
                            p1 = Get sound pressure level of nearest maximum: fp1
                            a1 = Get sound pressure level of nearest maximum: form1s
                            a1_min_p1$ = fixed$(a1 - p1, 3)
                            fa1$ = fixed$(form1s,3)
                            fa2$ = fixed$(form2s,3)
                            fp1$ = fixed$(fp1,3)
                            f0$ = fixed$(f0,3)
                        endif
                        removeObject: "Spectrum " + w_name$ + "_part"
                        removeObject: "Spectrum " + w_name$ + "_part_smooth"
                    endif
                endif
            endif
            # debug
            if debug_mode
                pauseScript: "File " + s_name$ + "_" + c_channel$ + "_" + frag_start$ + "_" + fixed$(frag_end, 3) + "_" + s_text$
            else
                removeObject: "Sound " + w_name$
#                if s$ != "N"
#                    removeObject: "Spectrum " + w_name$ + "_part"
#                    removeObject: "Spectrum " + w_name$ + "_part_smooth"
#                endif
            endif
            removeObject: "Table starts"
            removeObject: "TextGrid starts"
            removeObject: "Table ends"
            removeObject: "TextGrid ends"
            removeObject: "IntensityTier " + p_name$
            removeObject: "IntensityTier " + p_name$ + "_diff"
            removeObject: "IntensityTier " + p_name$ + "_diff_diff"
            removeObject: "TextGrid starts_ends"
        else
            appendInfoLine: frag_file$, " not readable"
            nn_start$ = "NA"
            nn_start_e$ = "NA"
            nn_end$ = "NA"
            nn_end_b$ = "NA"
            nn_trace$ = "NA"
        endif

        selectObject: "Table chunks"
        Set string value: annotateChunk.id, "nn_" + s$ + "_start_b", nn_start$
        Set string value: annotateChunk.id, "nn_" + s$ + "_start_e", nn_start_e$
        Set string value: annotateChunk.id, "nn_" + s$ + "_end_b", nn_end_b$
        Set string value: annotateChunk.id, "nn_" + s$ + "_end_e", nn_end$
        Set string value: annotateChunk.id, "nn_" + s$ + "_trace", nn_trace$
        if s$ != "N"
            Set string value: annotateChunk.id, s$ + "_A1minP1", a1_min_p1$
            Set string value: annotateChunk.id, s$ + "_fA1", fa1$
            Set string value: annotateChunk.id, s$ + "_fP1", fp1$
            Set string value: annotateChunk.id, s$ + "_fA2", fa2$
            Set string value: annotateChunk.id, s$ + "_f0", f0$
        endif
    endfor
endproc


num_chunks = Get number of rows
for id from 1 to num_chunks
    selectObject: "Table chunks"
    @inspectChunk: id
endfor

selectObject: "Table chunks"
Save as comma-separated file: tensusers$ + "classifier_evaluation/en/grid_search_output2/" + "e-" + network_type_schwa$ + "-" + context_frames_schwa$ + "_n-" + network_type_n$ + "-" + context_frames_n$ + "_N-" + network_type_N$ + "-" + context_frames_N$ + "_" + string$(kal_diff_weight) + "_" + string$(apply_penalty) + "_" + string$(n_prec_values) + "_" + string$(n_subs_values) + "_" + string$(n_smooths) + "_" + string$(take_sqrt) + "_" + string$(threshold_schwa) + "_" + string$(threshold_n) + "_" + string$(threshold_N) + "_" + string$(subtract_n) + ".csv"
