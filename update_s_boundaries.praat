par_set$ = "overall"

b2i["overall"] = 1
b2i["startb"] = 2
b2i["starte"] = 3
b2i["endb"] = 4
b2i["ende"] = 5
p2i["apply_penalty"] = 1
p2i["kal_diff_weight"] = 2
p2i["n_smooths"] = 3
p2i["take_sqrt"] = 4
p2i["n_prec_values"] = 5
p2i["n_subs_values"] = 6

parameters## = {{ 1, 0.5, 1, 1, 10, 10 }, { 0, 0.5, 1, 0, 10, 0 }, { 0, 0.5, 2, 0, 10, 0 }, { 0, 0.5, 0, 1, 10, 0 }, { 0, 0.5, 4, 1, 0, 0 }}

apply_penalty = parameters##[b2i[par_set$],p2i["apply_penalty"]]                ; 1
kal_diff_weight = parameters##[b2i[par_set$],p2i["kal_diff_weight"]]            ; 0.5
n_smooths = parameters##[b2i[par_set$],p2i["n_smooths"]]                        ; 1
take_sqrt = parameters##[b2i[par_set$],p2i["take_sqrt"]]                        ; 1
n_prec_values = parameters##[b2i[par_set$],p2i["n_prec_values"]]                ; 10
n_subs_values = parameters##[b2i[par_set$],p2i["n_subs_values"]]                ; 10
context_frames$ = "15"      ; "15"
network_type$ = "FFNN"      ; "FFNN"

if macintosh
    tensusers$ = "/Volumes/tensusers/timzee/"
    bigdata2$ = "/Volumes/bigdata2/"
else
    tensusers$ = "/vol/tensusers/timzee/"
    bigdata2$ = "/vol/bigdata2/"
endif

double_derivative = 1
frag_buffer = 0.2

# Make sure input file has a header
Read Table from comma-separated file: tensusers$ + "cgn/synvoirelPL_s_comb_comp-c_timbl2.csv"
input_name$ = selected$("Table")
Rename: "chunks"
# Append column: "TR_start_b"
# Append column: "TR_start_e"
# Append column: "TR_end_b"
# Append column: "TR_end_e"
# Append column: "TR_reduction"
# Append column: "TS_start_b"
# Append column: "TS_start_e"
# Append column: "TS_end_b"
# Append column: "TS_end_e"
# Append column: "TS_reduction"
if par_set$ == "overall"
    Append column: "nn_start_b"
    Append column: "nn_start_e"
    Append column: "nn_end_b"
    Append column: "nn_end_e"
endif
# Append column: "kal_b"
# Append column: "kal_e"
# Append column: "wrd_e"
# Append column: "nn_trace"

procedure inspectChunk: annotateChunk.id
    selectObject: "Table chunks"
    filepath$ = Get value: annotateChunk.id, "wav"
    name_length = length(filepath$)
    if left$(filepath$, 1) == "p"
        pair_length = name_length - 10
        pair_folder$ = "PP" + mid$(filepath$, 3, pair_length)
        Read from file: tensusers$ + "ECSD/kaldi_annot/v2/" + pair_folder$ + "/" + filepath$ + ".awd"
        Rename: filepath$
        corpus$ = "ecsd"
        cgn = 0
    elsif left$(filepath$, 1) == "D"
        Read from file: tensusers$ + "IFADVcorpus/kaldi_annot/v2/" + filepath$ + ".awd"
        corpus$ = "ifadv"
        cgn = 0
    else
        Read from file: tensusers$ + "cgn/kaldi_annot/v2/comp-" + filepath$ + ".awd"
        corpus$ = left$(filepath$, 1)
        cgn = 1
    endif
    s_name$ = selected$("TextGrid")
    sound_end = Get end time
    selectObject: "Table chunks"
    w_ort$ = Get value: annotateChunk.id, "word_ort"
    c_start = Get value: annotateChunk.id, "chunk_start"
    c_start$ = fixed$(c_start, 3)
    c_end = Get value: annotateChunk.id, "chunk_end"
    c_end$ = fixed$(c_end, 3)
    c_tier = Get value: annotateChunk.id, "tier"
    c_tier$ = string$(c_tier)
    c_channel$ = Get value: annotateChunk.id, "chan"
    c_speaker$ = Get value: annotateChunk.id, "speaker"
    word_chunk_i = Get value: annotateChunk.id, "word_chunk_i"
    word_chunk_i$ = string$(word_chunk_i)
#    tier = c_tier * 4 - 3
    selectObject: "TextGrid " + s_name$
    tier_name$ = ""
    tier = 0
    while tier_name$ != c_speaker$
        tier += 1
        tier_name$ = Get tier name: tier
    endwhile
    speaker$ = Get tier name: tier
    assert c_speaker$ == speaker$
    word_int = Get high interval at time: tier, c_start
    word_int -= 1
    word_counter = 0
    while word_counter != word_chunk_i
        word_int += 1
        int_lab$ = Get label of interval: tier, word_int
        if int_lab$ != ""
            word_counter += 1
        endif
    endwhile
    int_lab$ = replace_regex$(int_lab$, "[.?! ]", "", 0)
    int_lab$ = replace_regex$(int_lab$, "\*[a-z]", "", 0)
#    appendInfoLine: "Asserting '" + int_lab$ + "' == '" + w_ort$ + "'"
    assert int_lab$ == w_ort$
    w_start = Get start time of interval: tier, word_int
    w_end = Get end time of interval: tier, word_int

    s_int = Get low interval at time: tier + 3, w_end - 0.002
    kal_start = Get start time of interval: tier + 3, s_int
    kal_end = Get end time of interval: tier + 3, s_int
    Rename: s_name$ + "_kal"


    # Get neural network boundaries
    frag_start = kal_start - frag_buffer
    frag_start$ = fixed$(frag_start, 3)
    frag_end = kal_end + frag_buffer
    if frag_start < 0
        frag_start = 0.000
        frag_start$ = "0.000"
    endif
    if frag_end > sound_end
        frag_end = sound_end
    endif
    if length(corpus$) == 1
        corp_fol$ = "cgn-" + corpus$
    else
        corp_fol$ = corpus$
    endif
    frag_path$ = tensusers$ + "af_classification/pred_textgrids_keras/" + corp_fol$ + "/" + network_type$ + "_" + context_frames$ + "/"
    if corpus$ = "ecsd"
        frag_file$ = frag_path$ + s_name$ + "_S_" + c_channel$ + "_" + frag_start$ + "_" + fixed$(frag_end, 3) + "_s.IntensityTier"
    else
        frag_file$ = frag_path$ + s_name$ + "_" + c_channel$ + "_" + frag_start$ + "_" + fixed$(frag_end, 3) + "_s.IntensityTier"
    endif
    if fileReadable(frag_file$)
        Read from file: frag_file$
        p_name$ = selected$("IntensityTier")
        # only measure where kaldi thinks the /s/ is, so our p_max is not determined by other /s/s in the fragment
#        Extract rows where: "number(self$[""Time (s)""]) > 'kal_start' and number(self$[""Time (s)""]) < 'kal_end'"
#        removeObject: "Table " + p_name$ + "_formula"
        selectObject: "IntensityTier " + p_name$
        s_time = Get start time
        e_time = Get end time
        Create TextGrid: s_time, e_time, "starts ends", "starts ends"
        selectObject: "IntensityTier " + p_name$
        # square root step
        if take_sqrt
            Formula: "sqrt(self)"
        endif
        # smoothing step
        # smooth window = 3
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
            nn_start_b$ = "NA"
            nn_start_e$ = "NA"
        else
            if double_derivative
                nn_start = Get value: nn_start_i, "tmin"
                nn_start$ = fixed$(nn_start, 3)
                nn_start_b = Get value: nn_start_i, "tmin2"
                if nn_start_b == undefined
                    nn_start_b$ = "NA"
                    appendInfoLine: "ERROR: could not find nn_start_b for " + filepath$ + "," + c_channel$ + "," + c_start$ + "," + c_end$
                else
                    nn_start_b$ = fixed$(nn_start_b, 3)
                    nn_start_index = Get value: nn_start_i, "tmin2_i"
                endif
                nn_start_e = Get value: nn_start_i, "tmin3"
                if nn_start_e == undefined
                    nn_start_e$ = "NA"
                    appendInfoLine: "ERROR: could not find nn_start_e for " + filepath$ + "," + c_channel$ + "," + c_start$ + "," + c_end$
#                    exitScript()
                else
                    nn_start_e$ = fixed$(nn_start_e, 3)
                endif
            else
                nn_start = Get value: nn_start_i, "tmin"
                nn_start$ = fixed$(nn_start, 3)
                nn_start_e$ = "NA"
                nn_start_b$ = "NA"
            endif
        endif
        removeObject: "Table starts"
        removeObject: "TextGrid starts"
        selectObject: "TextGrid starts_ends"
        Extract one tier: 2
        if nn_start_b$ == "NA"
            wrong_end_points = Get low index from time: 1, kal_start
        else
            wrong_end_points = Get low index from time: 1, nn_start_b
        endif
        for end_point from 1 to wrong_end_points
            Remove point: 1, 1
        endfor
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
            if prev_end and apply_penalty
                closest_start_i = Get low index from time: 1, prev_end
                closest_start = Get time of point: 1, closest_start_i
                if closest_start == nn_start_b
                    penalty = cum_strength * (prev_end - closest_start) * 2
                else
                    cum_strength = 0
                    penalty += prev_strength * (prev_end - closest_start) * 2
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
            nn_end$ = "NA"
            nn_end_e$ = "NA"
            nn_end_b$ = "NA"
        else
            if double_derivative
                nn_end = Get value: nn_end_i, "tmin"
                nn_end$ = fixed$(nn_end, 3)
                nn_end_e = Get value: nn_end_i, "tmin2"
                if nn_end_e == undefined
                    nn_end_e$ = "NA"
                    appendInfoLine: "ERROR: could not find nn_end_e for " + filepath$ + "," + c_channel$ + "," + c_start$ + "," + c_end$
#                    exitScript()
                else
                    nn_end_e$ = fixed$(nn_end_e, 3)
                    nn_end_index = Get value: nn_end_i, "tmin2_i"
                endif
                nn_end_b = Get value: nn_end_i, "tmin3"
                if nn_end_b == undefined
                    nn_end_b$ = "NA"
                    appendInfoLine: "ERROR: could not find nn_end_b for " + filepath$ + "," + c_channel$ + "," + c_start$ + "," + c_end$
#                    exitScript()
                else
                    nn_end_b$ = fixed$(nn_end_b, 3)
                endif
            else
                nn_end = Get value: nn_end_i, "tmin"
                nn_end$ = fixed$(nn_end, 3)
                nn_end_b$ = "NA"
                nn_end_e$ = "NA"
            endif
        endif
        # if (nn_start_index != undefined) and (nn_end_index != undefined)
        #     trace_range$ = string$(nn_start_index) + ":" + string$(nn_end_index)
        #     selectObject: "IntensityTier " + p_name$
        #     Down to TableOfReal
        #     Extract row ranges: trace_range$
        #     To Table: "rowLabel"
        #     Remove column: "Time (s)"
        #     Remove column: "rowLabel"
        #     bla$ = List: "no"
        #     bla2$ = replace_regex$(bla$,"[^0-9]+\n", "c(", 1)
        #     bla3$ = replace_regex$(bla2$,"\n$", ")",0)
        #     nn_trace$ = replace_regex$(bla3$,"\n", ",",0)
        #     removeObject: "TableOfReal " + p_name$
        #     removeObject: "TableOfReal " + p_name$ + "_rows"
        #     removeObject: "Table " + p_name$ + "_rows"
        # else
        #     nn_trace$ = "NA"
        # endif
        removeObject: "Table ends"
        removeObject: "TextGrid ends"
        removeObject: "IntensityTier " + p_name$
        removeObject: "IntensityTier " + p_name$ + "_diff"
        removeObject: "IntensityTier " + p_name$ + "_diff_diff"
        removeObject: "TextGrid starts_ends"
    else
        nn_start$ = "NA"
        nn_start_b$ = "NA"
        nn_start_e$ = "NA"
        nn_end$ = "NA"
        nn_end_b$ = "NA"
        nn_end_e$ = "NA"
        # nn_trace$ = "NA"
    endif

    if nn_start$ != "NA"
        base_dur$ = string$(nn_start - w_start)
    else
        base_dur$ = "NA"
    endif

    removeObject: "TextGrid " + s_name$ + "_kal"

    selectObject: "Table chunks"
    if par_set$ == "overall"
        Set numeric value: annotateChunk.id, "s_dur", kal_end - kal_start
        Set numeric value: annotateChunk.id, "kal_start", kal_start
        Set numeric value: annotateChunk.id, "kal_end", kal_end
        Set string value: annotateChunk.id, "nn_start", nn_start$
        Set string value: annotateChunk.id, "nn_start_b", nn_start_b$
        Set string value: annotateChunk.id, "nn_start_e", nn_start_e$
        Set string value: annotateChunk.id, "nn_end_b", nn_end_b$
        Set string value: annotateChunk.id, "nn_end_e", nn_end_e$
        Set string value: annotateChunk.id, "nn_end", nn_end$
        Set string value: annotateChunk.id, "nn_start_score", nn_start_score$
        Set string value: annotateChunk.id, "nn_end_score", nn_end_score$
        Set string value: annotateChunk.id, "base_dur", base_dur$
        # Set string value: annotateChunk.id, "nn_trace", nn_trace$
    elsif par_set$ == "startb"
        Set string value: annotateChunk.id, "nn_start_b", nn_start_b$
    elsif par_set$ == "starte"
        Set string value: annotateChunk.id, "nn_start_e", nn_start_e$
    elsif par_set$ == "endb"
        Set string value: annotateChunk.id, "nn_end_b", nn_end_b$
        Set string value: annotateChunk.id, "base_dur", base_dur$
    elsif par_set$ == "ende"
        Set string value: annotateChunk.id, "nn_end_e", nn_end_e$
    endif
endproc


num_chunks = Get number of rows
for id from 1 to num_chunks
    selectObject: "Table chunks"
    @inspectChunk: id
endfor

selectObject: "Table chunks"
if corpus$ == "ecsd"
    output_fol$ = "ECSD/"
elsif corpus$ == "ifadv"
    output_fol$ = "IFADVcorpus/"
else
    output_fol$ = "cgn/"
endif
Save as comma-separated file: tensusers$ + output_fol$ + input_name$ + "_" + par_set$ + ".csv"
