corpus$ = "cgn"

if corpus$ == "cgn"
    component$ = "a"
endif


if macintosh
    chunk_path$ = "/Volumes/tensusers/timzee/" + corpus$ + "/"
    if corpus$ == "IFADVcorpus"
        frag_path$ = "/Volumes/tensusers/timzee/af_classification/pred_textgrids/ifadv/"
    elsif corpus$ == "ECSD"
        frag_path$ = "/Volumes/tensusers/timzee/af_classification/pred_textgrids/ecsd/"
    elsif corpus$ == "cgn"
        frag_path$ = "/Volumes/tensusers/timzee/af_classification/pred_textgrids/cgn-" + component$ + "/"
    else
        frag_path$ = "/Volumes/tensusers/timzee/af_classification/pred_textgrids/" + corpus$ + "/"
    endif
else
    chunk_path$ = "/vol/tensusers/timzee/" + corpus$ + "/"
    if corpus$ == "IFADVcorpus"
        frag_path$ = "/vol/tensusers/timzee/af_classification/pred_textgrids/ifadv/"
        output_path$ = "/vol/tensusers/timzee/IFADVcorpus/man_annot/textgrids/"
    elsif corpus$ == "ECSD"
        frag_path$ = "/vol/tensusers/timzee/af_classification/pred_textgrids/ecsd/"
        output_path$ = "/vol/tensusers/timzee/ECSD/man_annot/textgrids/"
    elsif corpus$ == "cgn"
        frag_path$ = "/vol/tensusers/timzee/af_classification/pred_textgrids/cgn-" + component$ + "/"
        output_path$ = "/vol/tensusers/timzee/cgn/man_annot/comp-"
    else
        frag_path$ = "/vol/tensusers/timzee/af_classification/pred_textgrids/" + corpus$ + "/"
    endif
endif

frag_buffer = 0.2
# Make sure input file has a header
Read Table from comma-separated file: chunk_path$ + "fa_eval_a.csv"
Rename: "chunks"
Append column: "tim_start"
Append column: "tim_end"
Append column: "kal_start"
Append column: "kal_end"
Append column: "nn_start"
Append column: "nn_end"

procedure inspectChunk: annotateChunk.id
    selectObject: "Table chunks"
    filepath$ = Get value: annotateChunk.id, "wav"
    name_length = length(filepath$)
    Read from file: output_path$ + filepath$ + ".awd"
    s_name$ = selected$("TextGrid")
    selectObject: "Table chunks"
    w_ort$ = Get value: annotateChunk.id, "word_ort"
    c_start = Get value: annotateChunk.id, "chunk_start"
    c_end = Get value: annotateChunk.id, "chunk_end"
    c_tier = Get value: annotateChunk.id, "tier"
    c_channel$ = Get value: annotateChunk.id, "chan"
    c_speaker$ = Get value: annotateChunk.id, "speaker"
    word_chunk_i = Get value: annotateChunk.id, "word_chunk_i"
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
    appendInfoLine: "Asserting '" + int_lab$ + "' == '" + w_ort$ + "'"
    assert int_lab$ == w_ort$
    w_start = Get start time of interval: tier, word_int
    w_end = Get end time of interval: tier, word_int

    Extract one tier: tier
    Rename: "words"
    selectObject: "TextGrid " + s_name$
    Extract one tier: tier + 3
    Rename: "tim"
    Set tier name: 1, "tim"

    if corpus$ == "cgn"
        Read from file: chunk_path$ + "kaldi_annot/v2/comp-" + filepath$ + ".awd"
    elif corpus$ == "IFADVcorpus"
        Read from file: chunk_path$ + "kaldi_annot/v2/" + filepath$ + ".awd"
    else
        pair_length = name_length - 10
        pair_folder$ = "PP" + mid$(filepath$, 3, pair_length)
        Read from file: chunk_path$ + "kaldi_annot/v2/" + pair_folder$ + "/" + filepath$ + ".awd"
    endif

#    Read from file: kaldi_path$ + filepath$ + ".awd"
    sound_end = Get end time
    Rename: "kaldi_all"
    Extract one tier: tier + 3
    Rename: "kaldi"
    Set tier name: 1, "kaldi"
    removeObject: "TextGrid kaldi_all"
    selectObject: "TextGrid words"
    plusObject: "TextGrid tim"
    plusObject: "TextGrid kaldi"
    Merge
    Rename: s_name$ + "_merged"
    removeObject: "TextGrid words"
    removeObject: "TextGrid tim"
    removeObject: "TextGrid kaldi"

    s_int = Get low interval at time: 3, w_end - 0.002
    tim_start = Get start time of interval: 2, s_int
    tim_end = Get end time of interval: 2, s_int
    kal_start = Get start time of interval: 3, s_int
    kal_end = Get end time of interval: 3, s_int

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
    if corpus$ = "ECSD"
        frag_file$ = frag_path$ + s_name$ + "_S_" + c_channel$ + "_" + frag_start$ + "_" + fixed$(frag_end, 3) + "_s.IntensityTier"
    else
        frag_file$ = frag_path$ + s_name$ + "_" + c_channel$ + "_" + frag_start$ + "_" + fixed$(frag_end, 3) + "_s.IntensityTier"
    endif
    if fileReadable(frag_file$)
        Read from file: frag_file$
        p_name$ = selected$("IntensityTier")
        Copy: p_name$ + "_diff"
        diff_win = 3
        Formula: "if (col < (ncol - diff_win)) and (col > diff_win) then IntensityTier_'p_name$'[col + diff_win] - IntensityTier_'p_name$'[col - diff_win] else 0 endif"
        Down to TableOfReal
        To Table: "rowLabel"
        # only measure where kaldi thinks the /s/ is, so our p_max is not determined by other /s/s in the fragment
        Extract rows where: "number(self$[""Time (s)""]) > 'kal_start' and number(self$[""Time (s)""]) < 'kal_end'"
        p_max = Get maximum: "Intensity (dB)"
        p_min = Get minimum: "Intensity (dB)"
        removeObject: "TableOfReal " + p_name$ + "_diff"
        removeObject: "Table " + p_name$ + "_diff"
        removeObject: "Table " + p_name$ + "_diff_formula"
        removeObject: "IntensityTier " + p_name$ + "_diff"
        selectObject: "IntensityTier " + p_name$
        s_time = Get start time
        e_time = Get end time
        Create TextGrid: s_time, e_time, "starts ends", "starts ends"
        selectObject: "IntensityTier " + p_name$
        Copy: p_name$ + "_diff"
        diff_win = 3
        Formula: "if (col < (ncol - diff_win)) and (col > diff_win) then IntensityTier_'p_name$'[col + diff_win] - IntensityTier_'p_name$'[col - diff_win] else 0 endif"
        num_points = Get number of points
        for point from 2 to (num_points - 1)
            selectObject: "IntensityTier " + p_name$ + "_diff"
            diff_point = Get value at index: point
            if diff_point > p_max / 10
                diff_prev = Get value at index: point - 1
                diff_next = Get value at index: point + 1
                if (diff_prev < diff_point) and (diff_next < diff_point)
                    t_maxpoint = Get time from index: point
                    t_maxstrength = diff_point
                    if t_maxstrength > p_max
                        t_maxstrength = p_max
                    endif
                    selectObject: "TextGrid starts_ends"
                    Insert point: 1, t_maxpoint, string$(t_maxstrength)
                endif
            elif diff_point < p_min / 10
                diff_prev = Get value at index: point - 1
                diff_next = Get value at index: point + 1
                if (diff_prev > diff_point) and (diff_next > diff_point)
                    t_minpoint = Get time from index: point + 2
                    t_minstrength = abs(diff_point)
                    if t_minstrength > abs(p_min)
                        t_minstrength = abs(p_min)
                    endif
                    selectObject: "TextGrid starts_ends"
                    Insert point: 2, t_minpoint, string$(t_minstrength)
                endif
            endif
        endfor
        removeObject: "IntensityTier " + p_name$ + "_diff"
        selectObject: "TextGrid starts_ends"
        Extract one tier: 1
        Down to Table: "no", 6, "yes", "no"
        Insert column: 1, "diff"
        n_starts = Get number of rows
        for start_cand from 1 to n_starts
            start_t = Get value: start_cand, "tmin"
            start_diff = 1 - abs(kal_start - start_t) * 10
            Set numeric value: start_cand, "diff", start_diff
        endfor
        Append sum column: "diff", "text", "score"
        nn_start_score = Get maximum: "score"
        nn_start_score$ = fixed$(nn_start_score, 3)
        nn_start_i = Search column: "score", string$(nn_start_score)
        removeObject: "Table starts"
        removeObject: "TextGrid starts"
        selectObject: "TextGrid starts_ends"
        if nn_start_i == 0
            nn_start$ = "NA"
        else
            nn_start = Get time of point: 1, nn_start_i
            nn_start$ = fixed$(nn_start, 3)
        endif
        Extract one tier: 2
        wrong_end_points = Get low index from time: 1, nn_start
        for end_point from 1 to wrong_end_points
            Remove point: 1, 1
        endfor
        Down to Table: "no", 6, "yes", "no"
        Insert column: 1, "mod_strength"
        Insert column: 1, "diff"
        n_ends = Get number of rows
        prev_strength = 0
        cum_strength = 0
        penalty = 0
        prev_end = 0
        for end_cand from 1 to n_ends
            end_t = Get value: end_cand, "tmin"
            end_diff = 1 - abs(kal_end - end_t) * 10
            Set numeric value: end_cand, "diff", end_diff
            cur_strength = Get value: end_cand, "text"
            selectObject: "TextGrid starts_ends"
            if prev_end
                closest_start_i = Get low index from time: 1, prev_end
                closest_start = Get time of point: 1, closest_start_i
                if closest_start == nn_start
                    penalty = cum_strength * (prev_end - closest_start) * 2
                else
                    cum_strength = 0
                    penalty += prev_strength * (prev_end - closest_start) * 2
                endif
            else
                penalty = 0
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
        removeObject: "Table ends"
        selectObject: "TextGrid ends"
        if nn_end_i == 0
            nn_end$ = "NA"
        else
            nn_end = Get time of point: 1, nn_end_i
            nn_end$ = fixed$(nn_end, 3)
        endif
        Remove
        removeObject: "IntensityTier " + p_name$
        removeObject: "TextGrid starts_ends"
    else
        nn_start$ = "NA"
        nn_end$ = "NA"
    endif

    selectObject: "Table chunks"
    Set numeric value: annotateChunk.id, "tim_start", tim_start
    Set numeric value: annotateChunk.id, "tim_end", tim_end
    Set numeric value: annotateChunk.id, "kal_start", kal_start
    Set numeric value: annotateChunk.id, "kal_end", kal_end
    Set string value: annotateChunk.id, "nn_start", nn_start$
    Set string value: annotateChunk.id, "nn_end", nn_end$

    removeObject: "TextGrid " + s_name$
    removeObject: "TextGrid " + s_name$ + "_merged"
endproc


num_chunks = Get number of rows
for id from 1 to num_chunks
    selectObject: "Table chunks"
    @inspectChunk: id
endfor

selectObject: "Table chunks"
Save as comma-separated file: chunk_path$ + "eval_boundaries_a.csv"
