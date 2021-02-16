if macintosh
    tensusers$ = "/Volumes/tensusers/timzee/"
    bigdata2$ = "/Volumes/bigdata2/"
else
    tensusers$ = "/vol/tensusers/timzee/"
    bigdata2$ = "/vol/bigdata2/"
endif

annotators$[1] = "TR"
annotators$[2] = "TS"
double_derivative = 1
frag_buffer = 0.2
# chunk_buffer = 0.3
# Make sure input file has a header
Read Table from comma-separated file: tensusers$ + "classifier_evaluation/s/" + "nn_eval20b.csv"
Rename: "chunks"
Append column: "TR_start_b"
Append column: "TR_start_e"
Append column: "TR_end_b"
Append column: "TR_end_e"
Append column: "TR_reduction"
Append column: "TS_start_b"
Append column: "TS_start_e"
Append column: "TS_end_b"
Append column: "TS_end_e"
Append column: "TS_reduction"
Append column: "nn_start_b"
Append column: "nn_start_e"
Append column: "nn_end_b"
Append column: "nn_end_e"
Append column: "kal_b"
Append column: "kal_e"
Append column: "wrd_e"

procedure getSegments: .int_lab$
    .num_phons = 0
    .num_letters = length(.int_lab$)
    .phon$ = ""
    for .let from 1 to .num_letters
        .let$ = mid$(.int_lab$, .let, 1)
        if .let$ != "+" and .let$ != "~" and .let$ != ":" and .let$ != "]"
            if .phon$ != ""
                .phons$[.num_phons] = .phon$
            endif
            .num_phons += 1
            .phon$ = .let$
        else
            .phon$ = .phon$ + .let$
            .phons$[.num_phons] = .phon$
            .phon$ = ""
        endif
        if .let == .num_letters and .phon$ != ""
            .phons$[.num_phons] = .phon$
        endif
    endfor
endproc

procedure getWordArray: .c_ort$
    .words$ = .c_ort$ + " "
    .space_index = -1
    .num_words = 0
    while .space_index != 0
        .num_words += 1
        .space_index = index(.words$, " ")
        .word$ = left$(.words$, .space_index - 1)
        .words$ = right$(.words$, length(.words$) - .space_index)
        if .word$ != ""
            .word_array$[.num_words] = .word$
        endif
    endwhile
    .num_words -= 1
endproc

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
    c_ort$ = Get value: annotateChunk.id, "ort"
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

    # get wrd boundaries
    if cgn == 1 and corpus$ != "d"
        wrd_path$ = bigdata2$ + "corpora2/CGN2/data/annot/text/wrd/comp-"
        runSystem_nocheck: "cp -f " + wrd_path$ + filepath$ + ".wrd.gz " + tensusers$ + "cgn/cgn_annot/"
        runSystem_nocheck: "gunzip -f " + tensusers$ + "cgn/cgn_annot/" + s_name$ + ".wrd.gz"
        Read from file: tensusers$ + "cgn/cgn_annot/" + s_name$ + ".wrd"
        runSystem_nocheck: "rm -f " + tensusers$ + "cgn/cgn_annot/" + s_name$ + ".wrd"
        Rename: s_name$ + "_wrd"
        selectObject: "TextGrid " + s_name$ + "_wrd"
        tier_name$ = Get tier name: c_tier * 2 - 1
        Extract one tier: c_tier * 2
        Rename: "tier"
        Extract part: c_start, c_end, "yes"
        Rename: "chunk"
        removeObject: "TextGrid tier"
        num_ints = Get number of intervals: 1
        # handle '=' and '-'
        for int from 1 to num_ints
            int_lab$ = Get label of interval: 1, int
            int_lab$ = replace$(int_lab$, "=", "", 0)
            if int < num_ints
                next_lab$ = Get label of interval: 1, int + 1
                if index(next_lab$, "-")
                    int_lab$ = replace_regex$(int_lab$, "-.$", "", 0)
                endif
            endif
            int_lab$ = replace_regex$(int_lab$, "^.-", "", 0)
            int_lab$ = replace_regex$(int_lab$, "#", "\[SPN\]", 0)
            Set interval text: 1, int, int_lab$
        endfor
        # handle '_'
        int = 0
        while int < num_ints
            int += 1
            int_lab$ = Get label of interval: 1, int
            int_start = Get start time of interval: 1, int
            int_end = Get end time of interval: 1, int
            int_dur = int_end - int_start
            if left$(int_lab$, 1) == "_" or right$(int_lab$, 1) == "_"
                if int_lab$ == "_" or left$(int_lab$, 1) == "_"
                    prev_lab$ = Get label of interval: 1, int - 1
                else
                    prev_lab$ = left$(int_lab$, length(int_lab$) - 1)
                endif
                @getSegments: prev_lab$
                prev_n = getSegments.num_phons
                for i from 1 to prev_n
                    prev_phons$[i] = getSegments.phons$[i]
                endfor
                if int_lab$ == "_" or right$(int_lab$, 1) == "_"
                    next_lab$ = Get label of interval: 1, int + 1
                else
                    next_lab$ = right$(int_lab$, length(int_lab$) - 1)
                endif
                @getSegments: next_lab$
                next_n = getSegments.num_phons
                for i from 1 to next_n
                    next_phons$[i] = getSegments.phons$[i]
                endfor
                shared_n = 0
                pre_segs$ = ""
                post_segs$ = "bla"
                while pre_segs$ != post_segs$
                    shared_n += 1
                    pre_segs$ = ""
                    for i from prev_n - (shared_n - 1) to prev_n
                        pre_segs$ = pre_segs$ + prev_phons$[i]
                    endfor
                    post_segs$ = ""
                    for i from 1 to shared_n
                        post_segs$ = post_segs$ + next_phons$[i]
                    endfor
    #                appendInfoLine: pre_segs$, " ", post_segs$, " ", f_path$, " ", c_start$, " ", c_end$
                endwhile
                if int_lab$ == "_"
                    if prev_n == shared_n
                        Set interval text: 1, int, ""
                        new_boundary = int_start + int_dur / 2
                        Insert boundary: 1, new_boundary
                        Remove left boundary: 1, int
                        Remove right boundary: 1, int
                    else
                        Set interval text: 1, int - 1, left$(prev_lab$, length(prev_lab$) - length(pre_segs$))
                        Set interval text: 1, int, ""
                        Remove right boundary: 1, int
                    endif
                elif left$(int_lab$, 1) == "_"
                    Set interval text: 1, int - 1, left$(prev_lab$, length(prev_lab$) - length(pre_segs$))
                    Set interval text: 1, int, next_lab$
                elif right$(int_lab$, 1) == "_"
                    Set interval text: 1, int, prev_lab$
                    new_right_boundary = int_end - (shared_n / prev_n) * 0.5 * int_dur
                    Insert boundary: 1, new_right_boundary
                    Remove right boundary: 1, int + 1
                endif
            endif
            num_ints = Get number of intervals: 1
        endwhile
        selectObject: "TextGrid " + s_name$ + "_kal"
        num_tiers = Get number of tiers
        kal_tier_name$ = ""
        t = 0
        while kal_tier_name$ != tier_name$
            t += 1
            kal_tier_name$ = Get tier name: t
        endwhile
        Extract one tier: t
        Rename: "ort_tier"
        Extract part: c_start, c_end, "yes"
        Rename: "ort_chunk"
        removeObject: "TextGrid ort_tier"
        selectObject: "TextGrid " + s_name$ + "_kal"
        Extract one tier: t + 2
        Rename: "tran_tier"
        Extract part: c_start, c_end, "yes"
        Rename: "tran_chunk"
        removeObject: "TextGrid tran_tier"
        selectObject: "TextGrid " + s_name$ + "_kal"
        Extract one tier: t + 3
        Rename: "phon_tier"
        Extract part: c_start, c_end, "yes"
        Rename: "phon_chunk"
        removeObject: "TextGrid phon_tier"
        selectObject: "TextGrid chunk"
        plusObject: "TextGrid ort_chunk"
        plusObject: "TextGrid tran_chunk"
        plusObject: "TextGrid phon_chunk"
        Merge
        Rename: "combined"
        removeObject: "TextGrid chunk"
        removeObject: "TextGrid ort_chunk"
        removeObject: "TextGrid tran_chunk"
        removeObject: "TextGrid phon_chunk"
        @getWordArray: c_ort$
        num_words = getWordArray.num_words
        for i from 1 to num_words
            word_array$[i] = getWordArray.word_array$[i]
        endfor
        wrd_i = 0
        kal_i = 0
        num_wrd_i = Get number of intervals: 1
        num_kal_i = Get number of intervals: 2
        for word_i from 1 to num_words + 1
            selectObject: "TextGrid combined"
            # get wrd info first
            wrd_tran$ = ""
            insert_wrd_sil = 0
            # second part of while statement is because we want to include trailing silences
            # by measuring sil start here subsequent silences (in case of leading/trailing sil) are treated as 1
            if wrd_i < num_wrd_i
                wrd_sil_start = Get start time of interval: 1, wrd_i + 1
            endif
            while (wrd_tran$ == "") and (wrd_i < num_wrd_i)
                wrd_i += 1
                wrd_tran$ = Get label of interval: 1, wrd_i
                if wrd_tran$ == ""
                    insert_wrd_sil = 1
                    wrd_sil_end = Get end time of interval: 1, wrd_i
                endif
            endwhile
            # get kal info
            kal_ort$ = ""
            insert_kal_sil = 0
            if kal_i < num_kal_i
                kal_sil_start = Get start time of interval: 2, kal_i + 1
            endif
            while (kal_ort$ == "") and (kal_i < num_kal_i)
                kal_i += 1
                kal_ort$ = Get label of interval: 2, kal_i
                if kal_ort$ == ""
                    insert_kal_sil = 1
                    kal_sil_end = Get end time of interval: 2, kal_i
                endif
            endwhile
            if word_i == word_chunk_i
                selectObject: "TextGrid combined"
                man_end = Get end time of interval: 1, wrd_i
                man_end$ = fixed$(man_end, 5)
                kal_word$ = Get label of interval: 2, kal_i
#                appendInfoLine: "Asserting '" + kal_word$ + "' == '" + w_ort$ + "'"
                assert replace_regex$(kal_word$, "[.?]", "", 0) == w_ort$
            endif
        endfor
        removeObject: "TextGrid combined"
        removeObject: "TextGrid " + s_name$ + "_kal"
        removeObject: "TextGrid " + s_name$ + "_wrd"
    else
        man_end$ = "NA"
    endif

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
    frag_path$ = tensusers$ + "af_classification/pred_textgrids_keras/af_eval_s/"
    if corpus$ = "ecsd"
        frag_file$ = frag_path$ + s_name$ + "_S_" + c_channel$ + "_" + frag_start$ + "_" + fixed$(frag_end, 3) + "_s.IntensityTier"
    else
        frag_file$ = frag_path$ + s_name$ + "_" + c_channel$ + "_" + frag_start$ + "_" + fixed$(frag_end, 3) + "_s.IntensityTier"
    endif
    if fileReadable(frag_file$)
        Read from file: frag_file$
        p_name$ = selected$("IntensityTier")
        Down to TableOfReal
        To Table: "rowLabel"
        # only measure where kaldi thinks the /s/ is, so our p_max is not determined by other /s/s in the fragment
        Extract rows where: "number(self$[""Time (s)""]) > 'kal_start' and number(self$[""Time (s)""]) < 'kal_end'"
        p_max = Get maximum: "Intensity (dB)"
        p_max = p_max / 2
        p_min = -p_max
        removeObject: "TableOfReal " + p_name$
        removeObject: "Table " + p_name$
        removeObject: "Table " + p_name$ + "_formula"
#        removeObject: "IntensityTier " + p_name$ + "_diff"
        selectObject: "IntensityTier " + p_name$
        s_time = Get start time
        e_time = Get end time
        Create TextGrid: s_time, e_time, "starts ends", "starts ends"
        selectObject: "IntensityTier " + p_name$
        if double_derivative
            Copy: p_name$ + "_diff_diff"
            Formula: "if (col < (ncol - 3)) and (col > 3) then (IntensityTier_'p_name$'[col + 3] - 2 * IntensityTier_'p_name$'[col] + IntensityTier_'p_name$'[col - 3]) else 0 endif"
        endif
        selectObject: "IntensityTier " + p_name$
        Copy: p_name$ + "_diff"
        # time derivative measured in time steps (which corresponds to 5 ms)
        Formula: "if (col < (ncol - 1)) and (col > 1) then (IntensityTier_'p_name$'[col + 1] - IntensityTier_'p_name$'[col - 1]) else 0 endif"
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
                    t_minpoint = Get time from index: point
                    t_minstrength = abs(diff_point)
                    if t_minstrength > abs(p_min)
                        t_minstrength = abs(p_min)
                    endif
                    selectObject: "TextGrid starts_ends"
                    Insert point: 2, t_minpoint, string$(t_minstrength)
                endif
            endif
        endfor
#        removeObject: "IntensityTier " + p_name$ + "_diff"
        selectObject: "TextGrid starts_ends"
        Extract one tier: 1
        Down to Table: "no", 6, "yes", "no"
        Insert column: 1, "diff"
        Insert column: 1, "tmin2"
        Insert column: 1, "tmin3"
        n_starts = Get number of rows
        for start_cand from 1 to n_starts
            start_t = Get value: start_cand, "tmin"
            if double_derivative
                selectObject: "IntensityTier " + p_name$ + "_diff_diff"
                dd_i = Get nearest index from time: start_t
                dd_i_stop = dd_i - 5
                repeat
                    dd_i = dd_i - 1
                    dd_cur = Get value at index: dd_i
                    if dd_cur > p_max / 10
                        dd_prev = Get value at index: dd_i - 1
                        dd_next = Get value at index: dd_i + 1
                        if (dd_prev < dd_cur) and (dd_next < dd_cur)
                            dd_time = Get time from index: dd_i
                            selectObject: "Table starts"
                            Set numeric value: start_cand, "tmin2", dd_time
                            goto FOUND_start_b
                        endif
                    endif
                until dd_i == dd_i_stop
                label FOUND_start_b
                selectObject: "IntensityTier " + p_name$ + "_diff_diff"
                dd_i_start = Get nearest index from time: start_t
                dd_i_stop = dd_i + 5
                for dd_i from dd_i_start to dd_i_stop
                    dd_cur = Get value at index: dd_i
                    if dd_cur < p_min / 10
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
            start_diff = 1 - abs(kal_start - start_t) * 10
            Set numeric value: start_cand, "diff", start_diff
        endfor
        Append sum column: "diff", "text", "score"
        nn_start_score = Get maximum: "score"
        nn_start_score$ = fixed$(nn_start_score, 3)
        nn_start_i = Search column: "score", string$(nn_start_score)
        if nn_start_i == 0
            nn_start$ = "NA"
            nn_start_e$ = "NA"
        else
            if double_derivative
                nn_start = Get value: nn_start_i, "tmin2"
                if nn_start == undefined
                    nn_start$ = "NA"
                else
                    nn_start$ = fixed$(nn_start, 3)
                endif
                nn_start_e = Get value: nn_start_i, "tmin3"
                if nn_start_e == undefined
                    nn_start_e$ = "NA"
                else
                    nn_start_e$ = fixed$(nn_start_e, 3)
                endif
            else
                nn_start = Get value: nn_start_i, "tmin"
                nn_start$ = fixed$(nn_start, 3)
                nn_start_e$ = "NA"
            endif
        endif
        removeObject: "Table starts"
        removeObject: "TextGrid starts"
        selectObject: "TextGrid starts_ends"
        Extract one tier: 2
        if nn_start$ == "NA"
            wrong_end_points = Get low index from time: 1, kal_start
        else
            wrong_end_points = Get low index from time: 1, nn_start
        endif
        for end_point from 1 to wrong_end_points
            Remove point: 1, 1
        endfor
        Down to Table: "no", 6, "yes", "no"
        Insert column: 1, "mod_strength"
        Insert column: 1, "diff"
        Insert column: 1, "tmin2"
        Insert column: 1, "tmin3"
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
            if double_derivative
                selectObject: "IntensityTier " + p_name$ + "_diff_diff"
                dd_i_start = Get nearest index from time: end_t
                dd_i_stop = dd_i_start + 5
                for dd_i from dd_i_start to dd_i_stop
                    dd_cur = Get value at index: dd_i
                    if dd_cur > p_max / 10
                        dd_prev = Get value at index: dd_i - 1
                        dd_next = Get value at index: dd_i + 1
                        if (dd_prev < dd_cur) and (dd_next < dd_cur)
                            dd_time = Get time from index: dd_i
                            selectObject: "Table ends"
                            Set numeric value: end_cand, "tmin2", dd_time
                            goto FOUND_end_e
                        endif
                    endif
                endfor
                label FOUND_end_e
                selectObject: "IntensityTier " + p_name$ + "_diff_diff"
                dd_i = Get nearest index from time: end_t
                dd_i_stop = dd_i_start - 5
                repeat
                    dd_i = dd_i - 1
                    dd_cur = Get value at index: dd_i
                    if dd_cur < p_min / 10
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
        if nn_end_i == 0
            nn_end$ = "NA"
            nn_end_b$ = "NA"
        else
            if double_derivative
                nn_end = Get value: nn_end_i, "tmin2"
                if nn_end == undefined
                    nn_end$ = "NA"
                else
                    nn_end$ = fixed$(nn_end, 3)
                endif
                nn_end_b = Get value: nn_end_i, "tmin3"
                if nn_end_b == undefined
                    nn_end_b$ = "NA"
                else
                    nn_end_b$ = fixed$(nn_end_b, 3)
                endif
            else
                nn_end = Get value: nn_end_i, "tmin"
                nn_end$ = fixed$(nn_end, 3)
                nn_end_b$ = "NA"
            endif
        endif
        removeObject: "Table ends"
        removeObject: "TextGrid ends"
        removeObject: "IntensityTier " + p_name$
        removeObject: "IntensityTier " + p_name$ + "_diff"
#        removeObject: "IntensityTier " + p_name$ + "_diff_diff"
        removeObject: "TextGrid starts_ends"
    else
        nn_start$ = "NA"
        nn_start_e$ = "NA"
        nn_end$ = "NA"
        nn_end_b$ = "NA"
    endif

    # get annotator boundaries
    annot_chunk_name$ = replace$(filepath$, "/", "_", 0) + "_" + c_channel$ + "_" + c_start$ + "_" + c_end$ + "_" + c_tier$ + "_" + word_chunk_i$ + ".TextGrid"
    for annotator_i from 1 to 2
        annot_chunk_path$ = tensusers$ + "classifier_evaluation/s/man_annot/" + annotators$[annotator_i] + "_copy/" + corpus$ + "/" + annot_chunk_name$
        if fileReadable(annot_chunk_path$)
            Read from file: annot_chunk_path$
            annot_chunk$ = selected$("TextGrid")
            n_boundary_ints = Get number of intervals: 2
            an_start_b = Get start time of interval: 2, 2
            an_start_b$ = string$(an_start_b)
            an_start_e = Get end time of interval: 2, 2
            an_start_e$ = string$(an_start_e)
            if n_boundary_ints == 5
                an_end_b = Get start time of interval: 2, 4
                an_end_e = Get end time of interval: 2, 4
            elsif n_boundary_ints == 4
                an_end_b = Get start time of interval: 2, 3
                an_end_e = Get end time of interval: 2, 3
            endif
            an_end_b$ = string$(an_end_b)
            an_end_e$ = string$(an_end_e)
            an_reduction$ = Get label of interval: 3, 1
            removeObject: "TextGrid " + annot_chunk$
        else
            appendInfoLine: annot_chunk_name$ + " not readable"
            an_start_b$ = "NA"
            an_start_e$ = "NA"
            an_end_b$ = "NA"
            an_end_e$ = "NA"
            an_reduction$ = "NA"
        endif
        selectObject: "Table chunks"
        Set string value: annotateChunk.id, annotators$[annotator_i] + "_start_b", an_start_b$
        Set string value: annotateChunk.id, annotators$[annotator_i] + "_start_e", an_start_e$
        Set string value: annotateChunk.id, annotators$[annotator_i] + "_end_b", an_end_b$
        Set string value: annotateChunk.id, annotators$[annotator_i] + "_end_e", an_end_e$
        Set string value: annotateChunk.id, annotators$[annotator_i] + "_reduction", an_reduction$
    endfor

    selectObject: "Table chunks"
    Set numeric value: annotateChunk.id, "kal_b", kal_start
    Set numeric value: annotateChunk.id, "kal_e", kal_end
    Set string value: annotateChunk.id, "nn_start_b", nn_start$
    Set string value: annotateChunk.id, "nn_start_e", nn_start_e$
    Set string value: annotateChunk.id, "nn_end_b", nn_end_b$
    Set string value: annotateChunk.id, "nn_end_e", nn_end$
    Set string value: annotateChunk.id, "wrd_e", man_end$
endproc


num_chunks = Get number of rows
for id from 1 to num_chunks
    selectObject: "Table chunks"
    @inspectChunk: id
endfor

selectObject: "Table chunks"
Save as comma-separated file: tensusers$ + "classifier_evaluation/s/" + "s_eval_results.csv"
