if macintosh
    tensusers$ = "/Volumes/tensusers/timzee/"
    bigdata2$ = "/Volumes/bigdata2/"
else
    tensusers$ = "/vol/tensusers/timzee/"
    bigdata2$ = "/vol/bigdata2/"
endif

n_annotators = 2
annotators$[1] = "TZ"
annotators$[2] = "MW"

segments$[1] = "e"
segments$[2] = "n"

# Make sure input file has a header
Read Table from comma-separated file: tensusers$ + "classifier_evaluation/en/" + "nn_eval_en_o3.csv"
Rename: "chunks"
for a_i from 1 to n_annotators
    a$ = annotators$[a_i]
    for s_i from 1 to 2
        s$ = segments$[s_i]
        Append column: a$ + "_" + s$ + "_start_b"
        Append column: a$ + "_" + s$ + "_start_e"
        Append column: a$ + "_" + s$ + "_end_b"
        Append column: a$ + "_" + s$ + "_end_e"
        Append column: a$ + "_" + s$ + "_reduction"
    endfor
    Append column: a$ + "_N_b"
    Append column: a$ + "_N_e"
endfor

Append column: "kal_e_b"
Append column: "kal_e_e"
Append column: "kal_n_b"
Append column: "kal_n_e"
Append column: "wrd_e"
Append column: "kal_start"
Append column: "kal_end"
Append column: "sound_end"

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
        Open long sound file: tensusers$ + "ECSD/Speech/" + pair_folder$ + "/" + filepath$ + "_S.wav"
        Rename: filepath$
        Read from file: tensusers$ + "ECSD/kaldi_annot/v3/" + pair_folder$ + "/" + filepath$ + ".awd"
        Rename: filepath$
        corpus$ = "ecsd"
        cgn = 0
    elsif left$(filepath$, 1) == "D"
        Open long sound file: tensusers$ + "IFADVcorpus/Speech/" + filepath$ + ".wav"
        Read from file: tensusers$ + "IFADVcorpus/kaldi_annot/v3/" + filepath$ + ".awd"
        corpus$ = "ifadv"
        cgn = 0
    else
        Open long sound file: bigdata2$ + "corpora2/CGN2/data/audio/wav/comp-" + filepath$ + ".wav"
        Read from file: tensusers$ + "cgn/kaldi_annot/v3/comp-" + filepath$ + ".awd"
        corpus$ = left$(filepath$, 1)
        cgn = 1
    endif
    s_name$ = selected$("TextGrid")
    selectObject: "LongSound " + s_name$
    sound_end = Get end time
    sound_end$ = string$(sound_end)
    Remove
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
    kal_start$ = string$(kal_start)
    kal_end$ = string$(w_end)
    s_lab$ = Get label of interval: tier + 3, s_int
    if s_lab$ == "n"
        kal_n_b = Get start time of interval: tier + 3, s_int
        kal_n_e = Get end time of interval: tier + 3, s_int
        kal_n_b$ = string$(kal_n_b)
        kal_n_e$ = string$(kal_n_e)
        s_prev_lab$ = Get label of interval: tier + 3, s_int - 1
        if s_prev_lab$ == "@"
            kal_schwa_b = Get start time of interval: tier + 3, s_int - 1
            kal_schwa_e = Get end time of interval: tier + 3, s_int - 1
            kal_schwa_b$ = string$(kal_schwa_b)
            kal_schwa_e$ = string$(kal_schwa_e)
        else
            kal_schwa_b$ = "NA"
            kal_schwa_e$ = "NA"
        endif
    elsif s_lab$ == "@"
        kal_schwa_b = Get start time of interval: tier + 3, s_int
        kal_schwa_e = Get end time of interval: tier + 3, s_int
        kal_schwa_b$ = string$(kal_schwa_b)
        kal_schwa_e$ = string$(kal_schwa_e)
        kal_n_b$ = "NA"
        kal_n_e$ = "NA"
    else
        kal_n_b$ = "NA"
        kal_n_e$ = "NA"
        kal_schwa_b$ = "NA"
        kal_schwa_e$ = "NA"
    endif
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


    # get annotator boundaries
    annot_chunk_name$ = replace$(filepath$, "/", "_", 0) + "_" + c_channel$ + "_" + c_start$ + "_" + c_end$ + "_" + c_tier$ + "_" + word_chunk_i$ + ".TextGrid"
    for annotator_i from 1 to n_annotators
        a$ = annotators$[annotator_i]
        annot_chunk_path$ = tensusers$ + "classifier_evaluation/en/man_annot/" + a$ + "/" + corpus$ + "/" + annot_chunk_name$
        if fileReadable(annot_chunk_path$)
            Read from file: annot_chunk_path$
            annot_chunk$ = selected$("TextGrid")
            for s_i from 1 to 2
                selectObject: "TextGrid " + annot_chunk$
                s$ = segments$[s_i]
                an_reduction$ = Get label of interval: 4 + s_i, 1
                if an_reduction$ != "D"
                    n_boundary_ints = Get number of intervals: 2 * s_i
                    an_start_b = Get start time of interval: 2 * s_i, 2
                    an_start_b$ = string$(an_start_b)
                    an_start_e = Get end time of interval: 2 * s_i, 2
                    an_start_e$ = string$(an_start_e)
                    if n_boundary_ints == 5
                        an_end_b = Get start time of interval: 2 * s_i, 4
                        an_end_e = Get end time of interval: 2 * s_i, 4
                    elsif n_boundary_ints == 4
                        an_end_b = Get start time of interval: 2 * s_i, 3
                        an_end_e = Get end time of interval: 2 * s_i, 3
                    endif
                    an_end_b$ = string$(an_end_b)
                    an_end_e$ = string$(an_end_e)
                else
                    an_start_b$ = "NA"
                    an_start_e$ = "NA"
                    an_end_b$ = "NA"
                    an_end_e$ = "NA"
                endif
                selectObject: "Table chunks"
                Set string value: annotateChunk.id, a$ + "_" + s$ + "_start_b", an_start_b$
                Set string value: annotateChunk.id, a$ + "_" + s$ + "_start_e", an_start_e$
                Set string value: annotateChunk.id, a$ + "_" + s$ + "_end_b", an_end_b$
                Set string value: annotateChunk.id, a$ + "_" + s$ + "_end_e", an_end_e$
                Set string value: annotateChunk.id, a$ + "_" + s$ + "_reduction", an_reduction$
            endfor
            schwa_reduction$ = Get value: annotateChunk.id, a$ + "_e_reduction"
            n_reduction$ = Get value: annotateChunk.id, a$ + "_n_reduction"
            selectObject: "TextGrid " + annot_chunk$
            n_nasal_boundary_ints = Get number of intervals: 3
            if (n_nasal_boundary_ints == 1) or (schwa_reduction$ == "D" and n_reduction$ == "D")
                nasal_b$ = "NA"
                nasal_e$ = "NA"
            else
                nasal_b = Get start time of interval: 3, 2
                nasal_b$ = string$(nasal_b)
                nasal_e = Get end time of interval: 3, 2
                nasal_e$ = string$(nasal_e)
            endif
            selectObject: "Table chunks"
            Set string value: annotateChunk.id, a$ + "_N_b", nasal_b$
            Set string value: annotateChunk.id, a$ + "_N_e", nasal_e$
            removeObject: "TextGrid " + annot_chunk$
        else
            appendInfoLine: annot_chunk_name$ + " not readable"
            selectObject: "Table chunks"
            for s_i from 1 to 2
                s$ = segments$[s_i]
                Set string value: annotateChunk.id, a$ + "_" + s$ + "_start_b", "NA"
                Set string value: annotateChunk.id, a$ + "_" + s$ + "_start_e", "NA"
                Set string value: annotateChunk.id, a$ + "_" + s$ + "_end_b", "NA"
                Set string value: annotateChunk.id, a$ + "_" + s$ + "_end_e", "NA"
                Set string value: annotateChunk.id, a$ + "_" + s$ + "_reduction", "NA"
            endfor
            Set string value: annotateChunk.id, a$ + "_N_b", "NA"
            Set string value: annotateChunk.id, a$ + "_N_e", "NA"
        endif
    endfor

    selectObject: "Table chunks"
    Set string value: annotateChunk.id, "kal_e_b", kal_schwa_b$
    Set string value: annotateChunk.id, "kal_e_e", kal_schwa_e$
    Set string value: annotateChunk.id, "kal_n_b", kal_n_b$
    Set string value: annotateChunk.id, "kal_n_e", kal_n_e$
    Set string value: annotateChunk.id, "wrd_e", man_end$
    Set string value: annotateChunk.id, "kal_start", kal_start$
    Set string value: annotateChunk.id, "kal_end", kal_end$
    Set string value: annotateChunk.id, "sound_end", sound_end$
endproc


num_chunks = Get number of rows
for id from 1 to num_chunks
    selectObject: "Table chunks"
    @inspectChunk: id
endfor

selectObject: "Table chunks"
Save as comma-separated file: tensusers$ + "classifier_evaluation/en/nn_eval_en_o3_annotated.csv"
