prop$ = Report system properties
os$ = extractWord$(prop$, newline$)

wrd_path$ = "/bigdata2/corpora2/CGN2/data/annot/text/wrd/comp-"
gs_num$ = "gs24"

if os$ == "macintosh"
    tens_path$ = "/Volumes/tensusers/timzee/grid_search/"
    wrd_tg_path$ = "/Volumes" + wrd_path$
else
    tens_path$ = "/vol/tensusers/timzee/grid_search/"
    wrd_tg_path$ = "/vol" + wrd_path$
endif

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

index_path$ = tens_path$ + "grid_search_index_sorted.txt"

Create Table with column names: "output", 0, "wav chan from to tier word cgn_tran kal_tran cgn_start cgn_end kal_start kal_end"

Read Table from comma-separated file: index_path$
Rename: "chunks"

old_name$ = ""
old_tier = 0
num_chunks = Get number of rows

for id from 1 to num_chunks
    selectObject: "Table chunks"
    f_path$ = Get value: id, "wav"
    f_name$ = right$(f_path$, length(f_path$) - rindex(f_path$, "/"))
    c_tier$ = Get value: id, "tier"
    c_tier = number(c_tier$)
    c_start$ = Get value: id, "from"
    c_start = number(c_start$)
    c_end$ = Get value: id, "to"
    c_end = number(c_end$)
    c_ort$ = Get value: id, "ort"
    c_chan = Get value: id, "chan"

    if f_name$ != old_name$
        appendInfoLine: "Line ", id, " File ", f_name$
        ## Remove old files
        if id > 1
            removeObject: "TextGrid " + old_name$ + "_wrd"
            removeObject: "TextGrid " + old_name$ + "_kal"
        endif
        ## Load TextGrids
        Read from file: tens_path$ + "kaldi_annot/" + gs_num$ + "/" + f_name$ + ".awd"
        Rename: f_name$ + "_kal"
        runSystem_nocheck: "cp -f " + wrd_tg_path$ + f_path$ + ".wrd.gz " + tens_path$
        runSystem_nocheck: "gunzip -f " + tens_path$ + f_name$ + ".wrd.gz"
        Read from file: tens_path$ + f_name$ + ".wrd"
        runSystem_nocheck: "rm -f " + tens_path$ + f_name$ + ".wrd"
        Rename: f_name$ + "_wrd"
        old_tier = 0
        old_name$ = f_name$
    endif
    selectObject: "TextGrid " + f_name$ + "_wrd"
    tier_name$ = Get tier name: c_tier * 2 - 1
    Extract one tier: c_tier * 2
    Rename: "tier"
    Extract part: c_start, c_end, "no"
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
    selectObject: "TextGrid " + f_name$ + "_kal"
    num_tiers = Get number of tiers
    kal_tier_name$ = ""
    t = 0
    while kal_tier_name$ != tier_name$
        t += 1
        kal_tier_name$ = Get tier name: t
    endwhile
    Extract one tier: t
    Rename: "ort_tier"
    Extract part: c_start, c_end, "no"
    Rename: "ort_chunk"
    removeObject: "TextGrid ort_tier"
    selectObject: "TextGrid " + f_name$ + "_kal"
    Extract one tier: t + 2
    Rename: "tran_tier"
    Extract part: c_start, c_end, "no"
    Rename: "tran_chunk"
    removeObject: "TextGrid tran_tier"
    selectObject: "TextGrid " + f_name$ + "_kal"
    Extract one tier: t + 3
    Rename: "phon_tier"
    Extract part: c_start, c_end, "no"
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
    # get info for output file
    @getWordArray: c_ort$
    num_words = getWordArray.num_words
    for i from 1 to num_words
        word_array$[i] = getWordArray.word_array$[i]
    endfor
    wrd_i = 0
    kal_i = 0
    num_wrd_i = Get number of intervals: 1
    num_kal_i = Get number of intervals: 2
#    wrd_word_i = 0
#    kal_word_i = 0
    for word_i from 1 to num_words + 1
#        word$ = word_array$[word_i]
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
        # handle inserted silences
        if insert_wrd_sil or insert_kal_sil
            selectObject: "Table output"
            Append row
            num_row = Get number of rows
            Set string value: num_row, "wav", f_path$
            Set string value: num_row, "word", "silence"
            Set numeric value: num_row, "chan", c_chan
            Set string value: num_row, "from", c_start$
            Set string value: num_row, "to", c_end$
            Set numeric value: num_row, "tier", c_tier
            if insert_wrd_sil
                Set string value: num_row, "cgn_tran", "SIL"
                Set string value: num_row, "cgn_start", string$(wrd_sil_start)
                Set string value: num_row, "cgn_end", string$(wrd_sil_end)
            else
                Set string value: num_row, "cgn_tran", "NA"
                Set string value: num_row, "cgn_start", "NA"
                Set string value: num_row, "cgn_end", "NA"
            endif
            if insert_kal_sil
                Set string value: num_row, "kal_tran", "SIL"
                Set string value: num_row, "kal_start", string$(kal_sil_start)
                Set string value: num_row, "kal_end", string$(kal_sil_end)
            else
                Set string value: num_row, "kal_tran", "NA"
                Set string value: num_row, "kal_start", "NA"
                Set string value: num_row, "kal_end", "NA"
            endif
        endif
        # if this cycle is not meant for a trailing silence
        if word_i <= num_words
            selectObject: "TextGrid combined"
            # get cgn info
            if wrd_tran$ == "[SPN]" or wrd_tran$ == "[]"
                cgn_tran$ = wrd_tran$
            else
                @getSegments: wrd_tran$
                n_phon = getSegments.num_phons
                cgn_tran$ = ""
                for i from 1 to n_phon
                    if i == 1
                        cgn_tran$ = cgn_tran$ + getSegments.phons$[i]
                    else
                        cgn_tran$ = cgn_tran$ + " " + getSegments.phons$[i]
                    endif
                endfor
            endif
            cgn_start = Get start time of interval: 1, wrd_i
            cgn_end = Get end time of interval: 1, wrd_i
            # Get kal info
            kal_start = Get start time of interval: 2, kal_i
            kal_end = Get end time of interval: 2, kal_i
            start_phon_i = Get high interval at time: 4, kal_start
            end_phon_i = Get low interval at time: 4, kal_end
            kal_tran$ = ""
            for p from start_phon_i to end_phon_i
                p_lab$ = Get label of interval: 4, p
                if p == start_phon_i
                    kal_tran$ = kal_tran$ + p_lab$
                else
                    kal_tran$ = kal_tran$ + " " + p_lab$
                endif
            endfor
            selectObject: "Table output"
            Append row
            num_row = Get number of rows
            Set string value: num_row, "wav", f_path$
            Set numeric value: num_row, "chan", c_chan
            Set string value: num_row, "from", c_start$
            Set string value: num_row, "to", c_end$
            Set numeric value: num_row, "tier", c_tier
            Set string value: num_row, "word", word_array$[word_i]
            Set string value: num_row, "cgn_tran", cgn_tran$
            Set string value: num_row, "cgn_start", string$(cgn_start)
            Set string value: num_row, "cgn_end", string$(cgn_end)
            Set string value: num_row, "kal_tran", kal_tran$
            Set string value: num_row, "kal_start", string$(kal_start)
            Set string value: num_row, "kal_end", string$(kal_end)
        endif
    endfor
    removeObject: "TextGrid combined"
endfor
selectObject: "Table output"
Save as comma-separated file: tens_path$ + gs_num$ + "_aligned.csv"
