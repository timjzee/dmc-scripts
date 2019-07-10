prop$ = Report system properties
os$ = extractWord$(prop$, newline$)


corpus$ = "cgn"
if corpus$ == "IFADVcorpus"
    o_path$ = "/tensusers/timzee/IFADVcorpus/Speech/"
elif corpus$ == "cgn"
    component$ = "k"
    ort_path$ = "/bigdata2/corpora2/CGN2/data/annot/text/ort/comp-"
    k_path$ = "/tensusers/timzee/cgn/kaldi_annot/comp-"
    o_path$ = "/bigdata2/corpora2/CGN2/data/annot/text/awd/comp-"
    wrd_path$ = "/bigdata2/corpora2/CGN2/data/annot/text/wrd/comp-"
    if component$ == "c"
        audio_path$ = "/tensusers/timzee/cgn/mono_comp-"
    else
        audio_path$ = "/bigdata2/corpora2/CGN2/data/audio/wav/comp-"
    endif
else
    o_path$ = "/tensusers/timzee/ECSD/Speech/"
endif


if os$ == "macintosh"
    tens_path$ = "/Volumes/tensusers/timzee/" + corpus$ + "/"
    ort_tg_path$ = "/Volumes" + ort_path$
    awd_tg_path$ = "/Volumes" + o_path$
    k_tg_path$ = "/Volumes" + k_path$
    wrd_tg_path$ = "/Volumes" + wrd_path$
    wav_path$ = "/Volumes" + audio_path$
    phone_path$ = "/Volumes/timzee/Docs/KALDI-CGN_phones2.txt"
else
    tens_path$ = "/vol/tensusers/timzee/" + corpus$ + "/"
    ort_tg_path$ = "/vol" + ort_path$
    awd_tg_path$ = "/vol" + o_path$
    k_tg_path$ = "/vol" + k_path$
    wrd_tg_path$ = "/vol" + wrd_path$
    wav_path$ = "/vol" + audio_path$
    phone_path$ = "/home/timzee/Docs/KALDI-CGN_phones2.txt"
endif

index_path$ = tens_path$ + "cgn_index_k_core.txt"

agreement_threshold = 0.02

Create Table with column names: "measurements", 0, "wav chunk_start chunk_end tier word_chunk_i raw_word awd_start kaldi_start wrd_start wrd_awd_start wrd_kaldi_start awd_end kaldi_end wrd_end wrd_awd_end wrd_kaldi_end awd_tran kaldi_tran wrd_tran speaker hnr"
Read Table from comma-separated file: index_path$
Rename: "chunks"

old_name$ = ""
old_tier = 0
old_chan = 0
num_chunks = Get number of rows
awd_prev_end = 0
wrd_prev_end = 0
kal_prev_end = 0
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
            removeObject: "TextGrid " + old_name$ + "_ort"
            removeObject: "TextGrid " + old_name$ + "_awd"
            removeObject: "TextGrid " + old_name$ + "_wrd"
            removeObject: "TextGrid " + old_name$ + "_kal"
            removeObject: "Sound " + old_name$
        endif
        ## Load TextGrids
        # .ort
        runSystem_nocheck: "cp -f " + ort_tg_path$ + f_path$ + ".ort.gz " + tens_path$
        runSystem_nocheck: "gunzip -f " + tens_path$ + f_name$ + ".ort.gz"
        Read from file: tens_path$ + f_name$ + ".ort"
        runSystem_nocheck: "rm -f " + tens_path$ + f_name$ + ".ort"
        Rename: f_name$ + "_ort"
        # .awd
        runSystem_nocheck: "cp -f " + awd_tg_path$ + f_path$ + ".awd.gz " + tens_path$
        runSystem_nocheck: "gunzip -f " + tens_path$ + f_name$ + ".awd.gz"
        Read from file: tens_path$ + f_name$ + ".awd"
        runSystem_nocheck: "rm -f " + tens_path$ + f_name$ + ".awd"
        Rename: f_name$ + "_awd"
        # .wrd
        runSystem_nocheck: "cp -f " + wrd_tg_path$ + f_path$ + ".wrd.gz " + tens_path$
        runSystem_nocheck: "gunzip -f " + tens_path$ + f_name$ + ".wrd.gz"
        Read from file: tens_path$ + f_name$ + ".wrd"
        runSystem_nocheck: "rm -f " + tens_path$ + f_name$ + ".wrd"
        Rename: f_name$ + "_wrd"
        # kaldi
        Read from file: k_tg_path$ + f_path$ + ".awd"
        Rename: f_name$ + "_kal"
        ## Load wav for HNR measure
        Read from file: wav_path$ + f_path$ + ".wav"
        old_tier = 0
        old_chan = 0
        old_name$ = f_name$
    endif
    # Get HNR measurement
    if c_tier != old_tier
        if c_chan != old_chan
            if id > 1
                removeObject: "Sound hnr"
            endif
            selectObject: "Sound " + f_name$
            Extract one channel: c_chan
            Rename: "hnr"
            old_chan = c_chan
        endif
        old_tier = old_tier
    endif
    selectObject: "Sound hnr"
    Extract part: c_start, c_end, "rectangular", 1, "no"
    To Harmonicity (cc): 0.01, 75, 0.1, 4.5
    hnr = Get mean: 0, 0
    if hnr == undefined
        hnr = -20
    endif
    Remove
    removeObject: "Sound hnr_part"
    # get speaker
    selectObject: "TextGrid " + f_name$ + "_ort"
    speaker$ = Get tier name: c_tier
    # get tier numbers in the different tgs
    selectObject: "TextGrid " + f_name$ + "_kal"
    t_name$ = ""
    kal_tier = 0
    while t_name$ != speaker$
        kal_tier += 1
        t_name$ = Get tier name: kal_tier
    endwhile
    selectObject: "TextGrid " + f_name$ + "_wrd"
    t_name$ = ""
    wrd_tier = 0
    while t_name$ != speaker$
        wrd_tier += 1
        t_name$ = Get tier name: wrd_tier
    endwhile
    selectObject: "TextGrid " + f_name$ + "_awd"
    t_name$ = ""
    awd_tier = 0
    while t_name$ != speaker$
        awd_tier += 1
        t_name$ = Get tier name: awd_tier
    endwhile
    # get total duration
    total_dur = Get total duration
    # get words
    space_i = -1
    sent$ = c_ort$
    num_words = 0
    while space_i != 0
        space_i = index(sent$, " ")
        sent_len = length(sent$)
        if space_i != 0
            num_words += 1
            words$[num_words] = left$(sent$, space_i - 1)
            sent$ = right$(sent$, sent_len - space_i)
        elif space_i == 0 and sent_len != 0
            num_words += 1
            words$[num_words] = sent$
        endif
    endwhile
    # loop over words
    for word_i from 1 to num_words
        word$ = words$[word_i]
        if word_i == 1
            selectObject: "TextGrid " + f_name$ + "_awd"
            awd_cur_i = Get high interval at time: awd_tier, c_start
            awd_fin_i = Get low interval at time: awd_tier, c_end
            if awd_cur_i == awd_fin_i
                skip_awd = 1
                awd_cur_i += 1
#                awd_prev_end = awd_cur_i
            else
                skip_awd = 0
            endif
            awd_cur_i -= 1
            awd_word_i = 0
            awd_prev_lab$ = ""
            selectObject: "TextGrid " + f_name$ + "_wrd"
            wrd_cur_i = Get high interval at time: wrd_tier, c_start
            wrd_fin_i = Get low interval at time: wrd_tier, c_end
            if wrd_cur_i == wrd_fin_i
                skip_wrd = 1
                wrd_cur_i += 1
#                wrd_prev_end = wrd_cur_i
            else
                skip_wrd = 0
            endif
            wrd_cur_i -= 1
            wrd_word_i = 0
            wrd_prev_lab$ = ""
            selectObject: "TextGrid " + f_name$ + "_kal"
            kal_cur_i = Get high interval at time: kal_tier, c_start
            kal_fin_i = Get low interval at time: kal_tier, c_end
            if kal_cur_i == kal_fin_i
                skip_kal = 1
                kal_cur_i += 1
#                kal_prev_end = kal_cur_i
            else
                skip_kal = 0
            endif
            kal_cur_i -= 1
            kal_word_i = 0
            kal_prev_lab$ = ""
        endif
        # get wrd boundaries
        if skip_wrd
            wrd_start$ = "NA"
            wrd_end$ = "NA"
            wrd_tran$ = "NA"
        else
            selectObject: "TextGrid " + f_name$ + "_wrd"
            while wrd_word_i != word_i
                wrd_cur_i += 1
                i_lab$ = Get label of interval: wrd_tier, wrd_cur_i
                if i_lab$ != "" and i_lab$ != "_" and i_lab$ != " " and wrd_prev_end != wrd_cur_i
#                    appendInfoLine: i_lab$, " ", word$, " ", c_start
                    assert replace_regex$(i_lab$, "[_ ]", "", 0) == word$
                    wrd_word_i += 1
                    wrd_tran$ = Get label of interval: wrd_tier + 1, wrd_cur_i
                    # Get start boundary
                    w_start = Get start time of interval: wrd_tier, wrd_cur_i
                    if wrd_prev_lab$ == "_"
                        us_start = Get start time of interval: wrd_tier, wrd_cur_i - 1
                        us_dur = w_start - us_start
                        wrd_start = w_start - 0.5 * us_dur
                    else
                        wrd_start = w_start
                    endif
                    wrd_start$ = string$(wrd_start)
                    # Get end boundary
                    w_end = Get end time of interval: wrd_tier, wrd_cur_i
                    if w_end == total_dur
                        wrd_end = w_end
                    else
                        next_lab$ = Get label of interval: wrd_tier, wrd_cur_i + 1
                        if next_lab$ == "_"
                            us_end = Get end time of interval: wrd_tier, wrd_cur_i + 1
                            us_dur = us_end - w_end
                            wrd_end = w_end + 0.5 * us_dur
                        else
                            wrd_end = w_end
                        endif
                    endif
                    wrd_end$ = string$(wrd_end)
                endif
                wrd_prev_lab$ = i_lab$
            endwhile
        endif
        # get awd boundaries
        if skip_awd
            awd_start$ = "NA"
            awd_end$ = "NA"
            awd_tran$ = "NA"
            wrd_awd_start$ = "NA"
            wrd_awd_end$ = "NA"
        else
            selectObject: "TextGrid " + f_name$ + "_awd"
            while awd_word_i != word_i
                awd_cur_i += 1
                i_lab$ = Get label of interval: awd_tier, awd_cur_i
                if i_lab$ != "" and i_lab$ != "_" and i_lab$ != " " and awd_prev_end != awd_cur_i
#                    appendInfoLine: i_lab$, " ", word$, " ", c_start, " ", awd_prev_end, " ", awd_cur_i
                    assert replace_regex$(i_lab$, "[_ ]", "", 0) == word$
                    awd_word_i += 1
                    awd_tran$ = Get label of interval: awd_tier + 1, awd_cur_i
                    # Get start boundary
                    w_start = Get start time of interval: awd_tier, awd_cur_i
                    if awd_prev_lab$ == "_"
                        us_start = Get start time of interval: awd_tier, awd_cur_i - 1
                        us_dur = w_start - us_start
                        awd_start = w_start - 0.5 * us_dur
                    else
                        awd_start = w_start
                    endif
                    awd_start$ = string$(awd_start)
                    if skip_wrd
                        wrd_awd_start$ = "NA"
                    else
                        wrd_awd_start$ = string$(wrd_start - awd_start)
                    endif
                    # Get end boundary
                    w_end = Get end time of interval: awd_tier, awd_cur_i
                    if w_end == total_dur
                        awd_end = w_end
                    else
                        next_lab$ = Get label of interval: awd_tier, awd_cur_i + 1
                        if next_lab$ == "_"
                            us_end = Get end time of interval: awd_tier, awd_cur_i + 1
                            us_dur = us_end - w_end
                            awd_end = w_end + 0.5 * us_dur
                        else
                            awd_end = w_end
                        endif
                    endif
                    awd_end$ = string$(awd_end)
                    if skip_wrd
                        wrd_awd_end$ = "NA"
                    else
                        wrd_awd_end$ = string$(wrd_end - awd_end)
                    endif
                endif
                awd_prev_lab$ = i_lab$
            endwhile
        endif
        # get kal boundaries
        if skip_kal
            kal_start$ = "NA"
            kal_end$ = "NA"
            kal_tran$ = "NA"
            wrd_kaldi_start$ = "NA"
            wrd_kaldi_end$ = "NA"
        else
            selectObject: "TextGrid " + f_name$ + "_kal"
            while kal_word_i != word_i
                kal_cur_i += 1
                i_lab$ = Get label of interval: kal_tier, kal_cur_i
                if i_lab$ != "" and i_lab$ != "_" and i_lab$ != " " and kal_prev_end != kal_cur_i
                    assert replace_regex$(i_lab$, "[_ ]", "", 0) == word$
                    kal_word_i += 1
                    kaldi_tran$ = Get label of interval: kal_tier + 2, kal_cur_i
                    # Get start boundary
                    w_start = Get start time of interval: kal_tier, kal_cur_i
                    if kal_prev_lab$ == "_"
                        us_start = Get start time of interval: kal_tier, kal_cur_i - 1
                        us_dur = w_start - us_start
                        kaldi_start = w_start - 0.5 * us_dur
                    else
                        kaldi_start = w_start
                    endif
                    kaldi_start$ = string$(kaldi_start)
                    if skip_wrd
                        wrd_kaldi_start$ = "NA"
                    else
                        wrd_kaldi_start$ = string$(wrd_start - kaldi_start)
                    endif
                    # Get end boundary
                    w_end = Get end time of interval: kal_tier, kal_cur_i
                    if w_end == total_dur
                        kaldi_end = w_end
                    else
                        next_lab$ = Get label of interval: kal_tier, kal_cur_i + 1
                        if next_lab$ == "_"
                            us_end = Get end time of interval: kal_tier, kal_cur_i + 1
                            us_dur = us_end - w_end
                            kaldi_end = w_end + 0.5 * us_dur
                        else
                            kaldi_end = w_end
                        endif
                    endif
                    kaldi_end$ = string$(kaldi_end)
                    if skip_wrd
                        wrd_kaldi_end$ = "NA"
                    else
                        wrd_kaldi_end$ = string$(wrd_end - kaldi_end)
                    endif
                endif
                kal_prev_lab$ = i_lab$
            endwhile
        endif
        selectObject: "Table measurements"
        Append row
        n_rows = Get number of rows
        Set string value: n_rows, "wav", f_path$
        Set string value: n_rows, "chunk_start", c_start$
        Set string value: n_rows, "chunk_end", c_end$
        Set numeric value: n_rows, "tier", c_tier
        Set numeric value: n_rows, "word_chunk_i", word_i
        Set string value: n_rows, "raw_word", word$
        Set string value: n_rows, "awd_start", awd_start$
        Set string value: n_rows, "wrd_start", wrd_start$
        Set string value: n_rows, "kaldi_start", kaldi_start$
        Set string value: n_rows, "wrd_awd_start", wrd_awd_start$
        Set string value: n_rows, "wrd_kaldi_start", wrd_kaldi_start$
        Set string value: n_rows, "awd_end", awd_end$
        Set string value: n_rows, "wrd_end", wrd_end$
        Set string value: n_rows, "kaldi_end", kaldi_end$
        Set string value: n_rows, "wrd_awd_end", wrd_awd_end$
        Set string value: n_rows, "wrd_kaldi_end", wrd_kaldi_end$
        Set string value: n_rows, "awd_tran", awd_tran$
        Set string value: n_rows, "wrd_tran", wrd_tran$
        Set string value: n_rows, "kaldi_tran", kaldi_tran$
        Set string value: n_rows, "speaker", speaker$
        Set numeric value: n_rows, "hnr", hnr
    endfor
    # in case the boundaries between sentences differ between index and tgs
    awd_prev_end = awd_cur_i
    wrd_prev_end = wrd_cur_i
    kal_prev_end = kal_cur_i
endfor

selectObject: "Table measurements"
Save as comma-separated file: tens_path$ + "comp-k_word_boundaries.csv"
Extract rows where column (text): "wrd_awd_end", "is not equal to", "NA"
Extract rows where column (text): "wrd_kaldi_end", "is not equal to", "NA"
Rename: "total"
total_rows = Get number of rows
Extract rows where column (number): "wrd_awd_end", "less than", agreement_threshold
Extract rows where column (number): "wrd_awd_end", "greater than", -1 * agreement_threshold
wrd_awd_agree = Get number of rows
selectObject: "Table total"
Extract rows where column (number): "wrd_kaldi_end", "less than", agreement_threshold
Extract rows where column (number): "wrd_kaldi_end", "greater than", -1 * agreement_threshold
wrd_kaldi_agree = Get number of rows

appendInfoLine: " "
appendInfoLine: "########################################################"
appendInfoLine: "Percent agreement between manual and HTK: ", 100 * (wrd_awd_agree / total_rows), "%"
appendInfoLine: "Percent agreement between manual and KALDI: ", 100 * (wrd_kaldi_agree / total_rows), "%"
