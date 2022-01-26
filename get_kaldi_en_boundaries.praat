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

vowels$[1] = "@"
vowels$[2] = "A"
vowels$[3] = "AU"
vowels$[4] = "E"
vowels$[5] = "E2"
vowels$[6] = "EI"
vowels$[7] = "EU"
vowels$[8] = "I"
vowels$[9] = "O"
vowels$[10] = "U"
vowels$[11] = "UI"
vowels$[12] = "a"
vowels$[13] = "e"
vowels$[14] = "i"
vowels$[15] = "o"
vowels$[16] = "u"
vowels$[17] = "y"

Read Table from tab-separated file: tensusers$ + "cgn/speakers.txt"

# Make sure input file has a header
Read Table from comma-separated file: tensusers$ + "en_morph_project/nn_all_en_o.csv"
Rename: "chunks"

Append column: "kal_e_b"
Append column: "kal_e_e"
Append column: "kal_n_b"
Append column: "kal_n_e"
Append column: "kal_start"
Append column: "kal_end"
Append column: "sound_end"
Append column: "num_syl_pron"
Append column: "speech_rate_pron"
Append column: "prev_mention"
Append column: "w_start"
Append column: "speaker_sex"
Append column: "birth_year"
Append column: "phrase_final"

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

    selectObject: "Table speakers"
    speaker_row = Search column: "ID", speaker$
    birth_year$ = Get value: speaker_row, "birthYear"
    speaker_sex$ = Get value: speaker_row, "sex"

    selectObject: "TextGrid " + s_name$
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
    w_start$ = fixed$(w_start, 3)
    w_end = Get end time of interval: tier, word_int

    # Calculate speech rate and number of syllables in s word
    s_int = Get low interval at time: tier + 3, w_end - 0.002
    s_word_int = Get high interval at time: tier + 3, w_start
    num_syl_pron = 0
    num_syl = 0
    first_phon = 1
    start_int = Get high interval at time: tier + 3, c_start
    end_int = Get low interval at time: tier + 3, c_end
    for int from start_int to end_int
        i_lab$ = Get label of interval: tier + 3, int
        if i_lab$ != "" and i_lab$ != "SIL" and i_lab$ != "[SPN]"
            if first_phon
                start_phon = Get start time of interval: tier + 3, int
                first_phon = 0
            endif
            final_phon = Get end time of interval: tier + 3, int
        endif
        for vowel from 1 to 17
            if i_lab$ == vowels$[vowel]
                num_syl += 1
                if int >= s_word_int and int <= s_int
                    num_syl_pron += 1
                endif
                goto SKIP
            endif
        endfor
        label SKIP
    endfor
    num_syl_pron$ = string$(num_syl_pron)
    speech_time = final_phon - start_phon
    speech_rate_pron = num_syl / speech_time
    speech_rate_pron$ = fixed$(speech_rate_pron, 3)

    # get previous mention
    raw_ort$ = Get label of interval: tier, word_int
    if word_int == 1
        prev_mention$ = "NA"
    else
        strip_ort$ = replace_regex$(raw_ort$, "[.?]", "", 0)
        if w_start > 30
            search_start_t = w_start - 30
        else
            search_start_t = 0
        endif
        search_start = Get high interval at time: tier, search_start_t
        search_end = word_int - 1
        for wrd_i from search_start to search_end
            wrd$ = Get label of interval: tier, wrd_i
            strp_wrd$ = replace_regex$(wrd$, "[.?]", "", 0)
            if strp_wrd$ == strip_ort$
                prev_mention$ = "TRUE"
                goto MENTIONED
            endif
        endfor
        prev_mention$ = "FALSE"
        label MENTIONED
    endif

    if rindex_regex(raw_ort$, "[^.?][.?]+$")
        phrase_final$ = "TRUE"
    else
        phrase_final$ = "FALSE"
    endif

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

    removeObject: "TextGrid " + s_name$ + "_kal"

    selectObject: "Table chunks"
    Set string value: annotateChunk.id, "kal_e_b", kal_schwa_b$
    Set string value: annotateChunk.id, "kal_e_e", kal_schwa_e$
    Set string value: annotateChunk.id, "kal_n_b", kal_n_b$
    Set string value: annotateChunk.id, "kal_n_e", kal_n_e$
    Set string value: annotateChunk.id, "kal_start", kal_start$
    Set string value: annotateChunk.id, "kal_end", kal_end$
    Set string value: annotateChunk.id, "sound_end", sound_end$
    Set string value: annotateChunk.id, "num_syl_pron", num_syl_pron$
    Set string value: annotateChunk.id, "speech_rate_pron", speech_rate_pron$
    Set string value: annotateChunk.id, "prev_mention", prev_mention$
    Set string value: annotateChunk.id, "w_start", w_start$
    Set string value: annotateChunk.id, "speaker_sex", speaker_sex$
    Set string value: annotateChunk.id, "birth_year", birth_year$
    Set string value: annotateChunk.id, "phrase_final", phrase_final$
endproc


num_chunks = Get number of rows
for id from 1 to num_chunks
    selectObject: "Table chunks"
    appendInfoLine: id
    @inspectChunk: id
endfor

selectObject: "Table chunks"
Save as comma-separated file: tensusers$ + "en_morph_project/nn_all_en_o_kal.csv"
