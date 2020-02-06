prop$ = Report system properties
os$ = extractWord$(prop$, newline$)


corpus$ = "cgn"
if corpus$ == "IFADVcorpus"
    o_path$ = "/tensusers/timzee/IFADVcorpus/Speech/"
elif corpus$ == "cgn"
    component$ = "d"
    if component$ == "c" or component$ == "d"
        o_path$ = "/tensusers/timzee/cgn/mono_comp-"
    else
        o_path$ = "/bigdata2/corpora2/CGN2/data/audio/wav/comp-"
    endif
else
    o_path$ = "/tensusers/timzee/ECSD/Speech/"
endif


if os$ == "macintosh"
    tens_path$ = "/Volumes/tensusers/timzee/" + corpus$ + "/"
    audio_path$ = "/Volumes" + o_path$
    frag_path$ = "/Volumes/tensusers/timzee/af_classification/pred_fragments/"
else
    tens_path$ = "/vol/tensusers/timzee/" + corpus$ + "/"
    audio_path$ = "/vol" + o_path$
    frag_path$ = "/vol/tensusers/timzee/af_classification/pred_fragments/"
endif

cog_window = 0.8
num_spectral_slices = 4
buffer = 0.5
#frag_buffer = 0.2
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

Read Table from tab-separated file: tens_path$ + "speakers.txt"

#Create Table with column names: "spectral_info", 0, "wav speaker speaker_sex birth_year chunk_start chunk_end word_chunk_i sent_i word_sent_i word_ort next_phon next_phon_pron prev_phon prev_phon_pron word_pos word_class type_of_s base_dur speech_rate_pron num_syl_pron mean_hnr time freq_bin Pa_per_Hz dB_per_Hz"

Read Table from comma-separated file: tens_path$ + "test_en_ndl.csv"
table_name$ = selected$("Table")
Append column: "en_phon"
Append column: "en_dur"
Append column: "en_start"
Append column: "en_end"
Append column: "en_cog_full"
Append column: "en_cog_window"
# add separate @ and n durations
Append column: "schwa_start"
Append column: "schwa_end"
Append column: "n_start"
Append column: "n_end"
Append column: "proportion_voiced"
Append column: "proportion_voiced2"
Append column: "mean_hnr"
Append column: "speech_rate_pron"
Append column: "base_dur"
Append column: "num_syl_pron"
Append column: "speaker_sex"
Append column: "birth_year"
Append column: "next_phon_dur"
Append column: "prev_phon_dur"
Append column: "prev_mention"
Append column: "phrase_final"
wav_name$ = ""
ali_name$ = ""
n_inputlines = Get number of rows
for en_line from 1 to n_inputlines
    selectObject: "Table " + table_name$
    speaker$ = Get value: en_line, "speaker"
    cur_path$ = Get value: en_line, "wav"
    pre_name = rindex(cur_path$, "/")
    name_length = length(cur_path$)
    cur_name$ = right$(cur_path$, name_length - pre_name)
    cur_start$ = Get value: en_line, "chunk_start"
    cur_start = number(cur_start$)
    cur_end$ = Get value: en_line, "chunk_end"
    cur_end = number(cur_end$)
    appendInfoLine: "Working on ", cur_name$, " from ", cur_start$, " to ", cur_end$
    ali_name1$ = replace$(cur_path$, "/", "_", 2)
    c_channel$ = Get value: en_line, "chan"
    c_channel = number(c_channel$)

    # We only want to load the .wav and .awd file once
    if cur_name$ != wav_name$
        if en_line > 1
            removeObject: "LongSound " + wav_name$
            removeObject: "TextGrid " + wav_name$
        endif
        if corpus$ == "cgn"
            Open long sound file: audio_path$ + cur_path$ + ".wav"
            wav_name$ = selected$("LongSound")
            Read from file: tens_path$ + "kaldi_annot/v2/comp-" + cur_path$ + ".awd"
        elif corpus$ == "IFADVcorpus"
            Open long sound file: audio_path$ + cur_path$ + ".wav"
            wav_name$ = selected$("LongSound")
            Read from file: tens_path$ + "kaldi_annot/v2/" + cur_path$ + ".awd"
        else
            pair_length = name_length - 10
            pair_folder$ = "PP" + mid$(cur_path$, 3, pair_length)
            Open long sound file: audio_path$ + pair_folder$ + "/" + cur_path$ + "_S.wav"
            wav_name$ = selected$("LongSound")
            Read from file: tens_path$ + "kaldi_annot/v2/" + pair_folder$ + "/" + cur_path$ + ".awd"
            Rename: wav_name$
        endif
    endif

    selectObject: "Table " + table_name$
    chunk_i$ = Get value: en_line, "word_chunk_i"
    chunk_i = number(chunk_i$)
    # now let's get the corresponding index in the tg
    selectObject: "TextGrid " + wav_name$

    tier_name$ = ""
    tier = 0
    while tier_name$ != speaker$
        tier += 1
        tier_name$ = Get tier name: tier
    endwhile

    word_int = Get high interval at time: tier, cur_start
    interval_i_end = Get low interval at time: tier, cur_end
    num_interval_i = interval_i_end - word_int + 1
    # if words are not aligned
    if chunk_i > num_interval_i
        goto END
    endif
    word_int -= 1
    word_counter = 0
    while word_counter != chunk_i
        word_int += 1
        int_lab$ = Get label of interval: tier, word_int
        if int_lab$ != ""
            word_counter += 1
        endif
    endwhile
    # word_int now contains the interval number for the word containing the /@n/
    oov_meta$ = Get label of interval: tier + 1, word_int
    # if words not aligned and chunk consists of a single word
    if oov_meta$ = "unintelligible"
        goto END
    endif
    # get start and end index and get labels
    word_end = Get end time of interval: tier, word_int
    en_end_int = Get low interval at time: tier + 3, word_end
    en_end_lab$ = Get label of interval: tier + 3, en_end_int
    if en_end_lab$ == "@"
        en_start_int = en_end_int
        en_start_lab$ = ""
    elif en_end_lab$ == "n"
        if en_end_int == 1
            en_start_lab$ = ""
            en_start_int = en_end_int
        else
            en_start_lab$ = Get label of interval: tier + 3, en_end_int - 1
            if en_start_lab$ == "@"
                en_start_int = en_end_int - 1
            else
                en_start_lab$ = ""
                en_start_int = en_end_int
            endif
        endif
    else
        en_end_lab$ = ""
        en_start_lab$ = ""
    endif
    en_phon$ = en_start_lab$ + en_end_lab$
    # Calculate speech rate and number of syllables in en word
    en_word_start = Get start time of interval: tier, word_int
    en_word_int = Get high interval at time: tier + 3, en_word_start
    num_syl_pron = 0
    num_syl = 0
    first_phon = 1
    start_int = Get high interval at time: tier + 3, cur_start
    end_int = Get low interval at time: tier + 3, cur_end
    for int from start_int to end_int
        int_lab$ = Get label of interval: tier + 3, int
        if int_lab$ != "" and int_lab$ != "SIL" and int_lab$ != "[SPN]"
            if first_phon
                start_phon = Get start time of interval: tier + 3, int
                first_phon = 0
            endif
            final_phon = Get end time of interval: tier + 3, int
        endif
        for vowel from 1 to 17
            if int_lab$ == vowels$[vowel]
                num_syl += 1
                if int >= en_word_int and int <= en_end_int
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
    speech_rate_pron$ = string$(speech_rate_pron)
    # Number of consonants preceding /@n/ doesn't make much sense, but get info on preceding phon
    num_prev_p_in_word = en_start_int - en_word_int
    if num_prev_p_in_word > 0 and en_start_int != 1
        for p from 1 to num_prev_p_in_word
            p_ind = en_start_int - p
            p_lab$ = Get label of interval: tier + 3, p_ind
            if p == 1
                if p_lab$ == ""
                    prev_phon_dur$ = "NA"
                    prev_phon_pron$ = "SIL"
                else
                    prev_phon_start = Get start time of interval: tier + 3, p_ind
                    prev_phon_end = Get end time of interval: tier + 3, p_ind
                    prev_phon_pron$ = p_lab$
                    prev_phon_dur = prev_phon_end - prev_phon_start
                    prev_phon_dur$ = fixed$(prev_phon_dur, 3)
                endif
            endif
        endfor
    else
        prev_phon_dur$ = "NA"
        prev_phon_pron$ = "NA"
    endif
    # get next pronounced phon dur
    if en_end_int < end_int
        next_phon_pron$ = Get label of interval: tier + 3, en_end_int + 1
        next_phon_start = Get start time of interval: tier + 3, en_end_int + 1
        next_phon_end = Get end time of interval: tier + 3, en_end_int + 1
        next_phon_dur = next_phon_end - next_phon_start
        next_phon_dur$ = fixed$(next_phon_dur, 3)
        if next_phon_pron$ == ""
            next_phon_pron$ = "SIL"
            next_phon_dur$ = "NA"
        endif
    else
        next_phon_pron$ = "NA"
        next_phon_dur$ = "NA"
    endif
    # get previous mention
    raw_ort$ = Get label of interval: tier, word_int
    if word_int == 1
        prev_mention$ = "NA"
    else
        strip_ort$ = replace_regex$(raw_ort$, "[.?]", "", 0)
        if en_word_start > 30
            search_start_t = en_word_start - 30
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
    # get phrase final; matches for ? and . but not for ...
    if rindex_regex(raw_ort$, "[^.?][.?]$")
        phrase_final$ = "TRUE"
    else
        phrase_final$ = "FALSE"
    endif
    # now get measurements
    if en_end_lab$ == ""
        en_duration$ = "NA"
        en_start$ = "NA"
        en_end$ = "NA"
        en_cog_window$ = "NA"
        en_cog_full$ = "NA"
        schwa_start$ = "NA"
        schwa_end$ = "NA"
        n_start$ = "NA"
        n_end$ = "NA"
        proportion_voiced$ = "NA"
        proportion_voiced2$ = "NA"
        base_dur$ = "NA"
        speech_rate_pron$ = "NA"
        num_syl_pron$ = "NA"
        mean_hnr$ = "NA"
        num_cons_pron$ = "NA"
        speaker_sex$ = "NA"
        birth_year$ = "NA"
        next_phon_dur$ = "NA"
        prev_phon_dur$ = "NA"
    else
        en_start = Get start time of interval: tier + 3, en_start_int
        en_end = Get end time of interval: tier + 3, en_end_int
        en_duration = en_end - en_start
        en_duration$ = fixed$(en_duration, 3)
        # get separate measurements if applicable
        # if en_phon$ == "@n"
        if en_start_int != en_end_int
            schwa_start$ = fixed$(en_start, 3)
            schwa_end = Get end time of interval: tier + 3, en_start_int
            schwa_end$ = fixed$(schwa_end, 3)
            n_start = Get start time of interval: tier + 3, en_end_int
            n_start$ = fixed$(n_start, 3)
            n_end$ = fixed$(en_end, 3)
        else
            if en_phon$ == "@"
                schwa_start$ = fixed$(en_start, 3)
                schwa_end$ = fixed$(en_end, 3)
                n_start$ = "NA"
                n_end$ = "NA"
            else
                schwa_start$ = "NA"
                schwa_end$ = "NA"
                n_start$ = fixed$(en_start, 3)
                n_end$ = fixed$(en_end, 3)
            endif
        endif
        #
        w_start = Get start time of interval: tier, word_int
        base_dur = en_start - w_start
        base_dur$ = fixed$(base_dur, 3)
        selectObject: "LongSound " + wav_name$
        Extract part: en_start + ((1 - cog_window) / 2) * en_duration, en_end - ((1 - cog_window) / 2) * en_duration, "yes"
        Extract one channel: c_channel
        To Spectrum: "yes"
        # we should probably low pass filter the spectrum, so all cog measurements (from different components) are based on the same range
        # filter 0-4000 because comp c & d are sampled at 8000 Hz
        Filter (pass Hann band): 0, 4000, 100
        en_cog_window = Get centre of gravity: 1
        en_cog_window$ = string$(en_cog_window)
        Remove
        removeObject: "Sound " + wav_name$ + "_ch" + c_channel$
        removeObject: "Sound " + wav_name$
        selectObject: "LongSound " + wav_name$
        Extract part: en_start, en_end, "yes"
        Extract one channel: c_channel
        removeObject: "Sound " + wav_name$
        selectObject: "Sound " + wav_name$ + "_ch" + c_channel$
        Rename: wav_name$
        To Spectrum: "yes"
        # filter 0-4000 because comp c & d are sampled at 8000 Hz
        Filter (pass Hann band): 0, 4000, 100
        en_cog_full = Get centre of gravity: 1
        en_cog_full$ = string$(en_cog_full)
        Remove
        removeObject: "Sound " + wav_name$
        # Get sound from 1 second before /@n/ until 1 second after /@n/ for accurate pitch/voicing measures
        selectObject: "LongSound " + wav_name$
        recording_dur = Get total duration
        if en_start > 1
            frag_start = en_start - 1
        else
            frag_start = 0
        endif
        if (recording_dur - en_end) > 1
            frag_end = en_end + 1
        else
            frag_end = recording_dur
        endif
        Extract part: frag_start, frag_end, "yes"
        Extract one channel: c_channel
        removeObject: "Sound " + wav_name$
        selectObject: "Sound " + wav_name$ + "_ch" + c_channel$
        Rename: wav_name$
        selectObject: "Table speakers"
        speaker_row = Search column: "ID", speaker$
        birth_year$ = Get value: speaker_row, "birthYear"
        speaker_sex$ = Get value: speaker_row, "sex"
        if speaker_sex$ == "sex1"
            pitch_floor = 75
            pitch_ceiling = 300
        else
            pitch_floor = 100
            pitch_ceiling = 500
        endif
        voicing_thresh = 0.6
        silence_thresh = 0.03
        selectObject: "Sound " + wav_name$
        To Pitch (cc): 0, pitch_floor, 15, "yes", silence_thresh, voicing_thresh, 0.01, 0.35, 0.14, pitch_ceiling
        runScript: preferencesDirectory$ + "/plugin_PraatZee/extractPeriodicity.praat", en_start, en_end, "no"
        time_step = Get time step
        voiced_frames = Count voiced frames
        all_frames = Get number of frames
        proportion_voiced = voiced_frames / all_frames
        proportion_voiced$ = string$(proportion_voiced)
        removeObject: "Pitch " + wav_name$ + "_part"
        selectObject: "Pitch " + wav_name$
        plusObject: "Sound " + wav_name$
        To PointProcess (cc)
        removeObject: "Pitch " + wav_name$
        num_periods = Get number of periods: en_start, en_end, 1 / pitch_ceiling, 1 / pitch_floor, 1.3
        if num_periods != 0
            mean_period = Get mean period: en_start, en_end, 0.0001, 0.02, 1.3
            proportion_voiced2 = (num_periods * mean_period) / en_duration
            if proportion_voiced2 > 1
                proportion_voiced2 = 1
            endif
        else
            proportion_voiced2 = 0
        endif
        proportion_voiced2$ = string$(proportion_voiced2)
        Remove
        if num_periods != 0
            periods_per_win = 4.5
            duration_needed = periods_per_win * (1 / pitch_floor)
            if num_periods < periods_per_win
#                appendInfoLine: "A"
                periods_per_win = num_periods - 0.001
            elsif duration_needed + 0.001 > en_duration
#                appendInfoLine: "B"
                periods_per_win = en_duration * pitch_floor - 0.001
            endif
            while en_duration - periods_per_win * (1 / pitch_floor) < 0.011
                periods_per_win -= 1.3
                if periods_per_win < 1
                    goto NO_VOICING
                endif
            endwhile
            selectObject: "Sound " + wav_name$
            To Harmonicity (cc): time_step, pitch_floor, silence_thresh, periods_per_win
            runScript: preferencesDirectory$ + "/plugin_PraatZee/extractPeriodicity.praat", en_start, en_end, "no"
            mean_hnr = Get mean: 0, 0
            mean_hnr$ = string$(mean_hnr)
            Remove
            removeObject: "Harmonicity " + wav_name$
        else
            # if 1% energy periodic and 99% noise: 10*log10(1/99) = -20
            label NO_VOICING
            mean_hnr$ = "NA"
        endif
        removeObject: "Sound " + wav_name$
    endif
    # now lets add duration, voicing and cog data to table
    selectObject: "Table " + table_name$
    Set string value: en_line, "next_phon_pron", next_phon_pron$
    Set string value: en_line, "prev_phon_pron", prev_phon_pron$
    Set string value: en_line, "en_phon", en_phon$
    Set string value: en_line, "en_dur", en_duration$
    Set string value: en_line, "en_start", fixed$(en_start, 3)
    Set string value: en_line, "en_end", fixed$(en_end, 3)
    Set string value: en_line, "en_cog_full", en_cog_full$
    Set string value: en_line, "en_cog_window", en_cog_window$
    Set string value: en_line, "schwa_start", schwa_start$
    Set string value: en_line, "schwa_end", schwa_end$
    Set string value: en_line, "n_start", n_start$
    Set string value: en_line, "n_end", n_end$
    Set string value: en_line, "proportion_voiced", proportion_voiced$
    Set string value: en_line, "proportion_voiced2", proportion_voiced2$
    Set string value: en_line, "base_dur", base_dur$
    Set string value: en_line, "speech_rate_pron", speech_rate_pron$
    Set string value: en_line, "num_syl_pron", num_syl_pron$
    Set string value: en_line, "mean_hnr", mean_hnr$
    Set string value: en_line, "speaker_sex", speaker_sex$
    Set string value: en_line, "birth_year", birth_year$
    Set string value: en_line, "next_phon_dur", next_phon_dur$
    Set string value: en_line, "prev_phon_dur", prev_phon_dur$
    Set string value: en_line, "prev_mention", prev_mention$
    Set string value: en_line, "phrase_final", phrase_final$
    label END
endfor

selectObject: "Table " + table_name$
Save as comma-separated file: tens_path$ + table_name$ + "_static.csv"
#selectObject: "Table spectral_info"
#Save as comma-separated file: tens_path$ + table_name$ + "_dynamic.csv"
