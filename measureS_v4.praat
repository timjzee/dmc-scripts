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
    tens_path$ = "/vol/tensusers/timzee/" + corpus$ + "/"
    audio_path$ = "/vol" + o_path$
    if corpus$ == "IFADVcorpus"
        frag_path$ = "/vol/tensusers/timzee/af_classification/pred_textgrids/ifadv/"
    elsif corpus$ == "ECSD"
        frag_path$ = "/vol/tensusers/timzee/af_classification/pred_textgrids/ecsd/"
    elsif corpus$ == "cgn"
        frag_path$ = "/vol/tensusers/timzee/af_classification/pred_textgrids/cgn-" + component$ + "/"
    else
        frag_path$ = "/vol/tensusers/timzee/af_classification/pred_textgrids/" + corpus$ + "/"
    endif
endif

cog_window = 0.8
num_spectral_slices = 4
buffer = 0.5
frag_buffer = 0.2
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

Read Table from comma-separated file: tens_path$ + "comp-d_s_vl_ndl.csv"
table_name$ = selected$("Table")
Append column: "s_dur"
Append column: "kal_start"
Append column: "kal_end"
Append column: "s_cog_full"
Append column: "s_cog_window"
Append column: "proportion_voiced"
Append column: "proportion_voiced2"
Append column: "mean_hnr"
Append column: "speech_rate_pron"
Append column: "base_dur"
Append column: "num_syl_pron"
Append column: "num_cons_pron"
Append column: "speaker_sex"
Append column: "birth_year"
Append column: "next_phon_dur"
Append column: "prev_phon_dur"
Append column: "prev_mention"
Append column: "phrase_final"

Append column: "nn_start"
Append column: "nn_end"
Append column: "nn_start_score"
Append column: "nn_end_score"

wav_name$ = ""
ali_name$ = ""
n_inputlines = Get number of rows

for s_line from 1 to n_inputlines
    selectObject: "Table " + table_name$
    speaker$ = Get value: s_line, "speaker"
    cur_path$ = Get value: s_line, "wav"
    pre_name = rindex(cur_path$, "/")
    name_length = length(cur_path$)
    cur_name$ = right$(cur_path$, name_length - pre_name)
    cur_start$ = Get value: s_line, "chunk_start"
    cur_start = number(cur_start$)
    cur_end$ = Get value: s_line, "chunk_end"
    cur_end = number(cur_end$)
    appendInfoLine: "Working on ", cur_name$, " from ", cur_start$, " to ", cur_end$
    ali_name1$ = replace$(cur_path$, "/", "_", 2)
    c_channel$ = Get value: s_line, "chan"
    c_channel = number(c_channel$)

    # We only want to load the .wav and .awd file once
    if cur_name$ != wav_name$
        if s_line > 1
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
    chunk_i$ = Get value: s_line, "word_chunk_i"
    chunk_i = number(chunk_i$)
    # now let's get the corresponding index in the tg
    selectObject: "TextGrid " + wav_name$
    sound_end = Get end time

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
    # word_int now contains the interval number for the word containing the /s/
    oov_meta$ = Get label of interval: tier + 1, word_int
    # if words not aligned and chunk consists of a single word
    if oov_meta$ = "unintelligible"
        goto END
    endif
    # get index of s
    word_end = Get end time of interval: tier, word_int
    s_int = Get low interval at time: tier + 3, word_end

    # Calculate speech rate and number of syllables in s word
    s_word_start = Get start time of interval: tier, word_int
    s_word_int = Get high interval at time: tier + 3, s_word_start
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
    speech_rate_pron$ = string$(speech_rate_pron)
    # Get number of consonants preceding S and info on preceding phon
    num_prev_p_in_word = s_int - s_word_int
    num_cons_pron = 0
    if num_prev_p_in_word > 0 and s_int != 1
        for p from 1 to num_prev_p_in_word
            p_ind = s_int - p
            p_lab$ = Get label of interval: tier + 3, p_ind
            if p == 1
                if p_lab$ == ""
                    prev_phon_dur$ = "NA"
                else
                    prev_phon_start = Get start time of interval: tier + 3, p_ind
                    prev_phon_end = Get end time of interval: tier + 3, p_ind
                    prev_phon_dur = prev_phon_end - prev_phon_start
                    prev_phon_dur$ = fixed$(prev_phon_dur, 3)
                endif
            endif
            for vowel from 1 to 17
                if p_lab$ == vowels$[vowel]
                    goto NON_CONS
                endif
            endfor
            if p_lab$ == "SIL" or p_lab$ == "[SPN]"
                goto NON_CONS
            endif
            num_cons_pron += 1
        endfor
        label NON_CONS
        num_cons_pron$ = string$(num_cons_pron)
    else
        prev_phon_dur$ = "NA"
        num_cons_pron$ = "NA"
    endif
    # get next pronounced phon dur
    if s_int < end_int
        next_phon_pron$ = Get label of interval: tier + 3, s_int + 1
        next_phon_start = Get start time of interval: tier + 3, s_int + 1
        next_phon_end = Get end time of interval: tier + 3, s_int + 1
        next_phon_dur = next_phon_end - next_phon_start
        next_phon_dur$ = fixed$(next_phon_dur, 3)
        if next_phon_pron$ == ""
            next_phon_dur$ = "NA"
        endif
    else
        next_phon_dur$ = "NA"
    endif
    # get previous mention
    raw_ort$ = Get label of interval: tier, word_int
    if word_int == 1
        prev_mention$ = "NA"
    else
        strip_ort$ = replace_regex$(raw_ort$, "[.?]", "", 0)
        if s_word_start > 30
            search_start_t = s_word_start - 30
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
    # get s label
    s_lab$ = Get label of interval: tier + 3, s_int
    # voorlopig zit er nog geen reductie regel in het lexicon die ervoor zorgt dat s een z wordt
    if s_lab$ != "s"
        s_duration$ = "NA"
        kal_start$ = "NA"
        kal_end$ = "NA"
        s_cog_window$ = "NA"
        s_cog_full$ = "NA"
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
        nn_start$ = "NA"
        nn_end$ = "NA"
        nn_start_score$ = "NA"
        nn_end_score$ = "NA"
    else
        kal_start = Get start time of interval: tier + 3, s_int
        kal_start$ = fixed$(kal_start, 3)
        kal_end = Get end time of interval: tier + 3, s_int
        kal_end$ = fixed$(kal_end, 3)

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
        frag_file$ = frag_path$ + wav_name$ + "_" + c_channel$ + "_" + frag_start$ + "_" + fixed$(frag_end, 3) + "_s.IntensityTier"
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

            # finished nn boundary selection
            if nn_end$ == "NA"
                s_end = kal_end
            else
                s_end = nn_end
            endif
            s_start = kal_start
            s_duration = s_end - s_start
            if s_duration < 0.005
                s_end = kal_end
                s_duration = s_end - s_start
            endif
        else
            appendInfoLine: "SKIPPING"
            nn_start$ = "NA"
            nn_end$ = "NA"
            nn_start_score$ = "NA"
            nn_end_score$ = "NA"
            s_duration = kal_end - kal_start
            s_start = kal_start
            s_end = kal_end
        endif
#        appendInfoLine: s_duration
        s_duration$ = fixed$(s_duration, 3)
        selectObject: "TextGrid " + wav_name$
        w_start = Get start time of interval: tier, word_int
        base_dur = s_start - w_start
        base_dur$ = fixed$(base_dur, 3)
        selectObject: "LongSound " + wav_name$
        Extract part: s_start + ((1 - cog_window) / 2) * s_duration, s_end - ((1 - cog_window) / 2) * s_duration, "yes"
        Extract one channel: c_channel
        To Spectrum: "yes"
        # we should probably low pass filter the spectrum, so all cog measurements (from different components) are based on the same range
        # filter 0-4000 because comp c & d are sampled at 8000 Hz
        Filter (pass Hann band): 0, 4000, 100
        s_cog_window = Get centre of gravity: 1
        s_cog_window$ = string$(s_cog_window)
        Remove
        removeObject: "Sound " + wav_name$ + "_ch" + c_channel$
        removeObject: "Sound " + wav_name$
        # get COG of of entire dur
        selectObject: "LongSound " + wav_name$
        Extract part: s_start, s_end, "yes"
        Extract one channel: c_channel
        removeObject: "Sound " + wav_name$
        selectObject: "Sound " + wav_name$ + "_ch" + c_channel$
        Rename: wav_name$
        To Spectrum: "yes"
        Filter (pass Hann band): 0, 4000, 100
        s_cog_full = Get centre of gravity: 1
        s_cog_full$ = string$(s_cog_full)
        Remove
        removeObject: "Sound " + wav_name$

        # Get sound from 1 second before /s/ until 1 second after /s/ for accurate pitch/voicing measures
        selectObject: "LongSound " + wav_name$
        recording_dur = Get total duration
        if s_start > 1
            frag_start = s_start - 1
        else
            frag_start = 0
        endif
        if (recording_dur - s_end) > 1
            frag_end = s_end + 1
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
        runScript: preferencesDirectory$ + "/plugin_PraatZee/extractPeriodicity.praat", s_start, s_end, "no"

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
        num_periods = Get number of periods: s_start, s_end, 1 / pitch_ceiling, 1 / pitch_floor, 1.3
        if num_periods != 0
            mean_period = Get mean period: s_start, s_end, 0.0001, 0.02, 1.3
            proportion_voiced2 = (num_periods * mean_period) / s_duration
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
            elsif duration_needed + 0.001 > s_duration
#                appendInfoLine: "B"
                periods_per_win = s_duration * pitch_floor - 0.001
            endif
            while s_duration - periods_per_win * (1 / pitch_floor) < 0.011
                periods_per_win -= 1.3
                if periods_per_win < 1
                    goto NO_VOICING
                endif
            endwhile
#            appendInfoLine: time_step, " ", pitch_floor, " ", silence_thresh, " ", periods_per_win
            selectObject: "Sound " + wav_name$
            # maybe comment out above code and use standard 4.5 for periods_per_win
            To Harmonicity (cc): time_step, pitch_floor, silence_thresh, periods_per_win
            # Get info for the harmonicity matrix for the S
            runScript: preferencesDirectory$ + "/plugin_PraatZee/extractPeriodicity.praat", s_start, s_end, "no"
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

#        selectObject: "LongSound " + wav_name$
#        Extract part: s_start - buffer, s_end + buffer, "yes"
#        To Spectrogram: 0.005, 5000, 0.002, 250, "Gaussian"
#        time_step = s_duration / num_spectral_slices
#        for i from 0 to num_spectral_slices
#            selectObject: "Spectrogram " + wav_name$
#            time = s_start + time_step * i
#            To Spectrum (slice): time
#            To Matrix
#            Transpose
#            To TableOfReal
#            Remove column (index): 2
#            To Table: "rowLabel"
#            Remove column: "rowLabel"
#            Set column label (index): 1, "Pa_per_Hz"
#            num_bins = Get number of rows
#            Rename: wav_name$ + "_Pa"
#            removeObject: "Matrix " + wav_name$
#            removeObject: "Matrix " + wav_name$ + "_transposed"
#            removeObject: "TableOfReal " + wav_name$ + "_transposed"
#            selectObject: "Spectrum " + wav_name$
#            To Ltas (1-to-1)
#            To Matrix
#            Transpose
#            To TableOfReal
#            To Table: "rowLabel"
#            Remove column: "rowLabel"
#            Set column label (index): 1, "dB_per_Hz"
#            Rename: wav_name$ + "_dB"
#            removeObject: "Ltas " + wav_name$
#            removeObject: "Matrix " + wav_name$
#            removeObject: "Matrix " + wav_name$ + "_transposed"
#            removeObject: "TableOfReal " + wav_name$ + "_transposed"
#            removeObject: "Spectrum " + wav_name$
#            selectObject: "Table " + table_name$
#            speaker$ = Get value: s_line, "speaker"
#            sent_i$ = Get value: s_line, "sent_i"
#            word_sent_i$ = Get value: s_line, "word_sent_i"
#            word_ort$ = Get value: s_line, "word_ort"
#            next_phon$ = Get value: s_line, "next_phon"
#            next_phon_pron$ = Get value: s_line, "next_phon_pron"
#            prev_phon$ = Get value: s_line, "prev_phon"
#            prev_phon_pron$ = Get value: s_line, "prev_phon_pron"
#            word_pos$ = Get value: s_line, "word_pos"
#            word_class$ = Get value: s_line, "word_class"
#            type_of_s$ = Get value: s_line, "type_of_s"
#            prop_time = i * (1 / num_spectral_slices)
#            for j from 1 to num_bins
#                selectObject: "Table " + wav_name$ + "_Pa"
#                pa_per_hz = Get value: j, "Pa_per_Hz"
#                selectObject: "Table " + wav_name$ + "_dB"
#                db_per_hz = Get value: j, "dB_per_Hz"
#                selectObject: "Table spectral_info"
#                Append row
#                num_rows = Get number of rows
#                Set string value: num_rows, "wav", cur_path$
#                Set string value: num_rows, "speaker", speaker$
#                Set string value: num_rows, "speaker_sex", speaker_sex$
#                Set string value: num_rows, "birth_year", birth_year$
#                Set string value: num_rows, "chunk_start", cur_start$
#                Set string value: num_rows, "chunk_end", cur_end$
#                Set string value: num_rows, "word_chunk_i", chunk_i$
#                Set string value: num_rows, "sent_i", sent_i$
#                Set string value: num_rows, "sent_i", sent_i$
#                Set string value: num_rows, "word_sent_i", word_sent_i$
#                Set string value: num_rows, "word_ort", word_ort$
#                Set string value: num_rows, "next_phon", next_phon$
#                Set string value: num_rows, "next_phon_pron", next_phon_pron$
#                Set string value: num_rows, "prev_phon", prev_phon$
#                Set string value: num_rows, "prev_phon_pron", prev_phon_pron$
#                Set string value: num_rows, "word_pos", word_pos$
#                Set string value: num_rows, "word_class", word_class$
#                Set string value: num_rows, "type_of_s", type_of_s$
#                Set string value: num_rows, "base_dur", base_dur$
#                Set string value: num_rows, "speech_rate_pron", speech_rate_pron$
#                Set string value: num_rows, "num_syl_pron", num_syl_pron$
#                Set string value: num_rows, "mean_hnr", mean_hnr$
#                Set numeric value: num_rows, "time", prop_time
#                Set numeric value: num_rows, "freq_bin", j
#                Set numeric value: num_rows, "Pa_per_Hz", pa_per_hz
#                Set numeric value: num_rows, "dB_per_Hz", db_per_hz
#            endfor
#            removeObject: "Table " + wav_name$ + "_Pa"
#            removeObject: "Table " + wav_name$ + "_dB"
#        endfor
#        removeObject: "Spectrogram " + wav_name$
#        removeObject: "Sound " + wav_name$
    endif
    # now lets add duration, voicing and cog data to table
#    appendInfoLine: s_duration, " ", s_cog_full, " ", s_cog_window, " ", proportion_voiced
    selectObject: "Table " + table_name$
    Set string value: s_line, "s_dur", s_duration$
    Set string value: s_line, "kal_start", kal_start$
    Set string value: s_line, "kal_end", kal_end$
    Set string value: s_line, "nn_start", nn_start$
    Set string value: s_line, "nn_end", nn_end$
    Set string value: s_line, "nn_start_score", nn_start_score$
    Set string value: s_line, "nn_end_score", nn_end_score$
    Set string value: s_line, "s_cog_full", s_cog_full$
    Set string value: s_line, "s_cog_window", s_cog_window$
    Set string value: s_line, "proportion_voiced", proportion_voiced$
    Set string value: s_line, "proportion_voiced2", proportion_voiced2$
    Set string value: s_line, "base_dur", base_dur$
    Set string value: s_line, "speech_rate_pron", speech_rate_pron$
    Set string value: s_line, "num_syl_pron", num_syl_pron$
    Set string value: s_line, "mean_hnr", mean_hnr$
    Set string value: s_line, "num_cons_pron", num_cons_pron$
    Set string value: s_line, "speaker_sex", speaker_sex$
    Set string value: s_line, "birth_year", birth_year$
    Set string value: s_line, "next_phon_dur", next_phon_dur$
    Set string value: s_line, "prev_phon_dur", prev_phon_dur$
    Set string value: s_line, "prev_mention", prev_mention$
    Set string value: s_line, "phrase_final", phrase_final$
    label END
endfor

selectObject: "Table " + table_name$
Save as comma-separated file: tens_path$ + table_name$ + "_static.csv"
#selectObject: "Table spectral_info"
#Save as comma-separated file: tens_path$ + table_name$ + "_dynamic.csv"
