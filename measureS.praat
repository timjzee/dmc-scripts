prop$ = Report system properties
os$ = extractWord$(prop$, newline$)
if os$ == "macintosh"
    tens_path$ = "/Volumes/tensusers/timzee/cgn/"
    cgn_path$ = "/Volumes/bigdata2/corpora2/CGN2/data/audio/wav/comp-"
else
    tens_path$ = "/vol/tensusers/timzee/cgn/"
    cgn_path$ = "/vol/bigdata/corpora2/CGN2/data/audio/wav/comp-"
endif


procedure makeKaldiTG
    selectObject: "Table " + ali_name$
    word_end = -1
    n_ali_lines = Get number of rows
    for ali_line from 1 to n_ali_lines
        selectObject: "Table " + ali_name$
        ali_lab$ = Get value: ali_line, "phone"
        underscore_index = index(ali_lab$, "_")
        if underscore_index == 0
            phon$ = ali_lab$
            phon_tag$ = "S"
        else
            phon$ = left$(ali_lab$, underscore_index - 1)
            phon_tag$ = right$(ali_lab$, length(ali_lab$) - underscore_index)
        endif
        start_time$ = Get value: ali_line, "start"
        start_time = number(start_time$)
        if ali_line == 1 and (cur_start + start_time) != 0
            selectObject: "TextGrid " + wav_name$
            Insert boundary: 1, cur_start + start_time
            selectObject: "Table " + ali_name$
        endif
        phon_dur$ = Get value: ali_line, "dur"
        phon_dur = number(phon_dur$)
        end_time = cur_start + start_time + phon_dur
        selectObject: "TextGrid " + wav_name$
        Insert boundary: 1, end_time
        phon_interval = Get low interval at time: 1, end_time
        Set interval text: 1, phon_interval, phon$
        if (phon_tag$ == "B") or (phon_tag$ == "S")
            word_start = cur_start + start_time
            word_tran$ = ""
            if word_start > word_end and word_start != 0
                Insert boundary: 2, word_start
            endif
        endif
        word_tran$ = word_tran$ + phon$
        if (phon_tag$ == "E") or (phon_tag$ == "S")
            word_end = end_time
            Insert boundary: 2, word_end
            word_interval = Get low interval at time: 2, word_end
            Set interval text: 2, word_interval, word_tran$
        endif
    endfor
endproc

cog_window = 0.8
num_spectral_slices = 4
buffer = 0.5
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

Create Table with column names: "spectral_info", 0, "wav speaker speaker_sex birth_year chunk_start chunk_end word_chunk_i sent_i word_sent_i word_ort next_phon next_phon_pron prev_phon_pron word_pos word_class type_of_s base_dur speech_rate_pron num_syl_pron mean_hnr time freq_bin Pa_per_Hz"

Read Table from comma-separated file: tens_path$ + "cgn_praat_1_part.csv"
table_name$ = selected$("Table")
Append column: "s_dur"
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
Append column: "next_phon_pron"
Append column: "next_phon_dur"
Append column: "prev_phon_pron"
Append column: "prev_phon_dur"
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
    ali_name1$ = replace$(cur_path$, "/", "_", 2)
    c_channel$ = Get value: s_line, "chan"
    c_channel = number(c_channel$)
    # Louis turned all 0 channels to 1; all 1 channels to 2; and >1 channels to 1
    if (c_channel == 1) or (c_channel == 2)
        ali_name2$ = "_" + string$(c_channel) + "_"
    else
        ali_name2$ = "_1_"
    endif
    ali_file$ = ali_name1$ + ali_name2$ + cur_start$ + "_" + cur_end$
    # We only want to load & convert the .ali file to a tg once
    if ali_name$ != replace$(ali_file$, ".", "_", 0)
        if s_line > 1
            selectObject: "Table " + ali_name$
            Remove
            selectObject: "TextGrid " + wav_name$
            Remove
        endif
        if fileReadable(tens_path$ + "KALDI_output/CGN_expanded_lex_v4/" + ali_file$ + ".ali")
            Read Table from tab-separated file: tens_path$ + "KALDI_output/CGN_expanded_lex_v4/" + ali_file$ + ".ali"
            ali_name$ = replace$(ali_file$, ".", "_", 0)
            # We only want to load a .wav file once
            if cur_name$ != wav_name$
                if s_line > 1
                    selectObject: "LongSound " + wav_name$
                    Remove
                endif
                Open long sound file: cgn_path$ + cur_path$ + ".wav"
                wav_name$ = selected$("LongSound")
            endif
            selectObject: "LongSound " + wav_name$
            To TextGrid: "phones words", ""
            @makeKaldiTG
        else
            # make dummy tg and table so script won't crash trying to delete them
            Create TextGrid: 0, 1, wav_name$, ""
            ali_name$ = replace$(ali_file$, ".", "_", 0)
            Create Table without column names: ali_name$, 1, 1
            s_duration$ = "NA"
            s_cog_full$ = "NA"
            s_cog_window$ = "NA"
            proportion_voiced$ = "NA"
            proportion_voiced2$ = "NA"
            base_dur$ = "NA"
            speech_rate_pron$ = "NA"
            num_syl_pron$ = "NA"
            mean_hnr$ = "NA"
            num_cons_pron$ = "NA"
            speaker_sex$ = "NA"
            birth_year$ = "NA"
            next_phon_pron$ = "NA"
            next_phon_dur$ = "NA"
            prev_phon_pron$ = "NA"
            prev_phon_dur$ = "NA"
            goto END
        endif
    endif
    selectObject: "Table " + table_name$
    chunk_i$ = Get value: s_line, "word_chunk_i"
    chunk_i = number(chunk_i$)
    # now let's get the corresponding index in the tg
    selectObject: "TextGrid " + wav_name$
    num_ints = Get number of intervals: 2
    word_counter = 0
    word_int = 0
    while word_counter < chunk_i
        word_int += 1
        int_lab$ = Get label of interval: 2, word_int
        if int_lab$ != "" and int_lab$ != "SIL"
            word_counter += 1
        endif
    endwhile
    # word_int now contains the interval number for the word containing the /s/
    # get index of s
    word_end = Get end time of interval: 2, word_int
    s_int = Get low interval at time: 1, word_end
    # Calculate speech rate and number of syllables in s word
    s_word_start = Get start time of interval: 2, word_int
    s_word_int = Get high interval at time: 1, s_word_start
    num_syl_pron = 0
    num_syl = 0
    first_phon = 1
    num_phons = Get number of intervals: 1
    for int from 1 to num_phons
        int_lab$ = Get label of interval: 1, int
        if int_lab$ != "" and int_lab$ != "SIL" and int_lab$ != "[SPN]"
            if first_phon
                start_phon = Get start time of interval: 1, int
                first_phon = 0
            endif
            final_phon = Get end time of interval: 1, int
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
    num_cons_pron = 0
    for p from 1 to s_int - s_word_int
        p_ind = s_int - p
        p_lab$ = Get label of interval: 1, p_ind
        if p == 1
            if p_lab$ == ""
                prev_phon_pron$ = "NA"
                prev_phon_dur$ = "NA"
            else
                prev_phon_pron$ = p_lab$
                prev_phon_start = Get start time of interval: 1, p_ind
                prev_phon_end = Get end time of interval: 1, p_ind
                prev_phon_dur = prev_phon_end - prev_phon_start
                prev_phon_dur$ = string$(prev_phon_dur)
            endif
        endif
        for vowel from 1 to 17
            if p_lab$ == vowels$[vowel]
                goto VOWEL_FOUND
            endif
        endfor
        num_cons_pron += 1
    endfor
    label VOWEL_FOUND
    num_cons_pron$ = string$(num_cons_pron)
    # get next pronounced phon
    if s_int < num_phons
        next_phon_pron$ = Get label of interval: 1, s_int + 1
        next_phon_start = Get start time of interval: 1, s_int + 1
        next_phon_end = Get end time of interval: 1, s_int + 1
        next_phon_dur = next_phon_end - next_phon_start
        next_phon_dur$ = string$(next_phon_dur)
        if next_phon_pron$ == ""
            next_phon_pron$ = "NA"
            next_phon_dur$ = "NA"
        endif
    else
        next_phon_pron$ = "NA"
        next_phon_dur$ = "NA"
    endif
    s_lab$ = Get label of interval: 1, s_int
    # voorlopig zit er nog geen reductie regel in het lexicon die ervoor zorgt dat s een z wordt
    if s_lab$ != "s"
        s_duration$ = "NA"
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
        next_phon_pron$ "NA"
        next_phon_dur$ = "NA"
        prev_phon_pron$ = "NA"
        prev_phon_dur$ = "NA"
    else
        s_start = Get start time of interval: 1, s_int
        s_end = Get end time of interval: 1, s_int
        s_duration = s_end - s_start
        s_duration$ = string$(s_duration)
        w_start = Get start time of interval: 2, word_int
        base_dur = s_start - w_start
        base_dur$ = string$(base_dur)
        selectObject: "LongSound " + wav_name$
        Extract part: s_start + ((1 - cog_window) / 2) * s_duration, s_end - ((1 - cog_window) / 2) * s_duration, "yes"
        Extract one channel: c_channel
        To Spectrum: "yes"
        # we should probably low pass filter the spectrum, so all cog measurements (from different components) are based on the same range
        s_cog_window = Get centre of gravity: 1
        s_cog_window$ = string$(s_cog_window)
        Remove
        selectObject: "Sound " + wav_name$ + "_ch" + c_channel$
        Remove
        selectObject: "Sound " + wav_name$
        Remove
        selectObject: "LongSound " + wav_name$
        Extract part: s_start, s_end, "yes"
        Extract one channel: c_channel
        removeObject: "Sound " + wav_name$
        selectObject: "Sound " + wav_name$ + "_ch" + c_channel$
        Rename: wav_name$
        To Spectrum: "yes"
        s_cog_full = Get centre of gravity: 1
        s_cog_full$ = string$(s_cog_full)
        Remove
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
        time_step = Get time step
        voiced_frames = Count voiced frames
        all_frames = Get number of frames
        proportion_voiced = voiced_frames / all_frames
        proportion_voiced$ = string$(proportion_voiced)
        Remove
        selectObject: "Sound " + wav_name$
        To PointProcess (periodic, cc): pitch_floor, pitch_ceiling
        num_periods = Get number of periods: 0, 0, 0.0001, 0.02, 1.3
        if num_periods != 0
            mean_period = Get mean period: 0, 0, 0.0001, 0.02, 1.3
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
                periods_per_win = num_periods - 0.001
                if s_duration - periods_per_win * (1 / pitch_floor) < 0.01
                    periods_per_win -= 1
                    if periods_per_win == 0
                        goto NO_VOICING
                    endif
                endif
            elsif duration_needed + 0.001 > s_duration
                periods_per_win = s_duration * pitch_floor - 0.001
            endif
            selectObject: "Sound " + wav_name$
            To Harmonicity (cc): time_step, pitch_floor, silence_thresh, periods_per_win
            mean_hnr = Get mean: 0, 0
            mean_hnr$ = string$(mean_hnr)
            Remove
        else
            # if 1% energy periodic and 99% noise: 10*log10(1/99) = -20
            label NO_VOICING
            mean_hnr$ = "NA"
        endif
        selectObject: "Sound " + wav_name$
        Remove
        selectObject: "LongSound " + wav_name$
        Extract part: s_start - buffer, s_end + buffer, "yes"
        To Spectrogram: 0.005, 5000, 0.002, 250, "Gaussian"
        time_step = s_duration / num_spectral_slices
        for i from 0 to num_spectral_slices
            selectObject: "Spectrogram " + wav_name$
            time = s_start + time_step * i
            To Spectrum (slice): time
            To Matrix
            Transpose
            To TableOfReal
            Remove column (index): 2
            To Table: "rowLabel"
            Remove column: "rowLabel"
            Set column label (index): 1, "Pa_per_Hz"
            num_bins = Get number of rows
            selectObject: "Spectrum " + wav_name$
            Remove
            selectObject: "Matrix " + wav_name$
            Remove
            selectObject: "Matrix " + wav_name$ + "_transposed"
            Remove
            selectObject: "TableOfReal " + wav_name$ + "_transposed"
            Remove
            selectObject: "Table " + table_name$
            speaker$ = Get value: s_line, "speaker"
            sent_i$ = Get value: s_line, "sent_i"
            word_sent_i$ = Get value: s_line, "word_sent_i"
            word_ort$ = Get value: s_line, "word_ort"
            next_phon$ = Get value: s_line, "next_phon"
            word_pos$ = Get value: s_line, "word_pos"
            word_class$ = Get value: s_line, "word_class"
            type_of_s$ = Get value: s_line, "type_of_s"
            prop_time = i * (1 / num_spectral_slices)
            for j from 1 to num_bins
                selectObject: "Table " + wav_name$ + "_transposed"
                pa_per_hz = Get value: j, "Pa_per_Hz"
                selectObject: "Table spectral_info"
                Append row
                num_rows = Get number of rows
                Set string value: num_rows, "wav", cur_path$
                Set string value: num_rows, "speaker", speaker$
                Set string value: num_rows, "speaker_sex", speaker_sex$
                Set string value: num_rows, "birth_year", birth_year$
                Set string value: num_rows, "chunk_start", cur_start$
                Set string value: num_rows, "chunk_end", cur_end$
                Set string value: num_rows, "word_chunk_i", chunk_i$
                Set string value: num_rows, "sent_i", sent_i$
                Set string value: num_rows, "sent_i", sent_i$
                Set string value: num_rows, "word_sent_i", word_sent_i$
                Set string value: num_rows, "word_ort", word_ort$
                Set string value: num_rows, "next_phon", next_phon$
                Set string value: num_rows, "next_phon_pron", next_phon_pron$
                Set string value: num_rows, "prev_phon_pron", prev_phon_pron$
                Set string value: num_rows, "word_pos", word_pos$
                Set string value: num_rows, "word_class", word_class$
                Set string value: num_rows, "type_of_s", type_of_s$
                Set string value: num_rows, "base_dur", base_dur$
                Set string value: num_rows, "speech_rate_pron", speech_rate_pron$
                Set string value: num_rows, "num_syl_pron", num_syl_pron$
                Set string value: num_rows, "mean_hnr", mean_hnr$
                Set numeric value: num_rows, "time", prop_time
                Set numeric value: num_rows, "freq_bin", j
                Set numeric value: num_rows, "Pa_per_Hz", pa_per_hz
            endfor
            selectObject: "Table " + wav_name$ + "_transposed"
            Remove
        endfor
        selectObject: "Spectrogram " + wav_name$
        Remove
        selectObject: "Sound " + wav_name$
        Remove
    endif
    # now lets add duration, voicing and cog data to table
    appendInfoLine: s_duration, " ", s_cog_full, " ", s_cog_window, " ", proportion_voiced
    label END
    selectObject: "Table " + table_name$
    Set string value: s_line, "s_dur", s_duration$
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
    Set string value: s_line, "next_phon_pron", next_phon_pron$
    Set string value: s_line, "next_phon_dur", next_phon_dur$
    Set string value: s_line, "prev_phon_pron", prev_phon_pron$
    Set string value: s_line, "prev_phon_dur", prev_phon_dur$
endfor

selectObject: "Table " + table_name$
Save as comma-separated file: tens_path$ + table_name$ + "_static.csv"
selectObject: "Table spectral_info"
Save as comma-separated file: tens_path$ + table_name$ + "_dynamic.csv"
