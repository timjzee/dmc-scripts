Text writing preferences: "UTF-8"
start_line = 1
end_line = -1
# 'Harmonicity ' or 'Intensity '
analysis_type$ = "Intensity "
use_silences = 1

prop$ = Report system properties
os$ = extractWord$(prop$, newline$)
if os$ == "macintosh"
    tens_path$ = "/Volumes/tensusers/timzee/cgn/"
    cgn_path$ = "/Volumes/bigdata2/corpora2/CGN2/data/"
    praat_pref$ = "/Users/tim/Library/Preferences/Praat Prefs/"
else
    tens_path$ = "/vol/tensusers/timzee/cgn/"
    cgn_path$ = "/vol/bigdata/corpora2/CGN2/data/"
    praat_pref$ = "/home/timzee/.praat-dir/"
endif

writeFileLine: tens_path$ + "cgn_index_o_final2" + ".txt", "wav,chan,from,to,ort,tier"
appendInfoLine: "Loading CGN index"
Read Table from comma-separated file: tens_path$ + "cgn_index_o.txt"
Rename: "cgn_index"
Set column label (label): "chan", "tier"
Append column: "chan"

if end_line == -1
    end_line = Get number of rows
endif
wav_name$ = ""
for s_line from start_line to end_line
    t = 0
    selectObject: "Table cgn_index"
    c_tier$ = Get value: s_line, "tier"
    c_tier = number(c_tier$)
    c_start$ = Get value: s_line, "from"
    c_start = number(c_start$)
    c_end$ = Get value: s_line, "to"
    c_end = number(c_end$)
    c_ort$ = Get value: s_line, "ort"
    cur_path$ = Get value: s_line, "wav"
    pre_name = rindex(cur_path$, "/")
    name_length = length(cur_path$)
    cur_name$ = right$(cur_path$, name_length - pre_name)
    if cur_name$ != wav_name$
        if s_line > start_line
            removeObject: "Sound " + wav_name$
            removeObject: "TextGrid " + wav_name$
            removeObject: "Table hnr_values"
            if num_channels > 1
                for i from 1 to num_channels
                    removeObject: "Sound " + wav_name$ + "_ch" + string$(i)
                    removeObject: analysis_type$ + wav_name$ + "_ch" + string$(i)
                endfor
            endif
        endif
        compress = 0
        attempts = 1
        Read from file: cgn_path$ + "audio/wav/comp-" + cur_path$ + ".wav"
        wav_name$ = selected$("Sound")
        appendInfoLine: "Working on " + wav_name$ + " Line " + string$(s_line)
        runSystem_nocheck: "cp -f " + cgn_path$ + "annot/text/ort/comp-" + cur_path$ + ".ort.gz " + tens_path$
        runSystem_nocheck: "gunzip -f " + tens_path$ + wav_name$ + ".ort.gz"
        Read from file: tens_path$ + wav_name$ + ".ort"
        runSystem_nocheck: "rm -f " + tens_path$ + wav_name$ + ".ort"
        selectObject: "Sound " + wav_name$
        num_channels = Get number of channels
        Create Table without column names: "hnr_values", num_channels, 1
        selectObject: "Sound " + wav_name$
        if num_channels == 1
            best_channel = 1
        else
            Extract all channels
            label COMPRESS
            appendInfoLine: "Attempt 'attempts'"
            if compress
                runScript: praat_pref$ + "plugin_VocalToolkit/compressor.praat", 50
                if analysis_type$ == "Harmonicity "
                    To Harmonicity (cc): 0.01, 75, 0.1, 4.5
                else
                    To Intensity: 100, 0, "yes"
                endif
                for c from 1 to num_channels
                    selectObject: analysis_type$ + wav_name$ + "_ch" + string$(c) + "_compressor_50"
                    Rename: wav_name$ + "_ch" + string$(c)
                    removeObject: "Sound " + wav_name$ + "_ch" + string$(c) + "_compressor_50"
                endfor
            else
                if analysis_type$ == "Harmonicity "
                    To Harmonicity (cc): 0.01, 75, 0.1, 4.5
                else
                    To Intensity: 100, 0, "yes"
                endif
            endif
            selectObject: "TextGrid " + wav_name$
            num_tiers = Get number of tiers
            for tier from 1 to num_tiers
                selectObject: "TextGrid " + wav_name$
                tier_name$ = Get tier name: tier
                if left$(tier_name$, 1) == "N" or left$(tier_name$, 1) == "V"
                    speaker$ = tier_name$
                    selectObject: "Table hnr_values"
                    col_lab$ = Get column label: 1
                    if col_lab$ == ""
                        Set column label (index): 1, speaker$
                    else
                        Append column: speaker$
                    endif
                    for i from 1 to num_channels
                        selectObject: "TextGrid " + wav_name$
                        num_intervals = Get number of intervals: tier
                        num_speech_int = 0
                        num_speech_int_sil = 0
                        cumul_hnr = 0
                        cumul_hnr_sil = 0
                        for int from 1 to num_intervals
                            selectObject: "TextGrid " + wav_name$
                            int_lab$ = Get label of interval: tier, int
                            if int_lab$ != ""
                                num_speech_int += 1
                                int_start = Get start time of interval: tier, int
                                int_end = Get end time of interval: tier, int
                                selectObject: analysis_type$ + wav_name$ + "_ch" + string$(i)
                                if analysis_type$ == "Harmonicity "
                                    hnr = Get mean: int_start, int_end
                                else
                                    hnr = Get mean: int_start, int_end, "dB"
                                endif
                                if hnr == undefined
                                    hnr = -20
                                endif
                                cumul_hnr += hnr
                            elsif int_lab$ == "" and use_silences == 1
                                num_speech_int_sil += 1
                                int_start = Get start time of interval: tier, int
                                int_end = Get end time of interval: tier, int
                                selectObject: analysis_type$ + wav_name$ + "_ch" + string$(i)
                                if analysis_type$ == "Harmonicity "
                                    hnr = Get mean: int_start, int_end
                                else
                                    hnr = Get mean: int_start, int_end, "dB"
                                endif
                                if hnr == undefined
                                    hnr = -20
                                endif
                                cumul_hnr_sil += hnr
                            endif
                        endfor
                        average_hnr = cumul_hnr / num_speech_int
                        if use_silences == 1
                            average_hnr_sil = cumul_hnr_sil / num_speech_int_sil
                            average_hnr = average_hnr - average_hnr_sil
                        endif
                        selectObject: "Table hnr_values"
                        Set numeric value: i, speaker$, average_hnr
                    endfor
                endif
            endfor
        endif
    endif
    # check if c_tier is correct
    if t == 0
        selectObject: "TextGrid " + wav_name$
        n_tiers = Get number of tiers
        for t from 1 to n_tiers
            c_i = Get high interval at time: t, c_start
            c_l$ = Get label of interval: t, c_i
            c_e = Get end time of interval: t, c_i
            c_s = Get start time of interval: t, c_i
#            appendInfoLine: c_l$, c_ort$
            if c_l$ == c_ort$ and c_s == c_start and c_e == c_end
                # change interval text to prevent problems if multiple speakers say the same thing at the same time
                Set interval text: t, c_i, "_x_X_x_"
                goto FOUNDTIER
            endif
        endfor
        label FOUNDTIER
        if t != c_tier
            c_tier = t
            c_tier$ = string$(t)
        endif
    endif
    if num_channels != 1
        selectObject: "TextGrid " + wav_name$
        cur_speaker$ = Get tier name: c_tier
        selectObject: "Table hnr_values"
        max_hnr = Get maximum: cur_speaker$
        min_hnr = Get minimum: cur_speaker$
        diff_hnr = abs(min_hnr - max_hnr)
        if (max_hnr < -2 or diff_hnr < 1) and attempts < 2
            attempts += 1
            compress = 1
            removeObject: "Table hnr_values"
            Create Table without column names: "hnr_values", num_channels, 1
            selectObject: "Sound " + wav_name$ + "_ch1"
            for c from 2 to num_channels
                plusObject: "Sound " + wav_name$ + "_ch" + string$(c)
            endfor
            goto COMPRESS
        endif
        best_channel = Search column: cur_speaker$, string$(max_hnr)
    endif
    selectObject: "Table cgn_index"
    Set numeric value: s_line, "chan", best_channel
    appendFileLine: tens_path$ + "cgn_index_o_final2" + ".txt", cur_path$ + ",", best_channel, "," + c_start$ + "," + c_end$ + "," + c_ort$ + "," + c_tier$
endfor
