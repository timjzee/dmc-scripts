form Command line parameters
    word cur_path 1
    integer chan 1
    integer tier 1
endform

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

Read from file: cgn_path$ + "audio/wav/comp-" + cur_path$ + ".wav"
wav_name$ = selected$("Sound")
runSystem_nocheck: "cp -f " + cgn_path$ + "annot/text/ort/comp-" + cur_path$ + ".ort.gz " + tens_path$
runSystem_nocheck: "gunzip -f " + tens_path$ + wav_name$ + ".ort.gz"
Read from file: tens_path$ + wav_name$ + ".ort"
runSystem_nocheck: "rm -f " + tens_path$ + wav_name$ + ".ort"
selectObject: "Sound " + wav_name$
Extract one channel: chan
runScript: praat_pref$ + "plugin_VocalToolkit/compressor.praat", 50
Rename: wav_name$ + "_ch" + string$(chan)
To Harmonicity (cc): 0.01, 75, 0.1, 4.5
selectObject: "TextGrid " + wav_name$
num_intervals = Get number of intervals: tier
num_speech_int = 0
cumul_hnr = 0
for int from 1 to num_intervals
    selectObject: "TextGrid " + wav_name$
    int_lab$ = Get label of interval: tier, int
    if int_lab$ != ""
        num_speech_int += 1
        int_start = Get start time of interval: tier, int
        int_end = Get end time of interval: tier, int
        selectObject: "Harmonicity " + wav_name$ + "_ch" + string$(chan)
        hnr = Get mean: int_start, int_end
        if hnr == undefined
            hnr = -20
        endif
        cumul_hnr += hnr
    endif
endfor
average_hnr = cumul_hnr / num_speech_int
writeInfoLine: average_hnr
