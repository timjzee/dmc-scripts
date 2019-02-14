prop$ = Report system properties
os$ = extractWord$(prop$, newline$)
if os$ == "macintosh"
    tens_path$ = "/Volumes/tensusers/timzee/IFAcorpus/"
else
    tens_path$ = "/vol/tensusers/timzee/IFAcorpus/"
endif

Read Table from comma-separated file: tens_path$ + "IFA_sentences_praat_v2.txt"
num_sentences = Get number of rows
for sent from 1 to num_sentences
    selectObject: "Table IFA_sentences_praat_v2"
    wav_path$ = Get value: sent, "wav"
    b_extract$ = Get value: sent, "begin"
    b_extract = number(b_extract$)
    e_extract$ = Get value: sent, "end"
    e_extract = number(e_extract$)
    final_slash = rindex(wav_path$, "/")
    length_wav = length(wav_path$)
    end_ind = length_wav - final_slash - 7
    sentence$ = mid$(wav_path$, final_slash + 1, end_ind)
    speaker$ = left$(sentence$, 4)
    tg_path$ = tens_path$ + "SLcorpus/Labels/validation_tim2/" + speaker$ + "/ASPEX/"
    strings = Create Strings as file list: "textgrids", tg_path$ + sentence$ + "_*"
    num_tgs = Get number of strings
    for tg from 1 to num_tgs
        selectObject: strings
        tg_file$ = Get string: tg
        final_us = rindex(tg_file$, "_")
        annotator$ = mid$(tg_file$, final_us + 1, 2)
        if annotator$ != "KA"
            appendInfoLine: "Working on " + tg_file$
            Read from file: tg_path$ + tg_file$
            tg_name$ = selected$("TextGrid")
            Extract part: b_extract, e_extract, "no"
            Save as short text file: tg_path$ + tg_file$
            selectObject: "TextGrid " + tg_name$
            Remove
            selectObject: "TextGrid " + tg_name$ + "_part"
            Remove
        endif
    endfor
    selectObject: strings
    Remove
endfor
