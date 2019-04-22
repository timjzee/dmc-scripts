form
    word input_path /vol/bigdata/corpora2/CGN2/data/audio/wav/comp-d/nl/
    word output_path /vol/tensusers/timzee/cgn/mono_comp-d/nl/
endform

Create Strings as file list: "fileList", input_path$ + "*.wav"

num_files = Get number of strings
for i from 1 to num_files
    selectObject: "Strings fileList"
    fn$ = Get string: i
    appendInfoLine: fn$
    Read from file: input_path$ + fn$
    n$ = selected$("Sound")
    Convert to mono
    Save as WAV file: output_path$ + fn$
    Remove
    removeObject: "Sound " + n$
endfor
