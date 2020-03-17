tens_path$ = "/vol/tensusers/timzee/cgn/"
folder_in$ = "/vol/bigdata2/corpora2/CGN2/data/annot/text/ort/comp-d/vl/"
folder_out$ = "Annotations/ort/comp-d/"

Create Strings as file list: "fileList", folder_in$ + "*.ort.gz"

n_strings = Get number of strings

for i from 1 to n_strings
    selectObject: "Strings fileList"
    file_n$ = Get string: i
    appendInfoLine: file_n$
    f_name$ = left$(file_n$, 12)

    ## Load TextGrids
    runSystem_nocheck: "cp -f " + folder_in$ + file_n$ + " " + tens_path$ + "cgn_annot/"
    runSystem_nocheck: "gunzip -f " + tens_path$ + "cgn_annot/" + file_n$
    Read from file: tens_path$ + "cgn_annot/" + f_name$
    runSystem_nocheck: "rm -f " + tens_path$ + "cgn_annot/" + f_name$

    Save as text file: tens_path$ + folder_out$ + f_name$
    Remove
endfor
