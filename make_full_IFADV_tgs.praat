folder_in$ = "/Volumes/tensusers/timzee/IFADVcorpus/Annotations/ort/"
folder_out$ = "/Volumes/tensusers/timzee/IFADVcorpus/Annotations/ort2/"

Create Strings as file list: "fileList", folder_in$ + "*.ort"

n_strings = Get number of strings

for i from 1 to n_strings
    selectObject: "Strings fileList"
    file_n$ = Get string: i
    Read from file: folder_in$ + file_n$
    Save as text file: folder_out$ + file_n$
    Remove
endfor
