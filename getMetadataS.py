import gzip
import re


tensusers_files = "/vol/tensusers/timzee/cgn/"
cgn_files = "/vol/bigdata/corpora2/CGN2/data/annot/"

header = "component,language,filename,chunk_id,word_id,channel,chunk_start,chunk_end,word_index,word_ort,word_phon,next_phon,word_pos\n"

with open(tensusers_files + "s_words.csv", "w") as f:
    f.write(header)

with open(tensusers_files + "cgn_index_171218_pron_s.txt", "r") as f:
    s_lines = f.readlines()


line_counter = 0
filename_old = ""
for line in s_lines:
    line_counter += 1
    if line_counter % 1000 == 0:
        print(line_counter / len(s_lines) * 100, " percent")
    line_list = line.split(",")
    component = "comp-" + line_list[0].split("/")[0]
    language, filename = line_list[0].split("/")[1:3]
    channel, chunk_start, chunk_end = line_list[1:4]
    if filename != filename_old:
        with gzip.open(cgn_files + "text/plk/" + component + "/" + language + "/" + filename + ".plk.gz", "rt", encoding='latin-1') as f:
            plk_text = f.read()
        with gzip.open(cgn_files + "xml/skp-ort/" + component + "/" + language + "/" + filename + ".skp.gz", "rt", encoding='ascii') as f:
            skp_text = f.read()
        plk_chunk_list = re.split(r"<.*>\n", plk_text)[1:]
        plk_lines_list = [chunk.split("\n") for chunk in plk_chunk_list]
        plk_marker_list = re.findall(r"<.*>", plk_text)
        plk_id_list = [re.search(r'(?<=id=")[0-9]+', marker).group(0) for marker in plk_marker_list]
        assert len(plk_id_list) == len(plk_lines_list)
        plk_dict = dict(zip(plk_id_list, plk_lines_list))
    
    phon_list = re.sub(r'\n', "", line_list[5].split(" ++ "))
    counter = 0
    for word_phon in phon_list:
        counter += 1
        if word_phon[-1] == "s":
#            print(filename, chunk_start, word_phon)
            skp_marker_g = re.search(r'<t.*tb="{}" te="{}.*(s|x|z|sch|ce)"/>'.format(chunk_start, chunk_end), skp_text, flags=re.I)
            if skp_marker_g == None:
                continue
            skp_marker = skp_marker_g.group(0)
            skp_text = re.sub(r'<t.*tb="{}" te="{}.*(s|x|z|sch|ce)"/>'.format(chunk_start, chunk_end), '', skp_text, count=1, flags=re.I)
            chunk_id_string = re.search(r'(?<=ref=")[a-z0-9.]+', skp_marker).group(0)
            chunk_id = chunk_id_string.split(".")[1]
            word_index = int(chunk_id_string.split(".")[2]) - 1
#            word_index += counter
            assert chunk_id in plk_dict
#            assert len(phon_list) == len(plk_dict[chunk_start]) + 1
            if word_index > len(plk_dict[chunk_id]):
                print("mislabelled chunk, continuing on...")
                continue
            word_info = plk_dict[chunk_id][word_index].split("\t")
            if len(word_info) < 3:
                print(word_phon, filename, chunk_id, word_index, "had an indexing error, continuing on...")
                continue
            word_ort = word_info[0]
            word_pos = re.sub(r',', ';', word_info[1])
            next_phon = phon_list[counter] if counter < len(phon_list) else "NA"
            with open(tensusers_files + "s_words.csv", "a") as f:
                f.write(",".join([component, language, filename, chunk_id, word_id, channel, chunk_start, chunk_end, str(word_index), word_ort, word_phon, next_phon, word_pos]) + "\n")
    filename_old = filename[:]
    
    
