import os
import shutil

def search_compressed_files(folder_path, text_list_file):
    filtered_folder = os.path.join(folder_path,'filteredfiles')
    os.makedirs(filtered_folder, exist_ok=True)
    with open(text_list_file, 'r') as f:
        text_list = f.read().split("\n")
    for file in os.listdir(folder_path):
        if file.endswith('.zip') or file.endswith('.tar'):
            for text in text_list:
                if text in file:
                    source_file = os.path.join(folder_path, file)
                    destination_file = os.path.join(filtered_folder, file)
                    shutil.move(source_file, destination_file)
    return filtered_folder

folder_path = 'Pathtoreads/reads'
text_list_file = '/path/to/text_list.txt'
filtered_folder = search_compressed_files(folder_path, text_list_file)
print(f'Matching files moved to {filtered_folder}')
