#### Load data ####

# 1. Set the source folder path
source_folder <- 'G:/Shared drives/Curated Dataset/data/source'

# 2. Get a list of files
file_list <- list.files(path = source_folder,
                        pattern = '\\.rds$',
                        full.names = TRUE)

# 3. Load each file as data frame
for (file_path in file_list) {
  base_name <- basename(file_path)
  object_name <- substr(gsub('\\.rds$', '', base_name), 18, nchar(gsub('\\.rds$', '', base_name)))
  assign(object_name, readRDS(file_path))
}
