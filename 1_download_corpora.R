# Author: Jeremy Boyd (kenyonboyd@gmail.com)
# Summary: Downloads CHILDES corpora. Users can specify the corpora they want
# to download by editing the childes_URL and corpora variables.

# Zip files containing different CHILDES corpora are located on different
# download pages. We're interested in corpora consisting of North American
# (NA) English that include a MOR tier (i.e., a tier that has PoS tags),
# hence the "Eng-NA-MOR" part of the URL.
childes_URL <- "http://childes.psy.cmu.edu/data/Eng-NA-MOR/"

# Specify the different corpora we want. These are all corpora with
# typically developing children.
corpora <- c("Bliss", "Brown", "Carterette", "Gleason", "Hall", "Kuczaj",
             "MacWhinney", "Warren")

# Create a directory to store corpus files in.
corpus_dir <- "~/Documents/studies/A-ADJ/AP/get_adj_uses/corpora/"
dir.create(corpus_dir)

# Loop over the different corpus names.
lapply(corpora, function(x) {
    
    # Specify a temporary filename for a corpus zip file.
    temp <- tempfile()
    
    # Download the zip file.
    download.file(url = paste(childes_URL, x, ".zip", sep = ""),
                  destfile = temp)
    
    # Unzip from temp to corpus directory.
    unzip(zipfile = temp, exdir = corpus_dir)
    
    # Delete the temporary file.
    unlink(temp)    
})