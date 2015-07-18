# Author: Jeremy Boyd (kenyonboyd@gmail.com)
# Summary: Identifies adjective uses in CHILDES corpus files;
# organizes them for later analysis.

# Specify the location of corpus files.
corpus_dir <- "~/Documents/studies/A-ADJ/AP/get_adj_uses/corpora"

# Create list; set adjective counter to zero
corpus.file.list <- list()
adjcount <- 1

# Iterate over all corpus files
for (file in dir(path = corpus_dir, full.names = TRUE, recursive = TRUE)) {
	
    # Only process .cha files.
    if (grepl("\\.cha", file, perl = TRUE)) {
    
        # User output
        cat("Processing ", file, "...", "\n", sep = "")
        
        # Store corpus and filenames
        corpusname <- unlist((strsplit(file, "\\/")))[10]
        filename <- unlist((strsplit(file, "\\/")))[length(
            unlist((strsplit(file, "\\/"))))]
        
        # Read in file
        corpus.file <- scan(file, what = "char", sep = "\n", quiet = TRUE)
        
        #**********PREP RAW CORPUS FOR ANALYSIS**********
        # Concatenate all lines into one, replacing newlines with "___".
        corpus.file.2 <- paste(corpus.file, collapse = "___")
        
        # Replace "___" followed by at least one whitespace character with a
        # single space
        corpus.file.3 <- gsub("___\\s+", " ", corpus.file.2, perl = TRUE)
        
        # Split string on "___" to break up single line
        corpus.file.4 <- unlist(strsplit(corpus.file.3, "___", perl = TRUE))
        
        # Get rid of extra spaces. Use this version below in order to get
        # correct line numbers
        corpus.file.4 <- gsub(" +", " ", corpus.file.4, perl = TRUE)
        
        # Find header line with child age in it.  Problem for
        # macwhinney/85a1.cha, because that file doesn't contain CHI. Instead,
        # there are two children coded MAR and ROS. I've included a kludgy
        # fix below. Funny that the only formatting error in all of the files
        # this script looks at comes from the guy who created the CHILDES
        # database.
        if (length(grep("@ID.+CHI\\|[0-9]", corpus.file.4, perl = TRUE)) == 1)
        {
                ageline <- grep("@ID.+CHI\\|[0-9]", corpus.file.4,
                                value = TRUE, perl = TRUE)
        } else {
            ageline <- "problem_getting_age | problem_getting_age | problem_getting_age | problem_getting_age"
        }
        
        # Split on '|' to get child age.
        age <- unlist(strsplit(ageline, "\\|"))[4]
        
        # Set previous line to empty
        preline <- ""
        
        # Set line number to 0
        linenum <- 0
        
        #**********POPULATE LIST**********
        # Iterate over lines
        for (line in corpus.file.4) {
            
            # Only process %mor lines that have adjectives.
            if (grepl("%mor", line, perl = TRUE) & grepl("adj\\|", line, perl = TRUE)) {
                
                # Get utterance info from preline
                speaker <- substr(preline, 2, 4)
                utt <- substr(preline, 7, nchar(preline))
                
                # Get %mor annotation
                mor <- substr(line, 7, nchar(line))
                
                # Get positions of all adjectives in line
                adjlist <- gregexpr("adj\\|[a-zA-Z&#\\-]+", mor, perl = TRUE)
                
                # Iterate over adjectives in adjlist (could be list of 1)
                for (j in 1:length(unlist(adjlist))) {
                    
                    # Get position of first and last letters of adj
                    firstletter <- unlist(adjlist)[j]+4
                    lastletter <- (unlist(adjlist)[j]+4) + (attr(adjlist[[1]], "match.length")[j]-5)
                    
                    # Get adjective from line
                    adj <- substr(mor, firstletter, lastletter)
                    
                    # Write adj entry to list
                    corpus.file.list[["corpus"]][adjcount] = corpusname
                    corpus.file.list[["file"]][adjcount] = filename
                    
                    # Note that line numbers are based off of the modified
                    # corpus file--corpus.file.4.
                    corpus.file.list[["speaker"]][adjcount] = speaker
                    corpus.file.list[["age"]][adjcount] = age
                    corpus.file.list[["adjective"]][adjcount] = adj
                    corpus.file.list[["utterance"]][adjcount] = utt
                    corpus.file.list[["mor"]][adjcount] = mor
                    
                    # Increment adjective count
                    adjcount <- adjcount + 1	
                }
            }
            
            # Save previous line; increment line number
            preline <- line
            linenum <- linenum + 1
        }
    }
}

#**********CONVERT TO DATAFRAME AND SAVE**********

# Convert list to dataframe
adjdata <- as.data.frame(sapply(corpus.file.list,
                                "[", 1:max(sapply(corpus.file.list, length))))

# Save dataframe to file
write.table(adjdata, file = "adj_data.txt", sep = "\t", eol = "\n",
            quote = FALSE, row.names = FALSE, col.names = TRUE, append = FALSE)