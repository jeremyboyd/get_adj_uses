# Author: Jeremy Boyd (kenyonboyd@gmail.com)
# Summary: Downloads CHILDES corpora, pulls out all adjective uses and puts them in a dataframe for later analysis.

# DATE: January 12, 2010
# SUMMARY: Finds all adjectives in CHAT-formatted corpora and organizes them into a dataframe.
# TODO: Add code to calculate numeric age from age line. For Hall corpus hardcode in 4;9 (57 months). For others, drop '.' if age string ends in '.'. Then split on ';' to get years and months separated. Then split on '.' to get months and days separated (for ages written years, months, days, e.g. 4;5.27 for 4 years, 5 months, 27 days). Can calculate overall age in years based on years, months and days.

# To run this file as a script:
# > source("a-adj corpus05.R", echo = FALSE)

# Set working directory
setwd("/Users/boyd/Documents/studies/A-ADJ/AP/get_adj_uses")

# Set input directory
inputdir = "/Users/boyd/Documents/corpora"
#inputdir = "/Users/boyd/Documents/corpora/Warren"

# Create list; set adjective counter to zero
corpus.file.list = list()
adjcount = 1

# Iterate over all corpus files
for (file in dir(path = inputdir, full.names = T, recursive = T)) {
	
	# User output
	cat("Processing", file, "...", "\n")
	
	# Store corpus and filenames
	corpusname = unlist((strsplit(file, "\\/")))[6]
	filename = unlist((strsplit(file, "\\/")))[length(unlist((strsplit(file, "\\/"))))]
	
	# Read in file
	corpus.file = scan(file, what = 'char', sep = "\n", quiet = T)
	
	#**********PREP RAW CORPUS FOR ANALYSIS**********
	# Concatenate all lines into one, replacing newlines with "___".
	corpus.file.2 = paste(corpus.file, collapse="___")

	# Replace "___" followed by at least one whitespace character with a single space
	corpus.file.3 = gsub("___\\s+", " ", corpus.file.2, perl=T)

	# Split string on "___" to break up single line
	corpus.file.4 = unlist(strsplit(corpus.file.3, "___", perl=T))

	# Get rid of extra spaces. Use this version below in order to get correct line numbers
	corpus.file.4 = gsub(" +", " ", corpus.file.4, perl=T)

	# Find header line with child age in it
	ageline = grep("@ID.+CHI\\|[0-9]", corpus.file.4, value = T, perl = T)
	
	# Split on '|' to get child age
	age = unlist(strsplit(ageline, "\\|"))[4]

	# Set previous line to empty
	preline = ""
	
	# Set line number to 0
	linenum = 0

	#**********POPULATE LIST**********
	#Iterate over lines
	for (line in corpus.file.4) {
		
		# Only process %mor lines that have adjectives
		if (length(grep("%mor", line, perl = T)) == 1 & length(grep("adj\\|", line, perl = T)) == 1) {
			
			# Get utterance info from preline
			speaker = substr(preline, 2, 4)
			utt = substr(preline, 7, nchar(preline))
			
			# Get %mor annotation
			mor = substr(line, 7, nchar(line))
			
			# Get positions of all adjectives in line
			adjlist = gregexpr("adj\\|[a-zA-Z&#\\-]+", mor, perl=T)
		
			# Iterate over adjectives in adjlist (could be list of 1)
			for (j in 1:length(unlist(adjlist))) {
				
				# Get position of first and last letters of adj
				firstletter = unlist(adjlist)[j]+4
				lastletter = (unlist(adjlist)[j]+4) + (attr(adjlist[[1]], "match.length")[j]-5)
				
				# Get adjective from line
				adj = substr(mor, firstletter, lastletter)
						
				# Write adj entry to list
				corpus.file.list[["Corp"]][adjcount] = corpusname
				corpus.file.list[["File"]][adjcount] = filename
				corpus.file.list[["Age"]][adjcount] = age
				corpus.file.list[["Line"]][adjcount] = linenum
				corpus.file.list[["Spkr"]][adjcount] = speaker
				corpus.file.list[["Adj"]][adjcount] = adj
				corpus.file.list[["Utt"]][adjcount] = utt
				corpus.file.list[["Mor"]][adjcount] = mor
					
				# Increment adjective count
				adjcount = adjcount + 1	
			}
		}
		
		# Save previous line; increment line number
		preline = line
		linenum = linenum + 1
 	}
}

#**********CONVERT TO DATAFRAME AND SAVE**********

# Convert list to dataframe
adjdata = as.data.frame(sapply(corpus.file.list, "[", 1:max(sapply(corpus.file.list, length))))

# Save dataframe to file
write.table(adjdata, file = "/Users/boyd/Documents/studies/A-ADJ/AP/AP corpus/adj-data.tab", sep = "\t", eol = "\n", quote = F, row.names = F, col.names = T, append = F)