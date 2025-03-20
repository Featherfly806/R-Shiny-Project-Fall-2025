# This section is to convert the txt data, for example , of Anne et al. into tsv file, both for cotyledons, or embryonic leaves.
# And next, generate the DE analysis.

data <- read.table("GSE61857_Cotyledon_normalized.txt", 
                   header = TRUE, 
                   sep = "\t",      
                   stringsAsFactors = FALSE)

write.csv(data, "GSE61857_Cotyledon_normalized.csv", row.names = FALSE)


data <- read.table("GSE61857_Leaf_normalized.txt", 
                   header = TRUE, 
                   sep = "\t",      
                   stringsAsFactors = FALSE)

write.csv(data, "GSE61857_Leaf_normalized.csv", row.names = FALSE)

