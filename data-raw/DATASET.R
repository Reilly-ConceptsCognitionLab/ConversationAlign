## code to prepare `DATASET` dataset goes here
library(stringi)

# load reilly lab default stopwords list
omPath <- r"(..\ConversationAlignPackageDevelopment\data\ReillyLab_Stopwords_25.rData)"
load(omPath)

View(ReillyLab_Stopwords_25)

# load lookup database
lookupPath <- r"(..\ConversationAlignPackageDevelopment\data\lookup_db.rda)"
load(lookupPath)

# test lookup database for non-ascii chars
testASCII_1 <- sapply(lookup_db$word, stringi::stri_enc_mark)
testASCII_1u <- unique(testASCII_1)
print(testASCII_1u)
# print words with native encoding
nativeRows <- lookup_db[testASCII_1 == "native", "word"]
print(nativeRows)
# convert any native encodings to ascii encodings
lookup_db$word <- sapply(lookup_db$word, stringi::stri_escape_unicode)
# check that native encodings have been converted to ascii
nativeRows <- lookup_db[testASCII_1 == "native", "word"]
print(nativeRows)
testASCII_2 <- unique(sapply(lookup_db$word, stringi::stri_enc_mark))
print(testASCII_2)


usethis::use_data(ReillyLab_Stopwords_25, lookup_db,
                  internal = TRUE, overwrite = TRUE, compress = T)
