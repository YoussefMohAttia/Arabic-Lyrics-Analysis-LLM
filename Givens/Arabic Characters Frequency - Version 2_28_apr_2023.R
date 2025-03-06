library(utf8)
library(tokenizers)

# Arabic letters and space
arabic.letters.space.frequency <- data.frame(
  
  c("U+0621", "U+0622", "U+0623", "U+0625", "U+0627", "U+0671", "U+0672", "U+0673", "U+0674", "U+0675",   
    "U+0628", "U+066E", "U+067B", "U+067E", "U+0680", "U+062A", "U+067A", "U+062B", "U+062C", 
    "U+062D", "U+062E", "U+062F", "U+0630", "U+0631", 
    "U+0632", "U+0633", "U+0634", "U+0635", "U+0636", 
    "U+0637", "U+0638", "U+0639", "U+063A", "U+0641", 
    "U+0642", "U+0643", "U+0644", "U+0645", "U+0646", 
    "U+0647", "U+06C1", "U+06C3",  
    "U+0648", "U+0624", "U+0620", "U+0649", "U+064A", "U+06CC", "U+0020"),
  
  c(as_utf8("\u0621"), as_utf8("\u0622"), as_utf8("\u0623"), as_utf8("\u0625"), as_utf8("\u0627"), 
    as_utf8("\u0671"), as_utf8("\u0672"), as_utf8("\u0673"), as_utf8("\u0674"), as_utf8("\u0675"), 
    as_utf8("\u0628"), as_utf8("\u066E"), as_utf8("\u067B"), as_utf8("\u067E"), as_utf8("\u0680"),
    as_utf8("\u062A"), as_utf8("\u067A"), as_utf8("\u062B"), as_utf8("\u062C"), as_utf8("\u062D"), 
    as_utf8("\u062E"), as_utf8("\u062F"), as_utf8("\u0630"), as_utf8("\u0631"), as_utf8("\u0632"), 
    as_utf8("\u0633"), as_utf8("\u0634"), as_utf8("\u0635"), as_utf8("\u0636"), as_utf8("\u0637"), 
    as_utf8("\u0638"), as_utf8("\u0639"), as_utf8("\u063A"), as_utf8("\u0641"), as_utf8("\u0642"), 
    as_utf8("\u0643"), as_utf8("\u0644"), as_utf8("\u0645"), as_utf8("\u0646"), as_utf8("\u0647"), as_utf8("\u06C1"), as_utf8("\u06C3"),
    as_utf8("\u0648"), as_utf8("\u0624"), as_utf8("\u0620"), as_utf8("\u0649"), as_utf8("\u064A"), as_utf8("\u06CC"),
    as_utf8("\u0020")), numeric(49))
colnames(arabic.letters.space.frequency) <- c('arabic.letters.unicode', 'arabic.letters', 'frequency')

arabic.letters.space.frequency  

#  Arabic characters Frequency with input as a corpus of text.
arabic_characters_frequency <- function(corpus_of_text) {
  for (i in 1:nrow(arabic.letters.space.frequency)) {
    for (j in 1:nrow(corpus_of_text)) {
      list_of_characters <- tokenize_characters(corpus_of_text[j,], strip_non_alphanum = FALSE)[[1]]
      for (k in 1:length(list_of_characters))
        if(arabic.letters.space.frequency[i, 'arabic.letters']==list_of_characters[k]){
          arabic.letters.space.frequency[i, 'frequency'] <- arabic.letters.space.frequency[i, 'frequency'] + 1
      }
    }
  }
  arabic.letters.space.frequency
}


corpus.of.text <- data.frame(c('على احمد محمد', 'محمد عبد العزيز خميس', 'وليد السيد على'))
colnames(corpus.of.text) <- 'corpus'
corpus.of.text

arabic.letters.space.frequency <- arabic_characters_frequency(corpus.of.text)