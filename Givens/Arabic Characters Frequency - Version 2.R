library(utf8)
library(tokenizers)

# Arabic letters and space
arabic.letters.space.frequency <- data.frame(c(as_utf8("\u0627"), as_utf8("\u0628"), as_utf8("\u062A"), as_utf8("\u062B"), as_utf8("\u062C"), 
                                     as_utf8("\u062D"), as_utf8("\u062E"), as_utf8("\u062F"), as_utf8("\u0630"), as_utf8("\u0631"), 
                                     as_utf8("\u0632"), as_utf8("\u0633"), as_utf8("\u0634"), as_utf8("\u0635"), as_utf8("\u0636"), 
                                     as_utf8("\u0637"), as_utf8("\u0638"), as_utf8("\u0639"), as_utf8("\u063A"), as_utf8("\u0641"), 
                                     as_utf8("\u0642"), as_utf8("\u0643"), as_utf8("\u0644"), as_utf8("\u0645"), as_utf8("\u0646"), 
                                     as_utf8("\u0647"), as_utf8("\u0648"), as_utf8("\u0649"), as_utf8("\u0020")), numeric(29))
colnames(arabic.letters.space.frequency) <- c('arabic.letters', 'frequency')

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