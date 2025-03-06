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

#  Arabic characters Frequency with input a list of tokens.
arabic_characters_frequency <- function(list_of_tokens) {
  for (i in 1:(nrow(arabic.letters.space.frequency)-1)) {
    for (j in 1:nrow(list_of_tokens)) {
      list_of_characters <- tokenize_characters(list_of_tokens[j,])[[1]]
      for (k in 1:length(list_of_characters))
       if(arabic.letters.space.frequency[i, 'arabic.letters']==list_of_characters[k]){
         arabic.letters.space.frequency[i, 'frequency'] <- arabic.letters.space.frequency[i, 'frequency'] + 1
      }
    }
    arabic.letters.space.frequency[nrow(arabic.letters.space.frequency), 'frequency'] <- nrow(list_of_tokens)-1
  }
  arabic.letters.space.frequency
}


list.of.tokens <- data.frame(c('على', 'احمد', 'محمد'))
colnames(list.of.tokens) <- 'tokens'
list.of.tokens

arabic.letters.space.frequency <- arabic_characters_frequency(list.of.tokens)


