library(tm)
library(RWeka)

(function() {
    
    CreateChatMessagesMatrix <<-
        function(messages.corpus, terms = NULL) {
            DocumentTermMatrix(messages.corpus,
                               control = list(tokenizer = WordTokenizer, dictionary = terms))
        }
    
    GetKnownTermsLevel <<- function(messages.corpus, known.terms) {
        messages.matrix <- CreateChatMessagesMatrix(messages.corpus)
        messages.matrix.terms <- Terms(messages.matrix)
        length(intersect(messages.matrix.terms, known.terms)) / length(messages.matrix.terms)
    }
    
})()