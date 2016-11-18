library(tm)
library(RWeka)

CreateChatMessagesMatrix <- function(messages.corpus, terms = NULL) {
    DocumentTermMatrix(messages.corpus,
                       control = list(tokenizer = WordTokenizer, dictionary = terms))
}

GetKnownTermsLevel <- function(messages.corpus, known.terms) {
    messages.matrix <- CreateChatMessagesMatrix(messages.corpus)
    messages.matrix.terms <- Terms(messages.matrix)
    if (length(messages.matrix.terms) > 0) {
        length(intersect(messages.matrix.terms, known.terms)) / length(messages.matrix.terms)
    } else {
        1
    }
}
