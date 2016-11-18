library(data.table)
library(stringi)
library(tm)

CHAT_MESSAGE_ALIASES <- fread(
    "data/chat-message-aliases.csv",
    header = T,
    encoding = "UTF-8",
    colClasses = c("character", "character")
)

CleanChatMessages <- function(messages) {
    corpus <-
        VCorpus(VectorSource(messages), readerControl = list(language = "ru"))
    transformations <- list(
        stemDocument,
        content_transformer(stri_trim),
        stripWhitespace,
        content_transformer(
            function(message)
                stri_replace_all_regex(message, "[:punct:]", " ")
        ),
        content_transformer(ApplyAliases),
        content_transformer(tolower)
    )
    tm_map(corpus, FUN = tm_reduce, tmFuns = transformations)
}

ApplyAliases <-
    function(message, aliases.table = CHAT_MESSAGE_ALIASES) {
        result <- message
        for (i in 1:nrow(aliases.table)) {
            entry <- aliases.table[i, ]
            result <-
                stri_replace_all_regex(result, entry$pattern, entry$alias)
        }
        result
    }
