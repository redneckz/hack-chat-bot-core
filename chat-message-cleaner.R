library(stringi)
library(tm)

(function() {
    
    MESSAGE_ALIASES <- list(
        "+1" = " plus ",
        "+" = " plus ",
        "-1" = " minus ",
        "-" = " minus ",
        "!" = " exclamation ",
        "?" = " question ",
        ":)" = " happiness ",
        ";)" = " happiness ",
        ":(" = " sadness ",
        "))" = " happiness ",
        "((" = " sadness ",
        "(y)" = " exclamation "
    )
    
    CleanChatMessages <<- function(messages) {
        corpus <-
            VCorpus(VectorSource(messages),
                    readerControl = list(language = "ru"))
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
    
    ApplyAliases <- function(message) {
        Reduce(
            f = function(message, pattern)
                stri_replace_all_fixed(message, pattern, MESSAGE_ALIASES[[pattern]]),
            x = names(MESSAGE_ALIASES),
            init = message
        )
    }
    
})()
