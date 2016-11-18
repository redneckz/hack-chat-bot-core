library(data.table)
library(tm)
library(RWeka)

source("url-decode.R", encoding = "UTF-8")
source("chat-message-cleaner.R", encoding = "UTF-8")
source("chat-message-classification.R", encoding = "UTF-8")
source("chat-message-matrix-factory.R", encoding = "UTF-8")

CHAT_MESSAGE_CLASSES <- list(
    UNKNOWN = "UNKNOWN",
    CRAP = "CRAP",
    DRIVER = "DRIVER",
    PASSENGER = "PASSENGER",
    PLACE = "PLACE",
    TIME = "TIME"
)

GetChatMessageClasses <- function() {
    bot.state$training.set
}

GetChatMessageClassById <- function(id) {
    id <- URLDecode(id)
    bot.state$training.set[message.class == id]
}

ClassifyMessage <- function(message) {
    message <- URLDecode(message)
    
    message.corpus <- CleanChatMessages(message)
    print(message.corpus[[1]]$content)
    known.terms.level <-
        GetKnownTermsLevel(message.corpus, Terms(bot.state$training.matrix))
    if (known.terms.level > 0.5) {
        message.matrix <- CreateChatMessagesMatrix(message.corpus,
                                                   terms = Terms(bot.state$training.matrix))
        result <-
            ClassifyModel(message.matrix, bot.state$training.model)
        message.class <- result[[1]]
        message.class.probability <- result[[2]]
        list(
            message.class = message.class,
            message.class.probability = message.class.probability,
            known.terms.level = known.terms.level
        )
    } else {
        list(
            message.class = CHAT_MESSAGE_CLASSES$UNKNOWN,
            message.class.probability = 0,
            known.terms.level = known.terms.level
        )
    }
}

TrainModelByMessage <- function(message.class, message) {
    message.class <- URLDecode(message.class)
    message <- URLDecode(message)
    
    if (is.null(CHAT_MESSAGE_CLASSES[[message.class]])) {
        return(list(error = "Unknown message class specified."))
    }
    
    message.words <- WordTokenizer(message)
    if (length(message.words) == 0) {
        return(list(error = "Empty message specified."))
    }
    
    bot.state <<-
        AppendTrainingSetToChatBotState(bot.state,
                                        data.table(message = message,
                                                   message.class = message.class))
    list(
        message.class = message.class,
        message.class.probability = 1,
        known.terms.level = 1
    )
}

# START State

InitChatBotState <- function() {
    training.corpus <- VCorpus(VectorSource(character()),
                               readerControl = list(language = "ru"))
    list(
        training.set = data.table(),
        training.corpus = training.corpus,
        training.matrix = DocumentTermMatrix(training.corpus),
        training.model = NULL
    )
}

AppendTrainingSetToChatBotState <-
    function(old.state, new.training.set) {
        training.set <- rbind(old.state$training.set, new.training.set)
        new.training.corpus <-
            CleanChatMessages(new.training.set[["message"]])
        training.corpus <-
            c(old.state$training.corpus, new.training.corpus)
        training.matrix <-
            c(old.state$training.matrix,
              CreateChatMessagesMatrix(new.training.corpus))
        training.model <-
            TrainModel(training.matrix, training.set[["message.class"]])
        print(training.model)
        list(
            training.set = training.set,
            training.corpus = training.corpus,
            training.matrix = training.matrix,
            training.model = training.model
        )
    }

bot.state <- AppendTrainingSetToChatBotState(
    InitChatBotState(),
    fread(
        "data/training-set.csv",
        header = T,
        encoding = "UTF-8",
        colClasses = c("character", "factor")
    )
)

# END State