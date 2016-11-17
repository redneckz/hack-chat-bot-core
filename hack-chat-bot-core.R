library(data.table)

source("url-decode.R", encoding = "UTF-8")
source("chat-message-cleaner.R", encoding = "UTF-8")
source("chat-message-classification.R", encoding = "UTF-8")
source("chat-message-matrix-factory.R", encoding = "UTF-8")

(function () {
    
    CHAT_MESSAGE_CLASSES <- list(
        UNKNOWN = "UNKNOWN",
        CRAP = "CRAP",
        DRIVER = "DRIVER",
        PASSENGER = "PASSENGER",
        PLACE = "PLACE",
        TIME = "TIME"
    )
    
    # START Init
    training.set <- fread(
        "data/training-set.csv",
        header = T,
        encoding = "UTF-8",
        colClasses = c("character", "factor")
    )
    training.corpus <- CleanChatMessages(training.set[[1]])
    training.matrix <- CreateChatMessagesMatrix(training.corpus)
    training.model <- TrainModel(training.matrix, training.set[[2]])
    # END Init
    
    GetChatMessageClasses <<- function() {
        training.set
    }
    
    GetChatMessageClassById <<- function(id) {
        id <- URLDecode(id)
        training.set[class.id == id]
    }
    
    ClassifyMessage <<- function(message) {
        message <- URLDecode(message)
        
        message.corpus <- CleanChatMessages(message)
        known.terms.level <- GetKnownTermsLevel(message.corpus, Terms(training.matrix))
        if (known.terms.level > 0.5) {
            message.matrix <- CreateChatMessagesMatrix(message.corpus,
                                                       terms = Terms(training.matrix))
            
            result <- ClassifyModel(message.matrix, training.model)
            message.class <- result[[1]]
            message.class.prob <- result[[2]]
            list(
                message.class = message.class,
                message.class.prob = message.class.prob,
                known.terms.level = known.terms.level
            )
        } else {
            list(
                message.class = CHAT_MESSAGE_CLASSES$UNKNOWN,
                message.class.prob = 0,
                known.terms.level = known.terms.level
            )
        }
    }
    
    TrainModel <<- function(class.name, message) {
        class.name <- URLDecode(class.name)
        message <- URLDecode(message)
        
    }
    
})()
