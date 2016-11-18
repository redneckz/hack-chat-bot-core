library(plumber)
library(jsonlite)

source("hack-chat-bot-core.R", encoding = "UTF-8")

router <- plumber$new()

router$addFilter(name = "encoding", expr = expression(function(req, res) {
    res$setHeader("Content-Encoding", "UTF-8")
    forward()
}))

router$addEndpoint(verbs = "GET", path = "/chat-message-classes",
                   expr = GetChatMessageClasses)
router$addEndpoint(verbs = "GET", path = "/chat-message-classes/<id>",
                   expr = GetChatMessageClassById)
router$addEndpoint(verbs = "GET", path = "/chat-message-classes/UNKNOWN/<message>",
                   expr = ClassifyMessage)
router$addEndpoint(verbs = "POST", path = "/chat-message-classes/<message.class>/<message>",
                   expr = TrainModelByMessage)

router$run(port = 8080)
