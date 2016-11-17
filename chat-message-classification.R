library(tm)
library(RTextTools)

(function() {

    TrainModel <<- function(messages.matrix, messages.labels) {
        container <- create_container(
            messages.matrix,
            labels = messages.labels,
            trainSize = 1:length(messages.labels),
            virgin = F
        )
        train_model(container, algorithm = "SVM")
    }
    
    ClassifyModel <<- function(messages.matrix, training.model) {
        container <- create_container(
            messages.matrix,
            labels = rep("", nDocs(messages.matrix)),
            testSize = 1:nDocs(messages.matrix),
            virgin = F
        )
        classify_model(container, training.model)
    }

})()
