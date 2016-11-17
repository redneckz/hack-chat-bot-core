(function() {
    
    URLDecode <<- function(url, encoding = "UTF-8") {
        result <- URLdecode(url)
        Encoding(result) <- encoding
        result
    }
    
})()
