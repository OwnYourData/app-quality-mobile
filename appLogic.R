# application specific logic
# last update: 2017-03-01

source('appLogicTopics.R', local=TRUE)

# any record manipulations before storing a record
appData <- function(record){
        record
}

getRepoStruct <- function(repo){
        appStruct[[repo]]
}

repoData <- function(repo){
        data <- data.frame()
        app <- currApp()
        if(length(app) > 0){
                url <- itemsUrl(app[['url']],
                                repo)
                data <- readItems(app, url)
        }
        data
}

# anything that should run only once during startup
appStart <- function(){
        topicList()
}

output$qualityEventList <- DT::renderDataTable(datatable({
        data <- currDataDateSelectTimestamp()
        if(nrow(data) > 0){
                data$myDat <- as.character(as.POSIXct(
                        as.numeric(data$timestamp), origin='1970-01-01'))
                rownames(data) <- 1:nrow(data)
                data <- data[, c('myDat', 'event', 'note'), drop = FALSE]
                colnames(data) <- c('Zeit', 'Ereignis', 'Anmerkung')
                data
        } else {
                data.frame()
        }
}, options = list(
        language = list(
                url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/German.json'),
                searching = FALSE,
                lengthChange = FALSE,
                pageLength = 10
        )
))