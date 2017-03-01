# configure quality topics
# last update: 2017-03-01

# get stored topics
readTopicItems <- function(){
        app <- currApp()
        topicItems <- data.frame()
        if(length(app) > 0){
                url <- itemsUrl(app[['url']], 
                                paste0(app[['app_key']],
                                       '.topics'))
                topicItems <- readItems(app, url)
                if(nrow(topicItems) > 0){
                        rownames(topicItems) <- topicItems$topic
                        topicItems <- topicItems[, c('options', 'id'), drop=FALSE]
                }
        }
        topicItems
        
}

topicList <- function(){
        allItems <- readTopicItems()
        if(nrow(allItems) > 0){
                updateSelectInput(session, 'topicSelect',
                                  choices = rownames(allItems))
                updateSelectInput(
                        session,
                        'inputTopicSelect',
                        choices = rownames(allItems),
                        selected = rownames(allItems)[1])
                opts <- lapply(strsplit(allItems$options, ';'), trimws)[[1]]
                updateSelectInput(
                        session,
                        'inputOptionsSelect',
                        choices = opts,
                        selected = opts[1])
        } else {
                updateSelectInput(
                        session,
                        'inputTopicSelect',
                        choices = c('keine Bereiche konfiguriert'),
                        selected = 'keine Bereiche konfiguriert')
                updateSelectInput(
                        session,
                        'inputOptionsSelect',
                        choices = ' ',
                        selected = ' ')
        }
}

observeEvent(input$inputTopicSelect, {
        allItems <- readTopicItems()
        selItem <- input$inputTopicSelect
        if(nrow(allItems) > 0){
                if(selItem %in% rownames(allItems)){
                        opts <- lapply(strsplit(
                                allItems[rownames(allItems) == selItem,
                                         'options'], ';'), 
                                trimws)[[1]]
                } else {
                        opts <- lapply(strsplit(allItems$options, ';'), 
                                       trimws)[[1]]
                }
                updateSelectInput(
                        session,
                        'inputOptionsSelect',
                        choices = opts,
                        selected = opts[1])
        } else {
                updateSelectInput(
                        session,
                        'inputTopicSelect',
                        choices = c('keine Bereiche konfiguriert'),
                        selected = 'keine Bereiche konfiguriert')
                updateSelectInput(
                        session,
                        'inputOptionsSelect',
                        choices = ' ',
                        selected = ' ')
        }
})

observeEvent(input$saveInputTopic, {
        inputMsg = ''
        inputTimestampTry = try(as.POSIXct(input$inputTopicTimestamp))
        if(is.POSIXct(inputTimestampTry)){
                app <- currApp()
                url <- itemsUrl(app[['url']], app[['app_key']])
                data <- list(
                        timestamp = inputTimestampTry,
                        topic     = input$inputTopicSelect,
                        event     = input$inputOptionsSelect,
                        note      = input$inputNotes,
                        '_oydRepoName' = 'Liste'
                )
                writeItem(app, url, data)
                updateTextInput(
                        session,
                        'inputTopicTimestamp',
                        value = as.character(Sys.time()))
                inputMsg = 'ein neuer Eintrag wurde im Qualitätstracker gespeichert'
        } else {
                inputMsg = 'Fehler: ungültiges Datum/Zeit Format, die Eingabe wurde nicht gespeichert'
        }
        output$inputTopicStatus <- renderUI(inputMsg)
})