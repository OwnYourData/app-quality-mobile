# functions for handling UI and initial setup
# last update: 2017-03-01

output$hdrImageLinkMobile <- renderUI({
        tags$div(
                tags$a(href=input$store$pia_url,
                       tags$img(height='25px', style='margin-top:-5px',
                                src=oydLogo)),
                tags$a(href=input$store$pia_url, appTitle)
        )
})

observe({
        session$sendCustomMessage(type='setPiaUrl',
                                  input$store$pia_url)
        urlParams <- parseQueryString(session$clientData$url_search)
        urlParamExist <- FALSE
        if(is.null(urlParams[['PIA_URL']])){
                piaUrl <<- input$store$pia_url
        } else {
                piaUrl <<- urlParams[['PIA_URL']]
                urlParamExist <- TRUE
        }
        if(is.null(urlParams[['APP_KEY']])){
                appKey <<- input$store$app_key
        } else {
                appKey <<- urlParams[['APP_KEY']]
                urlParamExist <- TRUE
        }
        if(is.null(urlParams[['APP_SECRET']])){
                appSecret <<- input$store$app_secret
        } else {
                appSecret <<- urlParams[['APP_SECRET']]
                urlParamExist <- TRUE
        }
        
        app <- setupApp(piaUrl, appKey, appSecret)
        if(urlParamExist){
                updateStore(session, "pia_url", piaUrl)
                updateStore(session, "app_key", appKey)
                updateStore(session, "app_secret", appSecret)
        }
        if(length(all.equal(app, logical(0)))>1){
                closeAlert(session, 'myPiaStatus')
                updateTextInput(session, 'pia_urlMobile', value=piaUrl)
                updateTextInput(session, 'app_keyMobile', value=appKey)
                updateTextInput(session, 'app_secretMobile', value=appSecret)
                output$currentToken <- renderUI({
                        HTML(paste0('<strong>aktueller Token:</strong><br>',
                                    app[['token']],
                                    '<br><br>'))
                })
        } else {
                updateTextInput(session, 'pia_urlMobile', value=piaUrl)
                updateTextInput(session, 'app_keyMobile', value=appKey)
                updateTextInput(session, 'app_secretMobile', value=appSecret)
                output$currentToken <- renderText('')
        }
        appStart()
        desktopUrl <- paste0(
                desktopUrl, 
                '?PIA_URL=', piaUrl,
                '&APP_KEY=', appKey,
                '&APP_SECRET=', appSecret)
        session$sendCustomMessage(type='setDesktopUrl', desktopUrl)
})

output$connectError <- renderUI({
        pia_url <- input$pia_urlMobile
        app_key <- input$app_keyMobile
        app_secret <- input$app_secretMobile
        auth_url <- paste0(pia_url, '/oauth/token')
        # reduce response timeout to 30s to avoid hanging app
        # https://curl.haxx.se/libcurl/c/CURLOPT_CONNECTTIMEOUT.html
        optTimeout <- curlOptions(connecttimeout = 30)
        response <- tryCatch(
                postForm(auth_url,
                         client_id     = app_key,
                         client_secret = app_secret,
                         grant_type    = 'client_credentials',
                         .opts         = optTimeout),
                error = function(e) { return(as.character(e)) })
        if (is.na(response)) {
                'Error: no response'
        } else {
                if(jsonlite::validate(response)){
                        ''
                } else {
                        if(grepl('error', response, ignore.case = TRUE)){
                                response
                        } else {
                                paste('Error:', response)
                        }
                }
        }
})

observeEvent(input$disconnectPIA, {
        updateStore(session, 'pia_url', NA)
        updateStore(session, 'app_key', NA)
        updateStore(session, 'app_secret', NA)
        updateTextInput(session, 'app_secretMobile', value='')
        updateTextInput(session, 'app_keyMobile', value='')
        updateTextInput(session, 'pia_urlMobile', value='')
        piaUrl <<- ''
        appKey <<- ''
        appSecret <<- ''
        output$currentToken <- renderText('')
})

observeEvent(input$mobilePiaSave, ({
        updateStore(session, "pia_url", isolate(input$pia_urlMobile))
        updateStore(session, "app_key", isolate(input$app_keyMobile))
        updateStore(session, "app_secret", isolate(input$app_secretMobile))
        updateTextInput(session, "pia_url", value=isolate(input$pia_urlMobile))
        updateTextInput(session, "app_key", value=isolate(input$app_keyMobile))
        updateTextInput(session, "app_secret", value=isolate(input$app_secretMobile))
        piaUrl <<- isolate(input$pia_urlMobile)
        appKey <<- isolate(input$app_keyMobile)
        appSecret <<- isolate(input$app_secretMobile)
        
        token <- getToken(piaUrl, appKey, appSecret)
        output$currentToken <- renderUI({
                HTML(paste0('<strong>aktueller Token:</strong><br>',
                            token,
                            '<br><br>'))
        })
        
}))


