uiPiaConfig <- function(){
        tabPanel(HTML(paste0('Verbindung zum Datentresor</a></li>',
                             '<li><a id="desktoplink" href="', 
                             desktopUrl, 
                             '">zur Desktop Version')),
                 h3('Datentresor'),
                 textInput('pia_urlMobile', 'Adresse:'),
                 textInput('app_keyMobile', 'Identifier:'),
                 textInput('app_secretMobile', 'Secret:'),
                 actionButton('mobilePiaSave', 'Speichern'),
                 br(), br(),
                 uiOutput('currentToken'),
                 conditionalPanel(
                         condition = "output.currentToken != ''",
                         actionButton('disconnectPIA', 
                                      'Verbindung zu Datentresor trennen', 
                                      icon('chain-broken'))
                 ),
                 br(),
                 uiOutput('connectError')
        )
}