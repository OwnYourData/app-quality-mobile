uiPiaConfig <- function(){
        tabPanel('Verbindung zum Datentresor',
                 h3('Datentresor'),
                 textInput('pia_urlMobile', 'Adresse:'),
                 textInput('app_keyMobile', 'Identifier:'),
                 textInput('app_secretMobile', 'Secret:'),
                 actionButton('mobilePiaSave', 'Speichern'),
                 br(), br(),
                 uiOutput('currentToken'),
                 conditionalPanel(
                         condition = "output.currentToken != ''",
                         actionButton('disconnectPIA', 'Verbindung zu Datentresor trennen', 
                                      icon('chain-broken'))
                 ),
                 br(),
                 uiOutput('connectError')
        )
}