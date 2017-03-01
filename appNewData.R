# mobile UI to collect data
# last update: 2017-03-01

appNewData <- function(){
        tabPanel('Datenerfassung',
                 helpText('Erfasse hier einen neuen Eintrag im Qualitäts-Tracker.'),
                 textInput('inputTopicTimestamp',
                           'Zeit:',
                           value = as.character(Sys.time())),
                 selectInput('inputTopicSelect',
                             label = 'Bereich',
                             choices = c('keine'),
                             selected = 'keine'),
                 selectInput('inputOptionsSelect',
                             label = 'Auswahl',
                             choices = c(' '),
                             selected = ' '),
                 tags$label('Anmerkung:'),
                 br(),
                 tags$textarea(id='inputNotes',
                               rows=2, cols=50,
                               ''),
                 br(),br(),
                 actionButton('saveInputTopic', 'Speichern',
                              icon('save')),
                 br(), br(), uiOutput('inputTopicStatus')
        )
}