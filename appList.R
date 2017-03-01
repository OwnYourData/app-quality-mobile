# layout for listing records
# last update: 2017-03-01

source('appSelect.R')

appList <- function(){
            tabPanel('aufgezeichnete Ereignisse', br(),
                     appSelect(),
                     bsAlert('dataStatus'),
                     DT::dataTableOutput('qualityEventList')
            )
}
