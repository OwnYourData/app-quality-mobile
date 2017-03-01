# top-level file for mobile UI
# last update:2017-03-02
shinyUI(
        tagList(
                initStore("store", "oydStore"),
                tags$script(
                        'Shiny.addCustomMessageHandler("setPiaUrl", function(x) {      
                        $("#returnPIAlink").attr("href", x);
                        })'),
                appMobile()
        )
)
