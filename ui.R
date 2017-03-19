# top-level file for mobile UI
# last update:2017-03-02
shinyUI(
        tagList(
                initStore("store", "oydStore"),
                tags$script(
                        'Shiny.addCustomMessageHandler("setDesktopUrl", function(x) {      
                        $("#desktoplink").attr("href", x);
                        })'),
                appMobile()
        )
)
