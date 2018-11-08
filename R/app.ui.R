
#' app.ui
#'
#' UI for the pupillometry app. When using the app locally, use `pupillometry.app()` instead.
#'
#' @return A shinyUI
#' @export
app.ui <- function(){
  library(shiny)
  library(plotly)
  library(shinydashboard)
  library(shinycssloaders)
  library(colourpicker)

  shinyUI( dashboardPage(
    dashboardHeader(title="Pupillometry"),
    dashboardSidebar(
      sidebarMenu(id="menu",
        menuItem("Files & samples", tabName="samples", badgeLabel=uiOutput("badgeText_samples"), badgeColor="blue"),
        menuItem("Bins", tabName="bins", badgeLabel=uiOutput("badgeText_bins"), badgeColor="blue"),
        menuItem("Settings", tabName="settings"),
        menuItem("Plot", tabName="plot"),
        menuItem("Statistical tests", tabName="tests"),
        menuItem("About", tabName="about")
      )
    ),
    dashboardBody(
      tabItems(
        tabItem("samples",
          box(width=6, title = "Samples", tableOutput("samples_info")),
          box(width=6, title = "Upload files",
            fileInput("sampleFileInput",label="Select one or more samples to upload", multiple=T),
            tags$p(style="font-weight:8;", "Data files should be of matlab matrices format. The metadata file should be a csv with the data filenames as first column, and further variables as additional columns.")
          ),
          box(width=8, title="Samples Metadata", tableOutput("samples_metadata"), tags$p(textOutput("check_metadata"))),
          box(width=4, title="Samples colors", uiOutput("sColorsInputs"))
        ),
        tabItem("bins",
          box(width=12,
            selectInput("preview_sample", "Sample for preview", choices=c(), selectize=F),
            plotlyOutput("preview_bins")
          ),
          box(width=12,
            column(6,
              textAreaInput( "baseline_bins", label="Enter the start and end time of the baseline bin(s)", placeholder="e.g. 0-10" )
            ),
            column(6,
              textAreaInput( "response_bins", label="Enter the start and end time of the response bin(s)", placeholder="e.g. 10-20" )
            ),
            tags$p("Start and end time should be separated by a dash ('-'), and different bins should be on different lines."),
            tags$p(style="color: ref; font-weight: 8;", renderText("check_bins"))
          )
        ),
        tabItem("settings",
          box( title="Plot",
               numericInput("interval", label="Interval size (in seconds)", min=0, max=2, step=0.05, value=1),
               checkboxInput("showPoints", label="Plot points", value=T),
               selectInput("plot_groupBy","Group by",choices=c(),selectize=T,multiple=T),
               selectInput("plot_errType","Error type",choices=c("Standard error"="SE", "Standard deviation"="SD"),selectize=F),
               sliderInput("opacity_SD", "Opacity of standard deviation/error", min=0, max=1, step=0.1, value=0.3)
          ),
          box(
            numericInput("timeFactor", "Time factor", min=0.01, max=1, value=0.2),
            tags$p("Factor by which to multiply the input time in order to obtain seconds. If the input time is expressed as frame number, this should be 1/(frames per second)")
          ),
          box( title="Normalization",
            checkboxInput("cb_normalize","Normalize to (first) baseline bin", value=T),
            selectInput("normDrift", "Normalize linear non-response drift across time", choices=c("No"="no","Global drift"="global","Run-specific drift"="specific"), selectize=F)
          ),
          box( title="Bins appearance", collapsible=T, collapsed=T,
             checkboxInput("cb_plot_bins","Display bins on plot",value = T),
             colourInput("color_baselineBins", "Color of baseline bin(s)", value="black"),
             sliderInput("opacity_baselineBins", "Opacity of baseline bin(s)", min=0, max=1, step=0.1, value=0.1),
             colourInput("color_responseBins", "Color of response bin(s)", value="steelblue"),
             sliderInput("opacity_responseBins", "Opacity of response bin(s)", min=0, max=1, step=0.1, value=0.1)
          )
        ),
        tabItem("plot",
          box(width=12, withSpinner(plotlyOutput("mainPlot", height="600px")))
        ),
        tabItem("tests",
          box(width=12,
            column(4, selectInput("test_var","Test for differences across", choices=c(), selectize=F)),
            column(2, tags$div(style="padding-top: 20px; text-align:center;", tags$h4("OR"))),
            column(6,
              textInput("test_formula", "Enter full formula"),
              textInput("test_formula0", "Enter reduced formula")
            )
          ),
          tags$div(style="margin: 10px;", fluidRow( verbatimTextOutput("test_results") ))
        ),
        tabItem("about",
          box(width=12, "some text saying that Lukas and ETH are awesome...")
        )
      )
    )
  ))
}

.app.ui.sColorsInputs <- function(filenames, idPrefix='colorI_'){
  if(is.null(filenames) || length(filenames)==0) return(NULL)
  defCols <- getQualitativePalette(length(filenames))
  lapply( 1:length(filenames), id=idPrefix, colors=defCols, filenames=filenames, FUN=function(x, id, colors, filenames){
    colourInput(paste0(id,filenames[x]), label=paste("Color",x), value=colors[x])
  })
}
