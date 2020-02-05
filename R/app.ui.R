
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
  library(stringr)
  library(DT)

  shinyUI( dashboardPage(
    dashboardHeader(title="Pupillometry"),
    dashboardSidebar(
      sidebarMenu(id="menu",
        menuItem("Files & samples", tabName="samples", badgeLabel=uiOutput("badgeText_samples"), badgeColor="blue"),
        menuItem("Bins", tabName="bins", badgeLabel=uiOutput("badgeText_bins"), badgeColor="blue"),
        menuItem("Clean up", tabName="cleanup"),
        menuItem("Settings", tabName="settings"),
        menuItem("Plot", tabName="plot"),
        menuItem("Statistical tests", tabName="tests"),
        menuItem("Export", tabName="export"),
        menuItem("User Manual", tabName="usermanual"),
        tags$div(id="lablink", tags$a(href="https://bohaceklab.ethz.ch/", "bohaceklab.ethz.ch"))
      )
    ),
    dashboardBody(
      tags$head(tags$style(HTML('
#lablink {
  position: absolute;
  bottom:2px;
  left:8px;
  font-size: 110%;
  font-weight: bold;
  text-decoration: none;
}
#lablink a:hover {
  text-decoration: underline;
}
    '))),

            tabItems(
        tabItem("samples", height="1500px",
          box(width=12, title = "Upload files",
              fileInput("sampleFileInput",label="Select one or more samples to upload", multiple=T),
              tags$p(style="font-weight:8;", "Data files should be of matlab matrices format. The metadata file should be a csv with the data filenames as first column, and further variables as additional columns."),
              actionButton("removesamples", "remove all"),
              actionButton("testsamples", "load example"),
              actionButton("loadfig2", "load Figure 2 data"),
              actionButton("loadfig3", "load Figure 3 data"),
              actionButton("loadfig4", "load Figure 4 data")
          ),
          box(width=12, title = "Upload and transform DLC files ", collapsible=T, collapsed=T,
              column(6,textAreaInput("DLC_center_point", label="center point name in DLC", value = "center")),
              column(6,textAreaInput("DLC_pupil_points", label="pupil points name in DLC (separated by ',')", value = "top,bottom,left,right,tr,br,bl,tl")),
              column(6,numericInput("DLC_likelihoodcutoff", "likelihood cutoff", min=0, max=1, value=0.95)),
              column(6,numericInput("DLC_completenesscutoff", "completeness cutoff", min=0, max=1, value=0.9)),
              fileInput("sampleDLCInput",label="Select one or more samples to upload", multiple=T),
              tags$p(style="font-weight:8;", "First ensure that the correct point names and cutoffs are set, then upload the files obtained by DLC (.csv exports). Press download button to get a metadata template .csv file. Fill in variables and re-upload the metadata file. See report for transform quality"),
              box(width = 8, title = "Report", collapsible = T, collapsed = T,
                  tags$div(style="margin: 10px;", fluidRow( verbatimTextOutput("DLCReport")))),
              downloadButton("downloadmetadatascaffold", "Download metadata template")
          ),
          box(width=4, title = "Samples",
              DT::dataTableOutput('samples_info'),
              span(textOutput("sampleWarning"), style="color:red"),
              actionButton("CorrectRange", "Align files")),

          box(width=5, title="Samples Metadata", DT::dataTableOutput('samples_metadata'), tags$p(textOutput("check_metadata"))),
          box(width=3, title="Samples colors", uiOutput("sColorsInputs"))
        ),
        tabItem("bins",
          box(width=12,
            selectInput("preview_sample", "Sample for preview", choices=c(), selectize=F),
            plotlyOutput("preview_bins")
          ),
          box(
            numericInput("timeFactor", "frames per second", min=0, max=100, value=5),
            tags$p("define the frames per second of pupillometry recordings to ensure that the recording time corresponds to the plotted time"),
            checkboxInput("TimeinMinutes", "Data plotted in minutes", value = F)
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
        tabItem("cleanup",
                box(width=12, title = "Clean-up",
                    selectInput("preview_sample_cleaning", "Sample for preview", choices=c(), selectize=F),
                    box(width = 6, title = "Raw sample", plotlyOutput("preview_raw")),
                    box(width = 6, title = "Cleaned sample", plotlyOutput("preview_clean")),
                    column(4,checkboxInput("cleanUp", "Clean up data", value = F), 
                           tags$p(style="font-weight:8;", "If checked outliers will be removed in all samples. Each individual measurement will be compared to the median within a window centered around it. If an outlier is detected, the value at this time-point will be set to the average of the previous and the next non-outlier measurement")),
                    column(4,numericInput("cleanWidth", label="Outlier-detection window size", value=10),
                           tags$p(style="font-weight:8;", "Size of the window used for outlier detection (in seconds or minutes, depending on the settings in the Bins tab)")),
                    column(4,numericInput("cleanStrength", label="Outlier-detection limit (% of median)", value=200),
                           tags$p(style="font-weight:8;", "If the value of a measurement is this percentage over the median value within the window, it will be considered an outlier"))
                )
        ),
        tabItem("settings",
            box(title="Plot",width =6,
               numericInput("interval", label="Interval size (in seconds)", min=0, max=10, step=0.1, value=0.5),
               tags$p("customize the interval at which data-points should be plotted"),
               checkboxInput("showPoints", label="Plot points", value=T),
               selectInput("plot_groupBy","Group by",choices=c(),selectize=T,multiple=T),
               selectInput("plot_errType","Error type",choices=c("Standard error"="SE", "Standard deviation"="SD","95% Confidence Interval"="CI"),selectize=F),
               sliderInput("opacity_SD", "Opacity of standard deviation/error", min=0, max=1, step=0.05, value=0.3)
            ),
            box( title="Normalization", width =6,
              checkboxInput("cb_normalize","Normalize to (first) baseline bin", value=F),
              selectInput("normDrift", "Normalize linear non-response drift across time", choices=c("No"="no","Global drift"="global","Run-specific drift"="specific"), selectize=F)
            ),
            box( title="Group colors",width =6, uiOutput("gColorsInputs")
            ),
            box(title="Bins appearance",width =6, offset = 0, collapsible=T, collapsed=T,
             checkboxInput("cb_plot_bins","Display bins on plot",value = T),
             colourInput("color_baselineBins", "Color of baseline bin(s)", value="black"),
             sliderInput("opacity_baselineBins", "Opacity of baseline bin(s)", min=0, max=1, step=0.05, value=0.1),
             colourInput("color_responseBins", "Color of response bin(s)", value="steelblue"),
             sliderInput("opacity_responseBins", "Opacity of response bin(s)", min=0, max=1, step=0.05, value=0.1)
            )
        ),
        tabItem("plot", height="1500px",
          box(title = "Interactive plot",width=12,collapsible=T, collapsed=F, withSpinner(plotlyOutput("mainPlot", height="600px"))),
          box(title = "Exportable plot",width=12, collapsible=T, collapsed=T,
              column(4, selectInput("plotType","Select plot type", choices=c("Line plot" = "LP","Ribbon plot"="RP"))),
              column(4, numericInput("PlotLineSize", label="Thickness of line", min=0, max=10, step=0.5, value=0.5)),
              column(4, textInput("plotTilte", label="Plot Title", value = "Pupillometry data")),
              column(4, textInput("legendTitle", label="Legend Title", value = "")),
              column(4, textInput("plotXax", label="x-axis label", value = "")),
              column(4, textInput("plotYax", label="y-axis label", value = "")),
              downloadButton("downloadPlot", "Download"),
              plotOutput(height = "650px","ExportablePlot"),
              box(title = "customize axes and export dimensions",width=12,collapsible=T, collapsed=T,
                column(4, numericInput("xax_low", label="lower boundary of x-axis", value=NA)),
                column(4, numericInput("xax_high", label="upper boundary of x-axis", value=NA)),
                column(4, numericInput("xax_step", label="Intervalsize of x-axis", value=NA)),
                column(4, numericInput("yax_low", label="lower boundary of y-axis", value=NA)),
                column(4, numericInput("yax_high", label="upper boundary of y-axis", value=NA)),
                column(4, numericInput("yax_step", label="intervalsize of y-axis", value=NA)),
                column(4, numericInput("plotwidth", label="export width (inch)", value=12)),
                column(4, numericInput("plotheigth", label="export heigth (inch)", value=8))
              )
          )
        ),
        tabItem("tests",
          box(width=12,
            column(6, selectInput("test_var","Test for differences across", choices=c(), selectize=F)),
            column(6, selectInput("animal_var","Animal variable", choices=c(), selectize=F)),
            column(12, tags$div(style="padding-top: 20px; text-align:center;", tags$h4("OR"))),
            column(6, textInput("test_formula", "Enter full formula")),
            column(6, textInput("test_formula0", "Enter reduced formula"))
          ),
          tags$div(style="margin: 10px;", fluidRow( (verbatimTextOutput("test_results")) ))
        ),
        tabItem("export",
                selectInput("dataset", "Choose a dataset", choices = c("Raw Data", "Normalized Data","Bin Results","Test Results")),
                downloadButton("downloadData", "Download"),
		tags$h3("Preview:"),
                tableOutput("exporttable")
        ),
        tabItem("usermanual",
                box(width=12, "Click download to access the pupillometry app user manual as pdf",
                    downloadButton("downloadmanual", "Download Manual"))
        )
      )
    )
  ))
}

.app.ui.sColorsInputs <- function(filenames, idPrefix='colorI_'){
  if(is.null(filenames) || length(filenames)==0) return(NULL)
  defCols <- getQualitativePalette(length(filenames))
  lapply( 1:length(filenames), id=idPrefix, colors=defCols, filenames=filenames, FUN=function(x, id, colors, filenames){
    colourInput(paste0(id,filenames[x]), label=paste(filenames[x]), value=colors[x])
  })
}
