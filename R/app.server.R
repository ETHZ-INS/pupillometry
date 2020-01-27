#' app.server
#'
#' Server function for the pupillometry app. When using the app locally, use `pupillometry.app()` instead.
#'
#' @return A function.
#' @export
app.server <- function(fromPackage=TRUE){
  library(data.table)

  function(input, output, session){

    #################
    # BEGIN Samples tab

    # File upload handler
    observeEvent( input$sampleFileInput,
      tryCatch({
        for(i in 1:length(input$sampleFileInput$name)){
          if(grepl("\\.mat$",input$sampleFileInput$name[[i]])){
            library(R.matlab)
            import <- as.data.frame(readMat(input$sampleFileInput$datapath[[i]])$R)
            names(import) <- c("Time","Diameter")
            dat$diameters[[basename(input$sampleFileInput$name[[i]])]] <- import
            updateSelectInput(session, "preview_sample", choices=names(dat$diameters))
            updateSelectInput(session, "preview_sample_cleaning", choices=names(dat$diameters))
          }else{
            if(grepl("\\.csv$",input$sampleFileInput$name[[i]])){
              metadat <- as.data.frame(fread(input$sampleFileInput$datapath[[i]]))
              rnames <- metadat[,1]
              metadat[,1] <- NULL
              metadat <- data.frame(sapply(metadat, FUN = function(x){gsub("[^[:alnum:]+.-]", ".", x)}))
              colnames(metadat) <- gsub("[^[:alnum:]+_.-]", ".", colnames(metadat))
              rownames(metadat) <- rnames
              dat$meta=metadat
              updateSelectInput(session, "plot_groupBy", choices=colnames(dat$meta))
              selTestVar <- input$test_var
              selAnimVar <- input$animal_var
              if(is.null(selTestVar) && "Group" %in% colnames(dat$meta)) selTestVar <- "Group"
              updateSelectInput(session, "test_var", choices=c("Response",colnames(dat$meta)), selected=selTestVar)
              updateSelectInput(session, "animal_var", choices=c(colnames(dat$meta)), selected=selAnimVar)
            }
          }
        }
      }, error=function(e){ stop(safeError(e)) })
    )
    
    observeEvent( input$sampleDLCInput,
                  tryCatch({
                    Report <- NULL
                    for(i in 1:length(input$sampleDLCInput$name)){
                      if(grepl("\\.csv$",input$sampleDLCInput$name[[i]])){
                        header <- as.data.frame(fread(input$sampleDLCInput$datapath[[i]],nrows = 2))
                        data <- as.data.frame(fread(input$sampleDLCInput$datapath[[i]],skip = 2))
                        Transformed <- TransformDLC(data,header,input$DLC_likelihoodcutoff, input$DLC_completenesscutoff, input$DLC_center_point, input$DLC_pupil_points)
                        dat$diameters[[basename(input$sampleDLCInput$name[[i]])]] <- Transformed$dat
                        Report <- append(Report,input$sampleDLCInput$name[[i]])
                        Report <- append(Report,Transformed$Report)
                        updateSelectInput(session, "preview_sample", choices=names(dat$diameters))
                        updateSelectInput(session, "preview_sample_cleaning", choices=names(dat$diameters))
                      }
                    }
                    output$DLCReport <- renderPrint(cat(Report,sep = "\n"))
                  }, error=function(e){ stop(safeError(e)) })
    )

    #remove all loaded samples
    observeEvent(input$removesamples, {
      dat$meta=NULL
      dat$diameters=list()
      updateSelectInput(session, "test_var", choices = character(0) , selected='')
      updateSelectInput(session, "animal_var", choices = character(0) , selected='')
      updateSelectInput(session, "plot_groupBy", choices = character(0) , selected='')
      updateSelectInput(session, "preview_sample", choices = character(0) , selected='')
      updateSelectInput(session, "preview_sample_cleaning", choices = character(0) , selected='')
    })
    
    #load examplary samples
    observeEvent(input$testsamples, {
      if(fromPackage && "pupillometry" %in% rownames(installed.packages())){
        data("example", package = "pupillometry")
      }else{
        # assume we're on shinyapps.io
        load("example.RData")
      }
      dat$meta <- example$meta
      dat$diameters <- example$diameters
      updateSelectInput(session, "plot_groupBy", choices=colnames(dat$meta))
      updateSelectInput(session, "preview_sample", choices=names(dat$diameters))
      updateSelectInput(session, "preview_sample_cleaning", choices=names(dat$diameters))
      selTestVar <- input$test_var
      selAnimVar <- input$animal_var
      updateSelectInput(session, "test_var", choices=c("Response",colnames(dat$meta)), selected=selTestVar)
      updateSelectInput(session, "animal_var", choices=c(colnames(dat$meta)), selected=selAnimVar)
    })
    
    # raw data and metadata will be stored in the `dat` reactive object
    dat <- reactiveValues(
      meta=NULL,
      diameters=list()
    )
    
    #alligns data of all files to be in the same range
    observeEvent(input$CorrectRange, {
      mins <- NULL
      aligned <- dat$diameters
      for(i in aligned){
        mins <- append(mins,min(i[,1]))
      }
      for(i in names(aligned)){
        aligned[[i]] <- aligned[[i]][aligned[[i]][,1] >= max(mins),]
        aligned[[i]][,1] <- aligned[[i]][,1] - (min(aligned[[i]][,1]) - max(mins))
      }
      maxs <- NULL
      for(i in aligned){
        maxs <- append(maxs,max(i[,1]))
      }
      for(i in names(aligned)){
        aligned[[i]] <- aligned[[i]][aligned[[i]][,1] <= min(maxs),]
      }
      dat$diameters <- aligned
    })
    
    # checks whether the data and metadata match
    datOk <- reactive({
      length( dat$diameters ) > 0 &&
      ( is.null(dat$meta) || all(names(dat$diameters) %in% row.names(dat$meta)) )
    })

    # prints some info about the uploaded files
    
    Filesanity <- reactive({
      length(unique(sapply(dat$diameters,FUN=function(x){ paste(range(x$Time),collapse="-") }))) <= 1
    })
    
    
    output$samples_info <- DT::renderDataTable({
      if(is.null(dat$diameters))return(NULL)
      DT::datatable(cbind(file=names(dat$diameters), range=sapply(dat$diameters,FUN=function(x){ paste(range(x$Time),collapse="-") })),rownames = FALSE)
    })
    
    output$sampleWarning <- renderText({
      if(!Filesanity())
        return("Warning: Samples are not in the same time range! This might interfere with plotting/analysis")
    })
    
    
    # prints the metadata
    output$samples_metadata <- DT::renderDataTable({
      if(is.null(dat$meta)) return(NULL)
      DT::datatable(cbind(filename=rownames(dat$meta), dat$meta),rownames = FALSE)
    })

    # prints error for non-matching data/metadata
    output$check_metadata <- renderText({
      if(datOk()){
        if(is.null(dat$meta)) return("No metadata given. Statistical tests will not be performed.")
        return("")
      }else{
        return("Some of the files uploaded do not have matching metadata.")
      }
    })
    
    # definition of default legend, x-axis and y-axis descriptions
    defaultleg <- reactive({
      if(!is.null(input$plot_groupBy)){
        input$plot_groupBy
      }else{
        "Sample"
      }
    })

    #default X axis description
    defaultXax <- reactive({
      if(input$TimeinMinutes){
        "Time (min)"
      }else{
        "Time (s)"
      }
    })

    #default Y axis description
    defaultYax <- reactive({
      if(input$cb_normalize){
        "Radius (% of baseline)"
      }else{
        "Radius (pixels)"
      }
    })

    # prints the color inputs
    output$sColorsInputs <- renderUI({
      .app.ui.sColorsInputs(names(dat$diameters))
    })
    output$gColorsInputs <- renderUI({
      if(!is.null(input$plot_groupBy)){
        groups <- apply(dat$meta[,input$plot_groupBy,drop=F],1,collapse="_",paste)
        .app.ui.sColorsInputs(unique(groups),idPrefix='colorG_')
      }
    })

    #sample colors
    sColors <- reactive({
      iN <- names(input)
      w <- grep("^colorI",iN)
      if(length(w)==0) return(c())
      sapply(iN[w],FUN=function(x) input[[x]] )
    })
    
    #group colors
    gColors <- reactive({
      iN <- names(input)
      groups <- apply(dat$meta[,input$plot_groupBy,drop=F],1,collapse="_",paste)
      groups <- paste("colorG",levels(as.factor(groups)),sep="_")
      w <- match(groups,iN)
      if(length(w)==0) return(c())
      sapply(iN[w],FUN=function(x) input[[x]] )
    })

    # prints the menu badge for samples
    output$badgeText_samples <- renderText({
      if(length(dat$diameters)==0) return("empty")
      if(datOk()) return("ok")
      return("error")
    })

    # END samples tab
    #################
    # BEGIN Bins tab

    # bin ranges are stored in the `bins()` reactive object
    bins <- reactive({
      list( baseline=.parseBins(input$baseline_bins),
            response=.parseBins(input$response_bins) )
    })

    # returns whether the bins are error-free and matching
    binsOk <- reactive({
      bb <- bins()
      !is.null(bb$baseline) && !is.null(bb$response) && !(nrow(bb$baseline)>1 && nrow(bb$baseline)!=nrow(bb$response))
    })

    # prints error msg for bins
    output$check_bins <- renderText({
      if(binsOk()) return("")
      bb <- bins()
      err <- ""
      if(is.null(bb$baseline)) err <- paste0(err, "\n", "Baseline bin(s) missing or malformed!")
      if(is.null(bb$response)) err <- paste0(err, "\n", "Response bin(s) missing or malformed!")
      if(err!="") return(err)
      if(nrow(bb$baseline)>1 && nrow(bb$baseline)!=nrow(bb$response)) return("The number of response bins does not match the baseline bins.")
    })

    # prints the bin preview plot
    output$preview_bins <- renderPlotly({
      if(is.null(input$preview_sample)) return(NULL)
      bb <- bins()
      d <- normDat()[[input$preview_sample]]
      yrange <- range(dat$diameters[[input$preview_sample]]$Diameter)
      shapes <- c( .getBinAreas(bb$baseline, yrange, color=input$color_baselineBins, opacity=input$opacity_baselineBins ),
                   .getBinAreas(bb$response, yrange, color=input$color_responseBins, opacity=input$opacity_responseBins )
                   )
      p <- plot_ly(d, x=~Time, y=~Diameter, type="scatter",mode="markers")
      if(input$TimeinMinutes){
        layout(p, shapes=shapes, xaxis=list(title="Time (min)"), yaxis=list(title="Radius (pixels)"))
        }else
        layout(p, shapes=shapes, xaxis=list(title="Time (s)"), yaxis=list(title="Radius (pixels)"))
    })
    
    #prints the cleaning preview (raw)
    output$preview_raw <- renderPlotly({
      if(is.null(input$preview_sample_cleaning)) return(NULL)
      bb <- bins()
      d <- dat$diameters[[input$preview_sample_cleaning]]
      yrange <- range(dat$diameters[[input$preview_sample_cleaning]]$Diameter)
      shapes <- c( .getBinAreas(bb$baseline, yrange, color=input$color_baselineBins, opacity=input$opacity_baselineBins ),
                   .getBinAreas(bb$response, yrange, color=input$color_responseBins, opacity=input$opacity_responseBins )
      )
      d$Time <- d$Time * normfactor()
      p <- plot_ly(d, x=~Time, y=~Diameter, type="scatter",mode="markers")
      if(input$TimeinMinutes){
        layout(p, shapes=shapes, xaxis=list(title="Time (min)"), yaxis=list(title="Radius (pixels)"))
      }else
        layout(p, shapes=shapes, xaxis=list(title="Time (s)"), yaxis=list(title="Radius (pixels)"))
    })

    #prints the cleaning preview (cleaned)
    output$preview_clean <- renderPlotly({
      if(is.null(input$preview_sample_cleaning)) return(NULL)
      bb <- bins()
      d <- normDat()[[input$preview_sample_cleaning]]
      yrange <- range(dat$diameters[[input$preview_sample_cleaning]]$Diameter)
      shapes <- c( .getBinAreas(bb$baseline, yrange, color=input$color_baselineBins, opacity=input$opacity_baselineBins ),
                   .getBinAreas(bb$response, yrange, color=input$color_responseBins, opacity=input$opacity_responseBins )
      )
      p <- plot_ly(d, x=~Time, y=~Diameter, type="scatter",mode="markers")
      if(input$TimeinMinutes){
        layout(p, shapes=shapes, xaxis=list(title="Time (min)"), yaxis=list(title="Radius (pixels)"))
      }else
        layout(p, shapes=shapes, xaxis=list(title="Time (s)"), yaxis=list(title="Radius (pixels)"))
    })
    
    # prints the menu badge for bins
    output$badgeText_bins <- renderText({
      if(binsOk()) return("ok")
      if(is.null(bins()$baseline) || is.null(bins()$response)) return("empty")
      return("error")
    })

    # END bins tab
    #################
    # BEGIN Stats

    #normalization factor for time
    normfactor <-reactive({
      1 / (input$timeFactor*(1+59*input$TimeinMinutes))
    })
    
    
    # normalized data
    normDat <- reactive({
      if(!datOk()) return(NULL)
      norm <- dat
      if( !input$cb_normalize & input$normDrift=="no" ) return({
        lapply(cleanDat(norm$diameters, input$cleanWidth/(2*normfactor()), input$cleanStrength, input$cleanUp), FUN=function(x){ x$Time <- x$Time*normfactor();x })
        })
      if(!datOk() | (!binsOk())) return(NULL)
      if(is.null(bins()$baseline) || is.null(bins()$response)) return(NULL)
      normDiameters(norm$diameters, bins(), tf=normfactor(), normalizeToBaseline=input$cb_normalize, normDrift=input$normDrift, input$cleanWidth/(2*normfactor()), input$cleanStrength, input$cleanUp)
    })
      
    test_results <- reactive({
      d <- normDat()
      if(is.null(d)) return(NULL)
      if(is.null(dat$meta)) return("No metadata given.")
      if(!binsOk()) return("Bins are not appropriately defined.")
      meta <- dat$meta[names(d),]
      nbl <- nrow(bins()$baseline)
      nrb <- nrow(bins()$response)
      allbins <- rbind( cbind(bins()$baseline, Time=1:nbl, Response=rep(0,nbl)),
                        cbind(bins()$response, Time=1:nrb, Response=rep(1,nrb)) )
      d <- cbind(do.call(rbind, .getBinData(d, allbins)), meta[rep(names(d),each = (nbl + nrb)),])
      testResponse(d,input$test_var, forms=list(full=input$test_formula, reduced=input$test_formula0), input$normDrift,input$cb_normalize,input$animal_var, input$cleanUp)
    })

    output$test_results <- renderPrint(cat(test_results(), sep="\n"))

    # END Stats
    #################
    # BEGIN plot tab

    output$mainPlot <- renderPlotly({
      ll <- normDat()
      
      bb <- bins()
      if(is.null(bb$baseline) & input$cb_normalize){
        stop("Warning, no baseline bin defined. Can not print normalized plot")
      }
      if(is.null(ll)) return(NULL)

      ll <- .avgIntervals(ll, input$interval * normfactor() * input$timeFactor)
      colors <- sColors()

      gb <- input$plot_groupBy
      if(!is.null(gb)){
        groups <- apply(dat$meta[,gb,drop=F],1,collapse="_",paste)
        colors <- gColors()
        ll <- .getRibbonData(ll,input$plot_errType,groups)
      }

      if(input$cb_plot_bins){
        bb <- bins()
        yrange <- range(unlist(lapply(ll,FUN=function(x) as.numeric(as.matrix(x[,-1])))))
        shapes <- c( .getBinAreas(bb$baseline, yrange, color=input$color_baselineBins, opacity=input$opacity_baselineBins ),
                     .getBinAreas(bb$response, yrange, color=input$color_responseBins, opacity=input$opacity_responseBins )
        )
      }else{
        shapes <- NULL
      }

      p <- plot_ly(type="scatter",mode="markers+lines")
      for(i in 1:length(ll)){
        a <- ll[[i]]
        if(all(c("Diameter_low","Diameter_high") %in% colnames(a))){
          p <- add_polygons(p, x=c(a$Time,rev(a$Time)), y=c(a$Diameter_low,rev(a$Diameter_high)), color=I(colors[i]), opacity=input$opacity_SD, name=paste(names(ll)[i],input$plot_errType) )
        }
        p <- add_trace(p, data=a, mode=ifelse(input$showPoints,"markers+lines","lines"), x=~Time, y=~Diameter, color=I(colors[i]), name=names(ll)[i])
      }
      layout(p, shapes=shapes, xaxis=list(title=defaultXax()), yaxis=list(title=defaultYax()))
      
    })
    
    output$ExportablePlot <- renderPlot({
      
      #load bins and data
      bb <- bins()
      if(is.null(bb$baseline) & input$cb_normalize){
        stop("No baseline bin defined. Can not print normalized plot")
      }
      ll <- normDat()
      ll <- .avgIntervals(ll, input$interval * normfactor() * input$timeFactor)
      gb <- input$plot_groupBy
      if(!is.null(gb)) groups <- apply(dat$meta[,gb,drop=F],1,collapse="_",paste)
      
      selplotType <- input$plotType
      updateSelectInput(session, "plotType", choices=c("Line plot" = "LP","Ribbon plot"="RP"), selected=selplotType)
      
      #legend title
      if(input$legendTitle != ""){
        legTit <- input$legendTitle
      }else{
        legTit <- defaultleg()
      }
      
      #line plot
      PlotData <- NULL
      if(selplotType == "LP"){
        for(i in names(ll)){
          PlotData <- rbind(PlotData,cbind(FileName = i, PlotBy = i, ll[[i]]))
        }
        cols <- sColors()
        
        if(!is.null(input$plot_groupBy)){
        PlotData$PlotBy <- groups[as.character(PlotData$FileName)]
        cols <- gColors()
        }
        
        p <- ggplot(PlotData,aes(Time,Diameter, group = FileName, color = PlotBy)) +
          geom_line(aes(color = PlotBy),size = input$PlotLineSize)
        ymax <- max(PlotData$Diameter)
        ymin <- min(PlotData$Diameter)
      }
      
      #ribbon plot
      if(selplotType == "RP"){
        if(is.null(gb))stop("No Groups defined. Can not print ribbon plot")
        ll <- .getRibbonData(ll,input$plot_errType,groups)
        for(i in names(ll)){
          PlotData <- rbind(PlotData,cbind(PlotBy = i, ll[[i]]))
        }
        cols <- gColors()
        
        p <- ggplot(PlotData,aes(Time,Diameter, group = PlotBy)) + 
          geom_line(aes(group = PlotBy, color = PlotBy), size = input$PlotLineSize) + 
          geom_ribbon(aes(ymin = Diameter_low,ymax = Diameter_high,color = PlotBy, fill = PlotBy)) +
          scale_fill_manual(values = alpha(cols, input$opacity_SD),name = legTit)
        
        ymax <- max(PlotData$Diameter_high)
        ymin <- min(PlotData$Diameter_low)
      }

      # ensure colors are assigned to correct sample/groups
      cols <- as.data.frame(cols)
      cols <- cols[match(substring(rownames(cols),8),levels(as.factor(PlotData$PlotBy))),1]

      #x and y axis descriptions
      if(input$plotXax != ""){
        p <- p + xlab(input$plotXax)
      }else{
        p <- p + xlab(defaultXax())
      }
      if(input$plotYax != ""){
        p <- p + ylab(input$plotYax)
      }else{
        p <- p + ylab(defaultYax())
      }
      
      #plot points?
      if(input$showPoints){
        p <- p + geom_point(aes(color = PlotBy))
      }

      #bins
      if(input$cb_plot_bins & binsOk()){
        p <- p + geom_rect(data = as.data.frame(bb$baseline),inherit.aes = FALSE, mapping =aes(xmin=V1,xmax=V2,ymin=ymin,ymax=ymax), fill=input$color_baselineBins, alpha = input$opacity_baselineBins)+
          geom_rect(data = as.data.frame(bb$response),inherit.aes = FALSE, mapping =aes(xmin=V1,xmax=V2,ymin=ymin,ymax=ymax), fill=input$color_responseBins, alpha = input$opacity_responseBins)
      }

      #x and y axis boundaries and intervals
      if(!is.na(input$xax_low * input$xax_high * input$xax_step)){
        p <- p + scale_x_continuous(limits = c(input$xax_low, input$xax_high), breaks = seq(from = input$xax_low, to = input$xax_high, by = input$xax_step))
      }
      if(!is.na(input$yax_low * input$yax_high * input$yax_step)){
        p <- p + scale_y_continuous(limits = c(input$yax_low, input$yax_high), breaks = seq(from = input$yax_low, to = input$yax_high, by = input$yax_step))
      }
      
      # cosmetics
      p <- p + 
        theme_bw() + 
        scale_color_manual(values = alpha(cols,1), name = legTit) +
        ggtitle(input$plotTilte) +
        theme(plot.title = element_text(hjust = 0.5))
      
      PlotExport$p <- p
      plot(PlotExport$p)
      
    },height = 600, width = 800)
    
    # END plot tab
    #################
    # BEGIN export
    
    datasetExport <- reactive({
      switch(input$dataset, 
             "Raw Data" = lapply(dat$diameters, FUN=function(x){ x$Time <- x$Time*normfactor();x }),
             "Normalized Data" = normDat(),
             "Bin Results" = {
               d <- normDat()
               if(is.null(d)) return(NULL)
               if(is.null(dat$meta)) stop("No metadata given.")
               meta <- dat$meta[names(d),]
               nbl <- nrow(bins()$baseline)
               nrb <- nrow(bins()$response)
               allbins <- rbind( cbind(bins()$baseline, Time=1:nbl, Response=rep(0,nbl)),
                                 cbind(bins()$response, Time=1:nrb, Response=rep(1,nrb)) )
               cbind(do.call(rbind, .getBinData(d, allbins)), meta[rep(names(d),each = (nbl + nrb)),])
             },
             "Test Results" = test_results())
      
    })
    PlotExport <- reactiveValues(
      p = NULL
    )
    
    output$exporttable <- renderTable({
      head(datasetExport())
    })
    
    output$downloadData <- downloadHandler(
      filename = function() {
        paste(input$dataset,ifelse(input$dataset=="Test Results",".txt",".csv"), sep = "")
      },
      content = function(file){
	if(input$dataset=="Test Results"){
		cat(datasetExport(), file=file, sep="\n")
	}else{
	        write.csv(datasetExport(), file, row.names = FALSE)
	}
      }
    )
    
    output$downloadmetadatascaffold <- downloadHandler(
      filename = function() {
        "metadata.csv"
      },
      content = function(file){
        write.table(data.frame(filename = names(dat$diameters),Animal = names(dat$diameters)),file, sep = ";",row.names = F)
      }
    )

    output$downloadPlot <- downloadHandler(
      filename = function() {
        "ExportedPlot.pdf"
      },
      content = function(file){
        pdf(file,width=input$plotwidth, height=input$plotheight)
        print(PlotExport$p)
        dev.off()
      }
    )
    
    output$downloadmanual <- downloadHandler(
      filename <- function() {
        "PupillometryApp_UserManual.pdf"
      },
      content <- function(file) {
        if(fromPackage && "pupillometry" %in% rownames(installed.packages())){
          fp <- system.file("docs/PupillometryApp_UserManual.pdf",package="pupillometry")
        }else{
          # assume we're on shinyapps.io
          fp <- "PupillometryApp_UserManual.pdf"
        }
        file.copy(fp, file)
      },
      contentType = "application/pdf"
    )
  }
}
