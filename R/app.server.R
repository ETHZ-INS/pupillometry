#' app.server
#'
#' Server function for the pupillometry app. When using the app locally, use `pupillometry.app()` instead.
#'
#' @return A function.
#' @export
app.server <- function(){
  library(data.table)

  function(input, output, session){

    #################
    # BEGIN Samples tab

    # File upload handler
    observeEvent( input$sampleFileInput,
      tryCatch({
        library(R.matlab)
        for(i in 1:length(input$sampleFileInput$name)){
          if(grepl("\\.mat$",input$sampleFileInput$name[[i]])){
            import <- as.data.frame(readMat(input$sampleFileInput$datapath[[i]])$R)
            names(import) <- c("Time","Diameter")
            dat$diameters[[basename(input$sampleFileInput$name[[i]])]] <- import
            updateSelectInput(session, "preview_sample", choices=names(dat$diameters))
          }else{
            if(grepl("\\.csv$",input$sampleFileInput$name[[i]])){
              metadat <- as.data.frame(fread(input$sampleFileInput$datapath[[i]]))
              row.names(metadat) <- metadat[,1]
              metadat[,1] <- NULL
              dat$meta=metadat
              updateSelectInput(session, "plot_groupBy", choices=colnames(dat$meta))
              selTestVar <- input$test_var
              if(is.null(selTestVar) && "Group" %in% colnames(dat$meta)) selTestVar <- "Group"
              updateSelectInput(session, "test_var", choices=c("Response",colnames(dat$meta)), selected=selTestVar)
            }
          }
        }
      }, error=function(e){ stop(safeError(e)) })
    )

    # raw data and metadata will be stored in the `dat` reactive object
    dat <- reactiveValues(
      meta=NULL,
      diameters=list()
    )

    # checks whether the data and metadata match
    datOk <- reactive({
      length( dat$diameters ) > 0 &&
      ( is.null(dat$meta) || all(names(dat$diameters) %in% row.names(dat$meta)) )
    })

    # prints some info about the uploaded files
    output$samples_info <- renderTable({
      cbind(file=names(dat$diameters), range=sapply(dat$diameters,FUN=function(x){ paste(range(x$Time),collapse="-") }))
    })

    # prints the metadata
    output$samples_metadata <- renderTable({
      if(is.null(dat$meta)) return(NULL)
      cbind(filename=rownames(dat$meta), dat$meta)
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

    # prints the color inputs
    output$sColorsInputs <- renderUI({
      .app.ui.sColorsInputs(names(dat$diameters))
    })

    sColors <- reactive({
      iN <- names(input)
      w <- grep("^colorI",iN)
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
      d <- dat$diameters[[input$preview_sample]]
      yrange <- range(dat$diameters[[input$preview_sample]]$Diameter)
      shapes <- c( .getBinAreas(bb$baseline, yrange, color=input$color_baselineBins, opacity=input$opacity_baselineBins ),
                   .getBinAreas(bb$response, yrange, color=input$color_responseBins, opacity=input$opacity_responseBins )
                   )
      d$Time <- d$Time * normfactor()
      p <- plot_ly(d, x=~Time, y=~Diameter, type="scatter",mode="markers")
      if(input$TimeinMinutes){
        layout(p, shapes=shapes, xaxis=list(title="Time (min)"), yaxis=list(title="Diameter (pixels)"))
        }else
        layout(p, shapes=shapes, xaxis=list(title="Time (s)"), yaxis=list(title="Diameter (pixels)"))
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

    normfactor <-reactive({
      1 / (input$timeFactor*(1+59*input$TimeinMinutes))
    })
    
    # normalized data
    normDat <- reactive({
      if(!datOk()) return(NULL)
      if( !input$cb_normalize & input$normDrift=="no" ) return({
        lapply(dat$diameters, FUN=function(x){ x$Time <- x$Time*normfactor();x })
        })
      if(!datOk() | (!binsOk())) return(NULL)
      if(is.null(bins()$baseline) || is.null(bins()$response)) return(NULL)
      normDiameters(dat, bins(), tf=normfactor(), normalizeToBaseline=input$cb_normalize, normDrift=input$normDrift)
    })


    output$test_results <- renderPrint({
      d <- normDat()
      if(is.null(d)) return(NULL)
      if(is.null(dat$meta)) stop("No metadata given.")
      meta <- dat$meta[names(d),]
      nbl <- nrow(bins()$baseline)
      nrb <- nrow(bins()$response)
      allbins <- rbind( cbind(bins()$baseline, Time=1:nbl, Response=rep(0,nbl)),
                        cbind(bins()$response, Time=1:nrb, Response=rep(1,nrb)) )
      d <- cbind(do.call(rbind, .getBinData(d, allbins)), meta[rep(names(d),each = (nbl + nrb)),])
      testResponse(d,input$test_var, forms=list(full=input$test_formula, reduced=input$test_formula0))
    })

    # END Stats
    #################
    # BEGIN plot tab


    output$mainPlot <- renderPlotly({
      ll <- normDat()
      if(is.null(ll)) return(NULL)

      ll <- .avgIntervals(ll, input$interval * normfactor())

      colors <- sColors()

      gb <- input$plot_groupBy
      if(!is.null(gb)){
        groups <- apply(dat$meta[,gb,drop=F],1,collapse=" ",paste)
        colors <- colors[!duplicated(groups)]
        ll <- lapply(split(ll,groups), FUN=function(x){
          if(length(x)==1) x[[1]]
          m <- rowMeans(sapply(x,FUN=function(x) x[,2]))
          SD <- apply(sapply(x,FUN=function(x) x[,2]),1,FUN=sd)
          if(input$plot_errType=="SE") SD <- SD/sqrt(length(x))
          data.frame(Time=x[[1]][,1], Diameter=m, Diameter_low=m-SD, Diameter_high=m+SD)
        })
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
          p <- add_polygons(p, x=c(a$Time,rev(a$Time)), y=c(a$Diameter_low,rev(a$Diameter_high)), color=colors[i], opacity=input$opacity_SD, name=paste(names(ll)[i],input$plot_errType) )
        }
        p <- add_trace(p, data=a, mode=ifelse(input$showPoints,"markers+lines","lines"), x=~Time, y=~Diameter, color=colors[i], name=names(ll)[i])
      }
      if(input$TimeinMinutes & input$cb_normalize){
      layout(p, shapes=shapes, xaxis=list(title="Time (min)"), yaxis=list(title="Diameter (% of baseline)"))
      }else if(input$TimeinMinutes & !input$cb_normalize){
      layout(p, shapes=shapes, xaxis=list(title="Time (min)"), yaxis=list(title="Diameter (pixels)"))
      }
      else if(!input$TimeinMinutes & input$cb_normalize){
      layout(p, shapes=shapes, xaxis=list(title="Time (s)"), yaxis=list(title="Diameter (% of baseline)"))
      }else{
      layout(p, shapes=shapes, xaxis=list(title="Time (s)"), yaxis=list(title="Diameter (pixels)"))
      }
    })
    
    # END plot tab
    #################
    # BEGIN export
    
    datasetExport <- reactive({
              meta <- dat$meta
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
             })
    })
    
    output$exporttable <- renderTable({
      head(datasetExport())
    })
    
    output$downloadData <- downloadHandler(
      filename = function() {
        paste(input$dataset,".csv", sep = "")
      },
      content = function(file){
        write.csv(datasetExport(), file, row.names = FALSE)
      }
    )
    
  }
}
