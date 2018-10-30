#' testResponse
#'
#' @param response_data A data.frame
#' @param testVar The variable to be tested (should be a column of `response_data`). Ignored if `forms` is given and complete.
#' @param forms An optional list with slots `full` and `reduced` containing the respective formulas.
#'
#' @export
testResponse <- function(response_data, testVar="Response", forms=list()){
  library(lme4)
  library(lmerTest)
  library(lsmeans)
  
  if(!is.null(response_data$Time)){
    response_data$Time <- as.factor(response_data$Time)
  }
  
  nbl <- length(unique(response_data[which(response_data$Response==0),"Time"]))
  nrb <- length(unique(response_data[which(response_data$Response==1),"Time"]))
  if(nbl>1 & nbl!=nrb) stop("There must either be a single baseline bin or the same number as the number of response bins.")

  if(is.character(response_data[[testVar]])) response_data[[testVar]] <- as.factor(response_data[[testVar]])
  if(length(levels(response_data[[testVar]]))==1) testVar <- "Response"

  if( !is.null(forms$full) && forms$full!="" && !is.null(forms$reduced) && forms$reduced!=""){
    tryCatch({
      form <- as.character(forms$full)
      form0 <- as.character(forms$reduced)
      x <- lapply(forms,FUN=function(x){
        x <- strsplit(x,"~",fixed=T)[[1]][[2]]
        for(f in c("*",":")) x <- gsub(f,"+",x,fixed=T)
        x <- strsplit(x,"+",fixed=T)[[1]]
      })
      tTerms <- setdiff(x$full,x$reduced)
    }, error=function(e){ stop("Cannot parse formula") })
    if(is(try(as.formula(form),silent=T),"try-error")) stop("Full formula is not valid")
    if(is(try(as.formula(form0),silent=T),"try-error")) stop("Reduced formula is not valid")
    useLmer <- grepl("\\(",form)
  }else{
    # building the models
    useLmer <- "Animal" %in% colnames(response_data)
    terms0 <- vector(mode="character",length=0)
    if(useLmer) terms0 <- "(1|Animal)"
    tTerms <- unique(c(testVar,"Response"))
    if(nbl>1 & nrb>1){
      # multiple response bins
      terms0 <- c(terms0, "Time")
    }
    form <- gsub("\\+$","",paste0("Diameter~",paste(paste(tTerms,collapse="*"),paste(terms0,collapse="+"),sep="+")))
    form0 <- paste0("Diameter~",ifelse(length(terms0),paste(terms0,collapse="+"),1))
  }

  cat(paste0("Testing model `",form,"` against `",form0,"`\n\n"))

  if(useLmer){
    model <- lmer(as.formula(form), data=response_data)
    model.null <- lmer(as.formula(form0), data=response_data, REML=F)
  }else{
    model <- lm(as.formula(form), data=response_data)
    model.null <- lm(as.formula(form0), data=response_data)
  }

  cat("\n =============== VALIDITY OF THE MODEL, COMPARING TO NULL MODEL: =============== \n\n")
  print(anova(model.null,model))
  cat("\n =============== STATISTICAL RESULTS: =============== \n\n")
  print(summary(model))
  cat("\n =============== PAIRWISE POST-HOC COMPARISONS: =============== \n\n")
  print(lsmeans(model, as.formula(paste0("pairwise~",paste(tTerms,collapse="+"))), adjust="tukey"))
}