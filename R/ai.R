##############################################################################################################
### Functions
library(ellmer)
get_current_time <- function(tz = "UTC") {
  format(Sys.time(), tz = tz, usetz = TRUE)
}
get_current_time <- ellmer::tool(
  get_current_time,
  name = "get_current_time",
  description = "Returns the current time.",
  arguments = list(
    tz = type_string(
      "Time zone to display the current time in. Defaults to `\"UTC\"`.",
      required = FALSE
    )
  )
)
screenshot_website <- function(url) {
    tmpf <- withr::local_tempfile(fileext = ".png")
    webshot2::webshot(url, file = tmpf)
    content_image_file(tmpf)
}
screenshot_website <- ellmer::tool(
  screenshot_website,
  name = "screenshot_website",
  description = "Take a screenshot of a website.",
  arguments = list(
    url = type_string("The URL of the website")
  )
)
##############################################################################################################
### Initialize 
####  1. The data-variables initializer 
AIasst <- R6::R6Class("AIasst",
  public = list(
    lmodel = NULL,
    chatobj = NULL,
    cinteg = NULL,
    backtext = NULL,
    filstr = list(),
    tools = list(),
    svar = NULL,
    gses = FALSE,
    sses = FALSE,
    qmdo = FALSE
  )
)

#### 2. The method initializer 
AIasst$set("public", "initialize", 
	     function(
          fill=NULL,
		      fild=NULL,
		      fils=NULL,
		      psys = "Default prompt",
		      init=FALSE,
		      gses=FALSE,
		      sses=FALSE,
		      lmodel = c("openai","groq","google_gemini","anthropic")[1], 
		      qmd=NULL,
		      tools=NULL,
		      refdr="AIrep") {
  self$filstr[["init"]] <- init
  self$filstr[["fill"]] <- fill
  self$filstr[["fild"]] <- fild
  self$filstr[["fils"]] <- fils
  self$filstr[["psys"]] <- psys
  self$filstr[["sses"]] <- sses
  self$filstr[["gses"]] <- gses
  self$filstr[["filr"]] <- paste0(fild,"/",refdr)
  self$filstr[["filn"]] <- sub("\\..*$", "",paste0(self$filstr[["filr"]],"/",self$filstr[["fils"]]))
  self$tools <- tools
  self$lmodel <- lmodel
  self$chatobj <- ellmer::chat(lmodel,system_prompt=self$filstr$psys)
  # Load
  if (isTRUE(grepl("\\.(qmd|md|R$)", self$filstr$fill[[1]]))) {
  #if (isTRUE(grepl("filename\\..*", self$filstr$fill[[1]]))) {
    flbt <- readLines(paste0(self$filstr$fild,"/",self$filstr$fill[[1]]))
    self$filstr[["flbt"]] <- flbt
  } else {
    flbt <- paste0(self$filstr[["fill"]])
    self$filstr[["flbt"]] <- flbt
  }
  # Save
  if (isTRUE(init)) {
     fils <- paste0(self$filstr[["filn"]], c(".qmd", ".rds"))
     exvc <- file.exists(fils)
     if (any(exvc)) {
       file.remove(fils[exvc])
     }
     if (!dir.exists(self$filstr$filr)) {
       dir.create(self$filstr$filr)
     }
  } else {
     obj <- paste0(self$filstr[["filn"]], c(".rds"))
     rRDS <- readRDS(obj)
     self$chatobj$set_turns(rRDS)
     self$filstr[["gses"]] <- self$chatobj$get_turns(include_system_prompt = sypr)
     #self$filstr[["gses"]] <- self$chatobj$get_turns()
  }
})
##############################################################################################################
### Methods
AIasst$set("public", "regtool", function(tools=NULL){
  for (t in tools) {
    tlfd <- get(t)
    self$chatobj$register_tool(tlfd)
  }
})

AIasst$set("public", "aichat", function(x=NULL,lagq=TRUE,lagt=TRUE,sypr=TRUE) {
  # 0. Spørsmål (fullstendig)
  inpu <- paste(x,":",self$backtext)
  # 1. SEND melding → oppdaterer intern state
  self$svar <- self$chatobj$chat(inpu, echo="none")
  # 2. HENT oppdatert samtalehistorikk
  self$sses <- self$chatobj$get_turns(include_system_prompt = sypr)
  if (isTRUE(lagq)) {
    filn <- paste0(self$filstr[["filr"]],"/",self$filstr[["fils"]])
    write(self$svar, file =filn ,append = !self$filstr[["init"]])
  }
  if (isTRUE(lagt)) {
    filt <- paste0(self$filstr[["filn"]],".rds")
    saveRDS(self$sses,file=filt)

  }
})
AIasst$set("public", "dialog", function() {
})
##############################################################################################################
### Function for R& objects
AIint <- function(
		  pque=NULL,
		  fill=NULL,
		  fild=paste0(rprojroot::find_rstudio_root_file()),
		  fils=NULL,
		  psys=NULL,
		  init=TRUE,
		  gses=FALSE,
		  sses=FALSE,
		  lmod="openai",
		  tools=NULL,
      refdr="AIrep"
		  )
		  {
  bankc <- AIasst$new(fill=fill,fild=fild,fils=fils,psys=psys,init=init,gses=gses,sses=sses,lmod=lmod,tools=tools,refdr=refdr)
  #is.null(tools)
  if (!is.null(tools)) {
    bankc$regtool(bankc$tools)
  }
  #is.null(pque)
  if (!is.null(pque)) {
    bankc$aichat(c(pque, bankc$filstr$flbt))
  }
  return(bankc)
}
##############################################################################################################

