## refai/korr.qmd
rp <- paste0(rprojroot::find_rstudio_root_file())
source(paste0(rp,"/R/ai.R"))
library(ellmer)
library(R6)

lmod <- c("openai","groq","google_gemini","anthropic","perplexity")[1]
psys <- "
- Language for output: Norsk
- Format: Markdown
"
### Ny samtale 
nysm <- T
pque <- " 
Gi meg navnet på tre  kjente leger
"
fill <- ""
AIint(pque,fils="conv.qmd",fill=fill,init=nysm,psys=psys,lmod=lmod)
# conv.qmd





