mergeRMD_hofc <- function (mergedFileName = "book.Rmd", title = ".", files, preamble, 
          print.paths = FALSE, print.newpage = TRUE) 
{
  dir = dirname(mergedFileName)
  old = setwd(dir)
  if (file.exists(mergedFileName)) {
    warning(paste0(mergedFileName, " already exists"))
  }
  text.input = vector("character", 1)
  # task.names = paste0("Aufgabe ", 1:length(files))
  for (i in 1:length(files)) {
    text.input = readLines(files[i], warn = FALSE)
    metaspan = grep("---", text.input)
    cell.insert = character(length = 1)
    j = 0
    #cell.insert[j] = task.names[i]
    cell.insert[j + 1] = "```{r, echo=FALSE, eval=TRUE,comment=NA}"
    cell.insert[j + 2] = "options(useFancyQuotes = FALSE)"
    cell.insert[j + 3] = paste("files<-c(", paste(sQuote(noquote(files)), 
                                                  collapse = ","), ")")
    cell.insert[j + 4] = paste("baseDir=dirname(files[", 
                               i, "])")

    cell.insert[j + 5] = "```"
    if (print.paths == T) {
      cell.insert[j + 6] = paste0("\\href{run:`r dirname(files[", 
                                  i, "])`}{`r paste0(\"...\", gsub(pattern=\"\\\\_\", replacement=\"\\\\\\\\_\",substring(dirname(files[", 
                                  i, "]),first=nchar(dirname(files[", i, "]))-76,last=nchar(dirname(files[", 
                                  i, "]))))) `}")
    }
    else {
      cell.insert[j + 6] = ""
    }
    
    text.input = c(text.input[1:metaspan[2]], cell.insert[1:(j + 
                                                               6)], text.input[(metaspan[2] + 1):length(text.input)])
    text.input = text.input[-c(metaspan[1]:metaspan[2])]
    if (print.newpage == T) {
      text.input[length(text.input) + 1] = "\\newpage"
    }
    else {
      text.input[length(text.input) + 1] = ""
    }
    text.input[length(text.input) + 1] = ""
    assign(paste0("text.chunk", i), text.input)
  }
  text.combined = eval(parse(text = paste0("c(", paste0("text.chunk", 
                                                        1:length(files), collapse = ","), ")")))
 
  ### Aufgabe NR in front of the excersice title
  count.exercise <- 1
  for(jj in 1:length(text.combined))
  {
    if( grepl(pattern = '##', x = text.combined[jj]) ){
    text.combined[jj] <- gsub(pattern = '##', x = text.combined[jj], replacement = paste('## Aufgabe ', count.exercise, ":", sep = "") )
    count.exercise <- count.exercise + 1
    }
  }
  rm(count.exercise)

  
  book_header = paste("---\noutput:", "pdf_document", "\n---")
  cell.insert2 = paste("\\begin{center} \\huge{", title, "} \\end{center}")
  
  if (!missing(preamble)) {
    preamble.input = vector("character", 1)
    preamble.input = readLines(preamble)
    metaspan = grep("---", preamble.input)
    preamble.input = preamble.input[-c(metaspan[1]:metaspan[2])]
    preamble.input[length(preamble.input) + 1] = ""
    text.final = c(book_header, cell.insert2, preamble.input, 
                   text.combined)
  }
  else {
    text.final = c(book_header, cell.insert2, text.combined)
  }
  write(text.final, sep = "\n", file = mergedFileName, append = F)
  rm(list = c("text.input", "text.final", "text.combined", 
              paste0("text.chunk", 1:length(files))))
  if (!missing(preamble)) {
    rm(list = c("preamble", "preamble.input"))
  }
  setwd(old)
}
environment(mergeRMD_tt) <- environment(mergeRMD_hofc)
