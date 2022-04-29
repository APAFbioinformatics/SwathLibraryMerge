###############################################################
### GenePattern job SwathLibraryMerge
### Version 20 - use SwathXtend_2.4
### JWu 
################################################################


library(SwathXtend, lib.loc="c:/RLibrary/SwathXtend_2.4")

libraryMergingGP<-function(...){
  
  versionNo  <- packageVersion("SwathXtend")
  
  args<-list(...)
  for(i in 1:length(args)){
    flag<- substring(args[[i]],0,2) ## -x
    value<-substring(args[[i]],3, nchar(args[[i]]))
                     
    if(flag == "-s") seedFile <- value
    if(flag == "-a") addonFile <- value  # a single addon lib file or a zip file
	if(flag == "-o") outputFile <- value
	if(flag == "-c") conf.cutoff <- as.numeric(value)
	if(flag == "-p") add.shared.pep <- value
  }

if(grepl(".txt$", addonFile)) {
	addonFiles = addonFile
} else if (grepl(".zip$", addonFile)) {
	addonFiles = unzip(addonFile)
	idx.seed = grepl(basename(seedFile), basename(addonFiles))
	addonFiles = addonFiles[!idx.seed]
	
} else {stop("addon files must be a sinlge .txt file or a .zip file")}

conflict.bypep = FALSE
 if(tolower(add.shared.pep) == "no") conflict.bypep = TRUE
 
 files <- c(seedFile, addonFiles)

 res <- try(mergeMultiLibs(files, mod.rm=FALSE, clean=TRUE, nomc=FALSE, nomod=FALSE, conflict.bypep=conflict.bypep, 
		conf.cutoff=conf.cutoff, include.z=TRUE) )
 
 if(inherits(res,"try-error")) stop("Error with function mergeMultiLibs!")
 
 outputFile <- paste(tools::file_path_sans_ext(outputFile), paste("SwathXtend", versionNo, ".txt", sep=""), sep="_")
 
 outputLib(res, outputFile)
 
 }
 



