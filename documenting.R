require(roxygen2)
require(devtools)

setwd("~/Documents/R/prosjekter")
projname <- "rollfun"
create(projname)

# turns objects found in "projname"/data.R (project root)
# into data files available through data()
# by saving them as .rda files in "projname"/data
add_data <- function(projname) {
	if (!dir.exists(projname)) {
		stop(paste("Could not find", projname, "in current directory"))
	}
	datapath <- file.path(projname, "data.R")
	if (!file.exists(datapath)) {
		stop(paste("Could not find", datapath))
	}
    dir.create(projname, "data", showWarnings=FALSE)
    tmp.env <- new.env()
	source(datapath, local=tmp.env)
	tmp.list <- as.list(tmp.env, sorted=TRUE)
    files <- file.path(projname, "data", paste0(names(tmp.list), ".rda"))
    obj <- mapply(save, list=names(tmp.list), file=files, 
      MoreArgs=list(compress="xz", envir=tmp.env))
    if (length(files) == 1) {
        cat("File added:")
    } else {
    	cat("Files added:")
    }
    dtf <- data.frame(x=paste(files, ""), 
      y=paste(sprintf("%.1f", file.size(files)/1000), "kB"))
    names(dtf) <- c(" ", " ")
    dtf
}

load_all(projname)
add_data(projname)
use_build_ignore(c("data.R", "documenting.R", "commit.command"), pkg=projname)
document(projname)
unload(projname)

# run convenience script to add, commit and maybe push change
system(paste0("open ", projname, "/commit.command"))

# dev_example(projname)

install_github(paste0("AkselA/R-", projname))
library(projname, character.only=TRUE)
