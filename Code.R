# detach all libraries

detachAllPackages <- function() {
  basic.packages <- c("package:stats", "package:graphics", "package:grDevices", "package:utils", "package:datasets", "package:methods", "package:base")
  package.list <- search()[ifelse(unlist(gregexpr("package:", search()))==1, TRUE, FALSE)]
  package.list <- setdiff(package.list, basic.packages)
  if (length(package.list)>0)  for (package in package.list) detach(package,  character.only=TRUE)
}
detachAllPackages()

# function to load libraries

pkgTest <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[,  "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg,  dependencies = TRUE)
  sapply(pkg,  require,  character.only = TRUE)
}

# loading packages 

lapply(c("shinylive"),pkgTest)
lapply(c("shinycustomloader"),pkgTest)
lapply(c("httpuv"),pkgTest)
library(shinylive)
library(httpuv)
library(shinycustomloader)

unlink("docs/app", recursive = TRUE)
shinylive::export(appdir = "hitop-app", destdir = "docs/app")

appdir <- normalizePath("docs/app")    
print(appdir)
file.exists(file.path(appdir, "index.html"))   
httpuv::runStaticServer(appdir, port = 8080)

