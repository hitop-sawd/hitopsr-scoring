# ---- deploy script -----------------------------------------------------------
# Set this to "preview" while testing branch changes; "official" only when
# a reviewed version is being promoted on main.
build <- "preview"                       # "preview" or "official"

dest <- if (build == "preview") "docs/preview" else "docs/app"

# function to load libraries
pkgTest <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg))
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}
invisible(lapply(c("shinylive", "httpuv"), pkgTest))

unlink(dest, recursive = TRUE)
shinylive::export(appdir = "hitop-app", destdir = dest)

appdir <- normalizePath(dest)
print(appdir)
<<<<<<< Updated upstream
stopifnot(file.exists(file.path(appdir, "index.html")))
httpuv::runStaticServer(appdir, port = 8080)
=======
file.exists(file.path(appdir, "index.html"))   
httpuv::runStaticServer(normalizePath("docs/preview"), port = 8087)

>>>>>>> Stashed changes
