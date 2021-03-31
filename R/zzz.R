.onLoad <- function(libname, pkgname) {
    cacheChange <- "1.15.1"
    pkgVersion <- as.character(packageVersion(pkgname))

    if ( utils::compareVersion(cacheChange, pkgVersion) < 0){

        require(rappdirs)

        olddefault <- rappdirs::user_cache_dir(appname="BiocFileCache")

        newlocation <- tools::R_user_dir("BiocFileCache", which="cache")

        if (dir.exists(olddefault) && (length(list.files(olddefault)) != 0)){
            msg <- sprintf(
                "As of %s (> %s), The default caching location has changed. To avoid redownloading previously cached files and use previouly existing default cache see BiocFileCache vignette section: 'Default Caching Location Update' . This message will not be displayed after Bioconductor 3.14",
                pkgname,
                cacheChange)


            packageStartupMessage(paste(strwrap(msg, exdent=2), collapse="\n"))

        }

    }
}
