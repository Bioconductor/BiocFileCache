.onLoad <- function(libname, pkgname) {
    
    cacheChange <- "1.15.1"
    pkgVersion <- as.character(packageVersion(pkgname))
    
    if ( utils::compareVersion(cacheChange, pkgVersion) < 0){
        
        require(rappdirs)

        olddefault <- rappdirs::user_cache_dir(appname="BiocFileCache")
        
        newlocation <- tools::R_user_dir("BiocFileCache", which="cache")
        
        if (dir.exists(olddefault) && (length(list.files(olddefault)) != 0)){
            
            msg <- sprintf(
                "As of %s (> %s), The default caching location has changed.\n  To avoid redownloading previously cached files and use previouly existing default cache\n  see BiocFileCache vignette section:\n  'Default Caching Location Update'",
                pkgname,
                cacheChange)
            
            
            packageStartupMessage(paste(strwrap(msg, exdent=2), collapse="\n"))
            
        }
        
    }
}
