############################################################
# 
# author: Ludwig Geistlinger
# date: 2021-01-05 18:08:41
# 
# descr: misc utils
# 
############################################################

.cacheResource <- function(rname, fpath, ucdir = "bugsigdbr", ...)
{
    cache.dir <- tools::R_user_dir(ucdir, which = "cache")
    bfc <- BiocFileCache::BiocFileCache(cache.dir)
    
    # check if fpath is being tracked 
    qgsc <-  BiocFileCache::bfcquery(bfc, fpath)
    if(BiocFileCache::bfccount(qgsc) == 0) 
    {
        rpath <- BiocFileCache::bfcadd(bfc, rname, fpath, ...) 
    }
    else rpath <- qgsc$rpath
    return(rpath)
}

.getResourceFromCache <- function(rname, ucdir = "bugsigdbr", ...)
{
    cache.dir <- tools::R_user_dir(ucdir, which = "cache")
    bfc <- BiocFileCache::BiocFileCache(cache.dir)
    
    qgsc <-  BiocFileCache::bfcquery(bfc, rname, exact = TRUE)

    # is there already a cached version?
    res <- NULL
    if(BiocFileCache::bfccount(qgsc))
    {
        rid <- qgsc$rid
        if(qgsc$rtype != "web") BiocFileCache::bfcremove(bfc, rid)
        else
        {    
            # is the cached version outdated?
            # FIXME: somehow github data files come with an expire tag
            # although web_etag and file_etag agree 
            BiocFileCache:::.sql_set_expires(bfc, rid, NA)
            nu <- BiocFileCache::bfcneedsupdate(bfc, rid)
            if(!isFALSE(nu)) BiocFileCache::bfcdownload(bfc, rid, ask = FALSE, ...)
            message("Using cached version from ", qgsc$create_time)
            res <- BiocFileCache::bfcrpath(bfc, rname)
        }
    }
    return(res)   
}
