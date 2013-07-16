# make a||b shorthand for paste(a,b,sep='')
"||" <- function(...) UseMethod("||")
"||.default" <- .Primitive("||")
"||.character" <- function(...) paste(...,sep="")

zeromin = function(X) {  
    # replace zeros with minimum of non-zero elements
    X[X==0] = min(X[X!=0]);
    return(X);
}

hist0 = function(X, xname) {   
    # plot histograms so it's easy to see empty bins
    h = hist(X,  breaks=40, plot=F)
    par(cex=0.75)
    plot(h$mids, h$counts, type='h', lwd=2, lend='square',
         ylab='Frequency', xlab=xname, main=xname, col=red)
}

onetri = function(x, hide=upper.tri) {
    # just show lower triangular half of symetric table
    xx = x
    xx[hide(xx)] = NA
    print(xx, na.print='')
}

normalize = function(x) {
    # scale vector x into a 0-1 range
    return( (x-min(x)) / (max(x)-min(x)) ) 
}

require(digest);

name_digest = function(filename) {
    # verbose md5 digest of a file, to verify inputs, 
    # i.e. the fingerprint file
    info = file.info(filename)
    return(c(
        filename, 
        info$size, 
        as.character(info$mtime), 
        digest(file=filename)
    ))
}

read_hash = function(filename) {
    # read csv file, but show name_digest() and records / fields
    show(name_digest(filename))
    ans = read.csv(filename)
    show(paste(nrow(ans), ’records:’, paste(names(ans), collapse=’ ’)))
    return(ans)
}
