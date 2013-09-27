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
    show(paste(nrow(ans), 'records:', paste(names(ans), collapse=' ')))
    return(ans)
}


summarize = function(dframe, exclude=c(), maxlen=12) {
    # more compact summary() output
    factors = data.frame()
    reals = data.frame()
    for (i in names(dframe)) {
        if (i %in% exclude) 
            next
        j = dframe[[i]]
        na = sum(is.na(j))
        j = j[!is.na(j)]
        if (is.factor(j)) { 
            l = levels(j)
            lvls = length(l)
            if (lvls == 0) {
                min=NA
                max=NA
            } else {
                min=substr(l[1], 1, maxlen)
                max=substr(tail(l,1), 1, maxlen)
            }
            ord = ""
            if (is.ordered(j)) {
                ord = "Y"
            }
            row = data.frame(name=i, min=factor(min), max=factor(max),
                             levels=lvls, na=na, ord=ord)
            factors = rbind(factors, row)
        } else {        
            row = data.frame(name=i, min=min(j), max=max(j), mean=mean(j), sd=sd(j), na=na)
            reals = rbind(reals, row)
        }
    }
    return(list(factors=factors, reals=reals))
}

viewh = function(x) {
    # view x in system web-browser
    argname = as.character(substitute(x))
    argname <- argname[argname != 'summarize']  # optimize for viewh(summarize(foo))
    fname = paste("/tmp/Rview.", sample(1:1000000, 1), ".xhtml", sep='')
    sink(fname)
    # use XHTML to allow use of CDATA so '<', '&' etc. display ok
    cat('<?xml version="1.0" encoding="UTF-8"?>\n',
        '<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Strict//EN"\n',
        '  "http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd">\n',
        '<html xmlns="http://www.w3.org/1999/xhtml" >\n',
        '<head><title>', argname, "</title></head><body>\n",
        "<h1 style='margin:0'>",argname, '</h1><div>', date(),
        "</div>\n<pre><![CDATA[")
    print(x)
    cat("]]></pre></body></html>")
    sink()
    system(paste('x-www-browser', fname, '&'))
}

