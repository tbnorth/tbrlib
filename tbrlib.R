get_login = function(URL, username, password, curl=NULL,
                     uname='username', pname='password', getpage=FALSE) {
    # login to a server which does its own login, not just HttpAuth
    # works with Django and HTTPS connections
    # e.g. curl = get_login("https://server.com/accounts/login/", "myname", "xxx")
    # returns a CURLHandle to use in subsequent RCurl calls
    # `curl` - pass in a previous return value from get_login() to
    #          log in to multiple sites
    # `uname` - change if the username field in the form is not named 'username'
    # `pname` - change if the password field in the form is not named 'password'
    # `getpage` - TRUE if you need to load the login page to get cookies etc.
    #             before filling in the form, use getpage=TRUE for Django
    require(RCurl)
    require(XML)

    # get a curl handle with some settings set
    # `autoreferer` insufficient, see explicit setting below
    if (is.null(curl)) {
        # cookiejar='' enables cookies
        curl = curlSetOpt(cookiejar='', followlocation=TRUE)
    }
    
    if (getpage) {
        # load the page
        html = getURLContent(URL, curl=curl)
        dom = htmlTreeParse(html, useInternalNodes=TRUE)
        # and check for a Django CSRF token
        token = getNodeSet(dom, "//*[@name='csrfmiddlewaretoken']/@value")[[1]]
    } else {
        token = NULL
    }
    
    params = list(username, password)
    names(params) = c(uname, pname)
    if (!is.null(token)) {
        params['csrfmiddlewaretoken'] = as.character(token)
    }
    
    # html will be whatever page you get after logging in, irrelevant
    html = postForm(URL, curl=curl, style="POST", 
                    .params=params, .opts=list(referer=URL)) 
    return(curl)
}

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

normalize = function(x, minx=NA, maxx=NA) {
    # scale vector x into a 0-1 range
    if (is.na(minx)) {
        minx = min(x)
    }
    if (is.na(maxx)) {
        maxx = max(x)
    }
    return( (x-minx) / (maxx-minx) ) 
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
                             levels=lvls, vals=nrow(dframe)-na, na=na, 
                             unique=length(unique(j))==length(j), ord=ord)
            factors = rbind(factors, row)
        } else {        
            row = data.frame(name=i, min=min(j), max=max(j), mean=mean(j), sd=sd(j), na=na)
            reals = rbind(reals, row)
        }
    }
    return(list(info=paste(nrow(dframe), 'rows', length(dframe), 'columns'),
                'factors/characters'=factors, reals=reals))
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

join <- function(to, from, to_col, from_col, copy_cols) {
    # copy columns named in vector `copy_cols` into `to` from
    # `from` where `to$to_col` == `from$from_col`
    # like merge(), but without eating all available RAM
    matches = to[, to_col] %in% from[, from_col]
    for (col in copy_cols) {
        to[matches, col] <- 
            from[match(to[matches, to_col], from[, from_col]), col]
    }
    return(to)
}

identify_all <- function(data, x, y = NULL, closest = 10) {
    # show lines from data.frame near clicked point on plot
    if (is.null(y)) {
        y <- x[,2]
        x <- x[,1]
    }

    rn <- identify(x, y, n=1)
    data$id.d.x <- x - x[rn]
    data$id.d.y <- y - y[rn]
    nx <- (x-min(x)) / (max(x)-min(x))
    ny <- (y-min(y)) / (max(y)-min(y))
    distance <- sqrt((nx-nx[rn])**2 + (ny-ny[rn])**2)
    data <- data[order(distance, y, x),]
    show(data[1:closest,])
}

