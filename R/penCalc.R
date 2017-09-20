
## Function to convert annualised returns to monthy return.

annual2mthly <- function(x,y) {100*(((1+x/100)^(1/y))-1)}

## Function to calcuate percentage change.

percentChange <- function(series){ 
  serieslag <- lag(series ,-1)
  100*(series-serieslag)/serieslag 
}

## ## Creating monthly returns for equity, nifty and corp bonds. 

## multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
##                                         # Make a list from the ... arguments and plotlist
##     plots <- c(list(...), plotlist)
    
##   numPlots = length(plots)

##   # If layout is NULL, then use 'cols' to determine layout
##   if (is.null(layout)) {
##     # Make the panel
##     # ncol: Number of columns of plots
##     # nrow: Number of rows needed, calculated from # of cols
##     layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
##                     ncol = cols, nrow = ceiling(numPlots/cols))
##   }

##  if (numPlots==1) {
##     print(plots[[1]])

##   } else {
##     # Set up the page
##     grid.newpage()
##     pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))

##     # Make each plot, in the correct location
##     for (i in 1:numPlots) {
##       # Get the i,j matrix positions of the regions that contain this subplot
##       matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))

##       print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
##                                       layout.pos.col = matchidx$col))
##      }
##   }
## }





# WageMatrix
wagematrix <- function(age=list(age.entry=25,age.exit=60),
                       wage=list(25000,0.05,0.2,initial.amount=0)){
                                        # Age
    if( age[[1]] >=18 &&
        age[[2]] > age[[1]]){
        years <- age[[1]]:age[[2]]
        nyears <- age[[2]]-age[[1]] +1
    } else { stop("Please enter a list of two elements :age of entry and age of exit")}
                                        # Wage
    if(length(wage[[1]])==1){
        w <- matrix(NA, nyears, 1)
        rownames(w) <- years
        w[1,1] <- wage[[1]]
    } else if ( length(wage[[1]])==nyears ) {
        w <- matrix(NA, nyears,1)
        rownames(w) <- years
        w[,1] <- wage[[1]]
    } else { stop("Please enter a single value of the starting wage or a vector of wages for each year working year")
    }
    w <- data.frame(w)
    # Wage Growth Rate
    if(length(wage[[2]]) ==1 && wage[[2]] <= 1){
        w$growth.rate <- wage[[2]]
        w$growth.rate[1] <- 0
    } else if ( length(wage[[2]])==nyears-1 ) {
        w$growth.rate <- c(0,wage[[2]])
    } else { stop("Please enter a single constant growth rate or a vector for each working year")
    }
                                        # Contribution Rate
    if( length(wage[[3]])==1 ){
        w$contribution.rate <- wage[[3]]
    } else if ( length(wage[[3]]) == nyears ){
        w$contribution.rate <- wage[[3]]
    } else { stop("Please enter a single constant contribution rate or a vector of contributions for each working year")
    }
    if(length(wage[[1]])>1 && length(wage[[2]])>1){
        stop("A wage vector and growth rate vector cannot be used at the same time in the wage argument.")
        }
    return(w)
}

# Investment Profile
invprofile <- function(age.entry, age.exit,w1,w2,w3){
    agemat <- matrix(NA, (age.exit-age.entry)+1, 3)
    rownames(agemat) <- age.entry:age.exit
    first <- matrix(c(10, 25, 65), 1,3)
    agemat[1:11,1] <- first[1,1]
    agemat[1:11,2] <- first[1,2]
    agemat[1:11,3] <- first[1,3]
    for(i in 12:nrow(agemat)){
        agemat[i,1] <- agemat[i-1,1] + w1
        agemat[i,2] <- agemat[i-1,2] - w2
        agemat[i,3] <- agemat[i-1,3] - w3
    }
    agemat <- as.data.frame(agemat)   
    agemat[rownames(agemat)>64,] <- data.frame(100,0,0)
    colnames(agemat) <- c("goi_bonds", "corp_bonds", "equity")
    agemat <- agemat/100
    return(agemat)
}


# Investment Matrix 
inv_weights <- function(inv.weights=list("lc"),w){
    if(inv.weights[[1]]=="lc" && length(inv.weights[[1]])==1){
        weights <- invprofile(as.numeric(rownames(w)[1]),
                              as.numeric(rownames(w)[length(rownames(w))]),
                              2.8,0.6,2.2)
        w <- cbind(w, weights)
    } else if(ncol(inv.weights[[1]])==3 &&
              nrow(inv.weights[[1]])==nrow(w) &&
              unique(rowSums(inv.weights[[1]]))==1){
        colnames(inv.weights[[1]]) <- c("goi_bonds", "corp_bonds", "equity")
        w <- cbind(w, inv.weights[[1]])
    } else { stop("Please enter \"lc\" or a matrix of investment weights with each column providing weights for government bonds, corporate bonds and equity respectively for each working year")
    }
    return(w)
}


# Inflation
inflation_func<- function(inflation,w){
    if(is.vector(inflation[[1]]) && length(inflation[[1]])==2){
        w$inf <- rnorm(nrow(w),
                       mean=inflation[[1]][1],
                       sd=inflation[[1]][2])
        w$inf[1] <- 0
    } else { stop("The first element of the list should be a vector providing the annual mean and standard deviation of inflation in the stated order")
    }    
    if( inflation[[2]] ==TRUE){
        w$realgrowth.rate <- w$growth.rate - w$inf
    } else if ( inflation[[2]]==FALSE) {
        w$realgrowth.rate <- w$growth.rate
    } else { stop("Please enter a logical argument:  TRUE/FALSE")
    }
    return(w)
}

# Function to generate complete wage matrix

wage_comp <- function(age,
                      wage,
                      inv.weights,
                      w){
    w$comp.wage <- NA
    w$comp.wage[1] <- w[1,1]
    nyears <- age[[2]]-age[[1]] +1
    if(length(wage[[1]])==1){
        for(i in 2:nrow(w)){
            w[i,"comp.wage"] <- w[i-1,"comp.wage"] + w[i,"realgrowth.rate"]*w[i-1,"comp.wage"]
        }
    } else if(length(wage[[1]])==nyears){
        w$comp.wage <- w$w
    }
    w$contr <- w$comp.wage * w$contribution.rate
    w$contr[1] <- w$contr[1]+ wage[[4]]
    w <- inv_weights(inv.weights,w)
    w$count <- 12
    w <- untable(df=w[,1:10], num=w[,11])
    return(w)
}

# Returns Generator

returns_gen <- function(returns, w){
    if(class(returns[[1]])=="data.frame" && nrow(returns[[1]])==3 && ncol(returns[[1]])==2){
                                                # Monthly tbill returns
            r.rf <- rnorm(nrow(w),
                          mean=annual2mthly(returns[[1]][1,1],12),
                          sd=annual2mthly(returns[[1]][1,2],12))
                                        # Monthly corporate bond returns
            r.cbond <- rnorm(nrow(w),
                             mean=annual2mthly(returns[[1]][2,1],12),
                             sd=annual2mthly(returns[[1]][2,2], 12))
                                        # Monthly equity returns
            r.equity <- rnorm(nrow(w),
                              mean=annual2mthly(returns[[1]][3,1],12),
                              sd=annual2mthly(returns[[1]][3,2],12))
        } else{ stop("Please enter a data frame with two columns: mean and standard deviation and three rows: Government bonds, Corporate bonds and equity respectively") }
        r.portfolio <- cbind(r.rf,
                         r.cbond,
                         r.equity)
        w <- cbind(w, r.portfolio)
        w$monthly.fees.expenses <- annual2mthly(returns[[2]][1],12)
        w$returns <- rowSums( cbind(w$goi_bonds * r.portfolio[,"r.rf"],
                                w$corp_bonds * r.portfolio[,"r.cbond"],
                                w$equity * r.portfolio[,"r.equity"])) - w$monthly.fees.expenses
    return(w)
}

# Function to generate real returns and final portfolio value

returns_inf <- function(inflation, returns, w){
    if( inflation[[2]] == TRUE){
        w$monthlyinf <- rnorm(nrow(w),
                              mean=annual2mthly(inflation[[1]][1],12),
                              sd=annual2mthly(inflation[[1]][2],12))
        w$real.returns <- w$returns-w$monthlyinf
    }
    if( inflation[[2]] == FALSE){
        w$monthlyinf <- 0
        w$real.returns <- w$returns
    }
    w$annual.fees <- c(rep(0,11), returns[[2]][2])
    portfolio <- 0
    w$contr[1] <- w$contr[1] 
    for(i in 1:nrow(w)){
        portfolio <- w$contr[i] +
            ((1+w$real.returns[i])*portfolio) - w$annual.fees[i]
                w$portfolio[i] <- portfolio
    }
    return(list(portfolio,w))
}

# Returns wrapper function
returns_func <- function(returns, inflation,w){
    w <- returns_gen(returns, w)
    w <- returns_inf(inflation, returns,w)
    return(w)
}

# Annuity function
annuity_func<- function( annuity=list(proportion.annuitised=0.40,
                                      price=4087),
                        terminal){
    for.annuity.terminal <- terminal * annuity[[1]]
    in.hand.terminal <- terminal - for.annuity.terminal
    if(is.numeric(annuity[[1]]) &&
       is.numeric(annuity[[2]])){
        x <- annuity[[2]]
        pension <- (365/12)*(for.annuity.terminal/x)
    }    else{
        stop("Please enter the percent to be annuitised and the prices of the annuity as two elements of a list")
    }
    annuity <- cbind(pension,in.hand.terminal,for.annuity.terminal)
    colnames(annuity) <- c("pension","in.hand.terminal","for.annuity.terminal")
    return(annuity)
}

#' Function to simulate pension outcomes
#' @usage pencalc(age=list(age.entry=25
#'                        ,age.exit=60),
#'                wage=list(wage=25000
#'                         ,growth.rate=0.08
#'                         ,contribution.rate=0.2
#'                         ,initial.amount=0),
#'                inflation=list(mean.sd.inflation=c(0.04,0)
#'                              ,real=FALSE),
#'                inv.weights=list("lc"),
#'                returns=list(returns=data.frame(mean=c(0.07, 0.10, 0.16),
#'                                                sd=c(0, 0, 0.25)),
#'                             fees=c(monthly.fees.expenses=0.01,100)),
#'                annuity=list(proportion.annuitised=0.40,
#'                             price=4087))
#' @title pencalc
#' @import zoo xtable reshape grid
#' @param age List of two elements : \enumerate{ \item age.entry: Age at which a person enters the pension system. \item age.exit: Age at which a person exits the pension system. }
#' 
#' @param wage List of four elements: \enumerate{ \item wage: There
#'     are two choices: \enumerate{ \item Single Wage: A numeric entry
#'     of the starting wage \item Wage Vector: A vector of wages for
#'     each year a person is in the pension system.}  \item
#'     growth.rate: Annual wage growth. If a vector of wages is
#'     entered, then the values set in growth.rate should be 0. Else,
#'     there are two choices: \enumerate{ \item Constant Growth Rate:
#'     A single numeric entry of the wage growth rate \item Growth
#'     Rate Vector: A vector of numeric entries of wage growth
#'     rate. The length should be equal to 1 less than the total
#'     number of years a person is in the pension system. }
#'
#' \item contribution.rate: Percentage of the wage to be invested in a
#'     portfolio. There are two available choices \enumerate{ \item
#'     Single contribution rate: A single numeric entry which will
#'     remain constant throughout. \item Contribution rate vector: A
#'     vector of numeric entries for each year the person is in the
#'     pension system.} \item initial.amount: Numeric entry signifying
#'     an amount accumulated before the simulation begins. }
#' @param inflation List of two elements: \enumerate{ \item
#'     mean.sd.inflation: A vector with the annual mean and standard
#'     deviation of the expected inflation. \item real: Logical symbol
#'     (TRUE) to get real prices.}
#' @param inv.weights List of one element. There are two choices:
#'     \enumerate{ \item Character "lc" to use the default life cycle
#'     weights \item A matrix of investment weights with 3 column
#'     providing weights for government bonds, corporate bonds and
#'     equity respectively}
#' @param returns List of two elements: \enumerate{ \item returns: A
#'     data frame of mean and standard deviation of government bonds,
#'     corporate bonds and equity. \item fees: \enumerate{ \item
#'     Monthly fees and expenses: The asset under management fees (in
#'     %) to be deducted from portfolio returns every month. \item
#'     Annual flat fee: Flat amount to be deducted at the end of each
#'     year.}}
#' @param annuity List of two elements: \enumerate{ \item
#'     proportion.annuitised: The proportion of accumulations to be
#'     annuitised. \item price: The price of an annuity that pay Rs.1
#'     for life. }
#' @return Mean and standard deviation of pension, terminal price and
#'     replacement rate
#' @examples pencalc(age=list(age.entry=25
#'                           ,age.exit=60),
#'                    wage=list(wage=25000
#'                             ,growth.rate=0.08
#'                          ,contribution.rate=0.2
#'                            ,initial.amount=0),
#'                    inflation=list(mean.sd.inflation=c(0.04,0)
#'                                  ,real=FALSE),
#'                    inv.weights=list("lc"),
#'                    returns=list(returns=data.frame(mean=c(0.07, 0.10, 0.16),
#'                                            sd=c(0, 0, 0.25)),
#'                                 fees=c(monthly.fees.expenses=0.01,100)),
#'                   annuity=list(proportion.annuitised=0.40,
#'                                price=4087))
#' @author Renuka Sane, Arjun Gupta
#' @export pencalc
pencalc <- function(age=list(age.entry=25
                            ,age.exit=60),
                    wage=list(wage=25000
                             ,growth.rate=0.08
                             ,contribution.rate=0.2
                             ,initial.amount=0),
                    inflation=list(mean.sd.inflation=c(0.04,0)
                                  ,real=FALSE),
                    inv.weights=list("lc"),
                    returns=list(returns=data.frame(mean=c(0.07, 0.10, 0.16),
                                            sd=c(0, 0, 0.25)),
                                 fees=c(monthly.fees.expenses=0.01,100)),
                    annuity=list(proportion.annuitised=0.40,
                                 price=4087)){
    w <- wagematrix(age, wage)
    w <- inflation_func(inflation,w)
    w <- wage_comp(age,
                   wage,
                   inv.weights,
                   w)
    terminal <- replicate(1000, returns_func(returns, inflation,w))
    pension <- annuity_func(annuity, terminal=do.call("rbind",terminal[seq(1, length(terminal), by=2)]))
    w <- data.frame(terminal[length(terminal)])
    lastwage <- tail(w$comp.wage,1)
    replacement <- 100*(pension[,"pension"]/lastwage)
    fin.data <- data.frame(pension=pension[,"pension"],
                           replacement=replacement,
                           in.hand.terminal=pension[,"in.hand.terminal"])
    fin.values <- data.frame(mean=apply(fin.data,2,mean), sd=apply(fin.data, 2, sd))
    rownames(fin.values) <- colnames(fin.data)
    fin <- list(fin.values, w, fin.data)
    class(fin) <- "penc"
    return(fin)
}


#' @export print.penc
print.penc <- function(x,...){
    cat("Mean and Standard Deviation","\n")
    print(x[[1]])
}

