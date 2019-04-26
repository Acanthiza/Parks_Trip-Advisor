  
# Create a colour palette for n groups

  col_pal <-  function(n) {
    if (n <= 8) {
      RColorBrewer::brewer.pal(n, "Set2")
    } else {
      hcl(h=seq(0,(n-1)/(n),length=n)*360,c=100,l=65,fixup=TRUE)
    }
  }
  

  # turn a vector into a comma separated list of values with a penultimate 'and'
  vec_to_sentence <- function(x,sep=",") {
    
    x[!is.na(x)] %>%
      paste(collapse = "JOINSRUS") %>%
      (function(x) if(sep == ";") {
        
        stringi::stri_replace_last_regex(x,"JOINSRUS", paste0(sep," and ")) %>%
          str_replace_all("JOINSRUS",paste0(sep," "))
        
      } else {
        
        stringi::stri_replace_last_regex(x,"JOINSRUS", " and ") %>%
          str_replace_all("JOINSRUS",paste0(sep," "))
        
      }
      )
    
  }

# https://github.com/ateucher/useful_code/blob/master/R/numbers2words.r

  numbers2words <- function(x){
    ## Function by John Fox found here:
    ## http://tolstoy.newcastle.edu.au/R/help/05/04/2715.html
    ## Tweaks by AJH to add commas and "and"
    helper <- function(x){

      digits <- rev(strsplit(as.character(x), "")[[1]])
      nDigits <- length(digits)
      if (nDigits == 1) as.vector(ones[digits])
      else if (nDigits == 2)
        if (x <= 19) as.vector(teens[digits[1]])
      else trim(paste(tens[digits[2]],
                      Recall(as.numeric(digits[1]))))
      else if (nDigits == 3) trim(paste(ones[digits[3]], "hundred and",
                                        Recall(makeNumber(digits[2:1]))))
      else {
        nSuffix <- ((nDigits + 2) %/% 3) - 1
        if (nSuffix > length(suffixes)) stop(paste(x, "is too large!"))
        trim(paste(Recall(makeNumber(digits[
          nDigits:(3*nSuffix + 1)])),
          suffixes[nSuffix],"," ,
          Recall(makeNumber(digits[(3*nSuffix):1]))))
      }
    }
    trim <- function(text){
      #Tidy leading/trailing whitespace, space before comma
      text=gsub("^\ ", "", gsub("\ *$", "", gsub("\ ,",",",text)))
      #Clear any trailing " and"
      text=gsub(" and$","",text)
      #Clear any trailing comma
      gsub("\ *,$","",text)
    }
    makeNumber <- function(...) as.numeric(paste(..., collapse=""))
    #Disable scientific notation
    opts <- options(scipen=100)
    on.exit(options(opts))
    ones <- c("", "one", "two", "three", "four", "five", "six", "seven",
              "eight", "nine")
    names(ones) <- 0:9
    teens <- c("ten", "eleven", "twelve", "thirteen", "fourteen", "fifteen",
               "sixteen", " seventeen", "eighteen", "nineteen")
    names(teens) <- 0:9
    tens <- c("twenty", "thirty", "forty", "fifty", "sixty", "seventy", "eighty",
              "ninety")
    names(tens) <- 2:9
    x <- round(x)
    suffixes <- c("thousand", "million", "billion", "trillion")
    if (length(x) > 1) return(trim(sapply(x, helper)))
    helper(x)
  }

# Fix html widget when not displayed
  widgetFix <- function(inputFile,outputFile){
    a = readLines(inputFile)
    output = paste(a,collapse="\n")
    output = gsub(">\n\n</div>","></div>",output)
    writeLines(output,outputFile)
    invisible(NULL)
  }

# Generate jenks breaks
# http://cainarchaeology.weebly.com/r-function-for-plotting-jenks-natural-breaks-classification.html
  
  plotJenks <- function(data, n=3, brks.cex=0.70, top.margin=10, dist=5){
    df <- data.frame(sorted.values=sort(data, decreasing=TRUE))
    Jclassif <- classIntervals(df$sorted.values, n, style = "jenks") #requires the 'classInt' package
    test <- jenks.tests(Jclassif) #requires the 'classInt' package
    df$class <- cut(df$sorted.values, unique(Jclassif$brks), labels=FALSE, include.lowest=TRUE) #the function unique() is used to remove non-unique breaks, should the latter be produced. This is done because the cut() function cannot break the values into classes if non-unique breaks are provided
    if(length(Jclassif$brks)!=length(unique(Jclassif$brks))){
      info <- ("The method has produced non-unique breaks, which have been removed. Please, check '...$classif$brks'")
    } else {info <- ("The method did not produce non-unique breaks.")}
    loop.res <- numeric(nrow(df))
    i <- 1
    repeat{
      i <- i+1
      loop.class <- classIntervals(df$sorted.values, i, style = "jenks")
      loop.test <- jenks.tests(loop.class)
      loop.res[i] <- loop.test[[2]]
      if(loop.res[i]>0.9999){
        break
      }
    }
    max.GoF.brks <- which.max(loop.res)
    plot(x=df$sorted.values, y=c(1:nrow(df)), type="b", main=paste0("Jenks natural breaks optimization; number of classes: ", n), sub=paste0("Goodness of Fit: ", round(test[[2]],4), ". Max GoF (", round(max(loop.res),4), ") with classes:", max.GoF.brks), ylim =c(0, nrow(df)+top.margin), cex=0.75, cex.main=0.95, cex.sub=0.7, ylab="observation index", xlab="value (increasing order)")
    abline(v=Jclassif$brks, lty=3, col="red")
    text(x=Jclassif$brks, y= max(nrow(df)) + dist, labels=sort(round(Jclassif$brks, 2)), cex=brks.cex, srt=90)
    results <- list("info"=info, "classif" = Jclassif, "breaks.max.GoF"=max.GoF.brks, "class.data" = df)
    return(results)
  }