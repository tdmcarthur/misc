# The MIT License (MIT)
# 
# Copyright (c) 2016 Travis McArthur
# 
# Permission is hereby granted, free of charge, to any person obtaining a copy of 
# this software and associated documentation files (the "Software"), to deal in 
# the Software without restriction, including without limitation the rights to 
# use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies 
# of the Software, and to permit persons to whom the Software is furnished to do 
# so, subject to the following conditions:
# 
# The above copyright notice and this permission notice shall be included in all
# copies or substantial portions of the Software.
# 
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR 
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, 
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE 
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER 
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
# SOFTWARE.


library("effsize")

massive.summary <- function(input.data, instructions.data, factors.as.numeric = TRUE, progress.bar = TRUE) {
  options(warn = -1)
  # Surpress warnings
  # BEGIN checking that input data is correcty formatted
  
  if (!is.data.frame(input.data)) stop("input.data must be a dataframe")
  if (!is.data.frame(instructions.data)) stop("instructions.data must be a dataframe")
  
  proper.column.names <- c("column", "category", "consolidate", 
                           "exclude", "quantile", "use.function", "cohen.d")
  if (!exists("cohen.d")) {stop("Must load effsize package")}
  
  if (factors.as.numeric) {
    column.factors <- list()
    for ( i in colnames(input.data) ) {
      if ( length(levels(input.data[, i])) == 0) {
        column.factors[[i]] <- NA
      } else {
        column.factors[[i]] <- levels(input.data[, i])
      }
    }
  }
  
  
  for ( i in 1:ncol(input.data)) {
    if ( is.factor(input.data[, i]) ) {
      input.data[, i] <- as.character(input.data[, i])
    }
  }
  
  for ( i in colnames(instructions.data)) {
    if ( i != "category" ) {
      instructions.data[, i] <- as.character(instructions.data[, i])
      instructions.data[is.na(instructions.data[, i]), i] <- ""
    }
  }
  
  if (! all(names(instructions.data)==proper.column.names)) {
    stop(paste0("instructions.data column names must be: ", paste0(proper.column.names, collapse=", ")))
  }
  
  if (! all(instructions.data$column %in% colnames(input.data) ) ) {
    stop("A variable specified in the 'column' of instructions.data does not appear in input.data")
  }
  
  if (! all(instructions.data$category %in% c(TRUE, FALSE) ) & is.logical(instructions.data$category) ) {
    stop("Elements of the 'category' column of instructions.data must be either 'TRUE' or 'FALSE'")
  }
  
    if (! all(instructions.data$cohen.d %in% c(TRUE, FALSE) ) & is.logical(instructions.data$cohen.d) ) {
    stop("Elements of the 'cohen.d' column of instructions.data must be either 'TRUE' or 'FALSE'")
  }
  
  
  
  
  for ( i in 1:nrow(instructions.data)) {
    
    
    consol.temp <- instructions.data$consolidate[i]
    # Grab the contents of the consolidate column
    
    if ( consol.temp != "" & ! ( grepl("[:]", consol.temp) & grepl("[|]", consol.temp) ) ) {
      stop(paste0("Something is wrong with the consolidate info for the ", 
                  instructions.data$column[i], " variable"))
    } 
    
    intended.exclude.temp <- strsplit(instructions.data$exclude, ";", fixed=TRUE)[[1]]
    if (! (instructions.data$exclude[i] == "" || 
           all(intended.exclude.temp %in% input.data[, instructions.data$column[i] ] ) )  ) {
      stop(paste0("Intended excluded category ", instructions.data$exclude[i] , 
                  " does not appear in ", instructions.data$column[i], " variable"))
    } 
    
    
    quantile.temp <- as.numeric(strsplit(instructions.data$quantile[i], ";")[[1]])
    
    if ( length(quantile.temp) > 0 ) {
      if (! is.numeric(input.data[, instructions.data$column[i] ]) ) {
        stop("Intended quantile variable " , instructions.data$column[i], " is not numeric")
      }
    }
    
    if ( any(is.na(quantile.temp)) || ! (all(quantile.temp < 1) & all(quantile.temp > 0)) ) {
      stop("Something is wrong with the quantile specification for the ", 
           instructions.data$column[i], " variable")
    }
    
    funcs.to.apply.temp <- strsplit( instructions.data$use.function[i] , ";")[[1]]
    for ( j in funcs.to.apply.temp) {
      if ( ! exists( j ) ) {
        stop("Function ", j , " intended for use on ", 
             instructions.data$column[i], "does not exist in the workspace")
      }
    }
    
    
    
  }
  
  # END checking that input data is correcty formatted
  
  
  
  instructions.data <- as.data.frame( lapply(instructions.data, FUN = function(x) {
    x[is.na(x)] <- ""
    x
  }), stringsAsFactors = FALSE )
  
  # instructions.data <- instructions.data[1, , drop=FALSE]
  
  final.mats.ls <- list()
  
  # The below will only work for Stata files; if the input file is not a Stata file,
  # then it will just result in a blank column in the output
  final.mats.ls[[ length(final.mats.ls) + 1 ]] <- c("", attr(input.data, "var.labels") )
  if (factors.as.numeric) {
    input.data.temp <- input.data
    for ( i in colnames(input.data.temp)) {
          if ( all(is.na( column.factors[[i]] )) ) { next }
          input.data.temp[, i] <- factor(input.data.temp[, i], levels = column.factors[[i]])
          input.data.temp[, i] <- as.numeric(input.data.temp[, i])
    }
    final.mats.ls[[ length(final.mats.ls) + 1 ]] <- 
      t( aggregate( input.data.temp, by = list( rep("TOTAL", nrow(input.data.temp)) ), 
                  FUN = mean, na.rm = TRUE) )
    rm(input.data.temp)
  } else {
    final.mats.ls[[ length(final.mats.ls) + 1 ]] <- 
      t( aggregate( input.data, by = list( rep("TOTAL", nrow(input.data)) ), 
                  FUN = mean, na.rm = TRUE) )
  }
  
  
  final.mats.ls[[ length(final.mats.ls) + 1 ]] <- 
    t( aggregate( input.data, by = list( rep("N non-missing", nrow(input.data)) ), 
                  FUN = function(x) { sum(!is.na(x)) } ))  
  
  if (progress.bar) {
    cat("Progress: \n")
    pb = txtProgressBar(min = 0, max = nrow(instructions.data), initial = 0, style = 3)
  }
  
  for (targ.split in 1:nrow(instructions.data) ) {
    
    targ.instructions.data <- instructions.data[targ.split, , drop=FALSE]
    
    category.T.F <- targ.instructions.data$category
    # Simply capture what type of data we are dealing with - 
    # categorical or numeric
    
    targ.col <- targ.instructions.data$column
    
    working.df <- input.data
    # Create a copy of the original dataset, since we may subset it
    
    intended.exclude <- strsplit(targ.instructions.data$exclude, ";", fixed=TRUE)[[1]]
    # separating the categories that we want to exlude, if they exist
    # Using the fact that we are denoting separate categories by semicolon
    
    if (category.T.F) {
      
      if ( length(intended.exclude) > 0 ) {
        # If there is nothing in the "exclude" column in the
        # split.info file, then length of intended.exclude is zero
        working.df <- working.df[
          ! working.df[, targ.instructions.data$column] %in% intended.exclude,
          ]	
      }
      
      consol <- targ.instructions.data$consolidate
      # Grab the contents of the consolidate column
      
      if ( consol != "" ) {
        
        consol <- strsplit(consol, ";", fixed=TRUE)[[1]]
        
        for (targ.consol in consol) {
          
          targ.consol <- strsplit(targ.consol, ":", fixed=TRUE)[[1]]
          
          to.be.consolidated <- strsplit(targ.consol[2], "|", fixed=TRUE)[[1]]
          
          working.df[ working.df[, targ.col] %in% to.be.consolidated , targ.col] <- 
            targ.consol[1]
          
        }
        
      }
      
    } else {
      # So if it is not categorical, it will do the piece of code that is meant for
      # quantiles
      
      targ.quantiles  <- sort(c(0, as.numeric(strsplit(targ.instructions.data$quantile, ";")[[1]]), 1))
      
      quantile.temp <- quantile(working.df[, targ.col], 
               probs = targ.quantiles, 
               na.rm=TRUE)
      
      #quantile.temp.fixed <- quantile.temp
      
      targ.quantiles <- targ.quantiles[!duplicated(quantile.temp)]
      
      
      working.df$temp.col.for.quant <- cut(working.df[, targ.col], 
                                           breaks = quantile(working.df[, targ.col], 
                                                             probs = targ.quantiles, 
                                                             na.rm=TRUE), 
                                           include.lowest=TRUE)
      
      quantile.names <- c()
      for ( i in 2:length(targ.quantiles)) {
        quantile.names <- c(quantile.names,
                            paste0(round(targ.quantiles[i-1] * 100), "%-", 
                                   round(targ.quantiles[i] * 100), "%") )
      }
      
      
      
      levels(working.df$temp.col.for.quant) <- 
        paste0(quantile.names, ":", levels(working.df$temp.col.for.quant))
      
      working.df$temp.col.for.quant <- as.character(working.df$temp.col.for.quant)
      
      # Thanks to http://stackoverflow.com/questions/4126326/how-to-quickly-form-groups-quartiles-deciles-etc-by-ordering-columns-in-a
      
    }
    

    agg.column <- ifelse(category.T.F, targ.col, "temp.col.for.quant")
    
    funcs.to.apply <- strsplit( targ.instructions.data$use.function , ";")[[1]]
    
    for ( targ.func in funcs.to.apply) {
      
      trycatch.targ.func <- function(x, ...) {
        func.name <- get(targ.func)
        tryCatch(func.name(x, ...), error=function(e) NA )
      }
      # This above is a bit advanced
      
      if ( all(intended.exclude!="nr")  ) { 
        working.df[is.na( working.df[, agg.column] ), agg.column] <- "NR/NA"
      } 
      
      
      working.df.save <- working.df
      
      if (factors.as.numeric) {
        for ( i in colnames(working.df)[! colnames(working.df) %in% 
                                        c( "temp.col.for.quant", agg.column) ]) {
          if ( all(is.na( column.factors[[i]] )) ) { next }
          working.df[, i] <- factor(working.df[, i], levels = column.factors[[i]])
          working.df[, i] <- as.numeric(working.df[, i])
        }
      }
      
      ret.agg <- aggregate( working.df[, colnames(working.df) != "temp.col.for.quant"], 
                            by = list(working.df[, agg.column]), 
                            FUN = trycatch.targ.func, na.rm = TRUE)
      
      ret.agg[, 1] <- paste0(targ.func, ":", targ.col, ":", ret.agg[, 1])
      
      final.mats.ls[[ length(final.mats.ls) + 1 ]] <- t(ret.agg)
      
      if ( targ.func == "mean" ) {
        
        unique.cats <- unique(working.df[, agg.column])
        
        if ( length(unique.cats ) == 2 ) {
          
          ttest.logic.v <- working.df[, agg.column] == unique.cats[1]
          # Careful with missings....
          # [, "services1", drop=FALSE]
          test.vec <- sapply(working.df[, colnames(working.df) != "temp.col.for.quant"], 
                             FUN = function(x) {
            # cat("one", "\n")
            if (sum(!is.na(x)) < 2) { return(NA) }
            if (is.factor(x) | is.character(x)) {
              tryCatch(chisq.test(table(x, ttest.logic.v))$p.value,
                       error = function(e) NA)
            
            } else {
              tryCatch(t.test(x =  x[  ttest.logic.v], 
                              y =  x[! ttest.logic.v] )$p.value,
                       error = function(e) NA)
            }
          } )
          
          test.vec[sapply(test.vec, length)==0] <- NA
          test.vec <- unlist(test.vec)
          
        } else {
          
          #print(str(working.df))
          test.vec <- sapply(working.df[, colnames(working.df) != "temp.col.for.quant"], 
                             FUN = function(x) {
            # cat("one", "\n")
            if (sum(!is.na(x)) < 2) { return(NA) }
            if (is.factor(x) | is.character(x)) {
              tryCatch(chisq.test(table(x, ttest.logic.v))$p.value,
                       error = function(e) NA)
            
            } else {
            tryCatch(
              summary(
                aov(as.formula(paste0("x ~ working.df[, agg.column]")) ) 
              )[[1]]$`Pr(>F)`[1]
              ,
              error = function(e) NA)
            }
          } )
          test.vec[sapply(test.vec, length)==0] <- NA
          test.vec <- unlist(test.vec)          
          
        }
        
        test.vec <- test.vec[names(test.vec)!="temp.col.for.quant"]
        test.stars <- vector(mode="character", length=length(test.vec))
        
        test.stars[test.vec < .1 ] <- "*"
        test.stars[test.vec < .05 ] <- "**"
        test.stars[test.vec < .01 ] <- "***"
        
        final.mats.ls[[ length(final.mats.ls) + 1 ]] <- c("eq.means.test", test.vec)
        final.mats.ls[[ length(final.mats.ls) + 1 ]] <- c("eq.means.stars", test.stars)
        working.df <- working.df.save
      }
      
    }
    
    do.cohen.d <- targ.instructions.data$cohen.d
    
    if (do.cohen.d) {
      
      unique.cats <- unique(working.df[, agg.column])
      cats.grid <- t(combn(unique.cats, 2))
      #cohen.mat.ls <- list()
      for ( i in 1:nrow(cats.grid)) {
        cohen.d.logic.v.1 <- working.df[, agg.column] == cats.grid[i, 1]
        cohen.d.logic.v.2 <- working.df[, agg.column] == cats.grid[i, 2]
        cohen.mat <- sapply(working.df[, colnames(working.df) != "temp.col.for.quant"], 
          FUN = function(x) {
          if (sum(!is.na(x)) < 2) { 
            ret <- list(estimate=NA, conf.int=c(NA, NA))
           # cat("NAs\n")
          } else {
        
            ret <- tryCatch(cohen.d(na.omit(x[cohen.d.logic.v.1]), na.omit(x[cohen.d.logic.v.2])),
                       error = function(e) list(estimate=NA, conf.int=c(NA, NA))
            )
            #cat("Calced cohens \n")
          }
          #print(ret)
           ret$conf.int <- paste0("(", ret$conf.int[1], ", ", ret$conf.int[2], ")")
           c(ret$estimate, ret$conf.int)
        } )
        cohen.name <- paste0(cats.grid[i, 1], " | ", cats.grid[i, 2])
        cohen.mat <- t(cohen.mat)
        colnames.cohen.mat <- c(paste0("cohen.est: ", cohen.name), 
                                 paste0("cohen.conf.int: ", cohen.name))
        cohen.mat <- rbind(colnames.cohen.mat, cohen.mat)
        final.mats.ls[[ length(final.mats.ls) + 1 ]] <- cohen.mat
        #cohen.mat.ls[[i]] <- cohen.mat
      }
      #cohen.mat.binded <- do.call(cbind, cohen.mat.ls)
      #final.mats.ls[[ length(final.mats.ls) + 1 ]] do.call(cbind, cohen.mat.ls)
 
    }
    
    
    if (progress.bar) { setTxtProgressBar(pb, targ.split) }
  }
  
  
  
  final.ret <- do.call(cbind, final.mats.ls)
  options(warn = 0)
  
  final.ret
  
} 




# DOCUMENTATION FOR massive.summary(input.data, instructions.data, 
#  factors.as.numeric = TRUE, progress.bar = TRUE)

# input.data is the dataframe on which to perform the summary.

# instructions.data is a dataframe with the following column names in the proper order: 
# "column"       "category"     "consolidate"  "exclude"      "quantile"     "use.function"
# Each row deals with a different variable on which to split the data and perform the summary statistics.
# Typically, this dataframe would be created by reading in a csv file. 
# The information in each column must conform to these specifications:

# * column: Must be the exact name of a column in the dataframe. This is the "target column"
# category: Must be either "TRUE" or "FALSE". Indicates whether the target column in the 
# dataframe is a categorical variable or not.

# * consolidate: A set of categories in the target column to consolidate. separated by colons, 
# pipes, and semicolons like in the following: WEST:California|Oregon|Nevada;EAST:New York|Virginia

# * exclude: Must be exact name(s) of categories in the target column that will be excluded when
# splitting the sample. Must be separated by semicolons, e.g. excluded.var.1;excluded.var.2;excluded.var.3

# * quantile: For numeric target columns, specify the quantile cut points to split the sample by.
# Quantiles must be separated by semicolons, e.g. .25;.5;.75

# * use.function: The name(s) of the R function(s) that will be applied to the data, separated by
# semicolons if more than one. Must be exactly as written in R. mean, sd, and median are known to
# work. Other functions are not guaranteed to work and may simply produce 
# all NA's. na.rm=TRUE is automatically added as an argument to the 
# function. Input example: mean;sd;median

# * cohen.d: Must be either "TRUE" or "FALSE". Indicates whether to calculate Cohen's d.

# The function will raise a bunch of warnings because some of the
# test statistics can't be calculated with all of the variables, 
# e.g. when the sample variance is zero.
# So warnings are surpressed within the function

