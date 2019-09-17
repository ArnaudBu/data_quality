# /usr/bin/Rscript

###############################################################################
#         _____        _           ____              _ _ _                    #
#        |  __ \      | |         / __ \            | (_) |                   #
#        | |  | | __ _| |_ __ _  | |  | |_   _  __ _| |_| |_ _   _            #
#        | |  | |/ _` | __/ _` | | |  | | | | |/ _` | | | __| | | |           #
#        | |__| | (_| | || (_| | | |__| | |_| | (_| | | | |_| |_| |           #
#        |_____/ \__,_|\__\__,_|  \___\_\\__,_|\__,_|_|_|\__|\__, |           #
#                                                             __/ |           #
#                                                             |__/            #
###############################################################################

library(stringr)
library(yaml)
library(data.table)
library(rpart)
library(R6)

'%_%' <- function(a, b) paste0(a, b)

###############################################################################

# Check if number
is.num <- function(x){
  if(is.numeric(x)) return(rep(TRUE, length(x)))
  
  return(stringr::str_detect(x, "^[0-9\\.,]+$"))
}

###############################################################################

# Convert to number
to.num <- function(x){
  if(is.numeric(x)) return(x)
  
  return(suppressWarnings(as.numeric(gsub(",", ".", as.character(x)))))
}

###############################################################################

# Check if boolean
is.bool <- function(x){
  if(is.logical(x)) return(rep(TRUE, length(x)))
  
  t <- tolower(x) %in% c("t", "f", "true", "false", "0", "1")
  t[is.na(x)] <- NA
  
  return(t)
}

###############################################################################

# Convert to boolean
to.bool <- function(x){
  if(is.logical(x)) return(x)
  
  return(tolower(x) %in% c("t", "true", "1"))
}

###############################################################################

# Check if date
is.date <- function(x, formats = c("%d-%m-%Y","%d/%m/%Y", "%Y-%m-%d", "%Y/%m/%d")){
  if(all(class(x) %in% c("Date", "POSIXct", "POSIXt", "POSIXlt"))){
    return(rep(TRUE, length(x)))
  }
  
  fn.check <- function(){
    t <- lapply(formats, 
                function(f) !is.na(as.Date(as.character(x), format = f))
    )
    t <- Reduce('|', t)
    t[is.na(x)] <- NA
    return(t)
  }
  
  t <- tryCatch({
    fn.check()
  }, error = function(e) return(rep(FALSE, length(x))))
  
  return(t)
}

###############################################################################

# Convert to date
to.date <- function(x, formats = c("%d-%m-%Y","%d/%m/%Y")){
  if(all(class(x) %in% c("Date", "POSIXct", "POSIXt", "POSIXlt"))){
    return(as.Date(x))
  } 
  
  t <- as.Date(rep(NA, length(x)))
  for(f in formats){
    t[is.na(t)] <- as.Date(as.character(x[is.na(t)]), f)
  }
  
  return(t)
}

###############################################################################

# Check if category
is.cat <- function(x, nb.cat = 10){
  if(is.factor(x)) return(rep(TRUE, length(x)))
  if(length(unique(x)) < nb.cat & length(unique(x)) < length(x)){
    return(rep(TRUE, length(x)))
  } else{
    return(rep(FALSE, length(x))) 
  }
}

###############################################################################

# Convert to category
to.cat <- function(x){
  return(factor(x))
}

###############################################################################

# Convert to character
to.char <- function(x){
  return(as.character(x))
}

###############################################################################

# Summarize a vector
sum.up <- function(x, formats = c("%d-%m-%Y","%d/%m/%Y"), nb.cat = 10){
  # Proportion of NA
  prop.na <- sum(is.na(x)) / length(x)
  
  # Check if number
  test.num <- sum(is.num(x), na.rm = T) / length(x[!is.na(x)])
  
  # Check if date
  test.date <- sum(is.date(x, formats), na.rm = T) / length(x[!is.na(x)])
  
  # Check if bool
  test.bool <- sum(is.bool(x), na.rm = T) / length(x[!is.na(x)])
  
  # Check if factor
  test.cat <- sum(is.cat(x, nb.cat), na.rm = T) / length(x[!is.na(x)])
  
  # Current type
  current.type <- class(x)
  
  # Vector type
  types <- c("num" = test.num,
             "date" = test.date,
             "cat" = test.cat,
             "bool" = test.bool,
             "char" = 1)
  
  # Suggested type
  suggested.type = names(which.max(types))
  
  # Return list
  r <- list(prop.na = prop.na,
            types = types,
            current.type = current.type,
            suggested.type = suggested.type)
  
  # Summary Numerical
  if(test.num >= 1){
    q = quantile(to.num(x), probs = c(0, 0.25, 0.5, 0.75, 1), na.rm = T)
    m = mean(to.num(x), na.rm = T)
    summary.num = c(q, m)
    names(summary.num) <- c("Min", "Q1", "Median", "Q3", "Max", "Mean")
    r$summary.num = summary.num
  }
  # Summary date
  if(test.date >= 1){
    r$summary.date = summary(to.date(x, formats))
  }
  # Summary factor
  if(test.cat >= 1){
    summary.cat = table(x)
    n = names(summary.cat)
    summary.cat = as.numeric(summary.cat)
    names(summary.cat) <- n
    r$summary.cat = summary.cat
  }
  
  # Return results
  return(r)
}

###############################################################################

# Class Data Quality

DataQuality <- R6Class("DataQuality",
                       
######################## Definition of fields
                       
                       private = list(
                         .data.ini = data.table(),
                         .config = list(),
                         .data = data.table()
                       ),
                       
######################## Getters
                       
                       active = list(
                         data.ini = function(value) {
                           if (missing(value)) {
                             private$.data.ini
                           } else {
                             stop("`$data.ini` is read only", call. = FALSE)
                           }
                         },
                         data = function(value) {
                           if (missing(value)) {
                             private$.data
                           } else {
                             stop("`$data` is read only", call. = FALSE)
                           }
                         },
                         config = function(value) {
                           if (missing(value)) {
                             private$.config
                           } else {
                             stop("`$config` is read only", call. = FALSE)
                           }
                         }
                       ),
                       
######################## Public methods
                       
                       public = list(
                         
########################## Initializer
                         
                         initialize = function(d) {
                           private$.data.ini = data.table(d)
                           private$.config = list()
                           private$.data = data.table()
                         },
                         
########################## Generate config file

                        generate.config = function(columns = "all",
                                                   dates = c("%d-%m-%Y","%d/%m/%Y", "%Y-%m-%d", "%Y/%m/%d"), 
                                                   nb.cat = 10,
                                                   verbose = TRUE,
                                                   output.file = NA) {
                          if(columns[1] == "all") columns = colnames(private$.data.ini)
                          
                          if(!is.na(output.file)){
                            f <- file(output.file, open = "at")
                          }
                          
                          private$.config <- list(params = list(dates.format = dates,
                                                                verbose = verbose),
                                                  columns = list())
                          
                          for(c in columns){
                            temp <- sum.up(private$.data.ini[, get(c)],
                                           dates,
                                           nb.cat)
                            
                            val <- list(type = temp$suggested.type,
                                        column = c,
                                        new_name = c,
                                        keep = temp$prop.na < 0.5,
                                        fill.na = ifelse(temp$suggested.type == "char", 
                                                         "", 
                                                         "tree")
                                        )
                            
                            private$.config$columns[[length(private$.config$columns) + 1]] = val
                            
                            if(verbose){
                              cat("- Column: " %_% c, "\n")
                              cat("          Percent of missings: " %_% round(temp$prop.na * 100), "%\n")
                              cat("          Current type: " %_% temp$current.type, "\n")
                              cat("          Suggested type: " %_% temp$suggested.type, "\n")
                              if("summary.num" %in% names(temp) & temp$suggested.type == "num"){
                                cat("          Summary:\n")
                                print(temp$summary.num)
                              }
                              if("summary.cat" %in% names(temp) & temp$suggested.type == "cat"){
                                cat("          Summary:\n")
                                print(temp$summary.cat)
                              }
                            }
                            if(!is.na(output.file)){
                              cat("\n\n##############################################################################\n\n",
                                  "- Column: " %_% c, "\n", file = f)
                              cat("          Percent of missings: " %_% round(temp$prop.na * 100), "%\n", file = f)
                              cat("          Current type: " %_% temp$current.type, "\n", file = f)
                              cat("          Suggested type: " %_% temp$suggested.type, "\n", file = f)
                              if("summary.num" %in% names(temp) & temp$suggested.type == "num"){
                                cat("          Summary:\n", file = f)
                                cat(paste(capture.output(temp$summary.num), collapse = "\n"), "\n", file = f)
                              }
                              if("summary.cat" %in% names(temp) & temp$suggested.type == "cat"){
                                cat("          Summary:\n", file = f)
                                cat(paste(capture.output(temp$summary.cat), collapse = "\n"), "\n", file = f)
                              }                              
                            }
                          }
                          if(!is.na(output.file)){
                            close(f)
                          }
                        },

########################## Save config file

                        save.config = function(name = tempfile(tmpdir = "~", fileext = ".yaml")){
                          yaml::write_yaml(private$.config, name)
                          private$.config$file <- name
                        },

########################## Load config file

                        load.config = function(name){
                          private$.config <- yaml::read_yaml(name)
                          private$.config$file <- name
                        },

########################## Clean function

                        clean = function(){
                          # Check that configuration file exists
                          if(length(private$.config) == 0){
                            stop("No configuration loaded. Please run generate.config " %_%
                                   "or load.config before.")
                          }
                          
                          # Check if a file is linked to it
                          if("file" %in% names(private$.config)){
                            tryCatch(self$load.config(private$.config$file),
                                     error = function(e) cat("Unable to open file",
                                                             "Using old configuration.",
                                                             "\n")
                            )
                          }
                          
                          # Check that all columns are in the data.table
                          cols <- sapply(private$.config$columns, function(x) x$column)
                          if(!(all(cols %in% colnames(private$.data.ini)))){
                            warning("/!\ Names not present in table columns" %_%
                                      paste(cols[!(cols %in% colnames(private$.data.ini))],
                                            collapse = "\n"))
                          }
                          
                          # Create data.table to work on
                          db <- copy(private$.data.ini)
                          cf <- private$.config
                          vb <- tryCatch(cf$params$verbose, error = function(e) TRUE)
                          
                          # Remove columns
                          if("col2keep" %in% names(cf$params)){
                            col2keep <- cf$params$col2keep
                            if(!(all(col2keep %in% colnames(db)))){
                              stop("/!\ Names in col2keep not present in table columns" %_%
                                     paste(col2keep[!(col2keep %in% colnames(db))],
                                           collapse = "\n"))
                            }
                          } else if("col2rm" %in% cf$params){
                            col2keep <- colnames(db)[!(colnames(db) %in% cf$params$col2rm)]
                          } else {
                            col2keep <- colnames(db)
                          }
                          
                          # Variables for trees
                          var_tree_glob <- col2keep
                          kpg <- sapply(cf$columns, function(x) x$type %in% c("num", "cat", "bool") & x$keep)
                          var_tree_glob <- var_tree_glob[var_tree_glob %in% cols[kpg]]
                          
                          # Loop on columns
                          for(i in 1:length(cf$columns)){
                            col <- cf$columns[[i]]

                            # Check if column needs to be kept
                            kp <- tryCatch(col$keep, error = function(e) TRUE)
                            if(!(col$column %in% col2keep & kp)){
                              db[, col$column := NULL]
                              next
                            }
                            
                            # Change type if needed
                            if("type" %in% names(col)){
                              switch(col$type,
                                     num ={
                                       db[,(col$column):=lapply(.SD, to.num),.SDcols = col$column] 
                                     },
                                     date = {
                                       to.date.cust <- function(x) to.date(x, cf$params$dates.format)
                                       db[,(col$column):=lapply(.SD, to.date.cust),.SDcols = col$column] 
                                     },
                                     cat = {
                                       db[,(col$column):=lapply(.SD, to.cat),.SDcols = col$column] 
                                     },
                                     bool = {
                                       db[,(col$column):=lapply(.SD, to.bool),.SDcols = col$column] 
                                     },
                                     char = {
                                       db[,(col$column):=lapply(.SD, as.character),.SDcols = col$column] 
                                     },
                                     {warning("Invalid type for column " %_%
                                                col$column %_%
                                                " with type " %_%
                                                col$type)})
                            }
                            
                          }
                          
                          # Loop on columns
                          for(i in 1:length(cf$columns)){
                            col <- cf$columns[[i]]
                            
                            # Check if column needs to be kept
                            kp <- tryCatch(col$keep, error = function(e) TRUE)
                            if(!(col$column %in% col2keep & kp)){
                              next
                            }
                            
                            # Interpolation
                            
                            if("fill.na" %in% names(col)){
                              if(col$fill.na %in% c("mean", "median")){
                                if(col$type == "num"){
                                  int <- function(x) interp(x, mode = col$fill.na)
                                  db[,(col$column):=lapply(.SD, int),.SDcols = col$column] 
                                } else {
                                  warning("Column " %_% col$column %_% ": " %_%
                                            "Invalid type " %_% col$type %_% 
                                            " for interpolation on category" %_% col$type)
                                }
                              } else if(col$fill.na == "tree"){
                                if("var_tree" %in% names(col)){
                                  if(col$var_tree[1] == "all"){
                                    var_tree <- var_tree_glob[!(var_tree_glob == col$column)]
                                  } else {
                                    var_tree <- col$var_tree
                                  }
                                } else{
                                  var_tree <- var_tree_glob[!(var_tree_glob == col$column)]
                                }
                                
                                int <- function(x) interp(x, 
                                                          mode = "tree", 
                                                          var = as.list(db[, var_tree, with = FALSE]))
                                db[,(col$column):=lapply(.SD, int),.SDcols = col$column] 
                                
                              } else {
                                int <- function(x) interp(x, mode = "value", value = col$fill.na)
                                db[, (col$column) := lapply(.SD, int), .SDcols = col$column]
                              }
                            }
                            
                            # Change column name
                            if("new_name" %in% names(col)){
                              setnames(db, col$column, col$new_name)
                              var_tree_glob[var_tree_glob == col$column] <- col$new_name
                            }
                            
                            if(vb){
                              cat(col$column, " treated.\n")
                            }
                            
                          }
                          
                          private$.data <- db
                          
                        }
                         
                       )
)


###############################################################################

# Data Quality initialization
data.quality <- function(d){
  DataQuality$new(d)
}

###############################################################################

# Interpolation function with tree
inter.tree <- function(x, var){
  if(sum(is.na(x)) == 0) return(x)
  
  d <- data.table(y = x)
  for(i in 1:length(var)){
    u <- var[[i]]
    d$u <- u
    colnames(d)[colnames(d) == "u"] <- "var" %_% i
  }
  
  
  isdate <- class(x) == "Date"
  dates <- colnames(d)[sapply(d, function(x) class(x) == "Date")]
  if(length(dates) > 0){
    d[,(dates):=lapply(.SD, as.numeric),.SDcols=dates]
  }
  
  na.pos <- which(is.na(d$y))
  dtrain <- na.omit(d[-na.pos])
  
  fit <- rpart::rpart(y~., 
                      method= ifelse(is.numeric(d$y), "anova", "class"), 
                      data=dtrain,
                      control = rpart::rpart.control(cp = 0.000000000000001))
  
  xfill <- predict(fit, d[na.pos], type = ifelse(is.numeric(d$y), "vector", "class"))
  
  if(isdate){
    x[na.pos] <- as.Date(xfill, origin = "1970-01-01")
  }
  else{
    x[na.pos] <- xfill
  }
  
  return(x)
}

###############################################################################

# Interpolation function
interp <- function(x, mode = "value", value = NA, var = list()){
  if(sum(is.na(x)) == 0) return(x)
  if(!(mode %in% c("mean", "median", "value", "tree"))){
    stop("Invalid mode for interpolation")
  }
  if(!(is.numeric(x)) & mode %in% c("mean", "median")){
    stop("Invalid type for numeric operation")
  }
  if(is.character(x) & mode != "value"){
    stop("Invalid operation for character")
  }
  
  if(mode == "tree"){
    return(inter.tree(x, var))
  } else if(mode == "value"){
    x[is.na(x)] <- value
    return(x)
  } else if(mode == "mean"){
    x[is.na(x)] <- mean(x, na.rm = T)
    return(x)
  } else if(mode == "median"){
    x[is.na(x)] <- median(x, na.rm = T)
    return(x)
  }
}
