ftab_fxn <- function(input_df, ds, rnd=0, show_pct=T, overall=F, grp_fcts){
    require(dplyr)
    require(stringr)
    ## input_df should be a data frame with columns: var, title, levels, labels, exclude. 
    ## exclude, title, and labels are optional 
    ## lower case everything for matching
    names(input_df) <- tolower(names(input_df))
    cvar <- input_df$var[1]
    ## if var is given as factor without levels then levels should be filled in
    if(class(ds[[cvar]]) == "factor" & !"levels" %in% names(input_df) & nrow(input_df) == 1) {
        input_df <- input_df[rep(1, length(levels(ds[[cvar]]))), ]
        input_df$levels <- levels(ds[[cvar]])
    }
    ## table can't be made without var and levels
    if(!all(c("var", "levels") %in% names(input_df))){
        stop("columns var and levels (if not factor) must be in data frame")
    }
    ## overall should be chosen to sum across grouping factors
    if(overall & missing(grp_fcts)){
        warning("Cannot provide overall if grp_fcts not given")
    }
    ## set title to a NA if not given
    if(!"title" %in% names(input_df)) input_df$title = as.character(NA)
    ## set labels to levels if not given
    if(!"labels" %in% names(input_df)) {
      input_df$labels = input_df$levels
    } else {
      if(any(is.na(input_df$labels))){
        ## label can't be missing. Could also allow relevel --- tbd
        warning(sprintf("%s has a missing label - using levels instead", cvar))
        input_df$labels <- input_df$levels
      }
    }
    ## multi works for comma separated multi-select data cells
    if(!"multi" %in% names(input_df)) input_df$multi = F
    ## exclude allows for certain levels to be excluded from the percentages (e.g. not recorded)
    ex_vars <- if("exclude" %in% names(input_df)){
        input_df$exclude[is.na(input_df$exclude)] <- F
        if(any(input_df$exclude)) input_df$labels[input_df$exclude]
        else NA
    } else NA
    
    
    if(!cvar %in% names(ds)){
        ## warn and return empty dataset so that a loop will still complete
        warning(sprintf("%s is not in given data set, left out of result.", cvar))
        return(data_frame())
    } else {
        if(!missing(grp_fcts)){
            ## if the grouping factor is not in the dataset then stop with error
            if(!all(grp_fcts %in% names(ds))){
                stop("grp_fcts variables are not in dataset")
            }
            ## grouped_df is used moving forward 
            grouped_df <- do.call("group_by_", c(list(ds), as.list(grp_fcts)))
        } else grouped_df <- ds %>% ungroup()
        
        ## ftab_fxn_helper defined within function so that input_df, cvar, etc are appropriately locally defined
        ftab_fxn_helper <- function(ds) {
            tmp <- if(input_df$multi[1]){
                ## if it's multi select check if any are the target level
                mvar <- as.character(ds[[cvar]])
                tmp <- as.data.frame(do.call("rbind", setNames(lapply(input_df$levels, function(y){
                    tots <- sum(unlist(lapply(strsplit(mvar, ","), function(x) y %in% str_trim(x))))
                    tots
                }), input_df$labels)), stringsAsFactors = F)
                tmp$f_var <- rownames(tmp)
                rownames(tmp) <- NULL
                total <- sum(!ds[[cvar]] %in% input_df$levels[input_df$exclude])
                tmp %>% select(f_var, Freq=V1) %>% mutate(total=total)
            } else {
                ## some fixes needed for class types
                if(class(ds[[cvar]]) %in% c("integer", "logical")) ds[[cvar]] <- as.numeric(ds[[cvar]])
                if(class(input_df$levels) %in% c("integer", "logical")) input_df$levels <- as.numeric(input_df$levels)
                
                if(class(ds[[cvar]]) != class(input_df$levels)){
                    ## this warning can be useful but is often superfluous
                    warning(sprintf("Given levels for %s are not the same class as %s in the dataset
                                dataset %s: %s, given %s: %s", cvar, cvar, cvar, class(ds[[cvar]]), cvar, class(input_df$levels)))
                }
                
                f_var <- factor(ds[[cvar]], input_df$levels, input_df$labels)
                data.frame(table(f_var), stringsAsFactors = F) %>% mutate(total= sum(Freq[!f_var %in% ex_vars]))
            }
            
            tab <- tmp %>% mutate(f_order = as.numeric(f_var),
                                  f_var = as.character(f_var), 
                                  pct = ifelse(f_var %in% ex_vars, NA, Freq/total), 
                                  var = cvar, 
                                  cpct = as.character(ifelse(is.na(pct), Freq, sprintf("%s (%s%s)", Freq, round(100*pct, rnd), ifelse(show_pct, "%", "")))))
            tab$rgrp <- input_df$title[1]
            tab
        }
        ## apply function to all levels of 
        tab <- grouped_df %>% do(ftab_fxn_helper(.))
        if(overall) {
            ## to get an overall column need to group by everything except the last grouping factor
            tmp <- do.call("group_by_", c(list(ds), as.list(grp_fcts[-1*length(grp_fcts)]))) %>% do(ftab_fxn_helper(.))
            tmp[[grp_fcts[length(grp_fcts)]]] <- "Overall"
            tab <- bind_rows(tab, tmp)
        }
        tab
    }
}