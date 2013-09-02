#=============================================================
# Net Stacked Distribution Graph
#-------------------------------------------------------------
# [Reference]
#    http://www.organizationview.com/net-stacked-distribution-a-better-way-to-visualize-likert-data
# [Description]
#   With a Likert scale we think that the viewer is most interested in the balance 
# between positive and negatives and how strong those feelings are.
#   Net stacked distribution is a better way to visualize Likert data.  It calculates
# the frequency/proportion of each response, and draw a bar-plot centered at the
# neutral response.
#=============================================================
#-------------------------------
# Functions
#-------------------------------
# Convert a collection of Likert scale response into Net-Stacked-Distribution
likert.nsd.convert <- function(x, n=6, returnList=F){
  # Check the number of items
  if(!is.list(x)){
    if(is.null(dim(x))){
      x <- as.data.frame(x)
    }
  }
  # Convert to frequency table
  listall <- lapply(names(x), function(y){
    # Omit mising values
    column <- (na.omit(x[[y]]))
    # Bin creation: force to have all scale labels
    xbin <- cut(column,0:n)
    # Making table
    xtable <- prop.table(table(xbin))
    # Combine results
    names(xtable) <- (1:n)
    # Create output
    if(returnList){
      out <- data.frame(Question = y, xtable)
      names(out) <- c("Question", "Response", "Freq")
    } else {
      out <- xtable
    }
    out    
  })
  
  output <- do.call(rbind, listall)
  if(!returnList){
    rownames(output) <- names(x)
  }
  return(output)
}

# Plot a series 
likert.nsd.plot <- function(x, nscale=6, ptitle=NULL, greyscale=F){
  ## Convert to frequency table
  ptable <- data.frame(likert.nsd.convert(x,n=nscale,returnList=T))
  ## Get all responses
  all_levels <- levels(ptable$Response)
 
  ## Identify middle and "negative" levels
  if(nscale %% 2 == 1)
    neutral <- all_levels[ceiling(nscale/2)]
  else
    neutral <- NULL
 
  negatives <- all_levels[1:floor(nscale/2)]
  positives <- setdiff(all_levels, c(negatives, neutral))
 
  ## remove neutral, summarize as proportion
  if(!is.null(neutral)){
    ptable <- ptable[ptable$Response != neutral,]
  }
 
  ## split by positive/negative
  pos <- ptable[ptable$Response %in% positives,]
  neg <- ptable[ptable$Response %in% negatives,]
 
  ## Negate the frequencies of negative responses, reverse order
  neg$Freq <- -neg$Freq
  neg$Response <- ordered(neg$Response, levels = rev(levels(neg$Response)))

  # Define color bar
  #colours <- rainbow_hcl(nscale, start = 210, end = -30)
  # Create plot layers
  require(ggplot2)
  require(colorspace)
  stackedchart <- ggplot() +
    aes(Question, Freq, fill = Response, order = Response) + 
    geom_bar(data = neg, stat = "identity") +
    geom_bar(data = pos, stat = "identity") + 
    geom_hline(yintercept=0) +
    scale_y_continuous(name = "",
                       labels = paste0(seq(-100, 100, 20), "%"),
                       limits = c(-1, 1),
                       breaks = seq(-1, 1, .2)) +
    scale_fill_discrete(limits = c(negatives, positives)) +
    coord_flip()
  if(greyscale){
    stackedchart <- stackedchart + scale_fill_grey(start = 0.8, end = 0.2)
  } else {
    stackedchart <- stackedchart + scale_fill_hue(h = c(210, -30) - (240/nscale),c = 50, l = 70)
  }
  # check title
  if(!is.null(ptitle)) stackedchart <- stackedchart + labs(title = ptitle)
  # Plot
  stackedchart
}


#-------------------------------
# Executing script
#-------------------------------
# Basic data extraction

# Cleanup
rm()