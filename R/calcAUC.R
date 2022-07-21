#' @title calcAUC
#'
#' @description Calculate an incremental trapezoidal area under the curve for biomarkers measured at a series of time points, with the first timepoint being a baseline measurement. This is a summary measure that captures the total magnitude of a response over a specified time interval; it is an estimate of total concentration.
#'
#' @param data A data frame of observed measurements, with columns being a series of time points for a biomarker (in the format `biomarker_timepoint`, where time point is a number), and rows being subjects. May or may not contain a column of subject IDs.
#' @param biomarker The name of the biomarker to calculate AUC on. This should match the column labels in the data frame (see `data`), and is not case sensitive. The function will automatically locate all columns labeled with this biomarker and use those time points in increasing numerical order, with the first time point being used as the baseline measurement. If `NULL`, it will use all columns in the dataset, excluding a sibject column (if present).
#' @param method Indicates which method of calculation to use when finding the area under the curve. All sum the area of all trapezoids formed using consecutive time points and their response value, with the first measurement used as baseline (except the `total` method). If a time point is missing in a series, it is skipped and a trapezoid is calculated between the nearest available time points, which has an interval width equal to the distance between those points. `positive` is the default method and only sums area that is above the baseline measurement, ignoring any area below the baseline (Wolever & Jenkins, 1986). `net` is the net area (or increase in area) and subtracts the area below baseline from the area above baseline (Le Floch et al., 1990). `total` is the total area under the curve. This calculates the area with respect to ground (a baseline of 0 instead of the first time point).
#' @param subjects Indicates whether there is a column for subject ID present in the data. If `TRUE`, the function will automatically locate a column named `subject` (not case sensitive) and use those IDs. If `FALSE`, subjects will be automatically labeled with integers 1, 2, ..., (and any column named `subject` will be ignored). If given an integer, it will use that column number as a column of subject IDs.
#' @param interval Indicates the unit for the width of the intervals. All intervals are assumed to be the same width. The default width is `hour`, 1 hour. Other options are `minute` (1 minute intervals), or `halfhour` (1 half hour). To achieve interval widths that are different than 1 unit, time points should be labeled as such. For example, to achieve 5 minute intervals, the interval argument is set to `minute` and the time points in the data columns names called `biomarker_0`, `biomarker_5`, `biomarker_10`, ... .
#' @param plot Indicated whether to produce plots along with AUC calculation (`TRUE`) or only perform the calculation (`FALSE`). Turning off plots saves on computing time.
#' @param sort Indicates if plot grid containing all subject plots should be sorted by calculated AUC value. If `FALSE`, subject plots will be sorted by subject ID (default); if `increaasing`, plots will be sorted by lowest AUC first and highest AUC last; if `decreasing`, plots will be sorted by highest AUC first and lowest AUC last.
#'
#' @return Nested lists of input data containing subject-wise calculations (summed AUC, AUC for each interval, plot of the AUC curve), a data frame of summed AUCs, and a plot matrix of AUC curves.
#'
#' @examples
#' measurements <- data.frame(
#' Biomarker_0 = rnorm(10,50,20),
#' Biomarker_1 = rnorm(10,70,20),
#' Biomarker_2 = rnorm(10,90,20),
#' Biomarker_3 = rnorm(10,90,20),
#' Biomarker_4 = rnorm(10,70,20),
#' Biomarker_5 = rnorm(10,60,20))
#'
#' output <- calcAUC(data = measurements)
#'
#' measurements <- data.frame(
#' Biomarker_2 = rnorm(10,50,20),
#' Biomarker_1 = rnorm(10,70,20),
#' Biomarker_6 = rnorm(10,90,20),
#' Biomarker_3 = rnorm(10,90,20),
#' Biomarker_5 = rnorm(10,70,20),
#' Subject = 1:10,
#' Biomarker_4 = rnorm(10,60,20))
#'
#' output <- calcAUC(data = measurements, method = "total", subjects = TRUE, interval = "minute", plot = FALSE)
#'
#' @references Brouns et al., 2005
#' @references Wolever & Jenkins, 1986
#' @references Le Floch et al., 1990
#' @references Weeding, 2016
#' @references Grantham, 2022
#'
#' @export

calcAUC <- function(data,
                    biomarker = NULL,
                    method = "positive",
                    subjects = FALSE,
                    interval = "hour",
                    plot = TRUE,
                    sort = FALSE) {

  ################### DATA & FORMATTING ###################

  # read data directly from an .xlsx or .csv file

  # if (xlsx==TRUE) {data <- read_xlsx(paste(data))
  # if (csv==TRUE) {data <- read.csv(paste(data))}

  # convert first row of data to variable names if characters

  # if (is.character(data[,1])) {colnames(data) <- data[,1]; data <- data[,-1]}

  # convert data to a data frame

  data <- as.data.frame(data)

  # check if there is a variable for subject labels, create or read ID labels

  if (subjects==FALSE) {subject <- 1:nrow(data) # assign numbers to each subject
  } else if (subjects==TRUE) {subject <- data %>% select(contains("subject", ignore.case = TRUE)) %>% unlist() %>% as.vector() # search for a variable called subject(s)
  } else if (subjects>0 & length(subjects)==1) {subject <- data %>% select(subject) # select a specified column number containing subject IDs
  } else {errorCondition("The argument `subjects` should be TRUE, FALSE, or a column number.")}

  # find variables with biomarker measurements, subset data and create biomarker label

  if (is.null(biomarker)) {data <- data %>% select(!contains("subject", ignore.case = TRUE)) # select all columns except subject column
  } else if (sum(grepl(biomarker, colnames(data), ignore.case = TRUE))>0) {data <- data %>% select(contains(biomarker, ignore.case = TRUE)) # select all columns containing biomarker name
  } else {errorCondition("Biomarker not found. The argument `biomarker` should be a biomarker name or NULL.")}

  biomarker <- unlist(strsplit(colnames(data)[1], "_"))[1] # read name from column names for biomarker label

  # sort variables by increasing order of time point, create time point labels

  time <- unlist(strsplit(colnames(data),"_"))[seq(from = 2, to = length(colnames(data))*2, by = 2)] # labels
  time <- sort(as.numeric(time)) # ascending order time points

  if (length(unique(time))!=length(time)) {errorCondition("Time points labeled with same number value detected, time points should have a unique number value.")
  } else {data <- data %>% select(paste(biomarker, time, sep = "_"))} # order variables based on ascending time point order

  # index time points

  t <- 0:(length(time)-1) # index of time points
  n <- length(t)-1 # number of intervals

  # check there is at least 2 time point variables

  if (length(time)==1) {errorCondition(paste("Only 1 time point detected, need 2 or more for AUC calculation."))}
  if (length(time)==0) {errorCondition(paste("No time points detected, need 2 or more for AUC calculation."))}

  # impute missing values

  data[sapply(data, `%in%`, "NA")] <- NA # replace text "NA" with NA value
  # data[sapply(data, `%in%`, "LOD")] <- NA # replace text "LOD" with lowest level of detection
  # data[sapply(data, `%in%`, "MAX")] <- NA # replace text "MAX" with maximum level of detection

  # remove subjects less than 2 observations

  keep <- length(time) - rowSums(sapply(data, `%in%`, NA)) > 1 # subjects kept
  remove <- nrow(data) - sum(keep) # number removed
  data <- data[keep,] # data with subjects removed
  message(paste("Removed",remove,"subject(s) with less than two available measurements."))

  # re-index subject labels

  if (subjects==FALSE) {subject <- 1:nrow(data)} else {subject <- subject[which(keep)]}

  # label for interval width

  if (interval=="hour") {interval <- "hours"}
  if (interval=="minute") {interval <- "minutes"}
  if (interval=="halfhour") {interval <- "half-hours"}

  ################### CALCULATION & PLOTTING ###################

  # initialize objects for storing results

  output <- list() #list of subject-wise calculations and plots

  outputdf <- data.frame(Subject = NA, AUC = NA) #data frame of subjects and AUC calculation
  names(outputdf)[2] <- paste(biomarker,"AUC",sep="_") # label for biomarker in data frame

  plots <- list() # list of plots for all subjects, to use in to multiplot

  # loop through each subject for calculation

  for (j in 1:length(subject)) {

    G <- as.numeric(data[j,]) # vector of responses for each time point ("positive" notation)
    y <- as.numeric(data[j,]) # vector of responses for each time point ("net" notation)

    intervalAUC <- vector() # initialize storage vector for area under curve of each interval

    ################### calculate positive AUC (Brouns et al., 2005) ###################

    if (method=="positive") {

    # loop through intervals for a subject

    for (i in 1:n) {

      #x=1
      if (i==1) {
        #G1>G0
        if (G[2]>G[1])
        {intervalAUC[i] <- (G[2]-G[1]*(t[2]-t[1])/2)}
        #otherwise
        else {intervalAUC[i] <- 0}
      }

      #x>1
      else {
        #Gx>=G0 & Gx-1>=G0
        if (G[i+1]>=G[1] && G[i]>=G[1])
        {intervalAUC[i] <- (((G[i+1]-G[1])/2)+(G[i]-G[1]))*(t[i+1]-t[i])/2}
        #Gx>=G0 & Gx-1<G0
        else if (G[i+1]>=G[1] && G[i]<G[1])
        {intervalAUC[i] <- ((G[i+1]-G[1])^2/(G[i+1]-G[i]))*(t[i+1]-t[i])/2}
        #Gx<G0 & Gx-1>=G0
        else if (G[i+1]<G[1] && G[i]>=G[1])
        {intervalAUC[i] <- ((G[i]-G[1])^2/(G[i]-G[i+1]))*(t[i+1]-t[i])/2}
        #Gx<G0 & Gx-1<G0
        else if (G[i+1]<G[1] && G[i]<G[1])
        {intervalAUC[i] <- 0}
      }

    }

    # sum area under curve for all intervals

    AUC <- sum(intervalAUC)

    # plot AUC curve

    if(plot==TRUE) {
      curve <- tibble(x = c(t,t),
                      y = c(G,rep(G[1],length(G))),
                      f = c(rep("a",length(t)),rep("b",length(t))))

      ribbons <- ribbonize(curve,x,y,f)

      curve <- tibble(x = t,
                      y = G,
                      yend = G[1])

      plotj <- ggplot(curve) +
        geom_ribbon(data = ribbons, aes(x, ymin = ymin, ymax = ymax, fill = fill), alpha = 0.35) +
        geom_line(aes(x, y)) +
        geom_segment(aes(x = x, xend = x, y = y, yend = yend)) +
        geom_abline(slope = 0, intercept = G[1]) +
        geom_point(aes(x, y)) +
        theme_light() +
        guides(fill = "none") +
        labs(x = paste("Time (", interval,")", sep = ""),
             y = "Response",
             title = paste(biomarker, "\n", "Subject ", subject[j], "\n", "AUC = ", round(AUC,1), sep = "")) +
        scale_fill_manual(values = c(ifelse(FALSE %in% ribbons$fill, "white", "darkgray"),
                                     ifelse(FALSE %in% ribbons$fill, "darkgray", "white"))) +
        theme(plot.title = element_text(hjust = 0.5),
              panel.grid.minor = element_blank())
    }

    # store calculations and plot for each subject

    if(plot==TRUE) {output[[j]] <- list(AUC = AUC,
                                        intervalAUC = intervalAUC,
                                        plot = plotj)
    } else {output[[j]] <- list(AUC = AUC,
                                intervalAUC = intervalAUC)}

    # store calculation for each subject and ID in data frame

    outputdf[j,] <- c(subject[j],AUC)

    # add subject plot to list for multiplot

    if(plot==TRUE) {plots[j] <- list(plotj)}

    }

    ################### calculate total AUC (Weeding, 2016) ###################

    if (method=="total") {

      # loop through time points & responses for a subject

      for (i in 1:n) {

        intervalAUC[i] <- ((y[i]+y[i+1])*(t[i+1]-t[i]))/2

      }

      # sum area under curve for all intervals

      AUC <- sum(intervalAUC)

      # plot AUC curve

      if (plot==TRUE) {
        curve <- tibble(x = c(t,t),
                        y = c(y,rep(0,length(y))),
                        f = c(rep("a",length(t)),rep("b",length(t))))

        ribbons <- ribbonize(curve,x,y,f)

        curve <- tibble(x = t,
                        y = y,
                        yend = 0)

        plotj <- ggplot(curve) +
          geom_ribbon(data = ribbons, aes(x, ymin = ymin, ymax = ymax, fill = fill), alpha = 0.35) +
          geom_line(aes(x, y)) +
          geom_segment(aes(x = x, xend = x, y = y, yend = yend)) +
          geom_abline(slope = 0, intercept = 0) +
          geom_point(aes(x, y)) +
          theme_light() +
          guides(fill = "none") +
          labs(x = paste("Time (", interval,")", sep = ""),
               y = "Response",
               title = paste(biomarker, "\n", "Subject ", subject[j], "\n", "AUC = ", round(AUC,1), sep = "")) +
          scale_fill_manual(values = c(ifelse(FALSE %in% ribbons$fill, "red", "darkgray"),
                                       ifelse(FALSE %in% ribbons$fill, "darkgray", "red"))) +
          theme(plot.title = element_text(hjust = 0.5),
                panel.grid.minor = element_blank())
      }

      # store calculations and plot for each subject

      if(plot==TRUE) {output[[j]] <- list(AUC = AUC,
                                          intervalAUC = intervalAUC,
                                          plot = plotj)
      } else {output[[j]] <- list(AUC = AUC,
                                  intervalAUC = intervalAUC)}

      # store calculation for each subject and ID in data frame

      outputdf[j,] <- c(subject[j],AUC)

      # add subject plot to list for multiplot

      if(plot==TRUE) {plots[j] <- list(plotj)}

    }

    ################### calculate net AUC (Weeding, 2016) ###################

    if (method=="net") {

      # loop through time points & responses for a subject

      for (i in 1:n) {

        intervalAUC[i] <- ((y[i]+y[i+1])*(t[i+1]-t[i]))/2

      }

      # sum area under curve for all intervals

      AUC <- sum(intervalAUC) # AUC

      # subtract off AUC that would result if responses had remained constant over the entire time interval

      AUC <- AUC-(y[1]*(t[n+1]-t[1]))

      # plot AUC curve

      if (plot==TRUE) {
        curve <- tibble(x = c(t,t),
                        y = c(y,rep(y[1],length(y))),
                        f = c(rep("a",length(t)),rep("b",length(t))))

        ribbons <- ribbonize(curve,x,y,f)

        curve <- tibble(x = t,
                        y = y,
                        yend = y[1])

        plotj <- ggplot(curve) +
          geom_ribbon(data = ribbons, aes(x, ymin = ymin, ymax = ymax, fill = fill), alpha = 0.35) +
          geom_line(aes(x, y)) +
          geom_segment(aes(x = x, xend = x, y = y, yend = yend)) +
          geom_abline(slope = 0, intercept = y[1]) +
          geom_point(aes(x, y)) +
          theme_light() +
          guides(fill = "none") +
          labs(x = paste("Time (", interval,")", sep = ""),
               y = "Response",
               title = paste(biomarker, "\n", "Subject ", subject[j], "\n", "AUC = ", round(AUC,1), sep = "")) +
          scale_fill_manual(values = c(ifelse(FALSE %in% ribbons$fill, "red", "darkgray"),
                                       ifelse(FALSE %in% ribbons$fill, "darkgray", "red"))) +
          theme(plot.title = element_text(hjust = 0.5),
                panel.grid.minor = element_blank())
      }

      # store calculations and plot for each subject

      if(plot==TRUE) {output[[j]] <- list(AUC = AUC,
                                          plot = plotj)
      } else {output[[j]] <- list(AUC = AUC)}

      # store calculation for each subject and ID in data frame

      outputdf[j,] <- c(subject[j],AUC)

      # add subject plot to list for multiplot

      if(plot==TRUE) {plots[j] <- list(plotj)}

    }

  }

  ################### RESULTS ###################

  # create a multiplot from individual subject plots

  if (plot==TRUE & sort==FALSE) {multiplot <- plot_grid(plotlist = plots)}

  # sort plots by increasing AUC

  if (plot==TRUE & sort=="increasing") {
    order <- order(outputdf[,2])
    sortplots <- rep(list(NA),length(order))
    for (k in order) {sortplots[[which(order==k)]] <- plots[[k]]}
    multiplot <- plot_grid(plotlist = sortplots)
  }

  # sort plots by decreasing AUC

  if (plot==TRUE & sort=="decreasing") {
    order <- order(outputdf[,2], decreasing=TRUE)
    sortplots <- rep(list(NA),length(order))
    for (k in order) {sortplots[[which(order==k)]] <- plots[[k]]}
    multiplot <- plot_grid(plotlist = sortplots)
  }

  # reconstruct dataset used as input

  inputdata <- cbind(Subject = subject, data)

  # store calculation method used, input data

  info <- list(method = method,
               biomarker = biomarker,
               subjects = nrow(inputdata),
               removed = remove,
               timepoints = time,
               interval = interval,
               data = inputdata)

  # store output as lists

  if (plot==TRUE) {output <- list(input = info,
                                  subjects = output,
                                  dataframe = outputdf,
                                  multiplot = multiplot)
  } else {output <- list(input = info,
                         subjects = output,
                         dataframe = outputdf)}


  # return output

  return(output)

}

#' @title ribbonize
#'
#' @description calculate the ribbons required for geom_ribbon() to shade between two lines
#'
#' @param .x x-coordinates of a function, for two functions to shade between
#' @param .y y-coordinates of a function, for two functions to shade between
#' @param .f a two-level factor, indicates whether a set of x- and y- coordinates belongs to function "a" or function "b"
#'
#' @return a tibble with 4 variables: x, x-coordinates and interpolated points for both functions; ymax, corresponding y-coordinates for the upper-bound of shading; ymin, corresponding y-coordinates for the lower-bound of shading; fill, a logical variable to indicate if an interval is shaded or not
#'
#' @examples
#' df <- tibble(
#' x = c(1:8, 1:8),
#' y = c(1, 5, 6, 4, 1, 1, 3, 2, 1, 4, 5, 4, 2, 2, 2, 2),
#' f = c(rep("a", 8), rep("b", 8)))
#'
#' ribbons <- ribbonize(df, x, y, f)
#'
#' ggplot(df) +
#' geom_line(aes(x, y, linetype = f)) +
#' geom_ribbon(data = ribbons, aes(x, ymin = ymin, ymax = ymax, fill = fill))
#'
#' @author Neal Grantham, <https://www.nsgrantham.com/fill-between-two-lines-ggplot2>
#'
#' @export

ribbonize <- function(.data, .x, .y, .f) {

  # Check there are only 2 level in .f
  levels <- .data %>%
    pull({{ .f }}) %>%
    unique()

  stopifnot(length(levels) == 2)

  # Check that there is exactly 1 observation per level in .f at every .x
  level_counts_by_x <- .data %>%
    filter(!is.na({{ .y }})) %>%
    group_by({{ .x }}) %>%
    count() %>%
    pull(n)

  stopifnot(all(level_counts_by_x == 2))

  bounds <- .data %>%
    mutate({{ .f }} := recode({{ .f }}, a = levels[1], b = levels[2])) %>%
    pivot_wider(names_from = {{ .f }}, values_from = {{ .y }}) %>%
    mutate(
      ymax = pmax(a, b),
      ymin = pmin(a, b),
      fill = a >= b
    )

  intervals <- bounds %>%
    filter(ymax > ymin) %>%
    select(-a, -b)

  intersections <- bounds %>%
    mutate(lag_fill = lag(fill), lead_fill = lead(fill)) %>%
    filter(ymax == ymin) %>%
    select(-a, -b, -fill) %>%
    pivot_longer(lag_fill:lead_fill, names_to = NULL, values_to = "fill") %>%
    filter(!is.na(fill)) %>%
    distinct()

  other_intersections <- bounds %>%
    transmute(
      x1 = {{ .x }},       y1 = a,
      x2 = lead({{ .x }}), y2 = lead(a),
      x3 = {{ .x }},       y3 = b,
      x4 = lead({{ .x }}), y4 = lead(b)
    ) %>%
    filter(((y1 > y3) & (y2 < y4)) | ((y1 < y3) & (y2 > y4))) %>%
    mutate(
      d = (x1 - x2) * (y3 - y4) - (y1 - y2) * (x3 - x4),
      u = x1 * y2 - y1 * x2,
      v = x3 * y4 - y3 * x4,
      x = (u * (x3 - x4) - v * (x1 - x2)) / d,
      y = (u * (y3 - y4) - v * (y1 - y2)) / d
    ) %>%
    select(x, ymax = y, ymin = y)

  bind_rows(
    intervals,
    intersections,
    mutate(other_intersections, fill = TRUE),
    mutate(other_intersections, fill = FALSE)
  ) %>%
    arrange({{ .x }})
}
