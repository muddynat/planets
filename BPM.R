tap <- data.frame(rep = 0, Sys.time = NA, BPM = NA)
tap[nrow(tap) + 1 ,1] <- nrow(tap); tap[nrow(tap), 2] <- as.numeric(Sys.time()) ; tap[nrow(tap), 3] <- 60/(tap[nrow(tap), 2] - tap[nrow(tap) -1, 2]) ; cat('Mean:', round(mean(tap[,3], na.rm = T), 1), '\nMedian:', round(median(tap[,3], na.rm = T), 1)) # this works if you just run it orver and over

bpm <- function(df = NULL, timeLimit = 5, plot = F) {
  if (is.null(df)) {
    df <- data.frame(rep = 0, Sys.time = as.numeric(Sys.time()), BPM = NA)
    df[1:2,] <- c(0,1, Sys.time(), Sys.time(), NA, NA)
    bpm(df, timeLimit)
  } else if (nrow(df) < 2) {
    df <- data.frame(rep = 0, Sys.time = as.numeric(Sys.time()), BPM = NA)
    df[1:2,] <- c(0,1, Sys.time(), Sys.time(), NA, NA)
    bpm(df, timeLimit)
  } else if (df[nrow(df),2] - df[nrow(df)-1,2] < timeLimit) {
    if (nrow(df) > 3 & is.na(df[1,3])) df <- df[2:nrow(df),]
    readline(cat('Current median BPM:',round(median(df[,3], na.rm = T), 1),'\nPress Return\n'))
    y <- as.numeric(Sys.time())
    df[nrow(df) + 1, 1] <- nrow(df)
    df[nrow(df), 2] <- y
    df[nrow(df), 3] <- 60/(df[nrow(df), 2] - df[nrow(df) -1, 2])
    bpm(df, timeLimit)
  } else {
    df <- df[1:nrow(df)-1,]
    if (plot == F) {
      cat('Break: timeLimit reached.','\nMean:', round(mean(df[,3], na.rm = T), 1), 'bpm\nMedian:', round(median(df[,3], na.rm = T), 1), 'bpm\nEstimated beats per minute is:', round((round(mean(df[,3], na.rm = T), 1)+round(median(df[,3], na.rm = T), 1))/2), '\n')
      df
    } else if (plot == T) hist(df[,3])
  }
}
bpm(timeLimit = 2, plot = T)

hist()
shapiro.test(tap[,3])

tap[!tap %in% boxplot.stats(tap)$out, 3]