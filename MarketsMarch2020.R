library(data.table)
library(ggplot2)
library(magrittr)
library(reshape2)
library(lubridate)

# load data
cDataPath <- file.path(getwd(), "data", "data_feb_mar_2020")
lDataDict <- list("wti_oil_USD" = "cl_f_d.csv", "brent_oil_USD" = "cb_f_d.csv",
                  "sp500" = "spx_d.csv", "wig20" = "wig20_d.csv",
                  "copper_USD" = "hg_f_d.csv")
lData <- vector(mode = "list", length = length(lDataDict))
names(lData) <- names(lDataDict)
for (cIterName in names(lDataDict)) {
  # cIterName <- names(lDataDict)[[1]]
  dfIter <- read.csv(file = file.path(cDataPath, lDataDict[[cIterName]]),
           stringsAsFactors = FALSE)
  dtIter <- dfIter[, c("Data", "Zamkniecie")] %>% data.table::as.data.table()
  data.table::setnames(x = dtIter, old = c("Data", "Zamkniecie"), 
                       new = c("date", cIterName))
  lData[[cIterName]] <- dtIter
}

# join the data sets
dtData <- Reduce(f = function(x, y) {
  merge(x = x, y = y, by ="date", all = TRUE)
}, x = lData)
dtData$date <- as.Date(dtData$date, format = "%Y-%m-%d")


# plot March Oil and SP500
dtDataMain <- dtData[lubridate::month(date) == 3, .(date, wti_oil_USD, sp500)]
dtDataMain[, index_wti_oil_USD := wti_oil_USD/dtDataMain[["wti_oil_USD"]][1]]
dtDataMain[, index_sp500 := sp500/dtDataMain[["sp500"]][1]]
dtDataMain <- dtDataMain[, list(date, index_wti_oil_USD, index_sp500)]
dtDataMainMelted <- data.table::melt(data = dtDataMain, id.vars = "date", 
                 measure.vars = c("index_wti_oil_USD", "index_sp500"),
                 variable.name = "asset")


ggplot2::ggplot(data = dtDataMainMelted, mapping = ggplot2::aes(x = date, y = value,
                                                          color = asset)) +
  ggplot2::geom_line() + ggplot2::geom_point() +
  ggplot2::theme_bw() + 
  ggplot2::ggtitle(label = "Index of change of SP500 and prompt futures WTI OIL price in March 2020",
                   subtitle = "based on Stooq.pl data") +
  ggplot2::scale_x_date(name = "Quote date",
                        breaks = unique(dtDataMainMelted$date) %>% sort.default()) +
  ggplot2::scale_y_continuous(name = "Index: Price(t)/Price(0)",
                              breaks = seq(0.65, 1.05, 0.05),
                              labels = as.character(seq(0.65, 1.05, 0.05)),
                              limits = c(0.65, 1.05)) +
  ggplot2::theme(axis.title.x = ggplot2::element_text(hjust = 0.5, size = 14),
                 axis.title.y = ggplot2::element_text(hjust = 0.5, size = 14),
                 plot.title = ggplot2::element_text(size = 18, hjust = 0.5),
                 plot.subtitle = ggplot2::element_text(size = 16, hjust = 0.5),
                 legend.position = "bottom") +
  ggplot2::labs(legend = "Legend: ")

