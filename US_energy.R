library(data.table)
library(magrittr)
library(ggplot2)
library(reshape2)

# Data source:
# https://www.eia.gov/totalenergy/data/browser/ ==> "Natural gas" ==> "Natural gas overview"

dtNatgas <- read.csv(file = "data/yearly_natgas_data.csv", stringsAsFactors = FALSE) %>%
  data.table::as.data.table()
dtNatgas[, Description := toupper(Description) %>% gsub(pattern = " ", replacement = "_")]
dtNatgas[, Description := gsub(x = Description, pattern = "\\_\\(DRY\\)", replacement = "")]
lapply(dtNatgas[, list(Description, MSN)], unique)
cVariablesFoireginTrade <- c("NATURAL_GAS_EXPORTS", "NATURAL_GAS_IMPORTS")
cVariablesSupplyAndDemand <- c("NATURAL_GAS_CONSUMPTION", "NATURAL_GAS_PRODUCTION")

# keep only the variables of interest and prepare yearly data
dtNatgas <- dtNatgas[Description %in% c(cVariablesSupplyAndDemand, cVariablesFoireginTrade), ]
dtNatgas$MSN <- NULL
dtNatgas$Column_Order <- NULL
dtNatgas <- dtNatgas[grepl(pattern = "^[0-9]{4}[1]{1}[3]{1}", x = YYYYMM), ]
dtNatgas[, Unit := gsub(pattern = " ", replacement = "_", x = Unit) %>% toupper()]
unique(dtNatgas$Unit)
# all variables in billions of cubic feet
dtNatgas$Unit <- NULL
data.table::setnames(x = dtNatgas, old = c("YYYYMM", "Value", "Description"), 
                     new = c("value_year", "value", "variable"))
dtNatgas[, value_year := as.integer(substr(x = value_year, start = 1, stop = 4))]
unique(dtNatgas$variable)
dtNatgas[, variable := gsub(x = variable, pattern = "^NATURAL_GAS_", replacement = "")]
dtNatgas$variable <- tolower(dtNatgas$variable)
dtNatgas[, value := as.double(value)]
dtNatgas[, value_year := as.double(value_year)]

# calculate net exports
dtNatgasTrade <- dtNatgas[variable %in% c("exports", "imports"), ]
dtNatgasTradeWide <- data.table::dcast(data = dtNatgasTrade, 
                                       value.var = "value", 
                                       formula = value_year ~ variable, 
                                       fun.aggregate = sum)
dtNatgasTradeWide[, net_exports := exports - imports]
dtNatgasTradeLong <- data.table::melt(data = dtNatgasTradeWide, id.vars = "value_year", 
                                      measure.vars = c("exports", "imports", "net_exports"),
                                      variable.name = "variable", value.name = "value")
dtNatgasTradeLong$value_year <- as.integer(dtNatgasTradeLong$value_year)
dtNatgasTradeLong <- dtNatgasTradeLong[order(variable, value_year), ]
dtNatgasTradeLong <- dtNatgasTradeLong[value_year >= 1960L, ]
# plotForeignTradeNatGas <- 
ggplot2::ggplot(data = dtNatgasTradeLong,
                mapping = ggplot2::aes(x = value_year, y = value,
                                       color = variable)) + 
  ggplot2::geom_line() + ggplot2::theme_bw() + 
  ggplot2::ggtitle(label = "US Foreign Trade of Natural Gas (yearly data) 1960 - 2018",
                   subtitle = "EIA data (Total Energy data), all time series in billions of cubic feet") +
  ggplot2::labs(color = "Legend") +
  ggplot2::scale_x_continuous(name = "Year", breaks = seq(1960L, 2020L, 5L),
                            labels = as.character(seq(1960L, 2020L, 5L)),
                            limits = c(1960L, 2020L)) +
  ggplot2::scale_y_continuous(name = "Variables values (in billions of cubic feet)",
                              breaks = seq(-4000, 5000, 1000), 
                              labels = as.character(seq(-4000, 5000, 1000)), 
                              limits = c(-4000, 5000)) +
  ggplot2::theme(legend.position = "bottom",
                 axis.title.x = ggplot2::element_text(hjust = 0.5, size = 14),
                 axis.title.y = ggplot2::element_text(hjust = 0.5, size = 14),
                 plot.title = ggplot2::element_text(hjust = 0.5, size = 18),
                 plot.subtitle = ggplot2::element_text(hjust = 0.5, size = 16))
  


