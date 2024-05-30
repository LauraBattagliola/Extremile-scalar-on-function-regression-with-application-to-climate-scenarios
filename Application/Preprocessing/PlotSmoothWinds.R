library(ggplot2)
library(gridExtra)
library(grid)
library(gtable)

get_legend <- function(my_plot) {
  tmp <- ggplotGrob(my_plot)
  leg <- gtable_filter(tmp, "guide-box")
  return(leg)
}

load("/Code/Application/Data/Wind/RCP26_DRY_mean_wind_speed_smooth.Rdata")
load("/Code/Application/Data/Wind/RCP85_DRY_mean_wind_speed_smooth.Rdata")
load("/Code/Application/Data/Wind/RCP26_WS_mean_wind_speed_smooth.Rdata")
load("/Code/Application/Data/Wind/RCP85_WS_mean_wind_speed_smooth.Rdata")

S <- 116
s <- seq(1,24, length.out=S)

####### Warm summer

#### Climate action
RCP26.WS.mean.wind.speed.jun <- matrix(nrow=41, ncol=S)
RCP26.WS.mean.wind.speed.may <- matrix(nrow=41, ncol=S)
RCP26.WS.mean.wind.speed.sept <- matrix(nrow=41, ncol=S)
RCP26.WS.mean.wind.speed.jul <- matrix(nrow=41, ncol=S)
RCP26.WS.mean.wind.speed.aug <- matrix(nrow=41, ncol=S)



for(i in 1:41){
  RCP26.WS.mean.wind.speed.jun[i,] <- RCP26.WS.mean.wind.speed.smooth[[i]][6,]
  RCP26.WS.mean.wind.speed.may[i,] <- RCP26.WS.mean.wind.speed.smooth[[i]][5,]
  RCP26.WS.mean.wind.speed.sept[i,] <- RCP26.WS.mean.wind.speed.smooth[[i]][9,]
  RCP26.WS.mean.wind.speed.jul[i,] <- RCP26.WS.mean.wind.speed.smooth[[i]][7,]
  RCP26.WS.mean.wind.speed.aug[i,] <- RCP26.WS.mean.wind.speed.smooth[[i]][8,]
}


#### No climate action
RCP85.WS.mean.wind.speed.jun <- matrix(nrow=41, ncol=S)
RCP85.WS.mean.wind.speed.may <- matrix(nrow=41, ncol=S)
RCP85.WS.mean.wind.speed.sept <- matrix(nrow=41, ncol=S)
RCP85.WS.mean.wind.speed.jul <- matrix(nrow=41, ncol=S)
RCP85.WS.mean.wind.speed.aug <- matrix(nrow=41, ncol=S)



for(i in 1:41){
  RCP85.WS.mean.wind.speed.jun[i,] <- RCP85.WS.mean.wind.speed.smooth[[i]][6,]
  RCP85.WS.mean.wind.speed.may[i,] <- RCP85.WS.mean.wind.speed.smooth[[i]][5,]
  RCP85.WS.mean.wind.speed.sept[i,] <- RCP85.WS.mean.wind.speed.smooth[[i]][9,]
  RCP85.WS.mean.wind.speed.jul[i,] <- RCP85.WS.mean.wind.speed.smooth[[i]][7,]
  RCP85.WS.mean.wind.speed.aug[i,] <- RCP85.WS.mean.wind.speed.smooth[[i]][8,]
}



RCP26.DRY.mean.wind.speed.jun <- matrix(nrow=41, ncol=S)
RCP26.DRY.mean.wind.speed.may <- matrix(nrow=41, ncol=S)
RCP26.DRY.mean.wind.speed.sept <- matrix(nrow=41, ncol=S)
RCP26.DRY.mean.wind.speed.jul <- matrix(nrow=41, ncol=S)
RCP26.DRY.mean.wind.speed.aug <- matrix(nrow=41, ncol=S)

for(i in 1:41){
  RCP26.DRY.mean.wind.speed.jun[i,] <- RCP26.DRY.mean.wind.speed.smooth[[i]][6,]
  RCP26.DRY.mean.wind.speed.may[i,] <- RCP26.DRY.mean.wind.speed.smooth[[i]][5,]
  RCP26.DRY.mean.wind.speed.sept[i,] <- RCP26.DRY.mean.wind.speed.smooth[[i]][9,]
  RCP26.DRY.mean.wind.speed.jul[i,] <- RCP26.DRY.mean.wind.speed.smooth[[i]][7,]
  RCP26.DRY.mean.wind.speed.aug[i,] <- RCP26.DRY.mean.wind.speed.smooth[[i]][8,]
}


RCP85.DRY.mean.wind.speed.jun <- matrix(nrow=41, ncol=S)
RCP85.DRY.mean.wind.speed.may <- matrix(nrow=41, ncol=S)
RCP85.DRY.mean.wind.speed.sept <- matrix(nrow=41, ncol=S)
RCP85.DRY.mean.wind.speed.jul <- matrix(nrow=41, ncol=S)
RCP85.DRY.mean.wind.speed.aug <- matrix(nrow=41, ncol=S)

for(i in 1:41){
  RCP85.DRY.mean.wind.speed.jun[i,] <- RCP85.DRY.mean.wind.speed.smooth[[i]][6,]
  RCP85.DRY.mean.wind.speed.may[i,] <- RCP85.DRY.mean.wind.speed.smooth[[i]][5,]
  RCP85.DRY.mean.wind.speed.sept[i,] <- RCP85.DRY.mean.wind.speed.smooth[[i]][9,]
  RCP85.DRY.mean.wind.speed.jul[i,] <- RCP85.DRY.mean.wind.speed.smooth[[i]][7,]
  RCP85.DRY.mean.wind.speed.aug[i,] <- RCP85.DRY.mean.wind.speed.smooth[[i]][8,]
}


#-------------------------------------------------------------------------------
x_coord <- seq(1, 24, length.out = 116)

# Prepare a data frame to combine everything
df <- data.frame(Month = factor(), X = numeric(), Value = numeric(), Row = integer())

# Function to append data for a given month
append_month_data <- function(df, matrix, month) {
  n_rows <- nrow(matrix)
  n_cols <- ncol(matrix)
  
  # Repeat the month for each element in the matrix
  month_col <- factor(rep(month, each = n_cols * n_rows))
  
  # Create a repeating sequence of x_coord, once for each row in the matrix
  x_col <- rep(x_coord, n_rows)
  
  # Convert the matrix to a vector
  value_col <- as.vector(t(matrix))
  
  # Identify row numbers for reshaping
  row_col <- rep(1:n_rows, each = n_cols)
  
  # Combine and return
  rbind(df, data.frame(Month = month_col, X = x_col, Value = value_col, Row = row_col))
}

# Append data for each month
df <- append_month_data(df, RCP26.WS.mean.wind.speed.may, "May")
df <- append_month_data(df, RCP26.WS.mean.wind.speed.jun, "June")
df <- append_month_data(df, RCP26.WS.mean.wind.speed.jul, "July")
df <- append_month_data(df, RCP26.WS.mean.wind.speed.aug, "August")
df <- append_month_data(df, RCP26.WS.mean.wind.speed.sept, "Septemebr")

df$Station <- factor(rep(rep(c("ABO", "AIG", "ALT", "BAS", "BER", "BKLI", "BUS", "CDF", "CHU", "DAV", "DIS", "ENG",
                               "FRE", "GLA", "GSB", "GUT", "GVE", "INT", "KLO", "LUG", "LUZ", "MAG", "MVE", "NABBER",
                               "NABLAU", "OTL", "PIO", "PUY", "REH", "ROB", "RUE", "SAM", "SBE", "SCU", "SHA", "SIO",
                               "STG", "ULR", "VAD", "WYN", "ZER"), each=116),5))

names(df) <- c("Month", "Hour", "Wind", "Row", "Station")

ggplot(df, aes(x=Hour, y=Wind, color=Station)) + geom_line() + facet_grid( ~  Month) +scale_x_continuous(limits = c(1, 24), breaks = 1:24) +
  theme(legend.position = "none") +ylab(expression("Monthly Average Air Wind"))

df.WS.RCP26 <- df

#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------
x_coord <- seq(1, 24, length.out = 116)

# Prepare a data frame to combine everything
df1 <- data.frame(Month = factor(), X = numeric(), Value = numeric(), Row = integer())

# Function to append data for a given month
append_month_data <- function(df, matrix, month) {
  n_rows <- nrow(matrix)
  n_cols <- ncol(matrix)
  
  # Repeat the month for each element in the matrix
  month_col <- factor(rep(month, each = n_cols * n_rows))
  
  # Create a repeating sequence of x_coord, once for each row in the matrix
  x_col <- rep(x_coord, n_rows)
  
  # Convert the matrix to a vector
  value_col <- as.vector(t(matrix))
  
  # Identify row numbers for reshaping
  row_col <- rep(1:n_rows, each = n_cols)
  
  # Combine and return
  rbind(df, data.frame(Month = month_col, X = x_col, Value = value_col, Row = row_col))
}

# Append data for each month
df1 <- append_month_data(df1, RCP85.WS.mean.wind.speed.may, "May")
df1 <- append_month_data(df1, RCP85.WS.mean.wind.speed.jun, "June")
df1 <- append_month_data(df1, RCP85.WS.mean.wind.speed.jul, "July")
df1 <- append_month_data(df1, RCP85.WS.mean.wind.speed.aug, "August")
df1 <- append_month_data(df1, RCP85.WS.mean.wind.speed.sept, "Septemebr")

df1$Station <- factor(rep(rep(c("ABO", "AIG", "ALT", "BAS", "BER", "BKLI", "BUS", "CDF", "CHU", "DAV", "DIS", "ENG",
                                "FRE", "GLA", "GSB", "GUT", "GVE", "INT", "KLO", "LUG", "LUZ", "MAG", "MVE", "NABBER",
                                "NABLAU", "OTL", "PIO", "PUY", "REH", "ROB", "RUE", "SAM", "SBE", "SCU", "SHA", "SIO",
                                "STG", "ULR", "VAD", "WYN", "ZER"), each=116),5))

names(df1) <- c("Month", "Hour", "Wind", "Row", "Station")

ggplot(df1, aes(x=Hour, y=Wind, color=Station)) + geom_line() + facet_grid( ~  Month) +scale_x_continuous(limits = c(1, 24), breaks = 1:24) +
  theme(legend.position = "none") +ylab(expression("Monthly Average Air Wind"))


df.WS.RCP85 <- df1

#-------------------------------------------------------------------------------


#-------------------------------------------------------------------------------
x_coord <- seq(1, 24, length.out = 116)

# Prepare a data frame to combine everything
df <- data.frame(Month = factor(), X = numeric(), Value = numeric(), Row = integer())

# Function to append data for a given month
append_month_data <- function(df, matrix, month) {
  n_rows <- nrow(matrix)
  n_cols <- ncol(matrix)
  
  # Repeat the month for each element in the matrix
  month_col <- factor(rep(month, each = n_cols * n_rows))
  
  # Create a repeating sequence of x_coord, once for each row in the matrix
  x_col <- rep(x_coord, n_rows)
  
  # Convert the matrix to a vector
  value_col <- as.vector(t(matrix))
  
  # Identify row numbers for reshaping
  row_col <- rep(1:n_rows, each = n_cols)
  
  # Combine and return
  rbind(df, data.frame(Month = month_col, X = x_col, Value = value_col, Row = row_col))
}

# Append data for each month
df <- append_month_data(df, RCP26.DRY.mean.wind.speed.may, "May")
df <- append_month_data(df, RCP26.DRY.mean.wind.speed.jun, "June")
df <- append_month_data(df, RCP26.DRY.mean.wind.speed.jul, "July")
df <- append_month_data(df, RCP26.DRY.mean.wind.speed.aug, "August")
df <- append_month_data(df, RCP26.DRY.mean.wind.speed.sept, "Septemebr")

df$Station <- factor(rep(rep(c("ABO", "AIG", "ALT", "BAS", "BER", "BKLI", "BUS", "CDF", "CHU", "DAV", "DIS", "ENG",
                               "FRE", "GLA", "GSB", "GUT", "GVE", "INT", "KLO", "LUG", "LUZ", "MAG", "MVE", "NABBER",
                               "NABLAU", "OTL", "PIO", "PUY", "REH", "ROB", "RUE", "SAM", "SBE", "SCU", "SHA", "SIO",
                               "STG", "ULR", "VAD", "WYN", "ZER"), each=116),5))


names(df) <- c("Month", "Hour", "Wind", "Row", "Station")

ggplot(df, aes(x=Hour, y=Wind, color=Station)) + geom_line() + facet_grid( ~  Month) +scale_x_continuous(limits = c(1, 24), breaks = 1:24) +
  theme(legend.position = "none") +ylab(expression("Monthly Average Air Wind"))

df.DRY.RCP26 <- df

#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------
x_coord <- seq(1, 24, length.out = 116)

# Prepare a data frame to combine everything
df1 <- data.frame(Month = factor(), X = numeric(), Value = numeric(), Row = integer())

# Function to append data for a given month
append_month_data <- function(df, matrix, month) {
  n_rows <- nrow(matrix)
  n_cols <- ncol(matrix)
  
  # Repeat the month for each element in the matrix
  month_col <- factor(rep(month, each = n_cols * n_rows))
  
  # Create a repeating sequence of x_coord, once for each row in the matrix
  x_col <- rep(x_coord, n_rows)
  
  # Convert the matrix to a vector
  value_col <- as.vector(t(matrix))
  
  # Identify row numbers for reshaping
  row_col <- rep(1:n_rows, each = n_cols)
  
  # Combine and return
  rbind(df, data.frame(Month = month_col, X = x_col, Value = value_col, Row = row_col))
}

# Append data for each month
df1 <- append_month_data(df1, RCP85.DRY.mean.wind.speed.may, "May")
df1 <- append_month_data(df1, RCP85.DRY.mean.wind.speed.jun, "June")
df1 <- append_month_data(df1, RCP85.DRY.mean.wind.speed.jul, "July")
df1 <- append_month_data(df1, RCP85.DRY.mean.wind.speed.aug, "August")
df1 <- append_month_data(df1, RCP85.DRY.mean.wind.speed.sept, "Septemebr")

df1$Station <- factor(rep(rep(c("ABO", "AIG", "ALT", "BAS", "BER", "BKLI", "BUS", "CDF", "CHU", "DAV", "DIS", "ENG",
                                "FRE", "GLA", "GSB", "GUT", "GVE", "INT", "KLO", "LUG", "LUZ", "MAG", "MVE", "NABBER",
                                "NABLAU", "OTL", "PIO", "PUY", "REH", "ROB", "RUE", "SAM", "SBE", "SCU", "SHA", "SIO",
                                "STG", "ULR", "VAD", "WYN", "ZER"), each=116),5))

names(df1) <- c("Month", "Hour", "Wind", "Row", "Station")

ggplot(df1, aes(x=Hour, y=Wind, color=Station)) + geom_line() + facet_grid( ~  Month) +scale_x_continuous(limits = c(1, 24), breaks = 1:24) +
  theme(legend.position = "none") +ylab(expression("Monthly Average Air Wind"))


df.DRY.RCP85 <- df1

#-------------------------------------------------------------------------------

#----------------------------------------------------------------------------------------

df.hum.DRY <- rbind(df.DRY.RCP26,df.DRY.RCP85 )
df.hum.DRY$Scenario <- factor(rep(c("RCP2.6 (Climate mitigation actions)", "RCP8.5 (No climate mitigation actions)"), each=S*41*5))

df.hum.WS <- rbind(df.WS.RCP26,df.WS.RCP85 )
df.hum.WS$Scenario <- factor(rep(c("RCP2.6 (Climate mitigation actions)", "RCP8.5 (No climate mitigation actions)"), each=S*41*5))


min.hum <-min(min(df.hum.WS$Wind), min(df.hum.DRY$Wind))
max.hum <-max(max(df.hum.WS$Wind), max(df.hum.DRY$Wind))

df.hum.DRY$highlight.urb <- df.hum.DRY$Station %in% c("STG", "LUG", "REH")
df.hum.WS$highlight.urb <- df.hum.WS$Station %in%  c( "STG", "LUG", "REH")

library(viridis)
my_col<- viridis(3)

theme_set(theme_bw())
hour_labels <- c("12AM", paste(1:11, "AM"), "12PM", paste(1:11, "PM"))



p.WS.hum1.urb <- ggplot(data = df.hum.WS, aes(x=Hour, y = Wind, group=Station )) + facet_grid(Month ~  Scenario)+
  ylab(expression(paste("Monthly Average Wind Speed  (", m*s^-1, ")", sep = "")))+
  scale_x_continuous(limits = c(1, 24), breaks = 1:24,  labels = hour_labels) + labs(title = "Warm Summer Scenario")+ ylim(c(min.hum, max.hum))+
  geom_line(aes(color = "gray80", alpha=0.8), show.legend = FALSE) + # Plot all curves in gray
  scale_color_identity() + geom_line(data = df.hum.WS[df.hum.WS$highlight.urb, ], aes(color = Station), size = 1.2) +
  scale_color_manual(values = c("STG" = my_col[1], "LUG" =my_col[2], "REH" = my_col[3] ), labels = c( "Lugano",  "Zurich" ,"St. Gallen" )) +
  theme( legend.title=element_blank(), legend.position = "bottom",
         plot.title = element_text(size = rel(2), face = "bold"),
         axis.title.x = element_text(size = rel(1.8)),
         axis.title.y = element_text(size = rel(1.8)),
         axis.text.x = element_text(size = rel(1.5), angle = 90, hjust = 1),
         axis.text.y = element_text(size = rel(1.8)),
         legend.text = element_text(size = rel(1.8)), #
         strip.text.x = element_text(size = rel(1.5)),
         strip.text.y = element_text(size = rel(1.5))
  )


p.DRY.hum1.urb <- ggplot(data = df.hum.DRY, aes(x=Hour, y = Wind, group=Station )) + facet_grid(Month ~  Scenario)+
  ylab(expression(paste("Monthly Average Wind Speed  (", m*s^-1, ")", sep = "")))+
  scale_x_continuous(limits = c(1, 24), breaks = 1:24,  labels = hour_labels) + labs(title = "Design Reference Year Scenario")+ ylim(c(min.hum, max.hum))+
  geom_line(aes(color = "gray80", alpha=0.8), show.legend = FALSE) + # Plot all curves in gray
  scale_color_identity() + geom_line(data = df.hum.DRY[df.hum.DRY$highlight.urb, ], aes(color = Station), size = 1.2) +
  scale_color_manual(values = c("STG" = my_col[1], "LUG" =my_col[2], "REH" = my_col[3] ), labels = c( "Lugano",  "Zurich" ,"St. Gallen" )) +
  theme( legend.title=element_blank(), legend.position = "bottom",
         plot.title = element_text(size = rel(2), face = "bold"),
         axis.title.x = element_text(size = rel(1.8)),
         axis.title.y = element_text(size = rel(1.8)),
         axis.text.x = element_text(size = rel(1.5), angle = 90, hjust = 1),
         axis.text.y = element_text(size = rel(1.8)),
         legend.text = element_text(size = rel(1.8)), #
         strip.text.x = element_text(size = rel(1.5)),
         strip.text.y = element_text(size = rel(1.5))
  )


legend <- get_legend(p.DRY.hum1.urb)

dev.new()
grid.arrange(
  arrangeGrob(p.DRY.hum1.urb+ theme(legend.position = "none")  ,
              p.WS.hum1.urb+ theme(legend.position = "none"), ncol = 2),
  legend,
  heights = c(9/10, 1/10) 
)
dev.off()

