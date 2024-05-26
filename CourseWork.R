library(ggplot2)
library(dplyr)
library(tidyr)
library(tseries)  
library(forecast) 
library(stats)    
library(pracma)   
library(lubridate)
library(rstatix)
library(MASS)
library(patchwork)
library(scales)
library(tidyverse)
library(alphahull)
library(tsfeatures)
library(corrplot)
library(dbscan)
library(FactoMineR)

Duomenys <- read.csv("string_data.csv",check.names = FALSE)

Duomenys <- Duomenys[,-ncol(Duomenys)]

nrow(Duomenys)
ncol(Duomenys)
colnames(Duomenys)

pattern <- "\\[.*\\] String (\\d+-\\d+) String (current|voltage) \\((Vdc|A)\\)"

colnames(Duomenys)<- gsub(pattern, "\\1 \\3", colnames(Duomenys))
ncol(Duomenys)
colnames(Duomenys)
Duomenys[] <- lapply(Duomenys, function(x) ifelse(x < 0, 0, x))

Duomenys$Timestamp <- as.POSIXct(Duomenys$Timestamp, format = "%Y-%m-%d %H:%M:%S")
Duomenys$Timestamp
Duomenys$Timestamp

na_rows <- which(!complete.cases(Duomenys))
Duomenys[na_rows,]$Timestamp

Duomenys[which(is.na(Duomenys$`1-01 A`)),]$Timestamp

which(Duomenys$Timestamp=='2022-05-24 22:30:00 EET')
which(Duomenys$Timestamp=='2022-06-01 13:00:00 EET')

which(Duomenys$Timestamp=='2022-05-15 22:30:00 EET')
which(Duomenys$Timestamp=='2022-05-23 13:00:00 EET')

which(Duomenys$Timestamp=='2022-06-02 22:30:00 EET')
which(Duomenys$Timestamp=='2022-06-10 13:00:00 EET')

Duomenys[8150:8880,-1] <- (Duomenys[7286:8016,-1] + Duomenys[9014:9744,-1]) / 2

which(Duomenys$Timestamp=='2022-09-15 14:00:00 EET')
which(Duomenys$Timestamp=='2022-09-15 15:00:00 EET')

which(Duomenys$Timestamp=='2022-09-14 14:00:00 EET')
which(Duomenys$Timestamp=='2022-09-14 15:00:00 EET')

which(Duomenys$Timestamp=='2022-09-16 14:00:00 EET')
which(Duomenys$Timestamp=='2022-09-16 15:00:00 EET')
Duomenys[19060:19064,-1] <- (Duomenys[18964:18968,-1] + Duomenys[19156:19160,-1]) / 2

which(Duomenys$Timestamp=='2023-06-29 12:15:00 EET')
which(Duomenys$Timestamp=='2023-06-29 15:00:00 EET')

which(Duomenys$Timestamp=='2023-06-28 12:15:00 EET')
which(Duomenys$Timestamp=='2023-06-28 15:00:00 EET')

which(Duomenys$Timestamp=='2023-06-30 12:15:00 EET')
which(Duomenys$Timestamp=='2023-06-30 15:00:00 EET')

Duomenys[46605:46616,-1] <- (Duomenys[46509:46520,-1] + Duomenys[46701:46712,-1]) / 2

length(colnames(Duomenys))
sum(Duomenys  < 0)


valandiniaiDvyliktasInverteris <- Duomenys %>%
  mutate(Valanda = paste(year(Timestamp), '-', month(Timestamp), day(Timestamp),hour(Timestamp))) %>%
  group_by(Valanda) %>%
  summarise(across(`12-01 A`:`12-10 A`, sum))

valandiniaiDvyliktasInverteris$Valanda
valandiniaiDvyliktasInverteris$Valanda <- sapply(strsplit(valandiniaiDvyliktasInverteris$Valanda, " "), function(x) as.numeric(tail(x, 1)))
range(valandiniaiDvyliktasInverteris$Valanda)
typeof(valandiniaiDvyliktasInverteris$Valanda)
valandiniaiDvyliktasInverteris

long_format_data <- valandiniaiDvyliktasInverteris %>%
  pivot_longer(cols = starts_with("12"), 
               names_to = "Factor",      
               values_to = "Value")      

long_format_data$Valanda

long_format_data$Value[long_format_data$Value < 0] <- 0.0001
range(long_format_data$Value)

range(long_format_data$Value)
range(long_format_data$Value)

# DATA Transformation to Wh  -----------------------------------------------------------


result_list <- list()

for (grandineA in colnames(Duomenys[,2:144])) {
  grandineV <- paste(substr(grandineA, 1, nchar(grandineA) - 2), "Vdc")
  
  result <- Duomenys[[grandineA]] * Duomenys[[grandineV]] * 0.25
  
  result_list[[substr(grandineA, 1, nchar(grandineA) - 2)]] <- result
}

NaujiDuomenysWh <- as.data.frame(result_list)
colnames(NaujiDuomenysWh) <- substring(colnames(NaujiDuomenysWh),2)

# Hourly Data Transformation ------------------------------------------------------------------


Valandiniai <- NaujiDuomenysWh[-c(1,2,3,nrow(NaujiDuomenysWh)),]
Valandiniai <- mutate(Valandiniai, Hour = rep(1:ceiling(n() / 4), each = 4)[1:n()])

Valandiniai_new <- Valandiniai %>%
  group_by(Hour) %>%
  summarise(across(everything(), sum, na.rm = TRUE))


# Energy Generation Of Every Solar Panel String --------------------------------------------------------------------


TempNauji <- NaujiDuomenysWh
TempNauji$Timestamp <- Duomenys$Timestamp
TempNauji <- TempNauji[2980:3280,]
isskirti <- c("8.12","12.02")

color_palette <- c("#377EB8","#E41A1C") 
p <- ggplot(TempNauji, aes(x = Timestamp, y = `1.01`)) +
  geom_line(aes(color = "Other Variables"))

for(col in colnames(TempNauji)[2:143]){
  p <- p + geom_line(aes(x = Timestamp, y = !!sym(col), color = col))
}

for(i in seq_along(isskirti)){
  col <- isskirti[i]
  line_color <- ifelse(col %in% isskirti, color_palette[i], "black")
  line_size <- ifelse(col %in% isskirti, 1, 0.5)
  p <- p + geom_line(aes(x = Timestamp, y = !!sym(col), color = col), 
                     color = line_color, size = line_size)
}

p + labs(title = "Energy Generation Of Every Solar Panel String",
         x = "Timestamp",
         y = "Power (W)",
         color = "String") +
  theme_bw() +
  theme(
    panel.background = element_rect(fill = "white", color = "black"), 
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),  
    axis.line = element_line(color = "black"),  
    axis.text = element_text(color = "black", size = 12),  
    axis.title = element_text(color = "black", size = 14), 
    legend.text = element_text(color = "black", size = 12), 
    legend.title = element_text(color = "black", size = 14),
    plot.title = element_text(hjust = 0.5,size=16)
  ) + 
  scale_color_manual(values = c("black", color_palette), 
                     labels = c("Other strings", "8.12", "12.02"))

# Summer / Winter generation -------------------------------------------------------------

TempNauji <- NaujiDuomenysWh
TempNauji$Timestamp <-  as.POSIXct(Duomenys$Timestamp)

TempNaujiNew <- TempNauji %>% 
  mutate(YearMonth = format(Timestamp, "%Y-%m-%d")) %>% 
  mutate(YMonth = format(Timestamp, "%Y-%m"))

summarized_df <- TempNaujiNew %>%
  group_by(YearMonth) %>%
  summarize(across(.cols = contains("."), .fns = sum), .groups = "drop")

summarized_df <- summarized_df %>%
  rowwise() %>%
  mutate(SumOfAllValues = sum(c_across(contains("."))))

colnames(summarized_df)
summarized_df$YMonth 
summarized_df$SumOfAllValues

final_df <- summarized_df[, c("YearMonth", "SumOfAllValues")]

final_df_MOnth <- final_df

final_df_MOnth$YMonth <- substr(final_df_MOnth$YearMonth, 1, 7)

unique(final_df_MOnth$YMonth)

which.max(final_df_MOnth[final_df_MOnth$YMonth=="2022-08",]$SumOfAllValues)

dfBest <- data.frame()
for (menesis in unique(final_df_MOnth$YMonth))
{
  df <- final_df_MOnth[final_df_MOnth$YMonth==menesis,]
  kuris <-  which.max(df$SumOfAllValues)
  dfBest <- append(dfBest, list(Best = df[kuris, ]), after = 2)
}

dates_to_keep <- as.POSIXct(c("2022-12-03", "2023-01-31", "2023-02-27", "2023-06-05", "2023-07-07", "2023-08-04"))

filtered_df <- TempNauji %>%
  filter(as.Date(Timestamp) %in% as.Date(dates_to_keep))

breaks_to_use <- as.POSIXct(dates_to_keep)

isskirti <- c("8.12","12.02")

color_palette <- c("#377EB8","#E41A1C") 

filtered_df$Timestamp <- as.POSIXct(filtered_df$Timestamp)

GetMonthGraph <- function(day)
{
  filtered_dfDec <- filtered_df %>%
    filter(as.Date(Timestamp) %in% as.Date(as.POSIXct(day)))
  
  p <- ggplot(filtered_dfDec, aes(x = Timestamp, y = `1.01`)) +
    geom_line() +
    scale_x_datetime(labels = date_format("%H:%M"), date_breaks = "6 hours") + 
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
  
  for(col in colnames(filtered_dfDec)[2:143]){
    p <- p + geom_line(aes(x = Timestamp, y = !!sym(col)))
  }
  
  pDec <- p + labs(title = day,
                   x = "Timestamp",
                   y = "Power (W)",
                   color = "String") +
    theme_bw() +
    theme(
      panel.background = element_rect(fill = "white", color = "black"), 
      panel.grid.major = element_blank(), 
      panel.grid.minor = element_blank(),  
      axis.line = element_line(color = "black"),  
      axis.text = element_text(color = "black", size = 12),  
      axis.title = element_text(color = "black", size = 14), 
      legend.text = element_text(color = "black", size = 12), 
      legend.title = element_text(color = "black", size = 14),
      plot.title = element_text(hjust = 0.5,size=16) 
    ) 
  
  pDec <- pDec + ylim(0, 4500)
  
  return(pDec)
}

plotDec <- GetMonthGraph("2022-12-03")
plotJan <- GetMonthGraph("2023-01-31")
plotFeb <- GetMonthGraph("2023-02-27")
plotJun <- GetMonthGraph("2023-06-05")
plotJul <- GetMonthGraph("2023-07-07")
plotAug <- GetMonthGraph("2023-08-04")

plot_grid <- (plotDec | plotJan | plotFeb) / 
  (plotJun | plotJul | plotAug)

plot_grid


TempNaujiNewCalc <- TempNauji %>% 
  mutate(YearMonth = format(Timestamp, "%Y-%m-%d")) %>% 
  mutate(YMonth = format(Timestamp, "%Y-%m"))

summarized_dfCalc <- TempNaujiNewCalc %>%
  group_by(YMonth) %>%
  summarize(across(.cols = contains("."), .fns = sum), .groups = "drop")

summarized_dfCalc <- summarized_dfCalc %>%
  rowwise() %>%
  mutate(SumOfAllValues = sum(c_across(contains("."))))


# Solar Panel String Energy Generation Histogram ------------------------------------------------------------------

NaujasData <- data.frame(Inverteris = numeric(ncol(NaujiDuomenysWh)-1))

count <- 1
for(col in colnames(NaujiDuomenysWh)[-ncol(NaujiDuomenysWh)]) {
  
  
  inverteris <- floor(as.numeric(col))
  fractional_part <- strsplit(col, ".", fixed = TRUE)[[1]][2]
  
  
  total <- sum(NaujiDuomenysWh[[col]])
  
  NaujasData[count, "Total Power (kW)"] <- total/1000
  NaujasData[count, "Inverter number"] <- inverteris
  NaujasData[count, "String number"] <- as.numeric(fractional_part)
  
  count <- count + 1
}


NaujasData <- NaujasData[-nrow(NaujasData),]

ggplot(NaujasData, aes(x = `Total Power (kW)`)) +
  geom_histogram(bins = 143, fill = "skyblue", color = "black") +
  labs(title = "Distribution of Total Power",
       x = "Total Power (kW)",
       y = "Frequency") +
  theme(
    panel.background = element_rect(fill = "white", color = "black"), 
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),  
    axis.line = element_line(color = "black"),  
    axis.text = element_text(color = "black", size = 12),  
    axis.title = element_text(color = "black", size = 14), 
    legend.text = element_text(color = "black", size = 12), 
    legend.title = element_text(color = "black", size = 14),
    plot.title = element_text(hjust = 0.5,size=16) 
  ) +
  labs(title = "Solar Panel String Energy Generation Histogram") +
  scale_x_continuous(breaks = seq(27826,48058, by = 4000),
                     labels = seq(27826,48058, by = 4000)) +
  scale_y_continuous(breaks = seq(0,10, by = 1),
                     labels = seq(0,10, by = 1)) 

# Barplot -----------------------------------------------------------------

Summer <- summarized_dfCalc$SumOfAllValues[summarized_dfCalc$YMonth == "2023-06"] +
  summarized_dfCalc$SumOfAllValues[summarized_dfCalc$YMonth == "2023-07"] +
  summarized_dfCalc$SumOfAllValues[summarized_dfCalc$YMonth == "2023-08"]

Winter <- summarized_dfCalc$SumOfAllValues[summarized_dfCalc$YMonth == "2022-12"] +
  summarized_dfCalc$SumOfAllValues[summarized_dfCalc$YMonth == "2023-01"] +
  summarized_dfCalc$SumOfAllValues[summarized_dfCalc$YMonth == "2023-02"]

Summer
Winter

data <- data.frame(
  Season = c("Winter", "Summer"),
  Value = c(Winter, Summer)
)


percentage_difference <- ((Summer - Winter) / Winter) * 100

percentage_difference

ggplot(data, aes(x = Season, y = Value, fill = Season)) +
  geom_bar(stat = "identity") +
  labs(title = "Total generated energy of the entire power plant \n during summer and winter months", x = "Season", y = "Total generated energy (W)") +
  theme_bw() +
  theme(
    panel.background = element_rect(fill = "white", color = "black"), 
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),  
    axis.line = element_line(color = "black"),  
    axis.text = element_text(color = "black", size = 12),  
    axis.title = element_text(color = "black", size = 14), 
    legend.text = element_text(color = "black", size = 12), 
    legend.title = element_text(color = "black", size = 14),
    plot.title = element_text(hjust = 0.5, size = 16),
    legend.position = "none" 
  ) +
  scale_y_continuous(labels = scales::comma) + 
  geom_segment(aes(x = 2-0.45, y = Winter, xend = 2-0.45, yend = Summer), colour = "red", size = 0.9,
               arrow = arrow(type = "closed", length = unit(0.3, "cm"))) + 
  geom_segment(aes(x = 2-0.45, y = Summer, xend = 2-0.57, yend = Summer), colour = "red", size = 0.9, linetype = 'dashed') +
  annotate("text", x = 2-0.42, y = (Winter + Summer) / 2, label = paste(round(percentage_difference, 2), "%"), 
           hjust = -0.1, vjust = 0, size = 5, angle = 270, colour = "red")

# Correlation -----------------------------------------------------------------

nr <- ncol(NaujiDuomenysWh)
KoreliacijomDuomenys <- NaujiDuomenysWh[,-c(nr,nr-1)]
ncol(KoreliacijomDuomenys)

corrMatrix <- cor(KoreliacijomDuomenys)
corrMatrix

match_color <- c("black")
pos_colors <- colorRampPalette(c("#FFFFFB", "#1C9E48"))(1000)
neg_colors <- colorRampPalette(c("#41B6C4", "#FFFFFB"))(1000)

corrplot(corrMatrix, method = "color", col = c(neg_colors, pos_colors),
         tl.col = 'black', tl.srt = 45, type ="upper", xlab="sdv", addCoefasPercent = FALSE)

# Feature Extraction  --------------------------------------------------------------------

feature_list <- function(col, NaujiDuomenysWh) {
  NaujiDuomenysWh$Hour_Group <- cut(NaujiDuomenysWh$Hour, breaks = c(0, 3, 7, 11, 15, 19, 23), labels = FALSE, include.lowest = TRUE)
  
  summary_data <- aggregate(NaujiDuomenysWh[[col]], by = list(Hour_Group = NaujiDuomenysWh$Hour_Group), FUN = sum)
  hour_features <- as.data.frame(t(summary_data[-1]))
  colnames(hour_features) <- paste0("H", summary_data$Hour_Group * 4, "-", ((summary_data$Hour_Group + 1) * 4 - 1) %% 24)
  
  timeseries <- ts(NaujiDuomenysWh[[col]], frequency = 96)
  
  ts_features <- tsfeatures(timeseries)[c("e_acf1","trend","linearity","curvature",
                                          "seasonal_strength","entropy","peak","trough")]
  
  extra_features <- list(mean = mean(timeseries), var = var(timeseries),crossings = sum(timeseries > mean(timeseries)))
  
  all_features <- data.frame(hour_features, ts_features, extra_features)
  return(all_features)
}

features_list <- list()
for (i in 2:(ncol(Valandiniai_new))) {
  features <- feature_list(i, Valandiniai_new)
  features$Grandine <- colnames(Valandiniai_new[i])
  features_list[[i - 1]] <- features
}
features_list
features_df <- do.call(rbind, features_list)

features_dfNew <- features_df[, !names(features_df) %in% c("H4.7", "H24.3")]


# PCA Alpha HULL  --------------------------------------------------------------------

# PCA ---------------------------------------------------------------------



pca_result_facto <- PCA(features_dfNew[,-ncol(features_dfNew)], graph = FALSE)

eigenvalues <- pca_result_facto$eig[, 2] 
components <- 1:nrow(pca_result_facto$eig) 
data <- data.frame(Component = components, Eigenvalue = eigenvalues)
total_eigenvalues <- sum(data$Eigenvalue)
data$Percentage <- (data$Eigenvalue / total_eigenvalues) * 100

ggplot(data, aes(x = as.factor(Component), y = Eigenvalue)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  geom_text(aes(label = sprintf("%.2f%%", Percentage)), vjust = -0.5, color = "black") +
  labs(x = "Component", y = "Explained variance ratio", title = "Explained variance ratio for each principal component") +
  theme_bw() +
  theme(
    panel.background = element_rect(fill = "white", color = "black"), 
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),  
    axis.line = element_line(color = "black"),  
    axis.text = element_text(color = "black", size = 12),  
    axis.title = element_text(color = "black", size = 14), 
    legend.text = element_text(color = "black", size = 12), 
    legend.title = element_text(color = "black", size = 14),
    plot.title = element_text(hjust = 0.5,size=16)
  ) 



plot(pca_result_facto, choix = "ind", ind.names = features_dfNew$Grandine)

coords <- as.data.frame(pca_result_facto$ind$coord[, 1:2])
coords$grandine <- features_dfNew$Grandine

pca_plot <- ggplot(coords, aes(x = Dim.1, y = Dim.2, label = grandine)) +
  geom_point(aes(), size = 3, alpha = 0.6) +
  geom_text(vjust = 1.5, hjust = 0.5, size = 5) +  
  labs(title = "PCA projection",
       x = "PC1",
       y = "PC1",
       color = "Grandine Label") + 
  theme_bw() +
  theme(
    panel.background = element_rect(fill = "white", color = "black"), 
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),  
    axis.line = element_line(color = "black"),  
    axis.text = element_text(color = "black", size = 12),  
    axis.title = element_text(color = "black", size = 20), 
    legend.text = element_text(color = "black", size = 14), 
    legend.title = element_text(color = "black", size = 14),
    plot.title = element_text(hjust = 0.5, size = 22) 
  )


print(pca_plot)

dev.off()

# ALPHA HULL --------------------------------------------------------------------

# ALPHA = 0.5

hull <- ahull(coords$Dim.1, coords$Dim.2, 0.5)
hull$ashape.obj

generate_arc_points <- function(center, radius, start_angle, end_angle, n = 100) {
  seq_angles <- seq(start_angle, end_angle, length.out = n)
  x <- center[1] + radius * cos(seq_angles)
  y <- center[2] + radius * sin(seq_angles)
  data.frame(x = x, y = y)
}

arc_points_list <- lapply(1:nrow(hull$arcs), function(i) {
  arc <- hull$arcs[i, ]
  if (arc['r'] > 0) {  
    v_x <- arc['v.x']
    v_y <- arc['v.y']
    theta <- arc['theta']
    start_angle <- atan2(v_y, v_x)
    end_angle <- start_angle + theta
    
    generate_arc_points(c(arc['c1'], arc['c2']), arc['r'], start_angle, end_angle)
  } else {
    data.frame(x = numeric(0), y = numeric(0))  
  }
})

arc_points_list

arc_points_df <- bind_rows(arc_points_list)

arkos <- as.data.frame(hull$arcs)
arcs_df <- data.frame(
  x_start = coords$Dim.1[arkos$end1],
  y_start = coords$Dim.2[arkos$end1],
  x_end = coords$Dim.1[arkos$end2],
  y_end = coords$Dim.2[arkos$end2],
  group = seq_along(arkos$end1)
)

label_points <- coords %>%
  filter(grandine %in% c("8.12", "12.02", "12.03", 
                         "11.01", "8.11", "12.01",
                         "11.02", "8.10", "2.09",
                         "4.03", "1.13", "8.04",
                         "8.09", "2.11", "9.03",
                         "5.02", "10.10", "9.07", "9.11"))

alpha_hull_plot05 <- ggplot() +
  geom_polygon(data = arc_points_df, aes(x = x, y = y), color = "red", fill = NA) +
  geom_point(data = coords, aes(x = Dim.1, y = Dim.2), color = "black") +
  geom_text(data = label_points, aes(x = Dim.1, y = Dim.2, label = grandine), vjust = -0.5, hjust = 0.8, color = "darkgreen", size = 5) +
  theme_minimal() +
  labs(title = "Alpha-Hull plot | alpha=0.5", x ="PC1", y = "PC2")+
  theme_bw() +
  theme(
    panel.background = element_rect(fill = "white", color = "black"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "black"),
    axis.text = element_text(color = "black", size = 12),
    axis.title = element_text(color = "black", size = 20),
    legend.text = element_text(color = "black", size = 14),
    legend.title = element_text(color = "black", size = 14),
    plot.title = element_text(hjust = 0.5, size = 22)
  )

alpha_hull_plot05


# ALPHA = 0.75

hull <- ahull(coords$Dim.1, coords$Dim.2, 0.75)

generate_arc_points <- function(center, radius, start_angle, end_angle, n = 100) {
  seq_angles <- seq(start_angle, end_angle, length.out = n)
  x <- center[1] + radius * cos(seq_angles)
  y <- center[2] + radius * sin(seq_angles)
  data.frame(x = x, y = y)
}

arc_points_list <- lapply(1:nrow(hull$arcs), function(i) {
  arc <- hull$arcs[i, ]
  if (arc['r'] > 0) {  
    v_x <- arc['v.x']
    v_y <- arc['v.y']
    theta <- arc['theta']
    start_angle <- atan2(v_y, v_x)
    end_angle <- start_angle + theta
    
    generate_arc_points(c(arc['c1'], arc['c2']), arc['r'], start_angle, end_angle)
  } else {
    data.frame(x = numeric(0), y = numeric(0))  
  }
})

arc_points_df <- bind_rows(arc_points_list)

arkos <- as.data.frame(hull$arcs)
arcs_df <- data.frame(
  x_start = coords$Dim.1[arkos$end1],
  y_start = coords$Dim.2[arkos$end1],
  x_end = coords$Dim.1[arkos$end2],
  y_end = coords$Dim.2[arkos$end2],
  group = seq_along(arkos$end1)
)


label_points <- coords %>%
  filter(grandine %in% c("8.12", "12.02", "12.03", "11.01", "8.11", "12.01", "11.02", "8.10"))

alpha_hull_plot075 <- ggplot() +
  geom_polygon(data = arc_points_df, aes(x = x, y = y), color = "red", fill = NA) +
  geom_point(data = coords, aes(x = Dim.1, y = Dim.2), color = "black") +
  geom_text(data = label_points, aes(x = Dim.1, y = Dim.2, label = grandine), vjust = -0.5, hjust = 0.8, color = "darkgreen", size = 5) +
  theme_minimal() +
  labs(title = "Alpha-Hull plot | alpha=0.75", x ="PC1", y = "PC2")+
  theme_bw() +
  theme(
    panel.background = element_rect(fill = "white", color = "black"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "black"),
    axis.text = element_text(color = "black", size = 12),
    axis.title = element_text(color = "black", size = 20),
    legend.text = element_text(color = "black", size = 14),
    legend.title = element_text(color = "black", size = 14),
    plot.title = element_text(hjust = 0.5, size = 22)
  )

print(alpha_hull_plot075)

# ALPHA = 1
hull <- ahull(coords$Dim.1, coords$Dim.2, 1)


generate_arc_points <- function(center, radius, start_angle, end_angle, n = 100) {
  seq_angles <- seq(start_angle, end_angle, length.out = n)
  x <- center[1] + radius * cos(seq_angles)
  y <- center[2] + radius * sin(seq_angles)
  data.frame(x = x, y = y)
}

arc_points_list <- lapply(1:nrow(hull$arcs), function(i) {
  arc <- hull$arcs[i, ]
  if (arc['r'] > 0) {  
    v_x <- arc['v.x']
    v_y <- arc['v.y']
    theta <- arc['theta']
    start_angle <- atan2(v_y, v_x)
    end_angle <- start_angle + theta
    
    generate_arc_points(c(arc['c1'], arc['c2']), arc['r'], start_angle, end_angle)
  } else {
    data.frame(x = numeric(0), y = numeric(0))  
  }
})

arc_points_df <- bind_rows(arc_points_list)

arkos <- as.data.frame(hull$arcs)
arcs_df <- data.frame(
  x_start = coords$Dim.1[arkos$end1],
  y_start = coords$Dim.2[arkos$end1],
  x_end = coords$Dim.1[arkos$end2],
  y_end = coords$Dim.2[arkos$end2],
  group = seq_along(arkos$end1)
)



label_points <- coords %>%
  filter(grandine %in% c("8.12", "12.02", "12.03", "11.01", "8.11"))

alpha_hull_plot1 <- ggplot() +
  geom_polygon(data = arc_points_df, aes(x = x, y = y), color = "red", fill = NA) +
  geom_point(data = coords, aes(x = Dim.1, y = Dim.2), color = "black") +
  geom_text(data = label_points, aes(x = Dim.1, y = Dim.2, label = grandine), vjust = -0.5, hjust = 0.8, color = "darkgreen", size = 5) +
  theme_minimal() +
  labs(title = "Alpha-Hull plot | alpha=1", x ="PC1", y = "PC2")+
  theme_bw() +
  theme(
    panel.background = element_rect(fill = "white", color = "black"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "black"),
    axis.text = element_text(color = "black", size = 12),
    axis.title = element_text(color = "black", size = 20),
    legend.text = element_text(color = "black", size = 14),
    legend.title = element_text(color = "black", size = 14),
    plot.title = element_text(hjust = 0.5, size = 22)
  )

print(alpha_hull_plot1)

# ALPHA = 1.25

hull <- ahull(coords$Dim.1, coords$Dim.2, 1.25)


generate_arc_points <- function(center, radius, start_angle, end_angle, n = 100) {
  seq_angles <- seq(start_angle, end_angle, length.out = n)
  x <- center[1] + radius * cos(seq_angles)
  y <- center[2] + radius * sin(seq_angles)
  data.frame(x = x, y = y)
}

arc_points_list <- lapply(1:nrow(hull$arcs), function(i) {
  arc <- hull$arcs[i, ]
  if (arc['r'] > 0) {  
    v_x <- arc['v.x']
    v_y <- arc['v.y']
    theta <- arc['theta']
    start_angle <- atan2(v_y, v_x)
    end_angle <- start_angle + theta
    
    generate_arc_points(c(arc['c1'], arc['c2']), arc['r'], start_angle, end_angle)
  } else {
    data.frame(x = numeric(0), y = numeric(0))  
  }
})

arc_points_df <- bind_rows(arc_points_list)

arkos <- as.data.frame(hull$arcs)
arcs_df <- data.frame(
  x_start = coords$Dim.1[arkos$end1],
  y_start = coords$Dim.2[arkos$end1],
  x_end = coords$Dim.1[arkos$end2],
  y_end = coords$Dim.2[arkos$end2],
  group = seq_along(arkos$end1)
)


label_points <- coords %>%
  filter(grandine %in% c("8.12", "12.02", "12.03", "11.01", "8.11"))

alpha_hull_plot125 <- ggplot() +
  geom_polygon(data = arc_points_df, aes(x = x, y = y), color = "red", fill = NA) +
  geom_point(data = coords, aes(x = Dim.1, y = Dim.2), color = "black") +
  geom_text(data = label_points, aes(x = Dim.1, y = Dim.2, label = grandine), vjust = -0.5, hjust = 0.8, color = "darkgreen", size = 5) +
  theme_minimal() +
  labs(title = "Alpha-Hull plot | alpha=1.25", x ="PC1", y = "PC2")+
  theme_bw() +
  theme(
    panel.background = element_rect(fill = "white", color = "black"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "black"),
    axis.text = element_text(color = "black", size = 12),
    axis.title = element_text(color = "black", size = 20),
    legend.text = element_text(color = "black", size = 14),
    legend.title = element_text(color = "black", size = 14),
    plot.title = element_text(hjust = 0.5, size = 22)
  )

print(alpha_hull_plot125)


plot_grid <- (alpha_hull_plot05 | alpha_hull_plot075) / 
  (alpha_hull_plot1 | alpha_hull_plot125)

plot_grid

alpha_hull_plot05
alpha_hull_plot075
alpha_hull_plot1
alpha_hull_plot125


# Modeliu interpretacija -----------------------------------------------------
# iForest

String <- c("1.02", "1.08", "10.13", "11.12", "12.01", "12.02", "12.04", "12.08", "6.09", "7.10", "8.07", "8.08", "8.09", "8.10", "8.11")
Anomaly_Score <- c(0.005514, 0.002157, 0.077421, 0.000340, 0.267126, 0.083125, 0.033654, 0.003381, 0.011683, 0.002234, 0.000027, 0.019742, 0.024318, 0.043420, 0.157312)

iForest <- data.frame(String, Anomaly_Score)

min_max <- function(x) {
  return((x - min(x)) / (max(x) - min(x)))
}

iForest$Anomaly_Score <- min_max(iForest$Anomaly_Score)

print(iForest)

# LOF
String_lof <- c("1.02", "1.08", "10.13", "11.01", "11.02", "12.01", "12.02", "12.10", "6.01", "6.08", "7.03", "8.10", "8.11")
Anomaly_Score_lof <- c(1.523420, 1.316031, 1.740679, 1.393056, 1.307126, 2.955928, 2.086247, 1.453929, 1.390974, 1.322753, 1.342076, 1.431485, 2.049132)

LOF <- data.frame(String_lof, Anomaly_Score_lof)

min_max <- function(x) {
  return((x - min(x)) / (max(x) - min(x)))
}

LOF$Anomaly_Score_lof <- min_max(LOF$Anomaly_Score_lof)

print(LOF)

# ALPHA HULL 0.5

hull <- ahull(coords$Dim.1, coords$Dim.2, 0.5)
plot(hull)

hull_points <- hull$xahull  

hull_pointsdf <- as.data.frame(hull_points)
hull_pointsdf$stringType <- ""
coords
hull_pointsdf[1:(nrow(hull_pointsdf) - 6), ]$stringType <- coords$grandine 

useHullPoints <- hull_pointsdf[!(hull_pointsdf$stringType %in% c("8.12", "12.02", "12.03", 
                                                               "11.01", "8.11", "12.01",
                                                               "11.02", "8.10", "2.09",
                                                               "4.03", "1.13", "8.04",
                                                               "8.09", "2.11", "9.03",
                                                               "5.02", "10.10", "9.07", "9.11")), ]

centroid_x <- mean(useHullPoints[,1])
centroid_y <- mean(useHullPoints[,2])

centroid <- c(x = centroid_x, y = centroid_y)

print(centroid)

Outliers <- hull_pointsdf[(hull_pointsdf$stringType %in% c("8.12", "12.02", "12.03", 
                                                            "11.01", "8.11", "12.01",
                                                            "11.02", "8.10", "2.09",
                                                            "4.03", "1.13", "8.04",
                                                            "8.09", "2.11", "9.03",
                                                            "5.02", "10.10", "9.07", "9.11")), ]

Outliers$Distance <- sqrt((Outliers$c1 - centroid[1])^2 + (Outliers$c2 - centroid[2])^2)

min_max <- function(x) {
  return((x - min(x)) / (max(x) - min(x)))
}

Outliers$Anomaly_Score <- min_max(Outliers$Distance)

print(Outliers)

# ALPHA HULL 0.75

hull <- ahull(coords$Dim.1, coords$Dim.2, 0.75)
plot(hull)

hull_points <- hull$xahull  

hull_pointsdf <- as.data.frame(hull_points)
hull_pointsdf$stringType <- ""
coords
hull_pointsdf[1:(nrow(hull_pointsdf) - 1), ]$stringType <- coords$grandine 

useHullPoints <- hull_pointsdf[!(hull_pointsdf$stringType %in% c("8.12", "12.02", "12.03", 
                                                                 "11.01", "8.11", "12.01",
                                                                 "11.02", "8.10")), ]

centroid_x <- mean(useHullPoints[,1])
centroid_y <- mean(useHullPoints[,2])

centroid <- c(x = centroid_x, y = centroid_y)

print(centroid)

Outliers <- hull_pointsdf[(hull_pointsdf$stringType %in% c("8.12", "12.02", "12.03", 
                                                           "11.01", "8.11", "12.01",
                                                           "11.02", "8.10")), ]

Outliers$Distance <- sqrt((Outliers$c1 - centroid[1])^2 + (Outliers$c2 - centroid[2])^2)

min_max <- function(x) {
  return((x - min(x)) / (max(x) - min(x)))
}

Outliers$Anomaly_Score <- min_max(Outliers$Distance)

print(Outliers)

# ALPHA HULL 1

hull <- ahull(coords$Dim.1, coords$Dim.2, 1)
plot(hull)

hull_points <- hull$xahull  

hull_pointsdf <- as.data.frame(hull_points)
hull_pointsdf$stringType <- ""
coords
hull_pointsdf[1:(nrow(hull_pointsdf) - 1), ]$stringType <- coords$grandine 

useHullPoints <- hull_pointsdf[!(hull_pointsdf$stringType %in% c("8.12", "12.02", "12.03", 
                                                                 "11.01", "8.11")), ]

centroid_x <- mean(useHullPoints[,1])
centroid_y <- mean(useHullPoints[,2])

centroid <- c(x = centroid_x, y = centroid_y)

print(centroid)

Outliers <- hull_pointsdf[(hull_pointsdf$stringType %in% c("8.12", "12.02", "12.03", 
                                                           "11.01", "8.11")), ]

Outliers$Distance <- sqrt((Outliers$c1 - centroid[1])^2 + (Outliers$c2 - centroid[2])^2)

min_max <- function(x) {
  return((x - min(x)) / (max(x) - min(x)))
}

Outliers$Anomaly_Score <- min_max(Outliers$Distance)

print(Outliers)

# ALPHA HULL 1.25

hull <- ahull(coords$Dim.1, coords$Dim.2, 1.25)
plot(hull)

hull_points <- hull$xahull

hull_pointsdf <- as.data.frame(hull_points)
hull_pointsdf$stringType <- ""
coords
hull_pointsdf[1:(nrow(hull_pointsdf) - 2), ]$stringType <- coords$grandine 

useHullPoints <- hull_pointsdf[!(hull_pointsdf$stringType %in% c("8.12", "12.02", "12.03", 
                                                                 "11.01", "8.11")), ]

centroid_x <- mean(useHullPoints[,1])
centroid_y <- mean(useHullPoints[,2])

centroid <- c(x = centroid_x, y = centroid_y)

print(centroid)

Outliers <- hull_pointsdf[(hull_pointsdf$stringType %in% c("8.12", "12.02", "12.03", 
                                                           "11.01", "8.11")), ]

Outliers$Distance <- sqrt((Outliers$c1 - centroid[1])^2 + (Outliers$c2 - centroid[2])^2)

min_max <- function(x) {
  return((x - min(x)) / (max(x) - min(x)))
}

Outliers$Anomaly_Score <- min_max(Outliers$Distance)

print(Outliers)

# RANSAC  -----------------------------------------------------------------

ransac <- function(x, y, ntrial, threshout) {
  
  best_model <- NULL
  best_inliers <- NULL
  best_error <- Inf
  
  
  for (i in 1:ntrial) {
    VisiReziai <- list()
    inliers <- c()
    sample_indices <- sample(length(x), 2)
    x_sample <- x[sample_indices]
    y_sample <- y[sample_indices]
    
    model <- lm(y ~ x)
    
    y_predicted <- predict(model, newdata = data.frame(x = x))
    
    errors <- abs(y_predicted - y)
    errors2 <- (y_predicted - y)
    error <- sum(errors)
    inliers <- which(errors < threshout)
    
    
    if (length(inliers) > length(best_inliers) || (length(inliers) == length(best_inliers) && error < best_error)) {
      best_model <- model
      best_inliers <- inliers
      best_error <- error
      outliers <- which(errors2 > threshout)
    }
    
  }
  return(list(model = best_model, inliers = best_inliers, outliers = outliers,errors=errors))
}

klasterizavimas <- function(data,ransac_result, xlim1, xlim2, grandinesNr, optimalEps, optimalMinPoints){
  data$Timestamp <- Duomenys$Timestamp
  data$TimeDecimal <- as.numeric(format(data$Timestamp, "%H")) + as.numeric(format(data$Timestamp, "%M")) / 60  
  
  data$Month <- as.numeric(format(data$Timestamp, "%m"))
  
  dataLoaded <- data
  
  dataLoaded$Timestamp <- ymd_hms(dataLoaded$Timestamp)
  
  dataLoaded$Date <- as.Date(dataLoaded$Timestamp, format = "%Y-%m-%d")
  
  dataLoaded$NumericDate <- as.numeric(dataLoaded$Date - as.Date('1970-01-01'))
  dataLoaded
  
  outlier_filter <- rownames(dataLoaded) %in% ransac_result$outlier & dataLoaded$LyginamojiGrandine > 0
  
  outlier_data <- dataLoaded[outlier_filter, ]
  
  dbscan_data <- outlier_data[, c("TimeDecimal", "NumericDate")]
  dbscan_data$TimeDecimal <- (dbscan_data$TimeDecimal - min(dbscan_data$TimeDecimal)) / (max(dbscan_data$TimeDecimal) - min(dbscan_data$TimeDecimal))
  dbscan_data$NumericDate <- (dbscan_data$NumericDate - min(dbscan_data$NumericDate)) / (max(dbscan_data$NumericDate) - min(dbscan_data$NumericDate))
  
  eps_value <- optimalEps
  minPts_value <- optimalMinPoints
  dbscan_result <- dbscan(dbscan_data, eps = eps_value, minPts = minPts_value)
  print(unique(dbscan_result$cluster))
  dbscan_result$cluster[is.na(dbscan_result$cluster)]
  outlier_data$cluster <- dbscan_result$cluster
  outlier_data
  dataLoaded[outlier_filter, "cluster"] <- dbscan_result$cluster  
  dataLoaded
  dataLoaded$cluster <- as.factor(dataLoaded$cluster)
  filtered_data <- dataLoaded[rownames(dataLoaded) %in% ransac_result$outlier & dataLoaded$LyginamojiGrandine > 0,]
  
  unique_clusters <- levels(filtered_data$cluster)
  
  clusters_without_outlier <- unique_clusters[unique_clusters != "Outlier"]
  
  cluster_colors <- setNames(rainbow(length(clusters_without_outlier)), clusters_without_outlier)

  names(cluster_colors)[names(cluster_colors) == "0"] <- "noise"
  colors <- c(cluster_colors)
  
  q_dbscan <- ggplot(filtered_data, aes(x = TimeDecimal, y = Timestamp, color = cluster)) +
    geom_point() +
    scale_color_manual(values = colors) + 
    labs(
      x = "Hour",
      y = "Month",
      title = paste(grandinesNr, " Outliers separated from noise using DBSCAN | epsilon = ", optimalEps, ", MinPoints = ", optimalMinPoints , sep="")
    ) +
    theme_minimal()+
    theme(
      plot.title = element_text(hjust = 0.5, size = 16), 
      axis.text.x = element_text(size = 14), 
      axis.text.y = element_text(size = 14), 
      text = element_text(size = 14),
      panel.border = element_rect(colour = "black", fill = NA, size = 1)
    ) + 
    xlim(xlim1, xlim2) + 
    scale_y_datetime(breaks = date_breaks("2 months"), labels = date_format("%Y-%m"))
  
  dbRoad <- kNNdistplot(dbscan_data, k =  39) + abline(h = optimalEps, lty = 2, col = "green") + text(x = 150, y = optimalEps, labels = "Optimal Epsilon", col = "green", pos = 3)
  
  print(dbRoad)
  return(q_dbscan)
}


# 12.01, 12.02, 8.11, 10.13, and 8.10


# Non-faulty string -------------------------------------------------------

issiskirianciosGrandines <- c("4.03")
issiskirianciuGrandiniuInverteriai <- sub("\\..*", "", issiskirianciosGrandines)

for (grandinesNr in length(issiskirianciosGrandines)) {
  grandine <- issiskirianciosGrandines[grandinesNr]
  regex <- paste0("^", issiskirianciuGrandiniuInverteriai[grandinesNr], "\\.")
  cols_starting_with_nr <- grep(regex, colnames(NaujiDuomenysWh), value = TRUE)
  maxai <- apply(NaujiDuomenysWh[,cols_starting_with_nr], 1, max)
  data <- data.frame(
    LyginamojiGrandine = maxai,
    Grandine = NaujiDuomenysWh[[grandine]]
  )
  ransac_result = ransac(data$LyginamojiGrandine,data$Grandine,ntrial=500,threshout=250)
  p <- ggplot(data, aes(x = LyginamojiGrandine, y = Grandine)) +
    geom_point(color = "green") +
    geom_point(data = data[ransac_result$inliers, ], aes(x = LyginamojiGrandine, y = Grandine), color = "green") +
    geom_point(data = data[ransac_result$outliers, ], aes(x = LyginamojiGrandine, y = Grandine), color = "red")+
    geom_abline(intercept = coef(ransac_result$model)[1], slope = coef(ransac_result$model)[2], color = "blue") +
    labs(
      x = "Reference",
      y = grandine,
      title = "RANSAC Fit"
    ) +
    theme_minimal()+
    theme(
      plot.title = element_text(hjust = 0.5, size = 14), 
      axis.text.x = element_text(size = 12), 
      axis.text.y = element_text(size = 12), 
      text = element_text(size = 12) 
    )
  
  
  
  cat(grandine, "\n")
  data$Timestamp <- Duomenys$Timestamp
  data$TimeDecimal <- as.numeric(format(data$Timestamp, "%H")) + as.numeric(format(data$Timestamp, "%M")) / 60  
  data$Month <- as.numeric(format(data$Timestamp, "%m"))
  q <- ggplot(data[rownames(data) %in% ransac_result$outlier & data$LyginamojiGrandine > 0,],
              aes(y = Timestamp, x = TimeDecimal)) +
    geom_point(data = data[!((rownames(data) %in% ransac_result$outliers)) & data$LyginamojiGrandine > 0,], aes(y = Timestamp, x = TimeDecimal), color = 'green') +
    geom_point(color='red') +
    labs(
      x = "Hour",
      y = "Month",
      title = "Power generation points across months"
    ) +
    theme_minimal()+
    theme(
      plot.title = element_text(hjust = 0.5, size = 14),
      axis.text.x = element_text(size = 12), 
      axis.text.y = element_text(size = 12), 
      text = element_text(size = 12) 
    ) +
    xlim(min(data$TimeDecimal), max(data$TimeDecimal)) + 
    scale_y_datetime(breaks = date_breaks("2 months"), labels = date_format("%Y-%m"))
  
  qq <- klasterizavimas(data,ransac_result, min(data$TimeDecimal), max(data$TimeDecimal), grandine, 0.1, 60)
  combined_plot <- p + q + qq
  print(combined_plot)
}
qq

# Faulty Strings  -----------------------------------

issiskirianciosGrandines <- c("8.10", "8.11", "10.13", "12.02")
issiskirianciuGrandiniuInverteriai <- sub("\\..*", "", issiskirianciosGrandines)
ransac_list <- list()
for (grandinesNr in 1:length(issiskirianciosGrandines)) {
  
  grandine <- issiskirianciosGrandines[grandinesNr]
  regex <- paste0("^", issiskirianciuGrandiniuInverteriai[grandinesNr], "\\.")
  cols_starting_with_nr <- grep(regex, colnames(NaujiDuomenysWh), value = TRUE)
  maxai <- apply(NaujiDuomenysWh[,cols_starting_with_nr], 1, max)
  data <- data.frame(
    LyginamojiGrandine = maxai,
    Grandine = NaujiDuomenysWh[[grandine]]
  )
  ransac_result = ransac(data$LyginamojiGrandine,data$Grandine,ntrial=1000,threshout=250)
  ransac_list[[length(ransac_list) + 1]] <- ransac_result 
}

qqPlots <- list()

epsilonList <- c(0.09,0.07,0.12,0.055)
minPointsList <- c(100,100,53,60)

for (grandinesNr in 1:length(issiskirianciosGrandines)) {

  grandine <- issiskirianciosGrandines[grandinesNr]
  regex <- paste0("^", issiskirianciuGrandiniuInverteriai[grandinesNr], "\\.")
  cols_starting_with_nr <- grep(regex, colnames(NaujiDuomenysWh), value = TRUE)
  maxai <- apply(NaujiDuomenysWh[,cols_starting_with_nr], 1, max)
  data <- data.frame(
    LyginamojiGrandine = maxai,
    Grandine = NaujiDuomenysWh[[grandine]]
  )
  ransac_result = ransac_list[[grandinesNr]]
  
  data$Timestamp <- Duomenys$Timestamp
  data$TimeDecimal <- as.numeric(format(data$Timestamp, "%H")) + as.numeric(format(data$Timestamp, "%M")) / 60
  data$Month <- as.numeric(format(data$Timestamp, "%m"))  
  
  qq <- klasterizavimas(data,ransac_result, min(data$TimeDecimal), max(data$TimeDecimal), grandine, epsilonList[grandinesNr], minPointsList[grandinesNr])
  qqPlots[[length(qqPlots) + 1]] <- qq
}

plot_grid <- qqPlots[[1]]
for (i in 2:length(qqPlots)) {
  plot_grid <- plot_grid + qqPlots[[i]]
}

plot_grid <- plot_grid + plot_layout(ncol = 2)  

print(plot_grid)