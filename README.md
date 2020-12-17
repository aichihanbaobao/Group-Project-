# Group-Project

# Loop read

```
# for data3.csv: the 300 run simulation data are in /all_output/output, and associated p values are in /all_output/output1
# for data4.csv: the 540 run simulation data are in /all_output/output4, and associated p values are in /all_output/output5. the loop below will only produce data3.csv, and data4.csv is also provided in the zip file.

rm(list = ls())

# this is a read loop example which will produce a csv identical to data3.csv, which is provided. change the file path to the location of the /all_output/output folder

file_path <- dir("~/all_output/output", pattern = "*.csv$", recursive = T)
td <- function(x){
  data <- read.csv(file.path("~/all_output/output", x))
  return(data)
}

file_path1 <- dir("~/all_output/output1", pattern = "*.csv$", recursive = T)
td1 <- function(x){
  data <- read.csv(file.path("~/all_output/output1", x))
  return(data)
}

file_path=file_path[order(nchar(file_path), file_path)]
file_path1=file_path1[order(nchar(file_path1), file_path1)]

#loop read
data <- lapply(file_path, td)
data1 <- lapply(file_path1, td1)

data_df <- do.call(rbind, data)
data_df <- data.frame(data_df)

data_df1 <- do.call(rbind,data1)
data_df1 <- data.frame(data_df1)

x1=data.frame()
for (i in 1:nrow(data_df1)){
  x2=data.frame()
  x2[1:200,1] <- i
  x2[1:200,2:3] <- data_df1[i,2:3]
  x1 <- rbind(x1,x2)
}

data3 <- cbind(data_df,x1)

# change the colnames 
colnames(data3)=c("frames","trees","fires","empty","run","growp", "lightp")

  write.csv(data3, "data3.csv")
```

# Figure1

```
setwd("~") # set to filepath location of "data4.csv"
# load in required package
library("tidyverse")
library("viridis")
library("gifski")

# read in data
data4 <- read.csv("data4.csv")

# convert growp to a factor
data4$growp <- as.factor(data4$growp)
# subset
high <- filter(data4, growp == 0.05 & lightp >= 0.01)
low <- filter(data4, growp == 0.05 & lightp <= 0.01)

# produce the plot using ggplot
ggplot() +
	geom_line(data=low, aes(x = frames, y = trees, group = run), color = "black") +
	geom_line(data=high, aes(x = frames, y = trees, group = run), color = "blue") +
	labs(x = "Frame number", y = "Number of trees", color = "Lightning\nprobability") +
	theme(plot.background = element_rect(fill = "white"), panel.background = element_rect(fill = "gray90"), panel.grid.major = element_line(color = "gray30", size = 0.25), panel.grid.minor = element_line(color = "gray30", size = 0.25), legend.background = element_rect(fill = "gray90"))

ggsave(plot = last_plot(), "example_from_simulation.png")
```

# Figure2

```
# read in data
data3 <- read.csv("data3.csv")

# convert run and growp to factors to visualise
data3$run <- as.factor(data3$run)
data3$growp <- as.factor(data3$growp)

# depth error message can be circumvented by running the ggplot code twice
gg1 <- ggplot() + 
	geom_line(data=data3, aes(x = frames, y = trees, color = growp, group = run)) + 
	scale_color_manual(values = c("#FDE725", "#95D840", "#3CBB75", "#1F968B", "#2D708E", "#39568C", "#453781", "#482677", "#481567", "#440154"), breaks = c(0.01, 0.02, 0.03, 0.04, 0.05, 0.06, 0.07, 0.08, 0.09, 0.1)) + 
	labs(x = "Frame number", y = "Number of trees", color = "Growth\nprobability") + 	theme(plot.background = element_rect(fill = "white"), panel.background = element_rect(fill = "gray90"), panel.grid.major = element_line(color = "gray30", size = 0.25), panel.grid.minor = element_line(color = "gray30", size = 0.25), legend.background = element_rect(fill = "gray90"))

# save plot as a png
ggsave(filename = "trees_discrete_300runs.png", plot = gg1)
```

# Figure3

```
# depth error message can be circumvented by running the ggplot code twice
ggplot() +
	geom_smooth(data=data3, aes(x = frames, y = trees, color = growp)) +
	scale_color_manual(values = c("#FDE725", "#95D840", "#3CBB75", "#1F968B", "#2D708E", "#39568C", "#453781", "#482677", "#481567", "#440154"), breaks = c(0.01, 0.02, 0.03, 0.04, 0.05, 0.06, 0.07, 0.08, 0.09, 0.1)) +
	labs(x = "Frame number", y = "Number of trees", color = "Growth\nprobability") +
	theme(plot.background = element_rect(fill = "white"), panel.background = element_rect(fill = "white"), panel.grid.major = element_line(color = "gray30", size = 0.25), panel.grid.minor = element_line(color = "gray30", size = 0.25), legend.background = element_rect(fill = "white"))

# save plot as png
ggsave(filename = "method: gam: data3.png", plot = last_plot())
```

# Figure4

```
growp_list <- c(0.01,0.02,0.03,0.04,0.05,0.06,0.07,0.08,0.09,0.1)
gg_files <- c()

for (i in growp_list){
  x0 <- filter(data3, growp == i)
  x <- ggplot() + 
  geom_line(data=x0, aes(x = frames, y = trees, color = lightp, group = run)) + 
  scale_color_gradient2(low = "#440154", mid = "#29AF7F", high = "#FDE725", midpoint = 0.005, limits = c(0.0, 0.01), breaks = c(0, 0.002, 0.004, 0.006, 0.008, 0.01)) + 
  ylim(0, 22500) + 
  labs(x = "Frame number", y = "Number of trees", title = paste0('Growth probability = ',i), color = "Lightning\nprobability") + 
  theme(plot.background = element_rect(fill = "white"), panel.background = element_rect(fill = "gray90"), panel.grid.major = element_line(color = "gray30", size = 0.25), panel.grid.minor = element_line(color = "gray30", size = 0.25), plot.title = element_text(hjust = 0.5), legend.background = element_rect(fill = "gray90"))

ggsave(paste0('gg',i,'.png'), plot = x, width = 9, height = 6, units = c("in"))
gg_files <- c(gg_files,paste0('gg',i,'.png'))
}


gifski(png_files = gg_files, gif_file = "final.gif", delay = 1, loop = TRUE, width = 864, height = 576)
```


