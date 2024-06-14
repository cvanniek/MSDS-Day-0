library(ggplot2)

library(readxl)
survey <- read_excel("C:/Users/Chris/Downloads/MSDS-Orientation-Computer-Survey (1).xlsx")
View(survey)

# Histogram for Number of CPU Cores
ggplot(survey, aes(x = `CPU Number of Cores (int)`)) +
  geom_histogram(binwidth = 1, fill = "skyblue", color = "black") +
  labs(title = "Distribution of CPU Number of Cores", x = "Number of CPU Cores", y = "Frequency")
