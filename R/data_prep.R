source("R/packages.R")

url <- "https://docs.google.com/spreadsheets/d/1E4ROaW30dj4Hig3KH8qy4p1QUuXAAUUGrZ3voGUu3hM/edit?usp=sharing"

# Load curator data
curators <- read_sheet(url)

write_csv(curators, "data/curators.csv")
