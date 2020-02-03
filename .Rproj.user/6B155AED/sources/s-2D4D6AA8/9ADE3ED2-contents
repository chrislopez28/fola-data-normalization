library(readxl)
library(geosphere)
library(stringdist)


# Load --------------------------------------------------------------------

file1 <- "data/FoodFinders_March2019/FoodFinders_March2019-Website-List_Updated_with_XY.xlsx"
file2 <- "data/LA_LMS_food_assistance/Food_Assistance.csv"

ff <- readxl::read_excel(file1)
lms <- read.csv(file2, stringsAsFactors = FALSE)
names(ff)[2] <- "Name"
names(lms)[1] <- "X"

distHaversine(c(ff$X[1], ff$Y[1]), c(lms$X[1], lms$Y[1]))

t <- data.frame(id_foodfinders = double(), name1 = character(), 
                id_lms = double(), name2 = character(), dist = double(), stringsAsFactors = FALSE)

for (i in seq_along(ff$X)){
  min <- 100000000
  entry <- 0
  for (j in seq_along(lms$X)){
    dist <- distHaversine(c(ff$X[i], ff$Y[i]), c(lms$X[j], lms$Y[j]))
    if (is.numeric(dist)) {
      if (dist < min) {
        min <- dist
        entry <- j
      }
    }
  }
  print(paste0("Food Finders ", i, " - ", ff$Name[[i]]))
  print(paste0("lms ", entry, " - ", lms$Name[[entry]]))
  print(paste0("distance: ", min, "m"))
  print("")
  t[nrow(t) + 1, ] <- c(i, ff$Name[[i]], entry, lms$Name[[entry]], min)
}

t$id_foodfinders <- as.numeric(t$id_foodfinders)
t$id_lms <- as.numeric(t$id_lms)
t$dist <- as.numeric(t$dist)

sorted <- t[order(t$dist),]

export_name <- "pantries_dist.csv"
write.csv(sorted, file = paste0("export/", export_name), row.names = FALSE)

