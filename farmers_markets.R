library(stringdist)

# Load --------------------------------------------------------------------

lms <- read.csv("data/LA_LMS_Farmers_Markets/Farmers_Markets.csv", stringsAsFactors = FALSE)
calfresh <- read.csv("data/LA County Open Data Farmers Markets/CalFresh___SNAP_Authorized_Farmers_Markets.csv", stringsAsFactors = FALSE)
names(calfresh) <- c("Name", "Address", "Landmark")


# Levenschtein Distance ---------------------------------------------------

l <- data.frame(id1 = double(), name1 = character(), 
                id2 = double(), name2 = character(), levenshtein = double(), stringsAsFactors = FALSE)

for (i in seq_along(calfresh$Name)){
  min <- 100000
  entry <- 0
  for (j in seq_along(lms$Name)){
    dist <- stringdist(calfresh$Name[[i]], lms$Name[[j]])
    if (is.numeric(dist)) {
      if (dist < min) {
        min <- dist
        entry <- j
      }
    }
  }
  print(paste0("calfresh ", i, " - ", calfresh$Name[[i]]))
  print(paste0("lms ", entry, " - ", lms$Name[[entry]]))
  print(paste0("distance: ", min))
  print("")
  l[nrow(l) + 1, ] <- c(i, calfresh$Name[[i]], entry, lms$Name[[entry]], min)
}

l$id1 <- as.numeric(l$id1)
l$id2 <- as.numeric(l$id2)
l$levenshtein <- as.numeric(l$levenshtein)

sorted <- l[order(l$levenshtein),]

export_name <- "markets_dist.csv"
write.csv(sorted, file = paste0("export/", export_name), row.names = FALSE)
