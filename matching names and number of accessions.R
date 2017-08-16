new <- yve
head(new)
new$AccessionName <- new$AccessionNumber
new$AccessionName <- lapply(yve$AccessionNumber, function(x) bencki$AccessionName[match(x, bencki$AccessionNumber)])
new <- new[,c(2,3,7,6,4,5)]
new <- data.frame(new)
head(new)
str(new)
new$AccessionName <- vapply(new$AccessionName, paste, collapse = ", ", character(1L))
write.csv(new, "melted_tomato.csv")

