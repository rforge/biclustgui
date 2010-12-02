parallelCoordinates2 <-
function (x,bicResult,number, info=TRUE) {

    bicRows = which(bicResult@RowxNumber[, number])
    bicCols = which(bicResult@NumberxCol[number, ])
    NotbicCols = which(bicResult@NumberxCol[ number,]== F)

    mat <- x [bicRows, ]
    mat2 <- cbind(mat[,bicCols], mat[,NotbicCols ])
    colname <- c(bicCols,NotbicCols )

    plot(mat2[1,], type="l", col="white", ylim=c(min(mat), max(mat)), axes = F, xlab="columns", ylab="value")
    axis(1, 1:ncol(mat2) , labels = colname  )
    axis(2)

    if (length(bicCols) != ncol(mat2) ) {
      for (i in 1: nrow(mat2) ) 
        {
        pos <- (length(bicCols)+1) : ncol(mat2)
        lines(pos , mat2[i,pos ],type="l")
        }
     }

   for (i in 1: nrow(mat2) ) lines(mat2[i,c(1:length(bicCols)) ], type="l", col="red")

   if (info) {
        title(main = paste("Bicluster", number, "\n(rows=", length(bicRows), 
            ";", "columns=", length(bicCols), ")", sep = " "))
    }

 }

