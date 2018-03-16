##singular value decomposition on county wiki graph

countySparse <- Matrix(graphMatrix, sparse = TRUE) 
Dlcounty = Diagonal(nrow(countySparse), 1/sqrt(rowSums(countySparse)+10))
Drcounty = Diagonal(ncol(countySparse), 1/sqrt(colSums(countySparse)+10))
sCounty = svds(Dlcounty%*%countySparse%*%Drcounty, k = 10)
plot(sCounty$d[-1])
u = sCounty$u 

plot(as.data.frame(u[sample(nrow(countySparse),1000),]), pch = ".")
countyLinks[which(sCounty$u[,2] < 0),]
countyLinks[which(sCounty$u[,3] < -.02),]
