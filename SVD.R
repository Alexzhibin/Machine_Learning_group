library(MASS)

a <- matrix(c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0,
              0, 0, 0, 0, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1), 9, 4)

a.svd <- svd(a)
a.svd$d

ds <- diag(1/a.svd$d[1:3])
u <- a.svd$u
v <- a.svd$v
us <- as.matrix(u[, 1:3])
vs <- as.matrix(v[, 1:3])

(a.ginv <- vs %*% ds %*% t(us))
t(a.ginv)

# using the function ginv defined in MASS
ginv(a)
######
svd(train[, 2:ncol(train)], nu=n.comp, nv=n.comp)
z <- svd(train[, 2:ncol(train)], nu=n.comp, nv=n.comp)
s <- diag(z$d[1:n.comp])
train[, 2:ncol(train)] <- z$u %*% s %*% t(z$v)

a.svd <- svd(a[,2:ncol(a)],nu=2,nv=2)
a.svd$u
s = diag(a.svd$d[1:2])
s
a.svd$u %*% s %*% t(a.svd$v) 

###Example1 PCA
library(foreign)
auto <- read.dta("http://statistics.ats.ucla.edu/stat/data/auto.dta")

pca.m1 <- prcomp(~trunk + weight + length + headroom, data = auto,
                 scale = TRUE)

screeplot(pca.m1)
# spectral decomposition: eigen values and eigen vectors
xvars <- with(auto, cbind(trunk, weight, length, headroom))
corr <- cor(xvars)
a <- eigen(corr)
(std <- sqrt(a$values))
(rotation <- a$vectors)


# svd approach
df <- nrow(xvars) - 1
zvars <- scale(xvars)
z.svd <- svd(zvars)
z.svd$d/sqrt(df)
z.svd$v

##Example2 Metric Multi-dimentsion scaling with SVD

cnut <- read.dta("http://statistics.ats.ucla.edu/stat/data/cerealnut.dta")
# centering the variables
#Subtract the column mean
mds.data <- as.matrix(sweep(cnut[, -1], 2, colMeans(cnut[, -1])))
dismat <- dist(mds.data)
mds.m1 <- cmdscale(dismat, k = 8, eig = TRUE)
mds.m1$eig
mds.m1 <- cmdscale(dismat, k = 2, eig = TRUE)
x <- mds.m1$points[, 1]
y <- mds.m1$points[, 2]
plot(x, y)
text(x + 20, y, label = cnut$brand)

# eigenvalues
xx <- svd(mds.data %*% t(mds.data))
xx$d
# coordinates
xxd <- xx$v %*% sqrt(diag(xx$d))
x1 <- xxd[, 1]
y1 <- xxd[, 2]

plot(x1, y1)
text(x1 + 20, y1, label = cnut$brand)

