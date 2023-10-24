# Lien de l'outil RTools à télécharger en premier:
# https://cran.r-project.org/bin/windows/Rtools/rtools42/rtools.html

# Package à installer ensuite :
install.packages("HiCseg")

# Lien de la doc :
# https://cran.r-project.org/web/packages/HiCseg/HiCseg.pdf

library(HiCseg)
data(matrix)

n=200 # Size of the data matrix
Kmax=3 # Maximal number of change-points

hist(matrix,col='orange',breaks=15,freq=F, main='Distribution des observations')#,border='blue')
densite = density(matrix)
lines(densite, col='blue', lwd=2)
x=seq(from=-4, to=4, length=100)
lines(x,dnorm(x, mean(matrix, na.rm=TRUE),sd(matrix, na.rm=TRUE)),col='red',lwd=2)
pdf("hist.pdf")


res = HiCseg_linkC_R(n, Kmax, "G", matrix, "D")
print(res)

mean(matrix[160:200,160:200])





res$J # provides the values of the log-likelihood

res$t_hat # gives the list of the K change-points

# ”G”because we decided to model the observations as reaslizaions of Gaussian random variables.
# https://cran.r-project.org/web/packages/HiCseg/vignettes/HiCseg.pdf

image(1:n,1:n,matrix,xlab="",ylab="", main='Segmentation des blocs pour Kmax=4')
t_hat=c(1,res$t_hat[res$t_hat!=0]+1) 
for (i in 1:(length(t_hat)-1))
{
  lines(c(t_hat[i],t_hat[i]),c(t_hat[i],(t_hat[(i+1)]-1)))
  lines(c(t_hat[(i+1)]-1,t_hat[(i+1)]-1),c(t_hat[i],t_hat[(i+1)]-1))
  lines(c(t_hat[i],t_hat[(i+1)]-1),c(t_hat[i],t_hat[i]))
  lines(c(t_hat[i],t_hat[(i+1)]-1),c(t_hat[(i+1)]-1,t_hat[(i+1)]-1)) }

### essais matrix2

matrix2 = matrix[1:160,1:160]*1000000000
matrix2 = matrix2 - mean(matrix2)
hist(matrix2)

n2=160 # Size of the data matrix
Kmax=15 # Maximal number of change-points

hist(matrix2,col='orange',breaks=15,freq=F, main='Distribution des observations')#,border='blue')
densite = density(matrix2)
lines(densite, col='blue', lwd=2)
x=seq(from=-40, to=40, length=100)
lines(x,dnorm(x, mean(matrix2, na.rm=TRUE),sd(matrix2, na.rm=TRUE)),col='red',lwd=2)
pdf("hist.pdf")

res2 = HiCseg_linkC_R(n2, Kmax, "G", matrix2, "D")
print(res2)

image(1:n2,1:n2,matrix2,xlab="",ylab="", main='Segmentation des blocs pour n=160')
t_hat2=c(1,res2$t_hat[res2$t_hat!=0]+1) 
for (i in 1:(length(t_hat2)-1))
{
  lines(c(t_hat2[i],t_hat2[i]),c(t_hat2[i],(t_hat2[(i+1)]-1)))
  lines(c(t_hat2[(i+1)]-1,t_hat2[(i+1)]-1),c(t_hat2[i],t_hat2[(i+1)]-1))
  lines(c(t_hat2[i],t_hat2[(i+1)]-1),c(t_hat2[i],t_hat2[i]))
  lines(c(t_hat2[i],t_hat2[(i+1)]-1),c(t_hat2[(i+1)]-1,t_hat2[(i+1)]-1)) }



