#összegzés
sum=function(x)
{s=0
for(i in 1:length(x))
{s=s+x[i]}
s}

#átlag
emp=function(x)
{sum(x)/length(x)}

#korrigált empirikus szórásnégyzet
emp_sn=function(x)
{s=0
for(i in 1:length(x))
{s=s+(x[i]-emp(x))*(x[i]-emp(x))}
s/(length(x)-1)}

#kovariancia
m=function(x,y)
{s=0
for(i in 1:length(x)){
s=s+(x[i]-emp(x))*(y[i]-emp(y))}
s/(length(x)-1)}

#korrelációs együttható
r=function(x,y)
{m(x,y)/((sqrt(emp_sn(x))*sqrt(emp_sn(y))))}

#lineráis regressziós egyenlet a együtthatója (R kalap)
r_k=function(x,y)
{r(x,y)*(sqrt(emp_sn(y))/sqrt(emp_sn(x)))}
VAGY
a=function(x,y)
{m(x,y)/emp_sn(x)}

#lineráis regressziós egyenlet b együtthatója (R kalap)
b=function(x,y)
{emp(y)-emp(x)*(m(x,y)/emp_sn(x))}

#többváltozós lineáris regresszió
(solve(t(X)*X)*t(X))%*%y

#illeszkedésvizsgálat
k=c(83,91,122,107,74,123)
p=1/6
N=600
s=0
for(i in 1:length(k))
{s=s+((k[i]-(N*p))^2)/(N*p)}

#függetlenségvizsgálat
kiszamitott_ertek = function(X,i,j){
(sum(X[i,])*sum(X[,j]))/sum(X)
}
ÉS
chi_negyzet=function(X){
sum=0
for(i in 1:length(X[,1])){
for(j in 1:length(X[1,])){
sum=sum+((X[i,j]-kiszamitott_ertek(X,i,j))^2)/kiszamitott_ertek(X,i,j)}}
sum}

#exponenciális eloszlás lambda becslése
exp_lambda_becsles<-function(x){
1/emp(x)}
#exponenciláis eloszlás szórás becslése
exp_szoras_becsles<-function(x){
emp(x)}

#Poisson-eloszlás lambda becslése
poisson_lambda_becsles<-function(x){
emp(x)}
#Poisson-eloszlás szórás becslése
poisson_szoras_becsles<-function(x){
sqrt(emp(x))}



#weibull-loglikelihood függvény
set.seed(139)
xi=rweibull(1000,shape=1.7,scale=200)
weibull_loglik<-function(parm){
gamma<-parm[1]
lambda<-parm[2]
loglik<-sum(dweibull(vec, shape=gamma, scale=lambda, log=TRUE))
return(-loglik)}

#weibull-feladat
vec<-xi
weibull<-nlm(weibull_loglik, parm<-c(1,1), hessian=TRUE, iterlim=100)

#package installation
library(weibullness)
 
weibull.mle(xi)
weibull.mle(xi, threshold=0)

#cauchy-loglikelihood függvény
set.seed(139)
xi=rcauchy(500)*50+300
cauchy_loglik<-function(parm){
c<-parm[1]
s<-parm[2]
loglik<-sum(dcauchy(vec,location=c,scale=s,log=TRUE))
return(-loglik)}

#cauchy-feladat
vec<-xi
cauchy<-nlm(cauchy_loglik, parm<-c(median(xi),mad(xi)*qnorm(0.75)),hessian=TRUE,iterlim=1000)
cauchy$estimate

#cauchy robosztus megoldás
psi=function(x){
return(atan(x)/pi)}
T=median(xi)
s=mad(xi)
eps=1e-9
s2=mad(xi)*qnorm(0.75)
i=0
while(abs(s-s2)>eps){
s=s2
T=T+s*mean(psi((xi-T)/s))
s2=s*sqrt(12*mean(psi((xi-T)/s)^2))
i=i+1}
print(i); print(T); print(s);

#weibull robosztus megoldás
set.seed(139)
xi=rweibull(1000, shape=1.7, scale=200)
eta=log(xi)
psi=function(x){
return(0.5-exp(-exp(x)))}
T=median(eta)
s=mad(eta)
eps=1e-9
s2=mad(eta)*qnorm(0.75)
i=0
while(abs(s-s2)>eps){
s=s2
T=T+s*mean(psi((eta-T)/s))
s2=s*sqrt(12*mean(psi((eta-T)/s)^2))
i=i+1}
print(exp(T)); print(1/s);

#ferdeség -> a ferdeség megmutatja, hogy az eloszlás csúcsának középhelyzetéhez képest mennyire tolódik el; pozitív érték esetén jobbra tolódó eloszlás, míg negatív értéke esetén balra tolódó eloszlás; normális eloszlás esetén 0, vagy 0-hoz közeli érték
#a szimmetria mértéke
#szimmetrikus=0
skew<-function(x){
m3<-sum((x-mean(x))^3)/length(x)
s3<-sqrt(var(x))^3
m3/s3}
skew(x)

library(moments)
skewness(x)

#csúcsosság -> pozitív érték esetén csúcsosabb, negatív érték esetén laposabb a normális eloszláshoz képest
kurt<-function(x){
m4<-sum((x-mean(x))^4)/length(x)
s4<-var(x)^2
m4/s4}
kurt(x)

library(moments)
kurtosis(x)

#statisztikai adatok: átlag (mean) -> számtani közép; maximum (max) -> legnagyobb elem; minimum (min) -> legkisebb elem; medián (med) -> adatsor középső értéke, 1st quart (1. kvartilis) -> az értéktől a 25%-a kisebb, többi nagyobb; 3rd quart (3. kvartilis) -> az értéktől a 75%-a nagyobb, a többi kisebb
library(moments)
summary(x)

VIZSGA FELADAT

x="b8vnq7";#neptun kód
z=charToRaw(iconv(x, "latin1", "UTF-8"))
for (i in 1:6) v=paste("0x",z,sep="")
e=strtoi(v)
ax=e[1];ay=e[2];az=e[3];av=e[4];ss=sum(strtoi(v))+24
cat("ax =",ax,"\n")
cat("ay =",ay,"\n")
cat("az =",az,"\n")
cat("av =",av,"\n")
cat("ss =",ss,"\n")
ar=c("FB","AAPL","AMZN","GOOG","NFLX","TSLA")
ai=ss-6*floor(ss/6)
ev=2020-(ss-10*floor(ss/10))
cat("ev =",ev,"\n")
cat("reszveny =",ar[ai+1],"\n")

#mintarealizáció generálás
set.seed(ss)
nx=1000
v=matrix(c(ax,abs(ax-az),abs(ax-az),az),2)
w=chol(v)
z1=rexp(nx)
z2=rexp(nx)
zm=matrix(c(z1,z2),ncol=2)
zn=zm%*%w

library(moments)
#statisztikai elemzés
summary(zn)

#csúcsosság
kurtosis(zn) #beépített függvénnyel

#saját, létrehozott függvénnyel
kurt<-function(x){
m4<-sum((x-mean(x))^4)/length(x)
s4<-var(x)^2
m4/s4}
kurt(zn[,1])
kurt(zn[,2])

#ferdeség
skewness(zn) #beépített függvénnyel

#saját, létrehozott függvénnyel
skew<-function(x){
m3<-sum((x-mean(x))^3)/length(x)
s3<-sqrt(var(x))^3
m3/s3}
skew(zn[,1])
skew(zn[,2])

#eloszlás vizsgálata
library(ggpubr)
ggdensity(zn[,1], main="Sűrűségfüggvény", xlab="Értékek", ylab="Sűrűség") #megrajzolja a sűrűségfüggvényt
ggdensity(zn[,2], main="Sűrűségfüggvény", xlab="Értékek", ylab="Sűrűség") #megrajzolja a sűrűségfüggvényt

#hisztogram és rárajzolja a sűrűségfüggvényt
hist(zn)
lines(density(zn))

qqPlot(zn) #megrajzolja az adott összefüggést a mintánk, valamint a normális eloszlás között 45°-os referenciavonalon

#peremek függetlenségének vizsgálata
#ha van kapcsolat -> függenek egymástól; ha nincs kapcsolat -> függetlenek egymástól
#feltétel: minimum 5 elem
#szignifikáns, ha 0
chisq.test(abs(zn)) #0-hoz közel van, tehát szignifikáns

#perspektivikus ábrázolás
persp(zn, col="blue", shade=0.4, main="Perspektivikus ábrázolás")

#szórásdiagramból ábramátrix
pairs(zn)

#hisztogram ábrázolás
hist(zn)

#brown értékek generálása
brown_generalas <- function(szimulaciok_szama, t, mu = ax, sigma = (ax+az)/(ax+ay+az), S0, dt = 1./365) {
  gbm <- matrix(ncol = szimulaciok_szama, nrow = t)
  for (szimulacio in 1:szimulaciok_szama) {
    gbm[1, szimulacio] <- S0
    for (day in 2:t) {
      epsilon <- rnorm(1)
      dt = 1 / 365
      gbm[day, szimulacio] <- gbm[(day-1), szimulacio] * exp((mu - sigma * sigma / 2) * dt + sigma * epsilon * sqrt(dt))
    }
  }
  return(gbm)
}
 
szimulaciok_szama <- 20
t <- 500
mu <- ax
sigma <- (ax+az)/(ax+ay+az)
S0 <- 500
set.seed(ss+37)
gbm <- gbm_ciklus(szimulaciok_szama, t, mu, sigma, S0)
plot(gbm, type="l")
summary(gbm)

#brown folyamat ábrázolása

brown <- function(n.times){
	set.seed(ss+37)
	x <- y <- x.new <- y.new <- x.new.p <- y.new.p <- vector()
	for(i in 1:n.times){
		x <- rnorm(1, ax, (ax+az)/(ax+ay+az))
		y <- rnorm(1, ax, (ax+az)/(ax+ay+az))
    		x.new <- c(x.new, x)
    		y.new <- c(y.new, y)
		x.new.p <- cumsum(x.new)
		y.new.p <- cumsum(y.new)
		plot(x.new.p, y.new.p, type="b", main=paste("Generált Brown-folyamat\nIdőegység = ", i,sep=""),
		xlab="x koordinatak", ylab="y koordinatak", col=c(rep("blue", i-1), "red"), pch=c(rep(20,i-1),1))
    }
}

brown(100)

#brown
library(somebm)
b<-gbm(x0 = 1, mu = ax, sigma = (ax+az)/(ax+az+ay), t0 = 0, t = 1, n = 100)
plot(b)
summary(b)

#poisson folyamat generálása

poisson <- function(){
set.seed(ss+17)
x <- y <- x.new <- y.new <- x.new.p <- y.new.p <- vector()
for(i in 1:500){
x <- rpois(1, (ax+az)/(ax+ay+az))
y <- rpois(1, (ax+az)/(ax+ay+az))
    x.new <- c(x.new, x)
    y.new <- c(y.new, y)
x.new.p <- cumsum(x.new)
y.new.p <- cumsum(y.new)
plot(x.new.p, y.new.p, type="b", main=paste("Generált Poisson-folyamat\nIdőegység = ", i,sep=""),
xlab="x koordináták", ylab="y koordináták", col=c(rep("blue", i-1), "red"), pch=c(rep(20,i-1),1))
}
    poisson_g <- matrix(c(x.new.p,y.new.p), ncol = 2)
return(poisson_g)
}

poisson_generalt <- poisson()

summary(poisson_generalt)

#részvény feladat
AMZ_details<-read.csv("C:/Users/Szeli Márk/Downloads/NFLX.csv")
zaro=AMZ_details$Close
zaro
logreturn=c()
for(i in 1:(length(zaro)-1)){
logreturn[i]=abs(log(zaro[i+1]/zaro[i]))}
chisq.test(logreturn)
hist(logreturn, main="Záró árak változásának hisztogramja")
plot(logreturn, main="Záró árak változásának pontdiagramja")