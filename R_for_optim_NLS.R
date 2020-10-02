install.packages("dplyr")
install.packages("plyr")
library(dplyr)
library(plyr)

monthi <- substr(file_for_R$'Week_start', 6, 7)
myDf <- as.data.frame(monthi)



result_1 <- merge(myDf, file_for_R, by='row.names', all.x=TRUE)
result_1 <- result_1[!(result_1$units_of_commodity<=30),]

d1 <- rep(0, length(result_1$`Week_start`))
d2 <- rep(0, length(result_1$`Week_start`))
d3 <- rep(0, length(result_1$`Week_start`))
d4 <- rep(0, length(result_1$`Week_start`))
d5 <- rep(0, length(result_1$`Week_start`))
d6 <- rep(0, length(result_1$`Week_start`))
d7 <- rep(0, length(result_1$`Week_start`))
d8 <- rep(0, length(result_1$`Week_start`))
d9 <- rep(0, length(result_1$`Week_start`))
d10 <- rep(0, length(result_1$`Week_start`))
d11 <- rep(0, length(result_1$`Week_start`))
d12 <- rep(0, length(result_1$`Week_start`))

for (i in 1:length(result_1$`Week_start`)){ 
  if (result_1$monthi[i] == '01') {
    result_1$d1[i] <- 1
  } else {
    result_1$d1[i] <- 0
  }
  if (result_1$monthi[i] == '02') {
    result_1$d2[i] <- 1
  } else {
    result_1$d2[i] <- 0
  }
  if (result_1$monthi[i] == '03') {
    result_1$d3[i] <- 1
  } else {
    result_1$d3[i] <- 0
  }
  if (result_1$monthi[i] == '04') {
    result_1$d4[i] <- 1
  } else {
    result_1$d4[i] <- 0
  }
  if (result_1$monthi[i] == '05') {
    result_1$d5[i] <- 1
  } else {
    result_1$d5[i] <- 0
  }
  if (result_1$monthi[i] == '06') {
    result_1$d6[i] <- 1
  } else {
    result_1$d6[i] <- 0
  }
  if (result_1$monthi[i] == '07') {
    result_1$d7[i] <- 1
  } else {
    result_1$d7[i] <- 0
  }
  if (result_1$monthi[i] == '08') {
    result_1$d8[i] <- 1
  } else {
    result_1$d8[i] <- 0
  }
  if (result_1$monthi[i] == '09') {
    result_1$d9[i] <- 1
  } else {
    result_1$d9[i] <- 0
  }
  if (result_1$monthi[i] == '10') {
    result_1$d10[i] <- 1
  } else {
    result_1$d10[i] <- 0
  }
  if (result_1$monthi[i] == '11') {
    result_1$d11[i] <- 1
  } else {
    result_1$d11[i] <- 0
  }
  if (result_1$monthi[i] == '12') {
    result_1$d12[i] <- 1
  } else {
    result_1$d12[i] <- 0
  }
}

x <- c(
  result_1$units_of_commodity
)
y <- c(
  result_1$TV_resume_per1
)

i

boxplot(y)
maximum <- max(y, na.rm=TRUE)
boxplot.stats(y)$out
ind <- which(y %in% boxplot.stats(y)$out)
outler <- data.frame(x=x[ind], y=y[ind])
plot(x,y,col='blue', pch=16, ylim=c(0,maximum))
points(outler$x, outler$y, col='red',pch=16)

     
########################################################################                     
x <- c(
  result_1$units_of_commodity
)
y <- c(
  result_1$TV_resume_per1
)
x_may <-rep(0, length(result_1$d5))
y_may <-rep(0, length(result_1$d5))

for (i in 1:length(result_1$`d5`)){
  if (result_1$`d5`[i] == 1) {
    x_may[i] <- c(
      x[i], x_may[i]
    )
    y_may[i] <- c(
      y[i], y_may[i]
    )
    boxplot(y_may)
    maximum <- max(y_may, na.rm=TRUE)
    boxplot.stats(y_may)$out
    ind <- which(y %in% boxplot.stats(y)$out)
    outler <- data.frame(x_may=x_may[ind], y_may=y[ind])
    plot(x_may,y_may,col='blue', pch=16)
    points(outler$x_may, outler$y_may, col='red',pch=16)
  } #else {
  #result_1$d7[i] <- 0
}

for_plot_y <- as.data.frame(y_may)
for_plot_x <- as.data.frame(x_may)
for_plot <- merge(for_plot_y, for_plot_x, by="row.names", all.x=TRUE)
lines(lowess(for_plot$x_may ~ for_plot$y_may))

plot(for_plot$x, for_plot$y, main="May")


x<-x_may
y<-y_may
#for simple models nls find good starting values for the parameters even if it throw a warning
#m<-nls(y~b*d1*exp(1)**(result_1$'Avito units_of_commodity'))
#get some estimation of goodness of fit
#cor(y,predict(m))
##########################################################################
#simulate some data
result_1 <- na.omit(result_1)
x<-result_1$units_of_commodity
y<-result_1$TV_resume_per1
#for simple models nls find good starting values for the parameters even if it throw a warning

units_of_commodity <- as.data.frame(result_1$units_of_commodity)


b1 <- as.vector(rep(0, length(x)))
a1 <- as.vector(rep(0, length(x)))
b2 <- as.vector(rep(0, length(x)))
a2 <- as.vector(rep(0, length(x)))
b3 <- as.vector(rep(0, length(x)))
a3 <- as.vector(rep(0, length(x)))
b4 <- as.vector(rep(0, length(x)))
a4 <- as.vector(rep(0, length(x)))
b5 <- as.vector(rep(0, length(x)))
a5 <- as.vector(rep(0, length(x)))
b6 <- as.vector(rep(0, length(x)))
a6 <- as.vector(rep(0, length(x)))
b7 <- as.vector(rep(0, length(x)))
a7 <- as.vector(rep(0, length(x)))
b8 <- as.vector(rep(0, length(x)))
a8 <- as.vector(rep(0, length(x)))
b9 <- as.vector(rep(0, length(x)))
a9 <- as.vector(rep(0, length(x)))
b10 <- as.vector(rep(0, length(x)))
a10 <- as.vector(rep(0, length(x)))
b11 <- as.vector(rep(0, length(x)))
a11 <- as.vector(rep(0, length(x)))
b12 <- as.vector(rep(0, length(x)))
a12 <- as.vector(rep(0, length(x)))

d1 <- as.vector(result_1$d1)
d2 <- as.vector(result_1$d2)
d3 <- as.vector(result_1$d3)
d4 <- as.vector(result_1$d4)
d5 <- as.vector(result_1$d5)
d6 <- as.vector(result_1$d6)
d7 <- as.vector(result_1$d7)
d8 <- as.vector(result_1$d8)
d9 <- as.vector(result_1$d9)
d10 <- as.vector(result_1$d10)
d11 <- as.vector(result_1$d11)
d12 <- as.vector(result_1$d12)

y <- as.vector(result_1$TV_resume_per1)
x <- as.vector(result_1$units_of_commodity)

#Total for all months with plot and coefficient table

model <- nls(y ~ d1*b1*exp(a1*x*d1)+d2*b2*exp(a2*x*d2)+d3*b3*exp(a3*x*d3)+d4*b4*exp(a4*x*d4)+d5*b5*exp(a5*x*d5)+d6*b6*exp(a6*x*d6)+d7*b7*exp(a7*x*d7)+d8*b8*exp(a8*x*d8)+d8*b8*exp(a8*x*d8)+d9*b9*exp(a9*x*d9)+d10*b10*exp(a10*x*d10)+d11*b11*exp(a11*x*d11)+d12*b12*exp(a12*x*d12), start=list(b1= 326.375033, a1=-0.0105241, b2= 326.375033, a2= -0.0105241, b3= 326.375033, a3= -0.0105241, b4= 326.375033, a4= -0.0105241, b5= 326.375033, a5= -0.0105241, b6= 326.375033, a6= -0.0105241, b7= 326.375033, a7= -0.0105241, b8= 326.375033, a8= -0.0105241, b9= 326.375033, a9= -0.0105241, b10= 326.375033, a10= -0.0105241, b11= 326.375033, a11= -0.0105241, b12= 326.375033, a12= -0.0105241))
summary(model)
y_pred <- predict(model)
y_pred
plot(x,y_pred)
coeff<- summary(model)$coefficients
for_df_y_pred <- as.data.frame(y_pred)
result_1['y_pred'] <- for_df_y_pred[['y_pred']]


coeff <- as.data.frame(coeff)
coeff <- coeff[ -c(2, 3, 4, 5) ]
write.csv(coeff,'coeff_new.csv')

#How can we apply to the value from dataframe "coeff":
b1 = coeff[1, "Estimate"]
a1 = coeff[2, "Estimate"]
b2 = coeff[3, "Estimate"]
a2 = coeff[4, "Estimate"]
b3 = coeff[5, "Estimate"]
a3 = coeff[6, "Estimate"]
b4 = coeff[7, "Estimate"]
a4 = coeff[8, "Estimate"]
b5 = coeff[9, "Estimate"]
a5 = coeff[10, "Estimate"]
b6 = coeff[11, "Estimate"]
a6 = coeff[12, "Estimate"]
b7 = coeff[13, "Estimate"]
a7 = coeff[14, "Estimate"]
b8 = coeff[15, "Estimate"]
a8 = coeff[16, "Estimate"]
b9 = coeff[17, "Estimate"]
a9 = coeff[18, "Estimate"]
b10 = coeff[19, "Estimate"]
a10 = coeff[20, "Estimate"]
b11 = coeff[21, "Estimate"]
a11 = coeff[22, "Estimate"]
b12 = coeff[23, "Estimate"]
a12 = coeff[24, "Estimate"]