rm(list=ls(all=TRUE));
data = read.csv("404410030_97ba06.csv");
reduced_data = cbind(data[5:6], data[84:85]);
M = log(reduced_data$F635_B635,base=2) - log(reduced_data$F532_B532,base=2)
A = (log(reduced_data$F635_B635,base=2) + log(reduced_data$F532_B532,base=2))
regl_lowess = lowess(x=A, y=M)
F532_P = reduced_data$F532_B532 * 2^( regl_lowess[[2]] )
#m635 = median(reduced_data$F635_B635);#Using median
#m532 = median(reduced_data$F532_B532);
#m635 = mean(reduced_data$F635_B635);#Using mean
#m532 = mean(reduced_data$F532_B532);
#m635 = sum(reduced_data$F635_B635);#Using sum
#m532 = sum(reduced_data$F532_B532);
#totalR = m635/m532;
#F532_P = reduced_data$F532_B532 * totalR;
reduced_data = cbind(reduced_data, F532_P);
ratio_Cy5_Cy3 = reduced_data$F635_B635 / reduced_data$F532_P;
reduced_data = cbind(reduced_data, ratio_Cy5_Cy3);
reduced_data = reduced_data[order(reduced_data$ratio_Cy5_Cy3),]
logCy5_Cy3 = log(reduced_data$ratio_Cy5_Cy3)
reduced_data = cbind(reduced_data, logCy5_Cy3)
M_Cy5Cy3 = log(reduced_data$F635_B635,base=2) - log(reduced_data$F532_P,base=2)
A_Cy5Cy3 = (log(reduced_data$F635_B635,base=2) + log(reduced_data$F532_P,base=2))
reduced_data = cbind(reduced_data, M_Cy5Cy3, A_Cy5Cy3)
regl = lm(M_Cy5Cy3~A_Cy5Cy3)
plot(y=M_Cy5Cy3, x=A_Cy5Cy3,col='black')
par(new=TRUE)
#plot(y=log(reduced_data$F635_B635/reduced_data$F532_B532,base=2),x=log(reduced_data$F635_B635*reduced_data$F532_B532,base=2), col='black', xlab="", ylab="")
abline(regl,col='green')
MA_lowess = lowess(y=M_Cy5Cy3, x=A_Cy5Cy3)
lines(x=MA_lowess[[1]],y=MA_lowess[[2]], col='red')
plot(y=log(reduced_data$F635_B635,base=2),x=log(reduced_data$F532_P,base=2))
regl_curve = lowess(y=log(reduced_data$F635_B635,base=2),x=log(reduced_data$F532_P,base=2))
regl_line = lm(log(reduced_data$F635_B635,base=2)~log(reduced_data$F532_P,base=2))
abline(regl_line, col='green')
lines(x=regl_curve[[1]], y=regl_curve[[2]],  col='red')

plot(y=reduced_data$F635_B635, x=reduced_data$F532_P)
hist(reduced_data$logCy5_Cy3,breaks=c(-500:500/100))
#plot(reduced_data)