N<-c(10,10,10,9,11,10,10,10,10,10) # 条件当たりの試行数
prep<-c(0,1,1,2,3,7,8,9,9,10)
xdat<-c(1,2,3,4,5,6,7,8,9,10)/10
pratio<-prep/N
plot(xdat,prep,type='l', xlim=c(min(xdat),max(xdat)),ylim=c(0,1))
# 何に回帰するか。probitは累積正規分布、logitはロジスティック回帰、binomialの場合はlogitを使っていそう
#fit<-glm(cbind(prep,N-prep)~xdat, data=data.frame(xdat,prep), family='binomial')
fit<-glm(cbind(prep,N-prep)~xdat, data=data.frame(xdat,prep), family=binomial('probit'))
fit<-glm(cbind(prep,N-prep)~xdat, data=data.frame(xdat,prep), family=binomial('logit'))
ypred2<-predict(fit, newdata=data.frame(x=c(0.1,0.2,0.3,0.4,xi,0.6,0.7,0.8,0.9,1.0)), type='response')
ypred<-predict(fit, newdata=data.frame(x=xdat), type='response')
plot(xdat, ypred, type='l', xlim=c(min(xdat),max(xdat)),ylim=c(0,1))
ylim=c(0,1)
par(new=T)
plot(xdat, prep/N, axes=F, ann=F, xlim=c(min(xdat),max(xdat)),ylim=c(0,1))
ylim=c(0,1)
yi<-0.5

# logisticでのx推定
xi<-(-log(1/yi-1)-fit$coefficients[1])/fit$coefficients[2]
# probitでのx推定

#par(new=T)
#plot(xi,yi,pch=3,col='red',axes=F, ann=F)
par(new=T)
plot(xi,yi,pch=3,col='red',axes=F, ann=F, xlim=c(min(xdat),max(xdat)),ylim=c(0,1))
text(xi,yi,paste('x=',xi))
ylim=c(0,1)