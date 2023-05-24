dA<-c(71,91,76,80,82)
dB<-c(77,93,79,80,86)
t=(mean(dA-dB))/sqrt(var(dA-dB)/length(dA))
pt(abs(t),length(dA)-1,lower.tail = F)*2
t.test(dA,dB, paired = T)

# 対応のあるt検定GLM
d<-dA-dB
x<-rep(0,5)
summary(glm(d~x, family=gaussian()))

# 対応のないt検定GLM
y<-c(dA,dB)
x1<-factor(c(rep(1,5),rep(0,5)))
x2<-factor(c(rep(0,5),rep(1,5)))
summary(glm(y~x1+x2, family=gaussian()))
