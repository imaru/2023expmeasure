dA<-c(71,91,76,80,82)
dB<-c(77,93,79,80,86)
t=(mean(dA-dB))/sqrt(var(dA-dB)/length(dA))
pt(abs(t),length(dA)-1,lower.tail = F)*2
t.test(dA,dB, paired = F)