LSAT=c(576,635,558,578,666,580,555,661,651,605,653,575,545,572,594)
GPA=c(3.39,3.30,2.81,3.03,3.44,3.07,3.00,3.43,3.36,3.13,3.12,2.74,2.76,2.88,2.96)

cor(LSAT,GPA)
corb = numeric(B)
indb= numeric(B)
for (i in 1:B){
    indb = sample(1:15, replace = TRUE)
    LSAT[indb]
    LSAT[indb]
    corb[i] = cor(LSAT[indb],LSAT[indb])

}

sd(corb)

