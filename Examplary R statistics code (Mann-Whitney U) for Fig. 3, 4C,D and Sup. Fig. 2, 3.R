setwd("D:/USER/...")
data<-read.csv2("Data sheet.csv", header = TRUE)

group1 <- data$Data[data$Strains == 1]
group2 <- data$Data[data$Strains == 2]
group3 <- data$Data[data$Strains == 3]
group4 <- data$Data[data$Strains == 4]
group5 <- data$Data[data$Strains == 5]
group6 <- data$Data[data$Strains == 6]
group7 <- data$Data[data$Strains == 7]
group8 <- data$Data[data$Strains == 8]


x1 <-wilcox.test(group1,group2,paired = FALSE)[3]
x2 <-wilcox.test(group1,group3,paired = FALSE)[3]
x3 <-wilcox.test(group1,group4,paired = FALSE)[3]
x4 <-wilcox.test(group1,group5,paired = FALSE)[3]
x5 <-wilcox.test(group1,group6,paired = FALSE)[3]
x6 <-wilcox.test(group1,group7,paired = FALSE)[3]
x7 <-wilcox.test(group1,group8,paired = FALSE)[3]
x8 <-wilcox.test(group2,group3,paired = FALSE)[3]
x9 <-wilcox.test(group2,group4,paired = FALSE)[3]
x10 <-wilcox.test(group2,group5,paired = FALSE)[3]
x11 <-wilcox.test(group2,group6,paired = FALSE)[3]
x12 <-wilcox.test(group2,group7,paired = FALSE)[3]
x13 <-wilcox.test(group2,group8,paired = FALSE)[3]
x14 <-wilcox.test(group3,group4,paired = FALSE)[3]
x15 <-wilcox.test(group3,group5,paired = FALSE)[3]
x16 <-wilcox.test(group3,group6,paired = FALSE)[3]
x17 <-wilcox.test(group3,group7,paired = FALSE)[3]
x18 <-wilcox.test(group3,group8,paired = FALSE)[3]
x19 <-wilcox.test(group4,group5,paired = FALSE)[3]
x20 <-wilcox.test(group4,group6,paired = FALSE)[3]
x21 <-wilcox.test(group4,group7,paired = FALSE)[3]
x22 <-wilcox.test(group4,group8,paired = FALSE)[3]
x23 <-wilcox.test(group5,group6,paired = FALSE)[3]
x24 <-wilcox.test(group5,group7,paired = FALSE)[3]
x25 <-wilcox.test(group5,group8,paired = FALSE)[3]
x26 <-wilcox.test(group6,group7,paired = FALSE)[3]
x27 <-wilcox.test(group6,group8,paired = FALSE)[3]
x28 <-wilcox.test(group7,group8,paired = FALSE)[3]

y1 <- wilcox.test(group1,group2,paired = FALSE)[1]
y2 <-wilcox.test(group1,group3,paired = FALSE)[1]
y3 <-wilcox.test(group1,group4,paired = FALSE)[1]
y4 <-wilcox.test(group1,group5,paired = FALSE)[1]
y5 <-wilcox.test(group1,group6,paired = FALSE)[1]
y6 <-wilcox.test(group1,group7,paired = FALSE)[1]
y7 <-wilcox.test(group1,group8,paired = FALSE)[1]
y8 <-wilcox.test(group2,group3,paired = FALSE)[1]
y9 <-wilcox.test(group2,group4,paired = FALSE)[1]
y10 <-wilcox.test(group2,group5,paired = FALSE)[1]
y11 <-wilcox.test(group2,group6,paired = FALSE)[1]
y12 <-wilcox.test(group2,group7,paired = FALSE)[1]
y13 <-wilcox.test(group2,group8,paired = FALSE)[1]
y14 <-wilcox.test(group3,group4,paired = FALSE)[1]
y15 <-wilcox.test(group3,group5,paired = FALSE)[1]
y16 <-wilcox.test(group3,group6,paired = FALSE)[1]
y17 <-wilcox.test(group3,group7,paired = FALSE)[1]
y18 <-wilcox.test(group3,group8,paired = FALSE)[1]
y19 <-wilcox.test(group4,group5,paired = FALSE)[1]
y20 <-wilcox.test(group4,group6,paired = FALSE)[1]
y21 <-wilcox.test(group4,group7,paired = FALSE)[1]
y22 <-wilcox.test(group4,group8,paired = FALSE)[1]
y23 <-wilcox.test(group5,group6,paired = FALSE)[1]
y24 <-wilcox.test(group5,group7,paired = FALSE)[1]
y25 <-wilcox.test(group5,group8,paired = FALSE)[1]
y26 <-wilcox.test(group6,group7,paired = FALSE)[1]
y27 <-wilcox.test(group6,group8,paired = FALSE)[1]
y28 <-wilcox.test(group7,group8,paired = FALSE)[1]

W_values <- c(y1, y2, y3, y4, y5, y6, y7, y8, y9, y10, y11, y12, y13, y14, y15, y16, y17, y18, y19, y20, y21, y22, y23, y24, y25, y26, y27, y28)
print (W_values)
p_values <- c(x1$p.value, x2$p.value, x3$p.value, x4$p.value, x5$p.value, x6$p.value, x7$p.value, x8$p.value, x9$p.value, x10$p.value, x11$p.value, x12$p.value, x13$p.value, x14$p.value, x15$p.value, x16$p.value, x17$p.value, x18$p.value, x19$p.value, x20$p.value, x21$p.value, x22$p.value, x23$p.value, x24$p.value, x25$p.value, x26$p.value, x27$p.value, x28$p.value)
print (p_values)
BENJAMINI_HOCHBERG <- p.adjust(p_values, method = "BH", n = length(p_values))
print(BENJAMINI_HOCHBERG)

write.table(W_values,file="W values",quote=F,sep="\t",row.names=F,col.names=T)
write.table(p_values,file="P values",quote=F,sep="\t",row.names=F,col.names=T)
write.table(BENJAMINI_HOCHBERG,file="Adjusted P values",quote=F,sep="\t",row.names=F,col.names=T)
