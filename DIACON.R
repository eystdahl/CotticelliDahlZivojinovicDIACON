#Script for Cotticelli, Dahl, and Zivojinovic's DIACON paper

library(vcd)

#Make a matrix of absolute constructions in Jordanes and Gregory of Tours

AbsoluteConstructionsPtcp <- matrix(c(245, 361, 26, 19, 65, 84, 149, 254, 8, 10, 3, 4), ncol = 6, byrow = TRUE)

#Add column and row names

colnames(AbsoluteConstructionsPtcp) <- c("Ablative Jordanes", "Ablative Gregory", "Mixed Jordanes", "Mixed Gregory", "Accusative Jordanes", "Accusative Gregory")
rownames(AbsoluteConstructionsPtcp) <- c("Perfect participle", "Present Participle")

#Check

AbsoluteConstructionsPtcp

#Get proportions

AbsoluteConstructionsPtcpProp <- prop.table(AbsoluteConstructionsPtcp, 2)*100

#Check
AbsoluteConstructionsPtcpProp

#Make plots

barplot(AbsoluteConstructionsPtcpProp, ylim = range(0:100), ylab = 'Relative frequency (%)', cex.sub = 1.3, cex.lab = 1.3, cex.axis = 1.3, col = c('black', 'grey'), beside = TRUE) 
legend("topleft", fill = c("black", "grey"), cex = 1.3, legend = c("Perfect Participle", "Present Participle"))

chisq.test(AbsoluteConstructionsPtcp)

assocstats(AbsoluteConstructionsPtcp)

chisq.test(AbsoluteConstructionsPtcp)$expected

AblAbsPerfIord <- matrix(c(245, 555, 149, 279), ncol = 2, byrow = TRUE)
fisher.test(AblAbsPerfIord, alternative = "less")

AblAbsPerfGreg <- matrix(c(361, 439, 254, 176), ncol = 2, byrow = TRUE)
fisher.test(AblAbsPerfGreg, alternative = "less")

MixedAbsPerfIord <- matrix(c(26, 774, 8, 420), ncol = 2, byrow = TRUE)
fisher.test(MixedAbsPerfIord, alternative = "greater")

MixedAbsPerfGreg <- matrix(c(19, 781, 10, 418), ncol = 2, byrow = TRUE)
fisher.test(MixedAbsPerfGreg, alternative = "greater")

AccAbsPerfIord <- matrix(c(65, 735, 3, 425), ncol = 2, byrow = TRUE)
fisher.test(AccAbsPerfIord, alternative = "greater")

AccAbsPerfGreg <- matrix(c(84, 716, 4, 424), ncol = 2, byrow = TRUE)
fisher.test(AccAbsPerfGreg, alternative = "greater")

AblAbsPresIord <- matrix(c(149, 279, 245, 685), ncol = 2, byrow = TRUE)
fisher.test(AblAbsPresIord, alternative = "greater")

AblAbsPresGreg <- matrix(c(254, 174, 361, 439), ncol = 2, byrow = TRUE)
fisher.test(AblAbsPresGreg, alternative = "greater")

MixedAbsPresIord <- matrix(c(8, 420, 26, 774), ncol = 2, byrow = TRUE)
fisher.test(MixedAbsPresIord, alternative = "less")

MixedAbsPresGreg <- matrix(c(10, 420, 19, 781), ncol = 2, byrow = TRUE)
fisher.test(MixedAbsPresGreg, alternative = "less")

AccAbsPresIord <- matrix(c(3, 425, 65, 735), ncol = 2, byrow = TRUE)
fisher.test(AccAbsPresIord, alternative = "less")

AccAbsPresGreg <- matrix(c(4, 424, 84, 716), ncol = 2, byrow = TRUE)
fisher.test(AccAbsPresGreg, alternative = "less")


#Make a matrix of AccAbs and GER in authors

DIACONdata1 <- matrix(c(5, 2, 5, 55, 42, 29, 13, 120, 16, 115), ncol = 5, byrow = TRUE)

#Add row and column names
rownames(DIACONdata1) <- c('AccAbs', 'Gerund')
colnames(DIACONdata1) <- c('Mulomedicina Chironis', 'Peregrinatio Egeriae', 'Cassiodorus', 'Iordanes', 'Gregorius Turonensis')

#Check output

DIACONdata1

#Get proportional values
DIACONdata1Proportion <- prop.table(DIACONdata1, 2)*100

DIACONdata1Proportion

#Make barplot and plot

barplot(DIACONdata1Proportion, ylim = range(0:100), cex.names = 1.2, ylab = 'Relative frequency (%)', cex.sub = 1.3, cex.lab = 1.3, cex.axis = 1.3, col = c('black', 'grey'), beside = TRUE) 
legend("topright", cex= 1.3, fill = c("black", "grey"), legend = c("Absolute Accusative", "Gerund"))

plot(DIACONdata1Proportion[1,], type = "o", pch = 2, ylim = range(0:100), axes = FALSE, ylab = "Relative frequency (%)", xlab = "", cex.sub = 1.3, cex.lab = 1.3, cex.axis = 1.3)

axis(1, at = 1:5, labels = colnames(DIACONdata1))
axis(2)
box()
lines(DIACONdata1Proportion[2,], type = "o", lty = 2, pch = 0, col = "black")
legend("topright", lty = c(1,2), col = c("black", "black"), cex = 1.3, pch = c(2,0), legend = c("Absolute Accusative", "Gerund"))


#Perform Chi-squared test

chisq.test(DIACONdata1)

#Check expected values

chisq.test(DIACONdata1)$expected

#Get additional statistics

assocstats(DIACONdata1)

#Perform Fisher exact test

AccAbsMul <- matrix(c(3, 104, 29, 264), ncol = 2, byrow = TRUE)
fisher.test(AccAbsMul, alternative = 'less')

GerMul <- matrix(c(29, 264, 3, 104), ncol = 2, byrow = TRUE)
fisher.test(GerMul, alternative = 'greater')

AccAbsPeregr <- matrix(c(2, 105, 13, 280), ncol = 2, byrow = TRUE)
fisher.test(AccAbsPeregr, alternative = 'less')

GerPeregr <- matrix(c(13, 280, 2, 105), ncol = 2, byrow = TRUE)
fisher.test(GerPeregr, alternative = 'greater')

AccAbsCass <- matrix(c(5, 102, 120, 173), ncol = 2, byrow = TRUE)
fisher.test(AccAbsCass, alternative = 'less')

GerCass <- matrix(c(120, 173, 5, 102), ncol = 2, byrow = TRUE)
fisher.test(GerCass, alternative = 'greater')

AccAbsIord <- matrix(c(55, 52, 16, 277), ncol = 2, byrow = TRUE)
fisher.test(AccAbsIord, alternative = 'greater')

GerIord <- matrix(c(16, 277, 55, 52), ncol = 2, byrow = TRUE)
fisher.test(GerIord, alternative = 'less')

AccAbsGreg <- matrix(c(42, 65, 115, 178), ncol = 2, byrow = TRUE)
fisher.test(AccAbsGreg, alternative = 'greater')

GerGreg <- matrix(c(115, 178, 42, 65), ncol = 2, byrow = TRUE)
fisher.test(GerGreg, alternative = 'less')

#Association between the two constructions and transitive/intransitive predicates

TransIntrans <- matrix(c(89, 18, 139, 154), ncol = 2, byrow = TRUE)

colnames(TransIntrans) <- c("Transitive", "Intransitive")
rownames(TransIntrans) <- c("AccAbs", "Gerund")

TransIntrans

chisq.test(TransIntrans)

assocstats(TransIntrans)

chisq.test(TransIntrans)$expected

#Fisher test
TransAccAbs <- matrix(c(89, 18, 139, 154), ncol = 2, byrow = TRUE)
fisher.test(TransAccAbs, alternative = "greater")

TransGer <- matrix(c(139, 154, 89, 18), ncol = 2, byrow = TRUE)
fisher.test(TransGer, alternative = "less")

IntransAccAbs <- matrix(c(18, 89, 154, 139), ncol = 2, byrow = TRUE)
fisher.test(IntransAccAbs, alternative = "less")

IntransGer <- matrix(c(89, 18, 139, 154), ncol = 2, byrow = TRUE)
fisher.test(IntransGer, alternative = "greater")

#Association between the two constructions and subject co-reference

AgentSubjCoref <- matrix(c(92, 15, 260, 33), ncol = 2, byrow = TRUE)

chisq.test(AgentSubjCoref)

assocstats(AgentSubjCoref)

#Association with telic and atelic predicates

TelicAtelic <- matrix(c(103, 4, 120, 173), ncol =  2, byrow = TRUE)
colnames(TelicAtelic) <- c("Telic", "Atelic")
rownames(TelicAtelic) <- rownames(TransIntrans)

TelicAtelic

chisq.test(TelicAtelic)

assocstats(TelicAtelic)

chisq.test(TelicAtelic)$expected

#Apply fisher exact test

TelicAccAbs <- matrix(c(103, 4, 120, 173), ncol = 2, byrow = TRUE)
fisher.test(TelicAccAbs, alternative = "greater")

TelicGer <- matrix(c(120, 173, 103, 4), ncol = 2, byrow = TRUE)
fisher.test(TelicGer, alternative = "less")

AtelicAccAbs <- matrix(c(4, 103, 173, 120), ncol = 2, byrow = TRUE)
fisher.test(AtelicAccAbs, alternative = "less")

AtelicGer <- matrix(c(173, 120, 4, 103), ncol = 2, byrow = TRUE)
fisher.test(AtelicGer, alternative = "greater")



Authors_Constructions <- c("MulChir_AccAbs", "MulChir_GER", "Peregr_AccAbs", "Peregr_GER", "Cassiodorus_AbsAcc", "Cassiodorus_GER", "Iordanes_AbsAcc", "Iordanes_GER", "GregTur_AbsAcc", "GregTur_GER")
Transitive <- c(3, 11, 1, 3, 3, 66, 48, 9, 34, 50)
Intransitive <- c(2, 18, 1, 10, 0, 54, 7, 7, 8, 65)
SubjectCoreference <- c(3, 24, 1, 11, 3, 111, 49, 10, 36, 104)
Telic <- c(3, 7, 2, 3, 3, 57, 54, 5, 41, 47)
Atelic <- c(2, 22, 0, 10, 0, 63, 1, 9, 1, 68)

DIACONdata2 <- data.frame(Authors_Constructions, Transitive, Intransitive, SubjectCoreference, Telic, Atelic)

DIACONdata2

str(DIACONdata2)

library(FactoMineR, partykit)

set.seed(129)

DIACONdata2.rf <- ctree(Authors_Constructions ~ Transitive + Intransitive + SubjectCoreference + Telic + Atelic, data = DIACONdata2)

DIACONdata2.ca

head(DIACONdata2.ca)


