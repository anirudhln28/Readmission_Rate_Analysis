setwd("C:/Users/Anirudh LN/Desktop/SPRING 2017 SEMESTER/SPL/PROJECT")
patients <- read.csv("10kDiabetes.csv", stringsAsFactors = F)
patients <- subset(patients, discharge_disposition_id != "Expired")
patients <- subset(patients, discharge_disposition_id != "Hospice / home")  

patientData <- subset(patients,
                      select=c("A1Cresult","max_glu_serum","insulin","change","discharge_disposition_id","readmitted"))


patientData <- subset(patients,
                      select=c("A1Cresult","max_glu_serum","insulin","change","readmitted"))

patientData$A1Cresult <- as.character(patientData$A1Cresult)
patientData$A1Cresult[patientData$A1Cresult == "None"] <- 1
patientData$A1Cresult <- as.character(patientData$A1Cresult)
patientData$A1Cresult[patientData$A1Cresult == "Norm"] <- 2
patientData$A1Cresult <- as.character(patientData$A1Cresult)
patientData$A1Cresult[patientData$A1Cresult == ">7"] <- 3
patientData$A1Cresult <- as.character(patientData$A1Cresult)
patientData$A1Cresult[patientData$A1Cresult == ">8"] <- 4

patientData$A1Cresult <- as.numeric(patientData$A1Cresult)
patientData$A1Cresult[patientData$A1Cresult == "None"] <- 1
patientData$A1Cresult <- as.numeric(patientData$A1Cresult)
patientData$A1Cresult[patientData$A1Cresult == "Norm"] <- 2
patientData$A1Cresult <- as.numeric(patientData$A1Cresult)
patientData$A1Cresult[patientData$A1Cresult == ">7"] <- 3
patientData$A1Cresult <- as.numeric(patientData$A1Cresult)
patientData$A1Cresult[patientData$A1Cresult == ">8"] <- 4

patientData$A1Cresult
patientData$max_glu_serum <- as.character(patientData$max_glu_serum)
patientData$max_glu_serum[patientData$max_glu_serum == "None"] <- 1
patientData$max_glu_serum <- as.character(patientData$max_glu_serum)
patientData$max_glu_serum[patientData$max_glu_serum == "Norm"] <- 2
patientData$max_glu_serum <- as.character(patientData$max_glu_serum)
patientData$max_glu_serum[patientData$max_glu_serum == ">200"] <- 3
patientData$max_glu_serum <- as.character(patientData$max_glu_serum)
patientData$max_glu_serum[patientData$max_glu_serum == ">300"] <- 4


patientData$max_glu_serum <- as.numeric(patientData$max_glu_serum)
patientData$max_glu_serum[patientData$max_glu_serum == "None"] <- 1
patientData$max_glu_serum <- as.numeric(patientData$max_glu_serum)
patientData$max_glu_serum[patientData$max_glu_serum == "Norm"] <- 2
patientData$max_glu_serum <- as.numeric(patientData$max_glu_serum)
patientData$max_glu_serum[patientData$max_glu_serum == ">200"] <- 3
patientData$max_glu_serum <- as.numeric(patientData$max_glu_serum)
patientData$max_glu_serum[patientData$max_glu_serum == ">300"] <- 4

patientData$insulin <- as.character(patientData$insulin)
patientData$insulin[patientData$insulin == "No"] <- 1
patientData$insulin <- as.character(patientData$insulin)
patientData$insulin[patientData$insulin == "Down"] <- 2
patientData$insulin <- as.character(patientData$insulin)
patientData$insulin[patientData$insulin == "Steady"] <- 3
patientData$insulin <- as.character(patientData$insulin)
patientData$insulin[patientData$insulin == "Up"] <- 4


patientData$insulin <- as.numeric(patientData$insulin)
patientData$insulin[patientData$insulin == "No"] <- 1
patientData$insulin <- as.numeric(patientData$insulin)
patientData$insulin[patientData$insulin == "Down"] <- 2
patientData$insulin <- as.numeric(patientData$insulin)
patientData$insulin[patientData$insulin == "Steady"] <- 3
patientData$insulin <- as.numeric(patientData$insulin)
patientData$insulin[patientData$insulin == "Up"] <- 4

patientData$change <- as.character(patientData$change)
patientData$change[patientData$change == "No"] <- 0
patientData$change <- as.character(patientData$change)
patientData$change[patientData$change == "Ch"] <- 1

patientData$change <- as.numeric(patientData$change)
patientData$change[patientData$change == "No"] <- 0
patientData$change <- as.numeric(patientData$change)
patientData$change[patientData$change == "Ch"] <- 1

patientData$readmitted <- as.character(patientData$readmitted)
patientData$readmitted[patientData$readmitted == "TRUE"] <- 1
patientData$readmitted <- as.character(patientData$readmitted)
patientData$readmitted[patientData$readmitted == "FALSE"] <- 0

patientData$readmitted <- as.numeric(patientData$readmitted)
patientData$readmitted[patientData$readmitted == "TRUE"] <- 1
patientData$readmitted <- as.numeric(patientData$readmitted)
patientData$readmitted[patientData$readmitted == "FALSE"] <- 0


patientData$A1Cresult
patientData$max_glu_serum
patientData$insulin
patientData$change
patientMatrix <- cor(patientData)
patientMatrix
round(patientMatrix,2)

install.packages("pastecs")

library(pastecs)
round(stat.desc(patientMatrix),2)
install.packages("psych")
library(psych)
cortest.bartlett(patientMatrix)
pc1 <- principal(patientMatrix,nfactors = 3, rotate = "none")
pc1$values

plot(pc1$values, type= "b")

print.psych(pc1, cut=0.3, sort= TRUE)

newModel <- lm(readmitted ~ A1Cresult, data = patientData)

newModel1 <- lm(readmitted ~ A1Cresult + insulin, data = patientData)

newModel2 <- lm(readmitted ~ A1Cresult + change + insulin, data = patientData)

newModelGLM <- glm(readmitted ~ A1Cresult, data = patientData)

newModelGLM1 <- glm(readmitted ~ A1Cresult + insulin, data = patientData)

newModelGLM2 <- glm(readmitted ~ A1Cresult + change + insulin, data = patientData)


summary(newModelGLM)

anova(newModel,newModel1,newModel2)
anova(newModelGLM,newModelGLM1,newModelGLM2)
