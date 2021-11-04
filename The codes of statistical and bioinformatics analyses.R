#SEER database pretreatment

diff1<- read.csv('SEER-OSCC-1103-all.csv',sep= ',')

diff3<- subset(diff1, diff1$Diagnostic.Confirmation == "Positive histology")
table(diff3$Primary.Site...labeled)
diff4<- diff3
diff4<- diff4[-which(diff3$Primary.Site...labeled %in% 'C05.2-Uvula'),]
colnames(diff4)
diff4$Age <- (diff4$Year.of.diagnosis - diff4$Year.of.birth)
a<-diff4[1:10,]
colnames(diff4)
diff4<-diff4[,-c(3,4,8,22,25,26)]
a<-diff4[1:10,]
diff4<-diff4[,-5]
colnames(diff4)
colnames(diff4)[3:6]<- c("Region", "Race",'Grade','Site')
colnames(diff4)
colnames(diff4)[12:13]<-c('Radiation','Chemotherapy')
colnames(diff4)[7:9]<- c('Stage 7th','Stage 6th','Stage 3rd')
diff4<-diff4[,-19]
colnames(diff4)
colnames(diff4)[15]<- c('CSS')
colnames(diff4)[18]<- c('OS')
colnames(diff4)
table(diff4$Grade)
diff4$Grade<- as.character(diff4$Grade)
diff4$Grade[diff4$Grade == 'Moderately differentiated; Grade II']<- 'II'
diff4$Grade[diff4$Grade == 'Poorly differentiated; Grade III']<- 'III'
diff4$Grade[diff4$Grade == 'Undifferentiated; anaplastic; Grade IV']<- 'IV'
diff4$Grade[diff4$Grade == 'Well differentiated; Grade I']<- 'I'
diff4[diff4 == 'Unknown'] <- NA

diff4[diff4 == 'Blank(s)'] <- NA
table(diff4$Radiation)
diff4$Chemotherapy <-as.character(diff4$Chemotherapy)
diff4$Chemotherapy[diff4$Chemotherapy=="No/Unknown"] <- 'No'
diff4$Radiation <-as.character(diff4$Radiation)
diff4$Radiation[diff4$Radiation  == 'Beam radiation'|
                  diff4$Radiation  =='Combination of beam with implants or isotopes'|
                  diff4$Radiation  =='Other than beam radiation (1973-1987 cases only)'|
                  diff4$Radiation  =='Radiation, NOS  method or source not specified'|
                  diff4$Radiation  =='Radioisotopes (1988+)'|
                  diff4$Radiation  == 'Radioactive implants (includes brachytherapy) (1988+)'] <- "Yes"
diff4$Radiation[diff4$Radiation== 'None/Unknown'|
                  diff4$Radiation=='Refused (1988+)'] <- 'No'
diff4$Radiation[diff4$Radiation== 'Recommended, unknown if administered']<-'Unknown'



table(diff4$Radiation)
a<-diff4[1:10,]
colnames(diff4)[11]<- "Surgery"
diff4<- diff4[,-10]
colnames(diff4)
table(diff4$Surgery)
diff4$Surgery<- as.character(diff4$Surgery)
diff4$Surgery[diff4$Surgery== 'Not performed, patient died prior to recommended surgery'|
                diff4$Surgery=='Not recommended'|
                diff4$Surgery=='Not recommended, contraindicated due to other cond; autopsy only (1973-2002)'|
                diff4$Surgery=='Recommended but not performed, patient refused'|
                diff4$Surgery=='Recommended but not performed, unknown reason'] <- 'No'
diff4$Surgery[diff4$Surgery== 'Recommended, unknown if performed'|
                diff4$Surgery=='Unknown; death certificate; or autopsy only (2003+)']<- 'Unknown'
diff4$Surgery[diff4$Surgery== 'Surgery performed'] <- 'Yes'
table(diff4$Surgery)
table(diff4$`Stage 7th`,diff4$`Stage 6th`)
table(diff4$`Stage 7th`)
colnames(diff4)
colnames(diff4)[7]<- 'Stage' ###以Stage 7th作为最终的stage计算
colnames(diff4)
diff4<-diff4[,-c(8,9)]
diff4$Stage<- as.character(diff4$Stage)
table(diff4$Stage)
diff4$Stage[diff4$Stage=='IVA'|diff4$Stage=='IVB'|diff4$Stage=='IVC'|
              diff4$Stage=='IVNOS' ] <- 'IV'
diff4$Stage[diff4$Stage== 'UNK Stage']<- 'Unknown'
table(diff4$Stage)
table(diff4$Site)
diff4$Site<- as.character(diff4$Site)
diff4$Site[diff4$Site == 'C00.0-External upper lip'|
             diff4$Site== 'C00.1-External lower lip'|
             diff4$Site == 'C00.2-External lip, NOS'|
             diff4$Site == 'C00.3-Mucosa of upper lip'|
             diff4$Site == 'C00.4-Mucosa of lower lip'|
             diff4$Site == 'C00.5-Mucosa of lip, NOS'|
             diff4$Site== 'C00.6-Commissure of lip'|
             diff4$Site== 'C00.8-Overlapping lesion of lip'|
             diff4$Site == 'C00.9-Lip, NOS'] <-'Lip'

diff4$Site[diff4$Site == 'C01.9-Base of tongue, NOS'|
             diff4$Site== 'C02.2-Ventral surface of tongue, NOS'|
             diff4$Site == 'C02.0-Dorsal surface of tongue, NOS'|
             diff4$Site == 'C02.1-Border of tongue'|
             diff4$Site == 'C02.4-Lingual tonsil'|
             diff4$Site == 'C02.3-Anterior 2/3 of tongue, NOS'|
             diff4$Site == 'C02.8-Overlapping lesion of tongue'| 
             diff4$Site == 'C02.9-Tongue, NOS'] <-'Tongue'

diff4$Site[diff4$Site == 'C03.0-Upper gum'|
             diff4$Site== 'C03.1-Lower gum'|
             diff4$Site == 'C03.9-Gum, NOS'] <-'Gum'


diff4$Site[diff4$Site == 'C04.0-Anterior floor of mouth'|
             diff4$Site== 'C04.1-Lateral floor of mouth'|
             diff4$Site == 'C04.8-Overlapping lesion of floor of mouth'|
             diff4$Site == 'C04.9-Floor of mouth, NOS']<- 'Floor of Mouth'

diff4$Site[diff4$Site == 'C05.0-Hard palate'|diff4$Site == 'C05.1-Soft palate, NOS'|
             diff4$Site =='C05.8-Overlapping lesion of palate'|
             diff4$Site =='C05.9-Palate, NOS' ]<- 'Palate'

diff4$Site[diff4$Site =='C06.0-Cheek mucosa']<- 'Buccal Mucosa'

diff4$Site[diff4$Site =='C06.8-Overlapping lesion of other & unspecified mouth'|
             diff4$Site =='C06.9-Mouth, NOS' |
             diff4$Site =='C06.1-Vestibule of mouth' |
             diff4$Site =='C06.2-Retromolar area' ]<- 'Others'
table(diff4$Site)
diff4$Grade[diff4$Grade=='IV'] <- 'III'            
write.csv(diff4,"SEER data-20210829.csv")
library(ggplot2)
#Fig. 1a
library(survival)
library(survminer)
library(xtable)
Clin<- read.csv("SEER data-20210829.csv",sep=',')
mean(Clin$Survival.months, na.rm= T)
table(Clin$OS.5year)
Clin[Clin=='Unknown']<- NA
Clin$newStage<-Clin$Stage
Clin$newStage<- as.character(Clin$newStage)
Clin$newStage[Clin$newStage=='III'|Clin$newStage=='IV']<- 'III-IV'
Clin$newStage[Clin$newStage=='I'|Clin$newStage=='II']<- 'I-II'
Clin$newgarde<-Clin$Grade
Clin$newgarde<- as.character(Clin$newgarde)
Clin$newgarde[Clin$newgarde=='II'|Clin$newgarde=='I']<- 'I-II'
sur <- Surv(time = Clin$OS.time.5year, event = Clin$OS.5year, type = 'right')
prog <- function()  {
  title <- 'Overall Survival'
  sur.result <<- survfit(sur~Grade, data=Clin)
  g <- ggsurvplot (sur.result, data=Clin, conf.int=F, pval=T, 
                   pval.method=T, 
                   pval.method.size=15, 
                   pval.method.coord=c(0,0.5), 
                   pval.size = 15, 
                   ylim = c(0.3,1),
                   xlim = c(0,60),
                   pval.coord=c(0,0.4),
                   #font.main = c(16, "bold", "darkblue"), 
                   font.main = 40,
                   font.x = 30,
                   font.y = 0,
                   font.tickslab=30,
                   font.legend=35,
                   #pval.method.coord=c(1,0.1), 
                   #font.main = c(16, "bold", "darkblue"), 
                   
                   censor=F, 
                   censor.shape=3, 
                   censor.size=6, 
                   risk.table=F, 
                   #palette=c("blue", "dark green", "red"),  
                   palette="lancet",
                   legend.size= 16,
                   size = 4, 
                   #title=(hjust=0.5),
                   xlab='Time(months)',
                   #  legend = 'top',
                   legend.labs=c('I', 'II', 'III'), 
                   legend.title="Grade",
                   legend ='top',
                   ggtheme=theme_bw()+
                     theme(axis.title.x=element_text(size=12),
                           axis.title.y=element_text(size=12),
                           axis.text =element_text(size=12,color ='black'), 
                           axis.text.y=element_text(size=12,color ='black'),
                           legend.text=element_text(size=15),
                           panel.background=element_blank(), 
                           axis.line=element_line(colour='black'),
                           legend.title=element_text(size=18), 
                           #text=element_text(face='bold',size=12), 
                           panel.grid.major = element_blank(), 
                           panel.grid.minor = element_blank(),
                           panel.border = element_rect(fill=NA,color="black", size=3, linetype="solid")), 
                   title=paste0(title, '  in Different Grade') 
  )
  
  return(g)
  
}
tiff(paste0('SEER-Overall Survival for OSCC in Grade (OS)', '.tiff'), 
     width=12,height=8, units="in", compression="lzw", res=150)
print(prog())
dev.off()
#Fig. 1b
rm(list=ls())
library(survival)
library(survminer)
library(ggplot2)
library(ggpubr)
library(xtable)
Clin <- read.csv("SEER data-20210829.csv",header = T,stringsAsFactors = F,sep = ",")
Clin[Clin == 'Unknown'] <- NA
Clin$OS.time.5year<- as.numeric(Clin$OS.time.5year)
table(Clin$OS.5year)
#a<-Clin[1:5,]
table(Clin$Grade)
colnames(Clin)

Clin$newstage <-Clin$Stage
Clin$newstage <- as.character(Clin$newstage)
Clin$newstage[Clin$newstage=='I'|Clin$newstage=='II'] <- 'I-II'
Clin$newstage[Clin$newstage=='III'|Clin$newstage=='IV'] <- 'III-IV'

# cox for all
sur <- Surv(time = Clin$OS.time.5year, event = Clin$OS.5year, type = 'right')
cox <- coxph(sur~Age+Sex+Grade+newstage+Surgery+Chemotherapy+Radiation, data=Clin)
cox.sum1 <- xtable(summary(cox)$coefficient)
cox.sum2 <- xtable(summary(cox)$conf.int)
type <-c("Others", "Others", "MD","PD","Others","Others",'Others',"Others")
type=data.frame(type)
rownames(type)<-rownames(cox.sum1)
cox.sum <- cbind(cox.sum1, cox.sum2, type)
rownames(cox.sum) <- c('Age','Male', 'MD', 'PD', 'Stage III-IV',
                       'Surgery','Chemotherapy',  
                       'Radiation')
colnames(cox.sum)[c(2,8:9,10)] <- c('HR','lower','upper','type')
cox.sum$var <- rownames(cox.sum)
pd <- position_dodge(0.1)
g <- ggplot(cox.sum, aes(x=var, y=HR,color = type)) + 
  geom_point(size = 2) + 
  #ggtitle(paste0('Cox Propotional Hazard Regression Model')) +
  geom_errorbar(aes(ymin=lower, ymax=upper), size= 1.8,width=.2, position=pd) +
  theme_bw() + theme(panel.grid=element_blank())  +
  geom_line(position=pd, size = 1.8) + xlab('Variables') + ylab('Hazard Ratio (HR)') +
  geom_hline(yintercept = 1, linetype = 'dashed', color = "blue", size = 1) +
  theme(axis.text.x = element_text(angle = 90, hjust = 0.5, 
                                   vjust = 0.6, size = 20,color= 'black',face='bold'),
        axis.title.y = element_text(angle = 90, hjust = 0.5, 
                                    vjust = 0.6, size = 15,color= 'black',face='bold'),
        axis.text.y = element_text(angle = 90, hjust = 0.5, 
                                   vjust = 0.6, size = 15,color= 'black',face='bold'))  +
  geom_point(position=pd)+scale_colour_manual(values= c('red','black' ,'red'))+
  theme(legend.title =element_blank())+
  theme(panel.border = element_rect(fill=NA,color="black", size=1.5, linetype="solid"))

g
write.table(cox.sum, paste0('cox of all (OS,)-20210909', '.txt'), 
            quote = F, sep = '\t', row.names = T)

#Fig. 1c-e
ibrary(survival)
library(survminer)
library(xtable)

Clin<- read.csv("SEER data-20210829.csv",sep=',')
mean(Clin$Survival.months, na.rm= T)
table(Clin$OS.5year)
Clin[Clin=='Unknown']<- NA
Clin$newStage<-Clin$Stage
Clin$newStage<- as.character(Clin$newStage)
Clin$newStage[Clin$newStage=='III'|Clin$newStage=='IV']<- 'III-IV'
Clin$newStage[Clin$newStage=='I'|Clin$newStage=='II']<- 'I-II'
Clin$newgarde<-Clin$Grade
Clin$newgarde<- as.character(Clin$newgarde)
Clin$newgarde[Clin$newgarde=='II'|Clin$newgarde=='I']<- 'I-II'
sur <- Surv(time = Clin$OS.time.5year, event = Clin$OS.5year, type = 'right')

prog <- function(gra) {
  title <- 'Overall Survival'
  subs <<- Clin$Grade == gra
  sur.result <<- survfit(sur~Chemotherapy, data=Clin, subset = subs)
  g <- ggsurvplot(sur.result, data=Clin, conf.int=F, pval=T, 
                  pval.method=T,
                  pval.method.size=15, 
                  pval.method.coord=c(0,0.5),
                  pval.size = 15, 
                  ylim = c(0.3,1),
                  xlim = c(0,60),
                  pval.coord=c(0,0.4),
                  #font.main = c(16, "bold", "darkblue"),
                  font.main = 40,
                  font.x = 30,
                  font.y = 0,
                  font.tickslab=30,
                  font.legend=35,
                  #pval.method.coord=c(1,0.1),
                  #font.main = c(16, "bold", "darkblue"),
                  ggtheme=theme_bw()+
                    theme(axis.title.x=element_text(size=12),
                          axis.title.y=element_text(size=12),
                          axis.text =element_text(size=12,color ='black'), 
                          axis.text.y=element_text(size=12,color ='black'),
                          legend.text=element_text(size=15),
                          panel.background=element_blank(), 
                          axis.line=element_line(colour='black'),
                          legend.title=element_text(size=18), 
                          #text=element_text(face='bold',size=12),
                          panel.grid.major = element_blank(), 
                          panel.grid.minor = element_blank(),
                          panel.border = element_rect(fill=NA,color="black", size=3, linetype="solid")),
                  censor=F, 
                  censor.shape=3,
                  censor.size=6,
                  risk.table=F, 
                  #palette=c("blue", "dark green", "red"),  
                  palette="lancet",
                  legend.size= 16,
                  size = 4,  
                  legend.labs=c('No', 'Yes'), 
                  legend.title="Chemotherapy",
                  #palette=c("blue", "dark green", "red"),  
                  title=paste0(title, ' for OSCC in Grade ', gra), 
                  xlab='Time(months)',
                  legend ='top')
  #legend ='top' ))
  return(g)
}

for(g in c('I', 'II', 'III')) {
  tiff(paste0('SEER-Overall Survival for OSCC of Grade ', g,'(chemo,OS)', '.tiff'),
       width=12,height=8, units="in", compression="lzw", res=150) 
  print(prog(g))
  dev.off()
}

#Fig. 1f
library(forestplot)

ibrary(forestplot)
library(survival)
library(survival)
library(survminer)
library(ggplot2)
library(ggpubr)
library(xtable)
Clin <- read.csv("SEER data-20210829.csv",header = T,stringsAsFactors = F,sep = ",")
Clin[Clin == 'Unknown'] <- NA
Clin$OS.time.5year<- as.numeric(Clin$OS.time.5year)
table(Clin$OS.5year)
table(Clin$Grade)
colnames(Clin)

sur <- Surv(time = Clin$OS.time.5year, event = Clin$OS.5year, type = 'right')
cox <- coxph(sur~Grade*Chemotherapy, data=Clin)
cox.sum1 <- xtable(summary(cox)$coefficient)
cox.sum2 <- xtable(summary(cox)$conf.int)
cox.sum <- cbind(cox.sum1, cox.sum2)
rownames(cox.sum) <- c( 'MD', 'PD', 
                        'Chemotherapy','MD*Chemotherapy',  
                        'PD*Chemotherapy')
colnames(cox.sum)[c(2,8:9)] <- c('HR','lower','upper')
cox.sum$var <- rownames(cox.sum)
cox.sum$lowerc<-cox.sum$coef-1.96*cox.sum$`se(coef)`
cox.sum$upperc<-cox.sum$coef+1.96*cox.sum$`se(coef)`
write.table(cox.sum, paste0('intersection analysis cox of SEER(grade-chemotherapy)-20211009', '.txt'), 
            quote = F, sep = '\t', row.names = T)
## TCGA database pretreatment
library(limma)
library(TCGAbiolinks)

cancer.list <- c("CESC","HNSC","KIRC","LGG","LIHC","STAD","UCEC") 
for(cancer in cancer.list) {
  project.title <- paste('TCGA', cancer, sep = '-')
  
  
  query <- GDCquery(project = project.title,
                    data.category = 'Clinical',
                    file.type = 'xml')
  GDCdownload(query)
  clinical.list <- c('drug', 'follow_up', 'patient')
  for(clinical.file in clinical.list) {
    clin <- GDCprepare_clinic(query, clinical.file)
    write.table(clin, paste(cancer, '_clinical_', clinical.file, '.txt', sep = ''), 
                quote = F, sep = '\t', row.names = F)
  }
  write.table(clinical, paste(cancer, '_clinical.txt', sep = ''), 
              quote = F, sep = '\t', row.names = F)
}

#### select OSCC expression and clin ####
read.expr <- function() {
  expr <- read.table('EB++AdjustPANCAN_IlluminaHiSeq_RNASeqV2.geneExp.xena', T, sep='\t', 
                     stringsAsFactors = F)
  expr <- expr[grep('^[A-Za-z].+$', expr$sample), ]
  expr <- expr[!duplicated(expr$sample), ]
  row.names(expr) <- expr$sample
  expr <- expr[, -1]
  return(expr)
}
Expr <- read.expr()

read.clin <- function(cancer) {
  clin.radiation <- read.table(paste('./data/', 'HNSC', '_clinical_radiation.txt', sep=''), 
                               T, sep='\t', stringsAsFactors = F)
  clin.drug <- read.table(paste('./data/', 'HNSC', '_clinical_drug.txt', sep=''), 
                          T, sep='\t', stringsAsFactors = F)
  clin.followup <- read.table(paste('./data/', 'HNSC', '_clinical_follow_up.txt', sep=''), 
                              T, sep='\t', stringsAsFactors = F)
  clin.patient <- read.table(paste('./data/', 'HNSC', '_clinical_patient.txt', sep=''), 
                             T, sep='\t', stringsAsFactors = F)
  clinical <- unique(clin.patient)
  clinical[clinical == ''] <- NA
  if(cancer == 'OSCC') {
    clinical <- clinical[clinical$anatomic_neoplasm_subdivision %in%
                           c('Base of tongue', 'Oral Tongue', 'Alveolar Ridge',
                             'Buccal Mucosa', 'Floor of mouth', 'Oral Cavity', 'Lip',
                             'Hard Palate'),] 
  }
  clinical$sample <- paste(clinical$bcr_patient_barcode, '-01', sep='') 
  clinical$sample <- gsub('-', '.', clinical$sample) # replace char
  # grade
  clinical$grade <- NA
  clinical$grade[clinical$neoplasm_histologic_grade=='G1'|
                   clinical$neoplasm_histologic_grade=='G2'] <- 'G1-G2'
  clinical$grade[clinical$neoplasm_histologic_grade=='G3'|
                   clinical$neoplasm_histologic_grade=='G4'] <- 'G3-G4'
  # stage
  clinical$stage <- NA
  clinical$stage[grepl('^Stage I{1,2}[A-C]?$', 
                       clinical$stage_event_clinical_stage)] <- 'Stage I-II'
  clinical$stage[grepl('^Stage I(II|V)[A-C]?$', 
                       clinical$stage_event_clinical_stage)] <- 'Stage III-IV'
  # exposure
  clinical$smoke <- NA
  clinical$smoke[clinical$tobacco_smoking_history==1] <- 'NO'
  clinical$smoke[!is.na(clinical$tobacco_smoking_history) & 
                   clinical$tobacco_smoking_history!=1] <- 'YES'
  clinical$drink <- NA
  clinical$drink <- clinical$alcohol_history_documented
  # therapy
  clinical$chemotherapy <- 'NO'
  drugs <- unique(clin.drug$bcr_patient_barcode)
  clinical$chemotherapy[clinical$bcr_patient_barcode %in% drugs] <- 'YES'
  clinical$radiotherapy <- 'NO'
  radiation <- unique(clin.radiation$bcr_patient_barcode)
  clinical$radiotherapy[clinical$bcr_patient_barcode %in% radiation] <- 'YES'
  # survival
  clin.followup[clin.followup == ''] <- NA
  clin.followup <- clin.followup[!is.na(clin.followup$vital_status),]
  clin.followup$OS.time <- clin.followup$days_to_death
  clin.followup$OS.time[is.na(clin.followup$OS.time)] <- 
    clin.followup$days_to_last_followup[is.na(clin.followup$OS.time)] 
  bcrcode <- unique(clin.followup$bcr_patient_barcode)
  blank <- rep('', length(bcrcode))
  followup <<- data.frame(blank, blank, blank, stringsAsFactors = F)
  colnames(followup) <<- c('code', 'status', 'time')
  for(i in 1:length(bcrcode)) { 
    time <- max(clin.followup$OS.time[clin.followup$bcr_patient_barcode == bcrcode[i]])
    if(all(clin.followup$vital_status[clin.followup$bcr_patient_barcode == bcrcode[i]] 
           == 'Alive')) {status <- 'Alive'}
    else {status <- 'Dead'}
    followup$code[i] <<- bcrcode[i]
    followup$status[i] <<- status
    followup$time[i] <<- time
  }
  clinical$OS <- NA
  clinical$OS <- followup$status[match(clinical$bcr_patient_barcode, followup$code)]
  clinical$OS.time <- NA
  clinical$OS.time <- followup$time[match(clinical$bcr_patient_barcode, followup$code)]
  clinical$OS.time.month <- as.numeric(clinical$OS.time)/30
  clinical$OS.5.years <- clinical$OS
  clinical$OS.5.years[clinical$OS.time.month>60] <- 'Alive'
  clinical$OS.time.5.years <- clinical$OS.time.month
  clinical$OS.time.5.years[clinical$OS.time.month>60] <- 60
  clinical <- clinical[, c('sample', 'bcr_patient_barcode', 'gender', 
                           'age_at_initial_pathologic_diagnosis',
                           'grade', 'neoplasm_histologic_grade',
                           'stage', 'stage_event_clinical_stage', 'smoke', 'drink',
                           'chemotherapy', 'radiotherapy', 'OS', 'OS.time',
                           'OS.time.month', 'OS.time.5.years', 'OS.5.years',
                           'anatomic_neoplasm_subdivision')]
  
  write.csv(clinical, paste0('clinical_', cancer, '.csv'), quote=F, row.names=F)
  return(clinical)
}
Clin <- read.clin('OSCC')

sams <- intersect(Clin$sample, colnames(Expr))
Clin <- Clin[match(sams, Clin$sample),]
OSCC <- Expr[,sams]
write.csv(OSCC, 'OSCC_exp.csv', quote=F, row.names=T)

#### limma ####
Clin <- Clin[!is.na(Clin$grade),]
sams <- intersect(Clin$sample, colnames(OSCC))
Clin <- Clin[match(sams, Clin$sample),]
Exprc <- OSCC[,sams]
# design
Group <- factor(Clin$grade,levels=c('G1-G2','G3-G4'))
design <- model.matrix(~0+Group)
colnames(design) <- c('G12','G34')
design

# diff
fit <- lmFit(Exprc, design)
contrast.matrix <- makeContrasts(G34-G12,
                                 levels=design)
fit2 <- contrasts.fit(fit, contrast.matrix)
fit2 <- eBayes(fit2)

results <- topTable(fit2,adjust.method="BH",coef=1,p.value=1,
                    lfc=log(1,2),number=Inf)
write.csv(results, ' Grade gene-tcga.csv', quote=F, row.names=T)
#Fig. 2a
library(maftools)
laml.maf = read.csv("TCGA.LIHC.mutect.maf.csv",header=TRUE)
oncoplot(maf = laml.maf , top = 30, fontSize = 12 ,showTumorSampleBarcodes = F，clinicalFeatures = 'grade' )
laml.titv = titv(maf = laml, plot = FALSE, useSyn = TRUE)
plotTiTv(res = laml.titv)

#Fig. 2b 
library(maftools)
laml.maf = read.csv("TCGA.LIHC.mutect.maf.csv",header=TRUE)
oncoplot(maf = laml.maf , top = 30, fontSize = 12 ,showTumorSampleBarcodes = F，clinicalFeatures = 'grade' )
laml.titv = titv(maf = laml, plot = FALSE, useSyn = TRUE)
plotTiTv(res = laml.titv)

#Fig. 2c none
#Fig. 2d
library(ggplot2)

diff<-read.csv("Grade gene-tcga.csv",sep=',')
colnames(diff)
# set colors
diff$DEG = 'notDEG'
diff$DEG[diff$logFC > 1 & diff$adj.P.Val < 0.05] = 'Up'
diff$DEG[diff$logFC < -1 & diff$adj.P.Val < 0.05] = 'Down'
diff$colors = factor(diff$DEG,levels=c("Down","notDEG","Up"),
                     labels=c("darkblue","grey","red"))

## read in gene annotation data 
table(diff$DEG)
# draw scatter plot 

plot(diff$logFC, -log10(diff$adj.P.Val), pch=16, 
     col=as.character(diff$colors),
     xlab="Log2(Fold change)", ylab="-Log10(p value)",
     cex=1)
abline(h=-log(0.05,10),col="black",lty=3,lwd=2.3)
abline(v=c(1,-1),col="black",lty=3,lwd=2.3)

# add text
#text(-2.5,12,"Down-regulated\ngenes", col="darkgreen", font = 2,cex=0.6)
#text(2.5,12,"Up-regulated\ngenes", col="red", font = 2,cex=0.6)
legend('topright',title="Trend",c("Up","Not",'Down'),
       pch=c(16,16,16),col=c("red","grey",'darkblue'),
       bty="n",cex = 1.2)

#Fig. 2e
#install.packages('GOplot')
library('GOplot')
library(ggplot2)
library(ggdendro)
library(gridExtra)
library(RColorBrewer)
library(clusterProfiler)
#library(org.Hs.eg.db)
library(Hmisc)

diff1<-read.csv('Grade gene-tcga.csv',sep=',')
diff2<-subset(diff1,diff1$logFC>1 &diff1$adj.P.Val<0.05 )

diff3<-subset(diff1,diff1$logFC< -1 &diff1$adj.P.Val<0.05 )

diff23<-rbind(diff2,diff3)

#TCGA,GO
GOKEGG <- function(geneset=NULL, output, filename, trend) {
  if(output == 'GO') {
    ego <- enrichGO(gene          = bitr(geneset,
                                         fromType = 'SYMBOL', toType = 'ENTREZID',
                                         OrgDb = 'org.Hs.eg.db')$ENTREZID,
                    # universe      = names(geneList),
                    OrgDb         = org.Hs.eg.db,
                    ont           = "ALL",
                    pAdjustMethod = "BH",
                    pvalueCutoff  = 0.5,
                    qvalueCutoff  = 0.5,
                    readable      = TRUE)
    GO_ <- as.data.frame(ego)
    write.table(GO_, paste0(filename, '_GO_', trend, '(TCGA).txt'), 
                sep = '\t', row.names = F, quote = F)
    return(GO_)}
  if(output == 'KEGG') {
    kk <- enrichKEGG(gene          = bitr(geneset,
                                          fromType = 'SYMBOL', toType = 'ENTREZID',
                                          OrgDb = 'org.Hs.eg.db')$ENTREZID,
                     organism      = 'hsa',
                     pAdjustMethod = "BH",
                     pvalueCutoff  = 0.5,
                     qvalueCutoff  = 0.5)
    KEGG_ <- as.data.frame(setReadable(kk, OrgDb = org.Hs.eg.db, keyType="ENTREZID"))
    write.table(KEGG_, paste0(filename, '_KEGG_', trend, '(TCGA).txt'), 
                sep = '\t', row.names = F, quote = F)
    return(KEGG_)}
}

DrawEnrich <- function(dat, type, top.number = 5, col="blue", trend){
  if(type == 'BP') {tit <- 'Biological Process of '}
  if(type == 'CC') {tit <- 'Cellular Component of '}
  if(type == 'MF') {tit <- 'Molecular Function of '}
  if(type == 'KEGG') {tit <- 'KEGG Pathway of '}
  if(type == 'GO') {tit <- 'Gene Ontology Enrichment of '}
  dat1 = dat[c(1:top.number),]
  dat1$Description = capitalize(dat1$Description)
  dat1 = dat1[order(dat1$p.adjust),,drop=F]
  dat1$Description = factor(dat1$Description,levels=dat1$Description[length(dat1$Description):1])
  dat1$PValue = -log10(dat1$p.adjust)
  dat1$GeneRatio <- dat1$Count / as.numeric(gsub('^.*/', '', dat1$GeneRatio))
  p = ggplot(dat1,aes(GeneRatio, Description)) +
    geom_point(aes(size=Count,colour=PValue)) +
    scale_colour_gradient(low=col,high="red") + 
    labs(colour=expression(-log[10]("P Value")),size="Gene counts",  
         x="Gene Ratio",y="",title=paste0(tit, trend, '-regulated Genes (TCGA,top 50)')) +
    theme_bw() + theme(axis.text.x = element_text(size = 14), axis.text.y=element_text(size=12), 
                       plot.title=element_text(size=20, hjust=1), 
                       legend.text=element_text(size=12), legend.title=element_text(size=14),
                       axis.title.x=element_text(size=18)) +
    scale_x_continuous(limits = c(0,max(dat1$GeneRatio) * 1.2)) 
  return(p)
}
GO <- GOKEGG(geneset=diff2$Gene, output='GO',filename='sqh2021','UP')
GO <- GOKEGG(geneset=diff3$Gene, output='GO',filename='sqh2021','DOWN')


dat1<-read.table("sqh2021_GO_DOWN(TCGA).txt",sep='\t',header = T)
dat11<-subset(dat1,dat1$p.adjust<0.05)
dat11<-dat11[order(dat11$p.adjust,decreasing = F),]
dat11<-dat11[1:5,]

dat2<-read.table("sqh2021_GO_UP(TCGA).txt",sep='\t',header = T)
dat22<-subset(dat2,dat2$p.adjust<0.05)
dat22<-dat22[order(dat22$p.adjust,decreasing = F),]
dat22<-dat22[1:5,]

dat12<-rbind(dat11,dat22)
library(PerformanceAnalytics)
library(corrplot)
library(psych)
library(xtable)

dat12$geneID<-gsub('/', ', ', dat12$geneID)# Category,ID,term,Genes,adj_pval；
colnames(dat12)
dat12<-dat12[,c(1:3,9,7)]
colnames(dat12)<- c('Category','ID','term','Genes','adj_pval')
diff23<-diff23[,1:2]
colnames(diff23)<-c("ID","logFC") 
dat12$Category<-as.character(dat12$Category)
dat12$ID<-as.character(dat12$ID)
dat12$term<-as.character(dat12$term)
diff23$ID<-as.character(diff23$ID)
dat12<-as.data.frame(dat12)
diff23<-as.data.frame(diff23)
circ<-circle_dat(dat12,diff23) 

chord<-chord_dat(data = circ,genes =diff23, process= unique(circ$term) )
#chord<-chord[1:10,1:10]
chord<-as.matrix(chord)
GOChord(chord, gene.order = 'logFC',gene.size = 4,space=0.1,gene.space = 0.2,
        border.size=0.00001, 
        ribbon.col=brewer.pal(10 ,"Set3"),process.label = 10)

#Fig. 2f
library('GSVA')
library('GSVAdata')
library('org.Hs.eg.db')
library(genefilter)
library(Biobase)
library('clusterProfiler')
library("limma")
#install.packages("pheatmap")
library(pheatmap)
#data(c2BroadSets)
h<- clusterProfiler::read.gmt("c2.cp.kegg.v7.4.symbols.gmt")
kegg_list = split(h$gene, h$ont)
diff2<- read.csv("OSCC_exp-pjk-20200805.csv",sep=',')
rownames(diff2)<- diff2$Gene
diff2$Gene<- NULL
diff2<-as.matrix(diff2) 

kegg2 <- gsva(diff2, kegg_list, kcdf="Gaussian",method = "gsva",parallel.sz=12)
kegg2<-as.data.frame(kegg2)
kegg2$pathway<-rownames(kegg2)

write.table(kegg2,'./ gsva_h-TCGA.csv',col.names = T,row.names = F,sep = ",")


#GSVA differential analysis

library(limma)
setwd("GSVA/GEAV-KEGG-TCGA")
gsva_h<- read.csv('gsva_h-TCGA.csv',sep=',')
rownames(gsva_h) <-gsva_h$pathway
gsva_h$pathway<- NULL
gsva_h<- as.data.frame(t(gsva_h))
gsva_h$sample<- rownames(gsva_h)
a<-gsva_h$sample


Clin1<- read.csv("clinical-oscc-PAM-top 50.csv",sep=',')
Clin1$grade[Clin1$grade =='Unknown'] <-NA
Clin1<-Clin1[order(Clin1$grade),]
Clin1<-Clin1[-c(334:342),]
table(Clin1$grade)


b<-Clin1$sample
c<-intersect(a,b)
gsva_h$sample<- rownames(gsva_h)

gsva_h1<-gsva_h[,c(187,186)]
Clin = merge(Clin1,gsva_h1, by='sample') 
Clin<-Clin[,-28]
colnames(Clin)
Clin2<-Clin[,c(1,2)]
gsva_h1 = merge(gsva_h,Clin2, by='sample') 
gsva_h1<-gsva_h1[,-188]
rownames(gsva_h1)<- gsva_h1$sample
gsva_h1$sample<- NULL


gsva_h2<- as.data.frame(t(gsva_h1))
#gsva_h3<-apply(gsva_h2,2,as.numeric)

Clin$grade <-as.character(Clin$grade)

#model design
Clin$grade[Clin$grade== 'Grade III-IV'] <- 'poor'
Clin$grade[Clin$grade== 'Grade I-II'] <- 'high'

Group = factor(Clin$grade,levels=c('poor','high'))
design = model.matrix(~0+Group)
colnames(design) <- c('poor','high')

design


#linear model fitness
fit <- lmFit(gsva_h2, design)

#generate contrast matrix
contrast.matrix <- makeContrasts(poor- high, #1
                                 
                                 levels=design)


#constrast model fit 
fit2 <- contrasts.fit(fit, contrast.matrix)

#bayes model 
fit2 <- eBayes(fit2)

#get DEGs
diff = topTable(fit2,adjust.method="fdr",coef=1,p.value=1,
                lfc=log(1,2),number=50000,sort.by = 'logFC')
#View(diff)
diff$pathway<- rownames(diff)


write.csv(diff,'GSVA KEGG analysis in TCGA( grade(poor vs high)-DEGs-20210911).csv',row.names=T,quote=F)

#pheatmap-select differential pathway
library(pheatmap)
setwd("GSVA/GEAV-KEGG-TCGA")
Clin<- read.csv('GSVA KEGG analysis in TCGA( grade(high vs poor)-select pathway-DEGs-20210911).csv',header= T,sep=',')

Clin<-Clin[,c(1,2)]

dat1<- read.csv('GSVA KEGG with grade,cluster(averag)-20210901.csv',header= T,sep=',')
Clin1<-merge(dat1,Clin, by='pathway')
rownames(Clin1)<- Clin1$pathway
Clin1$pathway<- NULL
Clin1<-Clin1[,-5]
write.csv(Clin1,'TCGA-pathway merge data.csv')

library(pheatmap)
setwd("GSVA/GEAV-KEGG-TCGA")
Clin1<- read.csv("TCGA-pathway merge data - new.csv",sep=',')
rownames(Clin1) <-Clin1$pathway
Clin1$pathway<- NULL
Clin3<-Clin1[,1:2]

pheatmap(Clin3, cluster_rows = FALSE, cluster_cols = FALSE, color = colorRampPalette(c("navy", "white", "firebrick3"))(50),
         border=FALSE,fontsize_row=18,fontsize_col=18, gaps_row= c(5),cellwidth=40,cellheight=60)

#Fig. 3a
library(ggfortify)
library(sva)
library(mgcv)
library(nlme)
library(genefilter)
library(BiocParallel)
library(limma)
library(FactoMineR)# 
library(factoextra) 
library(bladderbatch)
#DEGs in GEO database
setwd("D:/SEER-OSCC-R-1013/GEO-cell line data")
#import expression data
datExpr1 = read.table("./GSE98942-um1,um2/Expdata.txt",header=T,sep="\t")
datExpr3 = read.table("./GSE146483-HSC2，3,4/Expdata.txt",header=T,sep="\t")

#import phenotype data
pheno1 = read.table('./GSE98942-um1,um2/target.txt',header=T,sep="\t")
pheno3 = read.table('./GSE146483-HSC2，3,4/target.txt',header=T,sep="\t")
pheno = rbind(pheno1, pheno3)
View(pheno)
anno1 = read.csv("./GSE98942-um1,um2/annotation.csv",head=T)
anno3 = read.csv("./GSE146483-HSC2，3,4/annotation.csv",head=T)

datExpr1$gene = anno1$GENE_SYMBOL[match(rownames(datExpr1), anno1$ID)]
datExpr3$gene = anno3$GENE_SYMBOL[match(rownames(datExpr3), anno3$ID)]

datExpr1 = datExpr1[datExpr1$gene != '',]
datExpr3 = datExpr3[datExpr3$gene != '',]

datExpr3 = datExpr3[!grepl('///', datExpr3$gene),]
datExpr1 = datExpr1[!grepl('///', datExpr1$gene),]

datExpr1 = datExpr1[!duplicated(datExpr1$gene),]
datExpr3 = datExpr3[!duplicated(datExpr3$gene),]

rownames(datExpr1) = datExpr1$gene
rownames(datExpr3) = datExpr3$gene

dat = merge(datExpr1, datExpr3, all.x = T)

dat = dat[!duplicated(dat$gene),]
rownames(dat) = dat$gene
dat$gene = NULL
#dat[is.na(dat)]=0
dat=dat+1
hist(dat[,4])
dat = log2(dat)
hist(dat[,4])
dat1<-na.omit(dat)
##PCA
dat2=as.data.frame(t(dat1))
palform<- as.character(pheno$platform)
dat.pca <- PCA(dat2, graph = FALSE)
fviz_pca_ind(dat.pca,
             geom.ind = "point", # show points only (nbut not "text")
             col.ind = palform, # color by groups
             #palette = c("#00AFBB", "#E7B800"),
             addEllipses = TRUE, # Concentration ellipses
             legend.title = "Groups"
)

par(mfrow=c(2,1))
boxplot(dat1,las = 2)

# imma-removeBatchEffect
ex_b_limma <- removeBatchEffect(dat1,
                                batch = pheno$platform)
boxplot(ex_b_limma,las = 2)
dist_mat1 <- dist(t(ex_b_limma))
clustering <- hclust(dist_mat1, method = "complete")
plot(clustering,labels = colnames(ex_b_limma))

###PCA
pca1 =as.data.frame(t(ex_b_limma))
dat.pca <- PCA(pca1, graph = FALSE)
fviz_pca_ind(dat.pca,
             geom.ind = "point", # show points only (nbut not "text")
             col.ind = palform, # color by groups
             #palette = c("#00AFBB", "#E7B800"),
             addEllipses = TRUE, # Concentration ellipses
             legend.title = "Groups"
)


dat3<-as.data.frame(t(ex_b_limma))

save(ex_b_limma,x_merge,x_merge1,batch1,file = "ex_b_limma.Rdata")

ex_b_limma<- ex_b_limma[,c(6,9,1:5,7,8,10)]
library(car)
Pvalue<-c(rep(0,nrow(ex_b_limma)))

log2_FC<-c(rep(0,nrow(ex_b_limma)))

ex_b_limma<-as.data.frame(ex_b_limma)

for(i in 1:nrow(ex_b_limma)){
  
  if(sd(ex_b_limma[i, 1:6])==0&&ex_b_limma[i,7:10]==0){
    
    Pvalue <- "NA"
    
    log2_FC<- "NA"
    
  }else{
    
    y=t.test(as.numeric(ex_b_limma[i,1:6]),as.numeric(ex_b_limma[i,7:10]))
    0
    Pvalue[i]<-y$p.value
    
    log2_FC[i]<-log2((mean(as.numeric(ex_b_limma[i,1:6]))+0.001)/(mean(as.numeric(ex_b_limma[i,7:10]))+0.001))
    
  }
  
}

fdr=p.adjust(Pvalue, "BH")

out<-cbind(ex_b_limma,log2_FC,Pvalue,fdr)

out1<-out[,11:13]
out1$gene<- rownames(out1)

write.table(out1,file="ttest.out-(HSC234,UM12, log2(dat+1))-20210822.txt",quote=FALSE,sep="\t")

#GO 
library(R.utils)
library(BiocManager)
library(clusterProfiler)
library(topGO)
library(Rgraphviz)
library(pathview)
library(org.Hs.eg.db)
library(limma)
library(ggplot2)
setwd("GEO/LOG2")
dat2<- read.table("ttest.out-(HSC234,UM12, log2(dat+1))-20210822.txt",sep='\t',header=T)
dat3<- subset(dat2 , dat2$log2_FC > 0 & dat2$Pvalue < 0.05)  #
dat4<- subset(dat2 , dat2$log2_FC < 0 & dat2$Pvalue < 0.05) #

#diff23<-rbind(dat3,dat4)

GOKEGG <- function(geneset=NULL, output, filename, trend) {
  if(output == 'GO') {
    ego <- enrichGO(gene          = bitr(geneset,
                                         fromType = 'SYMBOL', toType = 'ENTREZID',
                                         OrgDb = 'org.Hs.eg.db')$ENTREZID,
                    # universe      = names(geneList),
                    OrgDb         = org.Hs.eg.db,
                    ont           = "ALL",
                    pAdjustMethod = "BH",
                    pvalueCutoff  = 0.5,
                    qvalueCutoff  = 0.5,
                    readable      = TRUE)
    GO_ <- as.data.frame(ego)
    write.table(GO_, paste0(filename, '_GO_', trend, '(GEO all DEGs).txt'), 
                sep = '\t', row.names = F, quote = F)
    return(GO_)}
  if(output == 'KEGG') {
    kk <- enrichKEGG(gene          = bitr(geneset,
                                          fromType = 'SYMBOL', toType = 'ENTREZID',
                                          OrgDb = 'org.Hs.eg.db')$ENTREZID,
                     organism      = 'hsa',
                     pAdjustMethod = "BH",
                     pvalueCutoff  = 0.5,
                     qvalueCutoff  = 0.5)
    KEGG_ <- as.data.frame(setReadable(kk, OrgDb = org.Hs.eg.db, keyType="ENTREZID"))
    write.table(KEGG_, paste0(filename, '_KEGG_', trend, '(GEO all DEGs).txt'), 
                sep = '\t', row.names = F, quote = F)
    return(KEGG_)}
}

DrawEnrich <- function(dat, type, top.number = 5, col="blue", trend){
  if(type == 'BP') {tit <- 'Biological Process of '}
  if(type == 'CC') {tit <- 'Cellular Component of '}
  if(type == 'MF') {tit <- 'Molecular Function of '}
  if(type == 'KEGG') {tit <- 'KEGG Pathway of '}
  if(type == 'GO') {tit <- 'Gene Ontology Enrichment of '}
  dat1 = dat[c(1:top.number),]
  dat1$Description = capitalize(dat1$Description)
  dat1 = dat1[order(dat1$p.adjust),,drop=F]
  dat1$Description = factor(dat1$Description,levels=dat1$Description[length(dat1$Description):1])
  dat1$PValue = -log10(dat1$p.adjust)
  dat1$GeneRatio <- dat1$Count / as.numeric(gsub('^.*/', '', dat1$GeneRatio))
  p = ggplot(dat1,aes(GeneRatio, Description)) +
    geom_point(aes(size=Count,colour=PValue)) +
    scale_colour_gradient(low=col,high="red") + 
    labs(colour=expression(-log[10]("P Value")),size="Gene counts",  
         x="Gene Ratio",y="",title=paste0(tit, trend, '-regulated Genes(GEO)')) +
    theme_bw() + theme(axis.text.x = element_text(size = 14), 
                       axis.text.y=element_text(size=14), 
                       plot.title=element_text(size=20, hjust=1), 
                       
                       legend.text=element_text(size=12), legend.title=element_text(size=14),
                       axis.title.x=element_text(size=18)) +
    scale_x_continuous(limits = c(0,max(dat1$GeneRatio) * 1.2)) 
  return(p)
}
GO <- GOKEGG(geneset=dat3$Gene, output='GO',filename='sqh2021','UP')

GOKEGG <- function(geneset=NULL, output, filename, trend) {
  if(output == 'GO') {
    ego <- enrichGO(gene          = bitr(geneset,
                                         fromType = 'SYMBOL', toType = 'ENTREZID',
                                         OrgDb = 'org.Hs.eg.db')$ENTREZID,
                    # universe      = names(geneList),
                    OrgDb         = org.Hs.eg.db,
                    ont           = "ALL",
                    pAdjustMethod = "BH",
                    pvalueCutoff  = 0.5,
                    qvalueCutoff  = 0.5,
                    readable      = TRUE)
    GO_ <- as.data.frame(ego)
    write.table(GO_, paste0(filename, '_GO_', trend, '(GEO all DEGs).txt'), 
                sep = '\t', row.names = F, quote = F)
    return(GO_)}
  if(output == 'KEGG') {
    kk <- enrichKEGG(gene          = bitr(geneset,
                                          fromType = 'SYMBOL', toType = 'ENTREZID',
                                          OrgDb = 'org.Hs.eg.db')$ENTREZID,
                     organism      = 'hsa',
                     pAdjustMethod = "BH",
                     pvalueCutoff  = 0.5,
                     qvalueCutoff  = 0.5)
    KEGG_ <- as.data.frame(setReadable(kk, OrgDb = org.Hs.eg.db, keyType="ENTREZID"))
    write.table(KEGG_, paste0(filename, '_KEGG_', trend, '(GEO all DEGs).txt'), 
                sep = '\t', row.names = F, quote = F)
    return(KEGG_)}
}

DrawEnrich <- function(dat, type, top.number = 5, col="blue", trend){
  if(type == 'BP') {tit <- 'Biological Process of '}
  if(type == 'CC') {tit <- 'Cellular Component of '}
  if(type == 'MF') {tit <- 'Molecular Function of '}
  if(type == 'KEGG') {tit <- 'KEGG Pathway of '}
  if(type == 'GO') {tit <- 'Gene Ontology Enrichment of '}
  dat1 = dat[c(1:top.number),]
  dat1$Description = capitalize(dat1$Description)
  dat1 = dat1[order(dat1$p.adjust),,drop=F]
  dat1$Description = factor(dat1$Description,levels=dat1$Description[length(dat1$Description):1])
  dat1$PValue = -log10(dat1$p.adjust)
  dat1$GeneRatio <- dat1$Count / as.numeric(gsub('^.*/', '', dat1$GeneRatio))
  p = ggplot(dat1,aes(GeneRatio, Description)) +
    geom_point(aes(size=Count,colour=PValue)) +
    scale_colour_gradient(low=col,high="red") + 
    labs(colour=expression(-log[10]("P Value")),size="Gene counts",  
         x="Gene Ratio",y="",title=paste0(tit, trend, '-regulated Genes(GEO)')) +
    theme_bw() + theme(axis.text.x = element_text(size = 14), 
                       axis.text.y=element_text(size=14), 
                       plot.title=element_text(size=20, hjust=1), 
                       
                       legend.text=element_text(size=12), legend.title=element_text(size=14),
                       axis.title.x=element_text(size=18)) +
    scale_x_continuous(limits = c(0,max(dat1$GeneRatio) * 1.2)) 
  return(p)
}

GO <- GOKEGG(geneset=dat4$Gene, output='GO',filename='sqh2021','DOWN')
#Fig. 3b
library(ggplot2)
datA<-read.table("A-72-3000-90-PDD.csv",header = T,sep =",",stringsAsFactors = F)
datB<-read.table("C-72-3000-90-PDD.csv",header = T,sep =",",stringsAsFactors = F)
datC<-read.table("UM1-72-3000-90-PDD.csv",header = T,sep =",",stringsAsFactors = F)
datD<-read.table("UM2-72-3000-90-PDD.csv",header = T,sep =",",stringsAsFactors = F)
mat<-matrix(NA,nrow = 9,ncol = 5)
mat<-as.data.frame(mat)
colnames(mat)<-c("con","HSC2","HSC4","UM1",'UM2')
mat$con<-c(log10(300),log10(100),log10(30),log10(10),log10(3),log10(1),log10(0.3),log10(0.1),log10(0.03))

###
temp<-datA
temp<-temp[2:7,2:12]
for (i in 1:11) {
  temp[,i]<-ifelse(temp[,i]==max(temp[,i])|temp[,i]==min(temp[,i]),NA,temp[,i])
}
for (i in 1:9) {
  mat$HSC2[i]<-(mean(na.omit(temp[,i]))-mean(na.omit(temp[,11])))/(mean(na.omit(temp[,10]))-mean(na.omit(temp[,11])))
}

temp<-datB
temp<-temp[2:7,2:12]
for (i in 1:11) {
  temp[,i]<-ifelse(temp[,i]==max(temp[,i])|temp[,i]==min(temp[,i]),NA,temp[,i])
}
for (i in 1:9) {
  mat$HSC4[i]<-(mean(na.omit(temp[,i]))-mean(na.omit(temp[,11])))/(mean(na.omit(temp[,10]))-mean(na.omit(temp[,11])))
}

temp<-datC
temp<-temp[2:7,2:12]
for (i in 1:11) {
  temp[,i]<-ifelse(temp[,i]==max(temp[,i])|temp[,i]==min(temp[,i]),NA,temp[,i])
}
for (i in 1:9) {
  mat$UM1[i]<-(mean(na.omit(temp[,i]))-mean(na.omit(temp[,11])))/(mean(na.omit(temp[,10]))-mean(na.omit(temp[,11])))
}

temp<-datD
temp<-temp[2:7,2:12]
for (i in 1:11) {
  temp[,i]<-ifelse(temp[,i]==max(temp[,i])|temp[,i]==min(temp[,i]),NA,temp[,i])
}

for (i in 1:9) {
  mat$UM2[i]<-(mean(na.omit(temp[,i]))-mean(na.omit(temp[,11])))/(mean(na.omit(temp[,10]))-mean(na.omit(temp[,11])))
}

grs<-matrix(NA, ncol = 3,nrow = 36) 
grs<-as.data.frame(grs)
colnames(grs)<-c("cell_line","logcon","cell")

grs$cell_line<-rep(c("HSC2","HSC4","UM1",'UM2'),each=9)
grs$logcon<-rep(mat$con,n=4)
grs$cell<-c(mat$HSC2,mat$HSC4,mat$UM1,mat$UM2)

ggplot(grs, aes(x=logcon, y=cell, colour=cell_line)) + 
  geom_smooth(aes(group=cell_line),size=3.5,se=FALSE,method=loess) +
  geom_point(size=3)+ theme_bw() + 
  theme(axis.text.x=element_text(size=25,face='bold'),axis.title.x=element_text(''),
        legend.title=element_text(size=25,face="italic"),
        axis.text.y=element_text(size=25,face='bold'),legend.text=element_text(size=25,face="bold")) +
  theme(legend.position="right") + labs(title = "Chemotherapeutic resistance tests (PDD)")
#Fig. 3c
rm(list = ls())
library(ggplot2)
datA<-read.table("A-72-3000-90-5FU.csv",header = T,sep =",",stringsAsFactors = F)
datB<-read.table("C-72-3000-90-5FU.csv",header = T,sep =",",stringsAsFactors = F)
datC<-read.table("UM1-72-3000-90-5FU.csv",header = T,sep =",",stringsAsFactors = F)
datD<-read.table("UM2-72-3000-90-5FU.csv",header = T,sep =",",stringsAsFactors = F)
mat<-matrix(NA,nrow = 9,ncol = 5)
mat<-as.data.frame(mat)
colnames(mat)<-c("con","HSC2","HSC4","UM1",'UM2')
mat$con<-c(log10(300),log10(100),log10(30),log10(10),log10(3),log10(1),log10(0.3),log10(0.1),log10(0.03))

###
temp<-datA
temp<-temp[2:7,2:12]
for (i in 1:11) {
  temp[,i]<-ifelse(temp[,i]==max(temp[,i])|temp[,i]==min(temp[,i]),NA,temp[,i])
}
for (i in 1:9) {
  mat$HSC2[i]<-(mean(na.omit(temp[,i]))-mean(na.omit(temp[,11])))/(mean(na.omit(temp[,10]))-mean(na.omit(temp[,11])))
}

temp<-datB
temp<-temp[2:7,2:12]
for (i in 1:11) {
  temp[,i]<-ifelse(temp[,i]==max(temp[,i])|temp[,i]==min(temp[,i]),NA,temp[,i])
}
for (i in 1:9) {
  mat$HSC4[i]<-(mean(na.omit(temp[,i]))-mean(na.omit(temp[,11])))/(mean(na.omit(temp[,10]))-mean(na.omit(temp[,11])))
}

temp<-datC
temp<-temp[2:7,2:12]
for (i in 1:11) {
  temp[,i]<-ifelse(temp[,i]==max(temp[,i])|temp[,i]==min(temp[,i]),NA,temp[,i])
}
for (i in 1:9) {
  mat$UM1[i]<-(mean(na.omit(temp[,i]))-mean(na.omit(temp[,11])))/(mean(na.omit(temp[,10]))-mean(na.omit(temp[,11])))
}

temp<-datD
temp<-temp[2:7,2:12]
for (i in 1:11) {
  temp[,i]<-ifelse(temp[,i]==max(temp[,i])|temp[,i]==min(temp[,i]),NA,temp[,i])
}
for (i in 1:9) {
  mat$UM2[i]<-(mean(na.omit(temp[,i]))-mean(na.omit(temp[,11])))/(mean(na.omit(temp[,10]))-mean(na.omit(temp[,11])))
}

grs<-matrix(NA,ncol = 3,nrow = 36) 
grs<-as.data.frame(grs)
colnames(grs)<-c("cell_line","logcon","cell")

grs$cell_line<-rep(c("HSC2","HSC4","UM1",'UM2'),each=9)
grs$logcon<-rep(mat$con,n=4)
grs$cell<-c(mat$HSC2,mat$HSC4,mat$UM1,mat$UM2)

ggplot(grs, aes(x=logcon, y=cell, colour=cell_line)) + 
  geom_smooth(aes(group=cell_line),size=3.5,se=FALSE,method=loess) +
  geom_point(size=3)+ theme_bw() + 
  theme(axis.text.x=element_text(size=25,face='bold'),axis.title.x=element_text(''),
        legend.title=element_text(size=25,face="italic"),
        axis.text.y=element_text(size=25,face='bold'),legend.text=element_text(size=25,face="bold")) +
  theme(legend.position="top") + labs(title = "Chemotherapeutic resistance tests (5FU)")
#Fig. 4a
###KEGG,GO
library(R.utils)
library(BiocManager)
library(clusterProfiler)
library(topGO)
library(Rgraphviz)
library(pathview)
library(org.Hs.eg.db)
library(limma)
library(ggplot2)


diff2<- read.csv("Grade gene-tcga.csv",sep=',')
dat3<- subset(diff2 , diff2$logFC> 1 & diff2$adj.P.Val < 0.05)
dat4<- subset(diff2 , diff2$logFC< -1 & diff2$adj.P.Val < 0.05)

dat3<- dat3[order(-dat3$logFC),]
dat33<-dat3[1:50,]
dat4<- dat4[order(dat4$logFC),]
dat44<-dat4[1:50,]
#TCGA,GO
GOKEGG <- function(geneset=NULL, output, filename, trend) {
  if(output == 'GO') {
    ego <- enrichGO(gene          = bitr(geneset,
                                         fromType = 'SYMBOL', toType = 'ENTREZID',
                                         OrgDb = 'org.Hs.eg.db')$ENTREZID,
                    # universe      = names(geneList),
                    OrgDb         = org.Hs.eg.db,
                    ont           = "ALL",
                    pAdjustMethod = "BH",
                    pvalueCutoff  = 0.5,
                    qvalueCutoff  = 0.5,
                    readable      = TRUE)
    GO_ <- as.data.frame(ego)
    write.table(GO_, paste0(filename, '_GO_', trend, '(TCGA-top 50).txt'), 
                sep = '\t', row.names = F, quote = F)
    return(GO_)}
  if(output == 'KEGG') {
    kk <- enrichKEGG(gene          = bitr(geneset,
                                          fromType = 'SYMBOL', toType = 'ENTREZID',
                                          OrgDb = 'org.Hs.eg.db')$ENTREZID,
                     organism      = 'hsa',
                     pAdjustMethod = "BH",
                     pvalueCutoff  = 0.5,
                     qvalueCutoff  = 0.5)
    KEGG_ <- as.data.frame(setReadable(kk, OrgDb = org.Hs.eg.db, keyType="ENTREZID"))
    write.table(KEGG_, paste0(filename, '_KEGG_', trend, '(TCGA-top50).txt'), 
                sep = '\t', row.names = F, quote = F)
    return(KEGG_)}
}

DrawEnrich <- function(dat, type, top.number = 5, col="blue", trend){
  if(type == 'BP') {tit <- 'Biological Process of '}
  if(type == 'CC') {tit <- 'Cellular Component of '}
  if(type == 'MF') {tit <- 'Molecular Function of '}
  if(type == 'KEGG') {tit <- 'KEGG Pathway of '}
  if(type == 'GO') {tit <- 'Gene Ontology Enrichment of '}
  dat1 = dat[c(1:top.number),]
  dat1$Description = capitalize(dat1$Description)
  dat1 = dat1[order(dat1$p.adjust),,drop=F]
  dat1$Description = factor(dat1$Description,levels=dat1$Description[length(dat1$Description):1])
  dat1$PValue = -log10(dat1$p.adjust)
  dat1$GeneRatio <- dat1$Count / as.numeric(gsub('^.*/', '', dat1$GeneRatio))
  p = ggplot(dat1,aes(GeneRatio, Description)) +
    geom_point(aes(size=Count,colour=PValue)) +
    scale_colour_gradient(low=col,high="red") + 
    labs(colour=expression(-log[10]("P Value")),size="Gene counts",  
         x="Gene Ratio",y="",title=paste0(tit, trend, '-regulated Genes (TCGA,top 50)')) +
    theme_bw() + theme(axis.text.x = element_text(size = 14), axis.text.y=element_text(size=12), 
                       plot.title=element_text(size=20, hjust=1), 
                       legend.text=element_text(size=12), legend.title=element_text(size=14),
                       axis.title.x=element_text(size=18)) +
    scale_x_continuous(limits = c(0,max(dat1$GeneRatio) * 1.2)) 
  return(p)
}
GO <- GOKEGG(geneset=dat33$Gene, output='GO',filename='sqh2021','UP')
GO <- GOKEGG(geneset=dat44$Gene, output='GO',filename='sqh2021','DOWN')

library('GOplot')
library(ggplot2)
library(ggdendro)
library(gridExtra)
library(RColorBrewer)
library(clusterProfiler)
#library(org.Hs.eg.db)
library(Hmisc)

setwd("TCGA-KEGG,GO/KEGG,GO ,cutoff 0.5-20210821/KEGG,GO-TOP 50")

dat1<-read.table("sqh2021_GO_DOWN(TCGA)-top50 .txt",sep='\t',header = T)
dat11<-subset(dat1,dat1$p.adjust<0.05)
dat11<-dat11[order(dat11$p.adjust,decreasing = F),]
dat11<-dat11[1:5,]

dat2<-read.table("sqh2021_GO_UP(TCGA)-top50.txt",sep='\t',header = T)
dat22<-subset(dat2,dat2$p.adjust<0.05)
dat22<-dat22[order(dat22$p.adjust,decreasing = F),]
dat22<-dat22[1:5,]

dat12<-rbind(dat11,dat22)
library(PerformanceAnalytics)
library(corrplot)
library(psych)
library(xtable)

dat12$geneID<-gsub('/', ', ', dat12$geneID)# Category,ID,term,Genes,adj_pval；
colnames(dat12)
dat12<-dat12[,c(1:3,9,7)]
colnames(dat12)<- c('Category','ID','term','Genes','adj_pval')
diff23<-diff23[,1:2]
colnames(diff23)<-c("ID","logFC") 
dat12$Category<-as.character(dat12$Category)
dat12$ID<-as.character(dat12$ID)
dat12$term<-as.character(dat12$term)
diff23$ID<-as.character(diff23$ID)
dat12<-as.data.frame(dat12)
diff23<-as.data.frame(diff23)
#plot_data=list(DA=dat12, GE= diff23)
#circ_2= data.frame()

#circ2<-circle_dat(plot_data$DA,plot_data$DA) 

circ<-circle_dat(dat12,diff23) 

chord<-chord_dat(data = circ,genes =diff23, process= unique(circ$term) )
#chord<-chord[1:10,1:10]
GOChord(chord, gene.order = 'logFC',gene.size = 4,space=0.005,gene.space = 0.22,
        border.size=0.00001, 
        ribbon.col=brewer.pal(10 ,"Set3"),process.label = 10)
#Fig. 4b
library(tidyverse)  # data manipulation
library(cluster)    # clustering algorithms
library(factoextra)
library(ggplot2)
library(survival)
library(survminer)
library(xtable)
library(ggpubr)
set.seed(20210825)
diff2<- read.csv("Grade gene-tcga.csv",sep=',')
dat3<- subset(diff2 , diff2$logFC > 0 & diff2$adj.P.Val < 0.05)
dat4<- subset(diff2 , diff2$logFC< 0 & diff2$adj.P.Val < 0.05)

dat3<- dat3[order(-dat3$logFC),]
dat33<-dat3[1:50,]
dat4<- dat4[order(dat4$logFC),]
dat44<-dat4[1:50,]
mergegen<-rbind(dat44,dat33)
#mergegen<-rbind(dat4,dat3)
mergegen<-mergegen[,1:2]

diff3<-read.csv("OSCC_exp-pjk-20200805.csv",sep=',',header=T)

#diff3[diff3 == 0] <- NA
#diff4<-na.omit(diff3)

diff4<-merge(mergegen,diff3, by='Gene')
a<-diff4[1:10,1:10]
diff4<-diff4[,-2]

rownames(diff4)<- diff4$Gene
diff4$Gene<- NULL

#class(diff5)
diff5<- t(diff4)

kp<-pam(diff5,k=2,metric="euclidean",stand = TRUE)
kp$clustering
kp$clustering[kp$clustering == 1] <-'II' ##PDMG
kp$clustering[kp$clustering == 2] <-'I' ##WDMG

p<- fviz_cluster(kp, data = diff5, geom = 'point',
                 pointsize=4,  
                 
                 legend.title="Cluster"
)
p+theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

color = c('red','blue') 

res = data.frame(sample=colnames(diff4), cluster=kp$clustering)
table(res$cluster)
Clin1<-read.csv("clinical_OSCC-pjk-20200805.csv",sep=',',header=T)
Clin = merge(Clin1, res, by='sample') 
table(Clin$cluster)
write.csv(Clin, 'clinical-oscc-PAM-top 50.csv')
#Fig. 4c

Clin<- read.csv('clinical-oscc-PAM-top 50.csv',sep=',',header= T)
Clin[Clin == 'Unknown'] <- NA
#colnames(Clin)
library(ggstatsplot) 
Clin$newgrade[Clin$grade=='Grade I-II'] <- 'I-II'
Clin$newgrade[Clin$grade=='Grade III-IV'] <- 'III-IV'
table(Clin$newgrade,Clin$cluster)  
ggbarstats(Clin, cluster,newgrade,
           xlab='Grade',
           #  legend = 'top',
           legend.title="Cluster",
           palette = 'set2' )
p+theme(#panel.grid.major = element_blank(),
  #panel.grid.minor = element_blank(),
  text=element_text(face='bold',size=16,color = 'black'))
#axis.text =element_text(size=12,color ='black'), 
#axis.text.y=element_text(size=12,color ='black')))
table(Clin$cluster)
#Fig. 4d

Clin<- read.csv('clinical-oscc-PAM-top 50.csv',sep=',',header= T)
Clin[Clin == 'Unknown'] <- NA
#colnames(Clin) 
Clin$OS.time.5.years <- as.numeric(Clin$OS.time.5.years)
Clin$OS.5.years<- as.numeric(Clin$OS.5.years)
sur <- Surv(time = Clin$OS.time.5.years, event = Clin$OS.5.years, type = 'right')
table(Clin$Chemotherapy,Clin$OS.5.years)
table(Clin$cluster)
table(Clin$OS.5.years)
prog <- function() {
  title <- 'Overall Survival'
  sur.result <<- survfit(sur~cluster, data=Clin)
  g <- ggsurvplot(sur.result, data=Clin, conf.int=F, pval=T, 
                  pval.method=T, 
                  pval.method.size=15, 
                  pval.method.coord=c(0,0.5),
                  pval.size = 15, 
                  ylim = c(0.3,1),
                  xlim = c(0,60),
                  pval.coord=c(0,0.4),
                  #font.main = c(16, "bold", "darkblue"), 
                  font.main = 40,
                  font.x = 30,
                  font.y = 0,
                  font.tickslab=30,
                  font.legend=35,
                  #pval.method.coord=c(1,0.1)
                  #font.main = c(16, "bold", "darkblue"), 
                  ggtheme=theme_bw()+
                    theme(axis.title.x=element_text(size=12),
                          axis.title.y=element_text(size=12),
                          axis.text =element_text(size=12,color ='black'), 
                          axis.text.y=element_text(size=12,color ='black'),
                          legend.text=element_text(size=15),
                          panel.background=element_blank(), 
                          axis.line=element_line(colour='black'),
                          legend.title=element_text(size=18), 
                          #text=element_text(face='bold',size=12),
                          panel.grid.major = element_blank(), 
                          panel.grid.minor = element_blank(),
                          panel.border = element_rect(fill=NA,color="black", size=3, linetype="solid")),
                  censor=F, 
                  censor.shape=3, 
                  censor.size=6, 
                  risk.table=F, 
                  #palette=c("blue", "dark green", "red"),  
                  palette="lancet",
                  legend.size= 16,
                  size = 4,  
                  #title=(hjust=0.5), 
                  legend ='top',
                  legend.labs=c('I', 'II'), 
                  legend.title="Cluster",
                  title=paste0(title, ' for OSCC of Cluster'), 
                  xlab='Time(months)')
  
  
  return(g)
}
print(prog())
tiff(paste0('TCGA-Overall Survival for OSCC of Cluster','(TCGA-top 50 gens-2 type-PAM)','.tiff'),
     width=12,height=8, units="in", compression="lzw", res=150)
print(prog())
dev.off()
#Fig. 4e
library(factoextra)
library(survival)
library(survminer)
library(xtable)
library(ggpubr)
library(ggplot2)

Clin<- read.csv('clinical-oscc-PAM-top 50.csv',sep=',',header= T)
Clin[Clin == 'Unknown'] <- NA
#colnames(Clin) 
Clin$OS.time.5.years <- as.numeric(Clin$OS.time.5.years)
Clin$OS.5.years<- as.numeric(Clin$OS.5.years)
sur <- Surv(time = Clin$OS.time.5.years, event = Clin$OS.5.years, type = 'right')
table(Clin$Chemotherapy,Clin$OS.5.years)
table(Clin$cluster)
table(Clin$OS.5.years)

#####cox regression
colnames(Clin)
colnames(Clin)[26]<-'Cluster'
colnames(Clin)

Clin$Sex<- as.character(Clin$Sex)
Clin$stage<-as.character(Clin$stage)
Clin$Chemotherapy <- as.character(Clin$Chemotherapy)
Clin$Radiation <- as.character(Clin$Radiation)
Clin$Cluster <- as.character(Clin$Cluster)

sur <- Surv(time = Clin$OS.time.5.years, event = Clin$OS.5.years, type = 'right')
cox <- coxph(sur~Age+Sex+Cluster+stage+Chemotherapy
             +Radiation, data=Clin)
cox.sum1 <- xtable(summary(cox)$coefficient)
cox.sum2 <- xtable(summary(cox)$conf.int)
type <-c("Others", "Others","PDMG","Others",'Others',"Others")
type=data.frame(type)
rownames(type)<-rownames(cox.sum1)
cox.sum <- cbind(cox.sum1, cox.sum2, type)
rownames(cox.sum) <- c('Age','Male', 'PDMG', 'Stage III-IV',
                       'Chemotherapy',  
                       'Radiation')
colnames(cox.sum)[c(2,8:9,10)] <- c('HR','lower','upper','type')
cox.sum$var <- rownames(cox.sum)
pd <- position_dodge(0.1)
g <- ggplot(cox.sum, aes(x=var, y=HR,color = type)) + 
  geom_point(size = 3) + 
  #ggtitle(paste0('Cox Propotional Hazard Regression Model')) +
  geom_errorbar(aes(ymin=lower, ymax=upper), size= 1.8,width=.2, position=pd) +
  theme_bw() + theme(panel.grid=element_blank())  +
  geom_line(position=pd, size = 1.8) + xlab('Variables') + ylab('Hazard Ratio (HR)') +
  geom_hline(yintercept = 1, linetype = 'dashed', color = "blue", size = 1) +
  theme(axis.text.x = element_text(angle = 90, hjust = 0.5, 
                                   vjust = 0.6, size = 15,color= 'black',face='bold'),
        axis.title.y = element_text(angle = 90, hjust = 0.5, 
                                    vjust = 0.6, size = 15,color= 'black',face='bold'),
        axis.text.y = element_text(angle = 90, hjust = 0.5, 
                                   vjust = 0.6, size = 15,color= 'black',face='bold'))  +
  geom_point(position=pd)+scale_colour_manual(values= c('black','red'))+
  theme(legend.title =element_blank())

g
write.table(cox.sum, paste0('TCGA-cluster-cox of all (OS,)-20210909', '.txt'), 
            quote = F, sep = '\t', row.names = T)
tiff(paste0('TCGA-cluster-Cox Propotional Hazard Regression Model of all (OS,)', '.tiff'),
     width=6,height=4, units="in", compression="lzw", res=150) 
print(g)
dev.off()

#Fig. 4f

Clin<- read.csv('clinical-oscc-PAM-top 50.csv',sep=',',header= T)
Clin[Clin == 'Unknown'] <- NA
#colnames(Clin) 
Clin$OS.time.5.years <- as.numeric(Clin$OS.time.5.years)
Clin$OS.5.years<- as.numeric(Clin$OS.5.years)
sur <- Surv(time = Clin$OS.time.5.years, event = Clin$OS.5.years, type = 'right')
table(Clin$Chemotherapy,Clin$OS.5.years)
table(Clin$cluster)
table(Clin$OS.5.years)

prog <- function(grade) {
  title <- 'Overall Survival'
  subs <<-  Clin$cluster == grade
  sur.result <<- survfit(sur~Chemotherapy, data=Clin, subset = subs)
  g <- ggsurvplot(sur.result, data=Clin, conf.int=F, pval=T,
                  pval.method=T, 
                  pval.method.size=15, 
                  pval.method.coord=c(0,0.5),
                  pval.size = 15, 
                  ylim = c(0.3,1),
                  xlim = c(0,60),
                  pval.coord=c(0,0.4),
                  #font.main = c(16, "bold", "darkblue"), 
                  font.main = 40,
                  font.x = 30,
                  font.y = 0,
                  font.tickslab=30,
                  font.legend=35,
                  #pval.method.coord=c(1,0.1)
                  #font.main = c(16, "bold", "darkblue"), 
                  ggtheme=theme_bw()+
                    theme(axis.title.x=element_text(size=12),
                          axis.title.y=element_text(size=12),
                          axis.text =element_text(size=12,color ='black'), 
                          axis.text.y=element_text(size=12,color ='black'),
                          legend.text=element_text(size=15),
                          panel.background=element_blank(), 
                          axis.line=element_line(colour='black'),
                          legend.title=element_text(size=18), 
                          #text=element_text(face='bold',size=12),
                          panel.grid.major = element_blank(), 
                          panel.grid.minor = element_blank(),
                          panel.border = element_rect(fill=NA,color="black", size=3, linetype="solid")), 
                  censor=F, 
                  censor.shape=3, 
                  censor.size=6, 
                  risk.table=F, 
                  #palette=c("blue", "dark green", "red"),  
                  palette="lancet",
                  legend.size= 16,
                  size = 4,  
                  legend ='top',
                  legend.labs=c('No', 'Yes'), 
                  legend.title="Chemotherapy",
                  title=paste0(title, ' for OSCC of Cluster', grade), 
                  xlab='Time(months)')
  
  return(g)
}
for(g in c("I")) {
  tiff(paste0('TCGA-Overall Survival for OSCC of Cluster', g,
              ' (chemotherapy)','.tiff'),
       width=12,height=8, units="in", compression="lzw", res=150)
  print(prog(g))
  dev.off()
}

#Fig. 4g
library(factoextra)
library(survival)
library(survminer)
library(xtable)
library(ggpubr)
library(ggplot2)

Clin<- read.csv('clinical-oscc-PAM-top 50.csv',sep=',',header= T)
Clin[Clin == 'Unknown'] <- NA
#colnames(Clin) 
Clin$OS.time.5.years <- as.numeric(Clin$OS.time.5.years)
Clin$OS.5.years<- as.numeric(Clin$OS.5.years)
sur <- Surv(time = Clin$OS.time.5.years, event = Clin$OS.5.years, type = 'right')
table(Clin$Chemotherapy,Clin$OS.5.years)
table(Clin$cluster)
table(Clin$OS.5.years)

#####cox regression
colnames(Clin)
colnames(Clin)[26]<-'Cluster'
colnames(Clin)
Clin$Sex<- as.character(Clin$Sex)
Clin$stage<-as.character(Clin$stage)
Clin$Chemotherapy <- as.character(Clin$Chemotherapy)
Clin$Radiation <- as.character(Clin$Radiation)
Clin$Cluster <- as.character(Clin$Cluster)

sur <- Surv(time = Clin$OS.time.5.years, event = Clin$OS.5.years, type = 'right')

cox.multi <- function(gra) {
  
  cox <- coxph(sur~Age+Sex+stage+Chemotherapy+Radiation, data=Clin,
               subset = Clin$Cluster == gra)
  cox.sum1 <- xtable(summary(cox)$coefficient)
  cox.sum2 <- xtable(summary(cox)$conf.int)
  type <-c("Others", "Others","Others",'Chemotherapy',"Others")
  type=data.frame(type)
  rownames(type)<-rownames(cox.sum1)
  cox.sum <- cbind(cox.sum1, cox.sum2, type)
  cox.sum <- cox.sum[c(1:5),]
  rownames(cox.sum) <- c('Age','Male',  'Stage III-IV',
                         'Chemotherapy',  
                         'Radiation')
  colnames(cox.sum)[c(2,8:9,10)] <- c('HR','lower','upper','type')
  cox.sum$var <- rownames(cox.sum)
  write.table(cox.sum, paste0('TCGA-cluster-cox of Cluster',gra,'(OS 1)-20210909', '.txt'), 
              quote = F, sep = '\t', row.names = T)
  pd <- position_dodge(0.1)
  g <- ggplot(cox.sum, aes(x=var, y=HR,color = type),legend.title="Type") + 
    geom_point(size = 2) + 
    #ggtitle(paste0('Cox Propotional Hazard Regression Model of Grade',gra)) +
    geom_errorbar(aes(ymin=lower, ymax=upper), size= 1.8,width=.2, position=pd) +
    theme_bw() + theme(panel.grid=element_blank())  +
    geom_line(position=pd, size = 1.8) + xlab('Variables') + ylab('Hazard Ratio (HR)') +
    geom_hline(yintercept = 1, linetype = 'dashed', color = "blue", size = 1) +
    theme(axis.text.x = element_text(angle = 90, hjust = 0.5, 
                                     vjust = 0.6, size = 15,color= 'black',face='bold'),
          axis.title.y = element_text(angle = 90, hjust = 0.5, 
                                      vjust = 0.6, size = 15,color= 'black',face='bold'),
          axis.text.y = element_text(angle = 90, hjust = 0.5, 
                                     vjust = 0.6, size = 15,color= 'black',face='bold'))  +
    geom_point(position=pd)+scale_colour_manual(values= c('red', 'black'))
  return(g)
}

for(gra in c('I')) {
  tiff(paste0('TCGA-cluster-Cox Propotional Hazard Regression Model of Cluster', gra,' (OS 1,s)-20210909', '.tiff'),
       width=6,height=4, units="in", compression="lzw", res=150)
  print(cox.multi(gra))
  dev.off()
}

#Fig. 4h

Clin<- read.csv('clinical-oscc-PAM-top 50.csv',sep=',',header= T)
Clin[Clin == 'Unknown'] <- NA
#colnames(Clin) 
Clin$OS.time.5.years <- as.numeric(Clin$OS.time.5.years)
Clin$OS.5.years<- as.numeric(Clin$OS.5.years)
sur <- Surv(time = Clin$OS.time.5.years, event = Clin$OS.5.years, type = 'right')
table(Clin$Chemotherapy,Clin$OS.5.years)
table(Clin$cluster)
table(Clin$OS.5.years)

prog <- function(grade) {
  title <- 'Overall Survival'
  subs <<-  Clin$cluster == grade
  sur.result <<- survfit(sur~Chemotherapy, data=Clin, subset = subs)
  g <- ggsurvplot(sur.result, data=Clin, conf.int=F, pval=T,
                  pval.method=T, 
                  pval.method.size=15, 
                  pval.method.coord=c(0,0.5),
                  pval.size = 15, 
                  ylim = c(0.3,1),
                  xlim = c(0,60),
                  pval.coord=c(0,0.4),
                  #font.main = c(16, "bold", "darkblue"), 
                  font.main = 40,
                  font.x = 30,
                  font.y = 0,
                  font.tickslab=30,
                  font.legend=35,
                  #pval.method.coord=c(1,0.1)
                  #font.main = c(16, "bold", "darkblue"), 
                  ggtheme=theme_bw()+
                    theme(axis.title.x=element_text(size=12),
                          axis.title.y=element_text(size=12),
                          axis.text =element_text(size=12,color ='black'), 
                          axis.text.y=element_text(size=12,color ='black'),
                          legend.text=element_text(size=15),
                          panel.background=element_blank(), 
                          axis.line=element_line(colour='black'),
                          legend.title=element_text(size=18), 
                          #text=element_text(face='bold',size=12),
                          panel.grid.major = element_blank(), 
                          panel.grid.minor = element_blank(),
                          panel.border = element_rect(fill=NA,color="black", size=3, linetype="solid")), 
                  censor=F, 
                  censor.shape=3, 
                  censor.size=6, 
                  risk.table=F, 
                  #palette=c("blue", "dark green", "red"),  
                  palette="lancet",
                  legend.size= 16,
                  size = 4,  
                  legend ='top',
                  legend.labs=c('No', 'Yes'), 
                  legend.title="Chemotherapy",
                  title=paste0(title, ' for OSCC of Cluster', grade), 
                  xlab='Time(months)')
  
  return(g)
}
for(g in c("II")) {
  tiff(paste0('TCGA-Overall Survival for OSCC of Cluster', g,
              ' (chemotherapy)','.tiff'),
       width=12,height=8, units="in", compression="lzw", res=150)
  print(prog(g))
  dev.off()
}

#Fig. 4i
library(factoextra)
library(survival)
library(survminer)
library(xtable)
library(ggpubr)
library(ggplot2)

Clin<- read.csv('clinical-oscc-PAM-top 50.csv',sep=',',header= T)
Clin[Clin == 'Unknown'] <- NA
#colnames(Clin) 
Clin$OS.time.5.years <- as.numeric(Clin$OS.time.5.years)
Clin$OS.5.years<- as.numeric(Clin$OS.5.years)
sur <- Surv(time = Clin$OS.time.5.years, event = Clin$OS.5.years, type = 'right')
table(Clin$Chemotherapy,Clin$OS.5.years)
table(Clin$cluster)
table(Clin$OS.5.years)

#####cox regression
colnames(Clin)
colnames(Clin)[26]<-'Cluster'
colnames(Clin)

Clin$Sex<- as.character(Clin$Sex)
Clin$stage<-as.character(Clin$stage)

Clin$Chemotherapy <- as.character(Clin$Chemotherapy)
Clin$Radiation <- as.character(Clin$Radiation)
Clin$Cluster <- as.character(Clin$Cluster)

sur <- Surv(time = Clin$OS.time.5.years, event = Clin$OS.5.years, type = 'right')

cox.multi <- function(gra) {
  
  cox <- coxph(sur~Age+Sex+stage+Chemotherapy+Radiation, data=Clin,
               subset = Clin$Cluster == gra)
  cox.sum1 <- xtable(summary(cox)$coefficient)
  cox.sum2 <- xtable(summary(cox)$conf.int)
  type <-c("Others", "Others","Others",'Chemotherapy',"Others")
  type=data.frame(type)
  rownames(type)<-rownames(cox.sum1)
  cox.sum <- cbind(cox.sum1, cox.sum2, type)
  cox.sum <- cox.sum[c(1:5),]
  rownames(cox.sum) <- c('Age','Male',  'Stage III-IV',
                         'Chemotherapy',  
                         'Radiation')
  colnames(cox.sum)[c(2,8:9,10)] <- c('HR','lower','upper','type')
  cox.sum$var <- rownames(cox.sum)
  write.table(cox.sum, paste0('TCGA-cluster-cox of Cluster',gra,'(OS 1)-20210909', '.txt'), 
              quote = F, sep = '\t', row.names = T)
  pd <- position_dodge(0.1)
  g <- ggplot(cox.sum, aes(x=var, y=HR,color = type),legend.title="Type") + 
    geom_point(size = 2) + 
    #ggtitle(paste0('Cox Propotional Hazard Regression Model of Grade',gra)) +
    geom_errorbar(aes(ymin=lower, ymax=upper), size= 1.8,width=.2, position=pd) +
    theme_bw() + theme(panel.grid=element_blank())  +
    geom_line(position=pd, size = 1.8) + xlab('Variables') + ylab('Hazard Ratio (HR)') +
    geom_hline(yintercept = 1, linetype = 'dashed', color = "blue", size = 1) +
    theme(axis.text.x = element_text(angle = 90, hjust = 0.5, 
                                     vjust = 0.6, size = 15,color= 'black',face='bold'),
          axis.title.y = element_text(angle = 90, hjust = 0.5, 
                                      vjust = 0.6, size = 15,color= 'black',face='bold'),
          axis.text.y = element_text(angle = 90, hjust = 0.5, 
                                     vjust = 0.6, size = 15,color= 'black',face='bold'))  +
    geom_point(position=pd)+scale_colour_manual(values= c('red', 'black'))
  return(g)
}

for(gra in c('II')) {
  tiff(paste0('TCGA-cluster-Cox Propotional Hazard Regression Model of Cluster', gra,' (OS 1,s)-20210909', '.tiff'),
       width=6,height=4, units="in", compression="lzw", res=150)
  print(cox.multi(gra))
  dev.off()
}

#Fig. 4j
if (!requireNamespace("BiocManager", quietly = TRUE))     
  install.packages("BiocManager")  
BiocManager::install("GSVA")
if (!requireNamespace("BiocManager", quietly = TRUE))
  install.packages("BiocManager")

BiocManager::install("GSVAdata")
library('GSVA')
library('GSVAdata')
library('org.Hs.eg.db')
library(genefilter)
library(Biobase)
library('clusterProfiler')
library("limma")
#install.packages("pheatmap")
library(pheatmap)
#data(c2BroadSets)
setwd("GSVA")

setwd("GSVA")
h<- clusterProfiler::read.gmt("c2.cp.kegg.v7.4.symbols.gmt")
kegg_list = split(h$gene, h$ont)
diff2<- read.csv("OSCC_exp-pjk-20200805.csv",sep=',')
rownames(diff2)<- diff2$Gene
diff2$Gene<- NULL
diff2<-as.matrix(diff2) 

kegg2 <- gsva(diff2, kegg_list, kcdf="Gaussian",method = "gsva",parallel.sz=12)


kegg2<-as.data.frame(kegg2)
kegg2$pathway<-rownames(kegg2)

write.table(kegg2,'./ gsva_h-TCGA.csv',col.names = T,row.names = F,sep = ",")
#GSVA differential analysis
library(limma)
setwd("GSVA/GEAV-KEGG-TCGA")
gsva_h<- read.csv('gsva_h-TCGA.csv',sep=',')
rownames(gsva_h) <-gsva_h$pathway
gsva_h$pathway<- NULL
Clin1<- read.csv("clinical-oscc-PAM-top 50.csv",sep=',')



gsva_h2<- as.data.frame(t(gsva_h1))
#gsva_h3<-apply(gsva_h2,2,as.numeric)

Group = factor(Clin1$cluster_new.name,levels=c('WDL','PDL'))
design = model.matrix(~0+Group)
colnames(design) <- c('WDMG','PDMG')

design


#linear model fitness
fit <- lmFit(gsva_h, design)

#generate contrast matrix
contrast.matrix <- makeContrasts(PDMG -WDMG, #1
                                 
                                 levels=design)


#constrast model fit 
fit2 <- contrasts.fit(fit, contrast.matrix)

#bayes model 
fit2 <- eBayes(fit2)

#get DEGs
diff = topTable(fit2,adjust.method="fdr",coef=1,p.value=1,
                lfc=log(1,2),number=50000,sort.by = 'logFC')
#View(diff)
diff$pathway<- rownames(diff)


write.csv(diff,'GSVA KEGG analysis in TCGA(cluster-(II VS I)DEGs-20210911).csv',row.names=T,quote=F)

#pheatmap-select differential pathway
setwd("GSVA")
dat1<- read.csv('gsva_h-TCGA.csv',header =T,sep=',')
rownames(dat1) <-dat1$pathway
dat1$pathway<- NULL
dat1<-as.data.frame(t(dat1))
dat1$sample <-rownames(dat1)
Clin<- read.csv("clinical-oscc-PAM-top 50.csv")
colnames(Clin)
Clin<- Clin[,c(2,7,26)]##chose grade and cluster 
Clin1<- merge(Clin, dat1,by='sample')
colnames(Clin1)
a<-Clin1[1:10,]
write.csv(Clin1,'GSVA-TCGA-KEGG.csv')

library(pheatmap)
setwd("GSVA/GEAV-KEGG-TCGA")
Clin<- read.csv('GSVA KEGG analysis in TCGA( grade(high vs poor)-select pathway-DEGs-20210911).csv',header= T,sep=',')

Clin<-Clin[,c(1,2)]

dat1<- read.csv('GSVA KEGG with grade,cluster(averag)-20210901.csv',header= T,sep=',')
Clin1<-merge(dat1,Clin, by='pathway')
rownames(Clin1)<- Clin1$pathway
Clin1$pathway<- NULL
Clin1<-Clin1[,-5]
write.csv(Clin1,'TCGA-pathway merge data.csv')


###heatmap
library(pheatmap)
setwd("GSVA/GEAV-KEGG-TCGA")
Clin1<- read.csv("TCGA-pathway merge data - new.csv",sep=',')
rownames(Clin1) <-Clin1$pathway
Clin1$pathway<- NULL
Clin3<-Clin1[,1:2]

pheatmap(Clin3, cluster_rows = FALSE, cluster_cols = FALSE, color = colorRampPalette(c("navy", "white", "firebrick3"))(50),
         border=FALSE,fontsize_row=18,fontsize_col=18, gaps_row= c(5),cellwidth=40,cellheight=60)


#Fig. 5h
setwd("D:/SEER-OSCC-R-1013/AAAA final manuscript and data/original data-20211020")
dat1<-read.csv("20X-s-slide.csv",sep=',',header = T)
colnames(dat1)

dat2<-read.csv("TMA-fina1(340).csv",sep=',',header=T)
table(dat3$Grade)
Clin<-merge(dat1,dat3,by= "ID")
table(Clin$cluster.0.66.)
Clin$cluster.0.66.[Clin$cluster.0.66. == 'PDMG'] <-'ZPDMG'
table(Clin$Grade)
library(ggplot2)
library(survival)
library(survminer)
library(xtable)

Clin$OS_5year <- as.numeric(Clin$OS_5year)
Clin$OS_time_5year <-as.numeric(Clin$OS_time_5year)
Clin$Age <- as.numeric(Clin$Age)
sur <- Surv(time = Clin$OS_time_5year, event = Clin$OS_5year, type = 'right')
Clin$Stage[Clin$Stage==1 |Clin$Stage ==2] <-'I-II'
Clin$Stage[Clin$Stage==3 |Clin$Stage ==4] <-'III-IV'

Clin$cluster.0.66.[Clin$cluster.0.66. == 'PDMG'] <-'ZPDMG'
colnames(Clin)
sur <- Surv(time = Clin$OS_time_5year, event = Clin$OS_5year, type = 'right')
cox.multi <- function(gra) {
  
  cox <- coxph(sur~Age+Gender+Stage+Chemotherapy+Radiation, data=Clin,
               subset = Clin$cluster.0.66. == gra)
  cox.sum1 <- xtable(summary(cox)$coefficient)
  cox.sum2 <- xtable(summary(cox)$conf.int)
  type <-c("Others", "Others","Others",'Chemotherapy',"Others")
  type=data.frame(type)
  rownames(type)<-rownames(cox.sum1)
  cox.sum <- cbind(cox.sum1, cox.sum2, type)
  cox.sum <- cox.sum[c(1:5),]
  rownames(cox.sum) <- c('Age','Male',  'Stage III-IV',
                         'Chemotherapy',  
                         'Radiation')
  colnames(cox.sum)[c(2,8:9,10)] <- c('HR','lower','upper','type')
  cox.sum$var <- rownames(cox.sum)
  write.table(cox.sum, paste0('TMA-cox of cluster 0.66',gra,'(OS,340)', '.csv'), 
              quote = F, sep = ',', row.names = T)
  pd <- position_dodge(0.1)
  g <- ggplot(cox.sum, aes(x=var, y=HR,color = type),legend.title="Type") + 
    ggtitle( ' ')+
    geom_point(size = 2) +geom_errorbar(aes(ymin=lower, ymax=upper), size= 1.8,width=.2, position=pd) +
    theme_bw() + theme(panel.grid=element_blank())  +
    geom_line(position=pd, size = 1.8) + xlab(' ') + ylab('Hazard Ratio (HR)') +
    geom_hline(yintercept = 1, linetype = 'dashed', color = "blue", size = 1) +
    theme(axis.text.x = element_text(angle = 90, hjust = 0.5, 
                                     vjust = 0.6, size = 15,color= 'black',face='bold'),
          axis.title.y = element_text(angle = 90, hjust = 0.5, 
                                      vjust = 0.6, size = 15,color= 'black',face='bold'),
          axis.text.y = element_text(angle = 90, hjust = 0.5, 
                                     vjust = 0.6, size = 15,color= 'black',face='bold'))  +
    geom_point(position=pd)+scale_colour_manual(values= c('red', 'black'))
  return(g)
}

for(gra in c('WDMG')) {
  tiff(paste0('TMA-Cox Model of cluster 0.66',gra, ' (OS,340)', '.tiff'),
       width=6,height=4, units="in", compression="lzw", res=150)
  print(cox.multi(gra))
  dev.off()
}

#Fig. 5i
setwd("D:/SEER-OSCC-R-1013/AAAA final manuscript and data/original data-20211020")
dat1<-read.csv("20X-s-slide.csv",sep=',',header = T)
colnames(dat1)

dat2<-read.csv("TMA-fina1(340).csv",sep=',',header=T)
table(dat3$Grade)
Clin<-merge(dat1,dat3,by= "ID")
table(Clin$cluster.0.66.)
Clin$cluster.0.66.[Clin$cluster.0.66. == 'PDMG'] <-'ZPDMG'
table(Clin$Grade)
library(ggplot2)
library(survival)
library(survminer)
library(xtable)

Clin$OS_5year <- as.numeric(Clin$OS_5year)
Clin$OS_time_5year <-as.numeric(Clin$OS_time_5year)
Clin$Age <- as.numeric(Clin$Age)
table(Clin$Grade)
sur <- Surv(time = Clin$OS_time_5year, event = Clin$OS_5year, type = 'right')
Clin$Stage[Clin$Stage==1 |Clin$Stage ==2] <-'I-II'
Clin$Stage[Clin$Stage==3 |Clin$Stage ==4] <-'III-IV'

Clin$cluster.0.66.[Clin$cluster.0.66. == 'PDMG'] <-'ZPDMG'
colnames(Clin)
sur <- Surv(time = Clin$OS_time_5year, event = Clin$OS_5year, type = 'right')

colnames(Clin)
sur <- Surv(time = Clin$OS_time_5year, event = Clin$OS_5year, type = 'right')
cox.multi <- function(gra) {
  
  cox <- coxph(sur~Age+Gender+Stage+Chemotherapy+Radiation, data=Clin,
               subset = Clin$cluster.0.66. == gra)
  cox.sum1 <- xtable(summary(cox)$coefficient)
  cox.sum2 <- xtable(summary(cox)$conf.int)
  type <-c("Others", "Others","Others",'Chemotherapy',"Others")
  type=data.frame(type)
  rownames(type)<-rownames(cox.sum1)
  cox.sum <- cbind(cox.sum1, cox.sum2, type)
  cox.sum <- cox.sum[c(1:5),]
  rownames(cox.sum) <- c('Age','Male',  'Stage III-IV',
                         'Chemotherapy',  
                         'Radiation')
  colnames(cox.sum)[c(2,8:9,10)] <- c('HR','lower','upper','type')
  cox.sum$var <- rownames(cox.sum)
  write.table(cox.sum, paste0('TMA-cox of cluster 0.66',gra,'(OS,340)', '.csv'), 
              quote = F, sep = ',', row.names = T)
  pd <- position_dodge(0.1)
  g <- ggplot(cox.sum, aes(x=var, y=HR,color = type),legend.title="Type") + 
    ggtitle( ' ')+
    geom_point(size = 2) +geom_errorbar(aes(ymin=lower, ymax=upper), size= 1.8,width=.2, position=pd) +
    theme_bw() + theme(panel.grid=element_blank())  +
    geom_line(position=pd, size = 1.8) + xlab(' ') + ylab('Hazard Ratio (HR)') +
    geom_hline(yintercept = 1, linetype = 'dashed', color = "blue", size = 1) +
    theme(axis.text.x = element_text(angle = 90, hjust = 0.5, 
                                     vjust = 0.6, size = 15,color= 'black',face='bold'),
          axis.title.y = element_text(angle = 90, hjust = 0.5, 
                                      vjust = 0.6, size = 15,color= 'black',face='bold'),
          axis.text.y = element_text(angle = 90, hjust = 0.5, 
                                     vjust = 0.6, size = 15,color= 'black',face='bold'))  +
    geom_point(position=pd)+scale_colour_manual(values= c('red', 'black'))
  return(g)
}

for(gra in c('ZPDMG')) {
  tiff(paste0('TMA-Cox Model of cluster 0.66',gra, ' (OS,340)', '.tiff'),
       width=6,height=4, units="in", compression="lzw", res=150)
  print(cox.multi(gra))
  dev.off()
}


#Supplementary Fig. 1a-c
library(survival)
library(survminer)
library(xtable)

Clin<- read.csv("SEER data-20210829.csv",sep=',')
mean(Clin$Survival.months, na.rm= T)
table(Clin$OS.5year)
Clin[Clin=='Unknown']<- NA
sur <- Surv(time = Clin$OS.time.5year, event = Clin$OS.5year, type = 'right')
prog <- function(gra) {
  title <- 'Overall Survival'
  subs <<- Clin$Grade == gra
  sur.result <<- survfit(sur~Sex, data=Clin, subset = subs)
  g <- ggsurvplot(sur.result, data=Clin, conf.int=F, pval=T, 
                  pval.method=T,
                  pval.method.size=15, 
                  pval.method.coord=c(0,0.5),
                  pval.size = 15, 
                  ylim = c(0.3,1),
                  xlim = c(0,60),
                  pval.coord=c(0,0.4),
                  #font.main = c(16, "bold", "darkblue"),
                  font.main = 40,
                  font.x = 30,
                  font.y = 0,
                  font.tickslab=30,
                  font.legend=35,
                  #pval.method.coord=c(1,0.1),
                  #font.main = c(16, "bold", "darkblue"),
                  ggtheme=theme_bw()+
                    theme(axis.title.x=element_text(size=12),
                          axis.title.y=element_text(size=12),
                          axis.text =element_text(size=12,color ='black'), 
                          axis.text.y=element_text(size=12,color ='black'),
                          legend.text=element_text(size=15),
                          panel.background=element_blank(), 
                          axis.line=element_line(colour='black'),
                          legend.title=element_text(size=18), 
                          #text=element_text(face='bold',size=12),
                          panel.grid.major = element_blank(), 
                          panel.grid.minor = element_blank(),
                          panel.border = element_rect(fill=NA,color="black", size=3, linetype="solid")),
                  censor=F, 
                  censor.shape=3,
                  censor.size=6,
                  risk.table=F, 
                  #palette=c("blue", "dark green", "red"),  
                  palette="lancet",
                  legend.size= 16,
                  size = 4,  
                  legend.labs=c('Female', 'Male'), 
                  legend.title="Sex",
                  #palette=c("blue", "dark green", "red"),  
                  title=paste0(title, ' for OSCC in Grade ', gra), 
                  xlab='Time(months)',
                  legend ='top')
  #legend ='top' ))
  return(g)
}
for(g in c('I', 'II', 'III')) {
  tiff(paste0('SEER-Overall Survival for OSCC of Grade ', g,'(sex,OS)', '.tiff'),
       width=12,height=8, units="in", compression="lzw", res=150) 
  print(prog(g))
  dev.off()
}

# Supplementary Fig. 1d-f
library(survival)
library(survminer)
library(xtable)

Clin<- read.csv("SEER data-20210829.csv",sep=',')
mean(Clin$Survival.months, na.rm= T)
table(Clin$OS.5year)
Clin[Clin=='Unknown']<- NA
sur <- Surv(time = Clin$OS.time.5year, event = Clin$OS.5year, type = 'right')
prog <- function(gra) {
  title <- 'Overall Survival'
  subs <<- Clin$Grade == gra
  sur.result <<- survfit(sur~Surgery, data=Clin, subset = subs)
  g <- ggsurvplot(sur.result, data=Clin, conf.int=F, pval=T, 
                  pval.method=T,
                  pval.method.size=15, 
                  pval.method.coord=c(0,0.5),
                  pval.size = 15, 
                  ylim = c(0.3,1),
                  xlim = c(0,60),
                  pval.coord=c(0,0.4),
                  #font.main = c(16, "bold", "darkblue"),
                  font.main = 40,
                  font.x = 30,
                  font.y = 0,
                  font.tickslab=30,
                  font.legend=35,
                  #pval.method.coord=c(1,0.1),
                  #font.main = c(16, "bold", "darkblue"),
                  
                  censor=F, 
                  censor.shape=3,
                  censor.size=6,
                  risk.table=F, 
                  ggtheme=theme_bw()+
                    theme(axis.title.x=element_text(size=12),
                          axis.title.y=element_text(size=12),
                          axis.text =element_text(size=12,color ='black'), 
                          axis.text.y=element_text(size=12,color ='black'),
                          legend.text=element_text(size=15),
                          panel.background=element_blank(), 
                          axis.line=element_line(colour='black'),
                          legend.title=element_text(size=18), 
                          #text=element_text(face='bold',size=12),
                          panel.grid.major = element_blank(), 
                          panel.grid.minor = element_blank(),
                          panel.border = element_rect(fill=NA,color="black", size=3, linetype="solid")),
                  #palette=c("blue", "dark green", "red"),  
                  palette="lancet",
                  legend.size= 16,
                  size = 4,  
                  legend.labs=c('No', 'Yes'), 
                  legend.title="Surgery",
                  title=paste0(title, ' for OSCC in Grade ', gra), 
                  xlab='Time(months)',
                  legend ='top' )
  #legend ='top' ))
  return(g)
}

for(g in c('I', 'II', 'III')) {
  tiff(paste0('SEER-Overall Survival for OSCC of Grade ', g,'(surgery,OS)', '.tiff'),
       width=12,height=8, units="in", compression="lzw", res=150) 
  print(prog(g))
  dev.off()
}
Supplementary Fig. 1g-i
library(survival)
library(survminer)
library(xtable)
Clin<- read.csv("SEER data-20210829.csv",sep=',')
mean(Clin$Survival.months, na.rm= T)
table(Clin$OS.5year)
Clin[Clin=='Unknown']<- NA
sur <- Surv(time = Clin$OS.time.5year, event = Clin$OS.5year, type = 'right')
prog <- function(gra) {
  title <- 'Overall Survival'
  subs <<- Clin$Grade == gra
  sur.result <<- survfit(sur~Radiation, data=Clin, subset = subs)
  g <- ggsurvplot(sur.result, data=Clin, conf.int=F, pval=T, 
                  pval.method=T,
                  pval.method.size=15, 
                  pval.method.coord=c(0,0.5),
                  pval.size = 15, 
                  ylim = c(0.3,1),
                  xlim = c(0,60),
                  pval.coord=c(0,0.4),
                  #font.main = c(16, "bold", "darkblue"),
                  font.main = 40,
                  font.x = 30,
                  font.y = 0,
                  font.tickslab=30,
                  font.legend=35,
                  #pval.method.coord=c(1,0.1),
                  #font.main = c(16, "bold", "darkblue"),
                  ggtheme=theme_bw()+
                    theme(axis.title.x=element_text(size=12),
                          axis.title.y=element_text(size=12),
                          axis.text =element_text(size=12,color ='black'), 
                          axis.text.y=element_text(size=12,color ='black'),
                          legend.text=element_text(size=15),
                          panel.background=element_blank(), 
                          axis.line=element_line(colour='black'),
                          legend.title=element_text(size=18), 
                          #text=element_text(face='bold',size=12),
                          panel.grid.major = element_blank(), 
                          panel.grid.minor = element_blank(),
                          panel.border = element_rect(fill=NA,color="black", size=3, linetype="solid")),
                  censor=F, 
                  censor.shape=3,
                  censor.size=6,
                  risk.table=F, 
                  #palette=c("blue", "dark green", "red"),  
                  palette="lancet",
                  legend.size= 16,
                  size = 4,  
                  legend.labs=c('No', 'Yes'), 
                  legend.title="Radiation",
                  title=paste0(title, ' for OSCC in Grade ', gra), 
                  xlab='Time(months)',
                  
                  legend ='top' ))
return(g)
}

for(g in c('I', 'II', 'III')) {
  tiff(paste0('SEER-Overall Survival for OSCC of Grade ', g,'(radiation,OS)', '.tiff'),
       width=12,height=8, units="in", compression="lzw", res=150) 
  print(prog(g))
  dev.off()
}

# Supplementary Fig. 1j
library(survival)
library(survminer)
library(xtable)

Clin<- read.csv("SEER data-20210829.csv",sep=',')
mean(Clin$Survival.months, na.rm= T)
table(Clin$OS.5year)
Clin[Clin=='Unknown']<- NA
sur <- Surv(time = Clin$OS.time.5year, event = Clin$OS.5year, type = 'right')
prog <- function() {
  title <- 'Overall Survival'
  sur.result <<- survfit(sur~Chemotherapy, data=Clin)
  g <- ggsurvplot(sur.result, data=Clin, conf.int=F, pval=T, 
                  pval.method=T,
                  pval.method.size=15, 
                  pval.method.coord=c(0,0.5),
                  pval.size = 15, 
                  ylim = c(0.3,1),
                  xlim = c(0,60),
                  pval.coord=c(0,0.4),
                  #font.main = c(16, "bold", "darkblue"),
                  font.main = 40,
                  font.x = 30,
                  font.y = 0,
                  font.tickslab=30,
                  font.legend=35,
                  #pval.method.coord=c(1,0.1),
                  #font.main = c(16, "bold", "darkblue"),
                  ggtheme=theme_bw()+
                    theme(axis.title.x=element_text(size=12),
                          axis.title.y=element_text(size=12),
                          axis.text =element_text(size=12,color ='black'), 
                          axis.text.y=element_text(size=12,color ='black'),
                          legend.text=element_text(size=15),
                          panel.background=element_blank(), 
                          axis.line=element_line(colour='black'),
                          legend.title=element_text(size=18), 
                          #text=element_text(face='bold',size=12),
                          panel.grid.major = element_blank(), 
                          panel.grid.minor = element_blank(),
                          panel.border = element_rect(fill=NA,color="black", size=3, linetype="solid")),
                  censor=F, 
                  censor.shape=3,
                  censor.size=6,
                  risk.table=F, 
                  #palette=c("blue", "dark green", "red"),  
                  palette="lancet",
                  legend.size= 16,
                  size = 4,  
                  #title=(hjust=0.5),
                  legend.title="Chemotherapy",
                  title=paste0(title, '  in Chemotherapy'), 
                  #title=(hjust=0.5),
                  xlab='Time(months)',
                  legend ='top' 
                  #legend ='top' 
  )
  
  return(g)
  
}
#print(prog())
tiff(paste0('SEER-Overall Survival for OSCC in Chemotherapy (OS)', '.tiff'), 
     width=12,height=8, units="in", compression="lzw", res=150)
print(prog())
dev.off()


# Supplementary Fig. 1k
library(survival)
library(survminer)
library(xtable)

Clin<- read.csv("SEER data-20210829.csv",sep=',')
mean(Clin$Survival.months, na.rm= T)
table(Clin$OS.5year)
Clin[Clin=='Unknown']<- NA
sur <- Surv(time = Clin$OS.time.5year, event = Clin$OS.5year, type = 'right')
prog <- function() {
  title <- 'Overall Survival'
  sur.result <<- survfit(sur~Radiation, data=Clin)
  g <- ggsurvplot(sur.result, data=Clin, conf.int=F, pval=T, 
                  pval.method=T,
                  pval.method.size=15, 
                  pval.method.coord=c(0,0.5),
                  pval.size = 15, 
                  ylim = c(0.3,1),
                  xlim = c(0,60),
                  pval.coord=c(0,0.4),
                  #font.main = c(16, "bold", "darkblue"), 
                  font.main = 40,
                  font.x = 30,
                  font.y = 0,
                  font.tickslab=30,
                  font.legend=35,
                  #pval.method.coord=c(1,0.1), 
                  #font.main = c(16, "bold", "darkblue"), 
                  ggtheme=theme_bw()+
                    theme(axis.title.x=element_text(size=12),
                          axis.title.y=element_text(size=12),
                          axis.text =element_text(size=12,color ='black'), 
                          axis.text.y=element_text(size=12,color ='black'),
                          legend.text=element_text(size=15),
                          panel.background=element_blank(), 
                          axis.line=element_line(colour='black'),
                          legend.title=element_text(size=18), 
                          #text=element_text(face='bold',size=12), 
                          panel.grid.major = element_blank(), 
                          panel.grid.minor = element_blank(),
                          panel.border = element_rect(fill=NA,color="black", size=3, linetype="solid")), 
                  censor=F, 
                  censor.shape=3, 
                  censor.size=6, 
                  risk.table=F, 
                  #palette=c("blue", "dark green", "red"),  
                  palette="lancet",
                  legend.size= 16,
                  size = 4,  
                  #title=(hjust=0.5),
                  legend.title="Radiation",
                  title=paste0(title, '  in Radiation'), 
                  #title=(hjust=0.5),
                  xlab='Time(months)',
                  legend ='top' 
  )
  return(g)
  
}
#print(prog())
###KM-radiation
tiff(paste0('SEER-Overall Survival for OSCC in Radiation (OS)', '.tiff'), 
     width=12,height=8, units="in", compression="lzw", res=150)
print(prog())
dev.off()

# Supplementary Fig. 1l
rm(list=ls())
library(survival)
library(survminer)
library(ggplot2)
library(ggpubr)
library(xtable)

Clin <- read.csv("SEER data-20210829.csv",header = T,stringsAsFactors = F,sep = ",")
Clin[Clin == 'Unknown'] <- NA
Clin$OS.time.5year<- as.numeric(Clin$OS.time.5year)
table(Clin$OS.5year)
#a<-Clin[1:5,]
table(Clin$Grade)
colnames(Clin)

Clin$newstage <-Clin$Stage
Clin$newstage <- as.character(Clin$newstage)
Clin$newstage[Clin$newstage=='I'|Clin$newstage=='II'] <- 'I-II'
Clin$newstage[Clin$newstage=='III'|Clin$newstage=='IV'] <- 'III-IV'

sur <- Surv(time = Clin$OS.time.5year, event = Clin$OS.5year, type = 'right')
cox.multi <- function(gra) {
  
  cox <- coxph(sur~Age+Sex+newstage+Surgery+Chemotherapy+Radiation, data=Clin,
               subset = Clin$Grade == gra)
  cox.sum1 <- xtable(summary(cox)$coefficient)
  cox.sum2 <- xtable(summary(cox)$conf.int)
  type <-c("Others", "Others", "Others","Others",'Chemotherapy',"Others")
  type=data.frame(type)
  rownames(type)<-rownames(cox.sum1)
  cox.sum <- cbind(cox.sum1, cox.sum2, type)
  cox.sum <- cox.sum[c(1:6),]
  rownames(cox.sum) <- c('Age','Male', 'Stage III-IV',
                         'Surgery','Chemotherapy',  
                         'Radiation')
  colnames(cox.sum)[c(2,8:9,10)] <- c('HR','lower','upper','type')
  cox.sum$var <- rownames(cox.sum)
  write.table(cox.sum, paste0('cox of Grade',gra,'(OS 1)-20210909', '.txt'), 
              quote = F, sep = '\t', row.names = T)
  pd <- position_dodge(0.1)
  g <- ggplot(cox.sum, aes(x=var, y=HR,color = type),legend.title="Type") + 
    geom_point(size = 2) + 
    #ggtitle(paste0('Cox Propotional Hazard Regression Model of Grade',gra)) +
    geom_errorbar(aes(ymin=lower, ymax=upper), size= 1.8,width=.2, position=pd) +
    theme_bw() + theme(panel.grid=element_blank())  +
    geom_line(position=pd, size = 1.8) + xlab('Variables') + ylab('Hazard Ratio (HR)') +
    geom_hline(yintercept = 1, linetype = 'dashed', color = "blue", size = 1) +
    theme(axis.text.x = element_text(angle = 90, hjust = 0.5, 
                                     vjust = 0.6, size = 15,color= 'black',face='bold'),
          axis.title.y = element_text(angle = 90, hjust = 0.5, 
                                      vjust = 0.6, size = 15,color= 'black',face='bold'),
          axis.text.y = element_text(angle = 90, hjust = 0.5, 
                                     vjust = 0.6, size = 15,color= 'black',face='bold'))  +
    geom_point(position=pd)+scale_colour_manual(values= c('red', 'black'))
  return(g)
}

for(Grade in c('I', 'II', 'III')) {
  tiff(paste0('Cox Propotional Hazard Regression Model of Grade', Grade,' (OS 1,s)-20210909', '.tiff'))
  print(cox.multi(Grade))
  dev.off()
}

#Supplementary Fig. 2a

Clin1 <- read.csv("clinical_OSCC-pjk-20200805.csv",header = T,stringsAsFactors = F,sep = ",")
Clin<- Clin1
Clin[Clin == 'Unknown'] <- NA
table(Clin$OS.5.years)#Clin<- na.omit(Clin)

Clin$OS.time.5.years <- as.numeric(Clin$OS.time.5.years)
Clin$OS.5.years<- as.numeric(Clin$OS.5.years)
Clin$Age <- as.numeric(Clin$Age)

table(Clin$grade)

Clin$Grade <- Clin$grade

Clin$Grade[Clin$Grade=='Grade I-II'] <-'I-II'
Clin$Grade[Clin$Grade=='Grade III-IV'] <-'III-IV'

sur <- Surv(time = Clin$OS.time.5.years, event = Clin$OS.5.years, type = 'right')
table(Clin$Grade, Clin$Chemotherapy)
prog <- function() {
  title <- 'Overall Survival'
  sur.result <<- survfit(sur~Grade, data=Clin)
  g <- ggsurvplot(sur.result, data=Clin, conf.int=F, pval=T, 
                  pval.method=F,
                  pval.method.size=10, 
                  pval.size = 10, 
                  ylim = c(0.3,1),
                  xlim = c(0,60),
                  pval.coord=c(0,0.4),
                  #font.main = c(16, "bold", "darkblue"),
                  font.main = 40,
                  font.x = 30,
                  font.y = 0,
                  font.tickslab=30,
                  font.legend=35,
                  #pval.method.coord=c(1,0.1),
                  #font.main = c(16, "bold", "darkblue"),
                  ggtheme=theme_bw()+
                    theme(axis.title.x=element_text(size=12),
                          axis.title.y=element_text(size=12),
                          axis.text =element_text(size=12,color ='black'), 
                          axis.text.y=element_text(size=12,color ='black'),
                          legend.text=element_text(size=15),
                          panel.background=element_blank(), 
                          axis.line=element_line(colour='black'),
                          legend.title=element_text(size=18), 
                          #text=element_text(face='bold',size=12),
                          panel.grid.major = element_blank(), 
                          panel.grid.minor = element_blank(),
                          panel.border = element_rect(fill=NA,color="black", size=3, linetype="solid")),
                  censor=F, 
                  censor.shape=3,
                  censor.size=6,
                  risk.table=F, 
                  #palette=c("blue", "dark green", "red"),  
                  palette="lancet",
                  legend.size= 16,
                  size = 4,  
                  legend.labs=c('I-II', 'III-IV'), 
                  legend.title="Grade",
                  title=paste0(title, '  in Different Grade'), 
                  #title=(hjust=0.5),
                  xlab='Time(months)',
                  
                  legend = "top" 
  )
  
  return(g)
  
}
tiff(paste0('TCGA-Overall Survival for OSCC in Grade (OS)', '.tiff'), 
     width=12,height=8, units="in", compression="lzw", res=150)
print(prog())
dev.off()
#Supplementary Fig. 2b
rm(list=ls())
library(survival)
library(survminer)
library(ggplot2)
library(ggpubr)
library(xtable)

Clin1 <- read.csv("clinical_OSCC-pjk-20200805.csv",header = T,stringsAsFactors = F,sep = ",")
Clin<- Clin1
Clin[Clin == 'Unknown'] <- NA
table(Clin$OS.5.years)#Clin<- na.omit(Clin)
Clin$OS.time.5.years <- as.numeric(Clin$OS.time.5.years)
Clin$OS.5.years<- as.numeric(Clin$OS.5.years)
Clin$Age <- as.numeric(Clin$Age)
Clin$Grade <- Clin$grade
Clin$Grade[Clin$Grade=='Grade I-II'] <-'I-II'
Clin$Grade[Clin$Grade=='Grade III-IV'] <-'III-IV'
table(Clin$new_age)
sur <- Surv(time = Clin$OS.time.5.years, event = Clin$OS.5.years, type = 'right')
cox <- coxph(sur~Age+Sex+Grade+stage+Chemotherapy+Radiation, data=Clin)
cox.sum1 <- xtable(summary(cox)$coefficient)
cox.sum2 <- xtable(summary(cox)$conf.int)
type <-c( "Others","Others","PD","Others",'Others',"Others")
type=data.frame(type)
rownames(type)<-rownames(cox.sum1)
cox.sum <- cbind(cox.sum1, cox.sum2, type)
rownames(cox.sum) <- c('Age','Male','PD', 'Stage III-IV',
                       'Chemotherapy',  
                       'Radiation')
colnames(cox.sum)[c(2,8:9,10)] <- c('HR','lower','upper','type')
cox.sum$var <- rownames(cox.sum)
pd <- position_dodge(0.1)
g <- ggplot(cox.sum, aes(x=var, y=HR,color = type)) + 
  geom_point(size = 3) + 
  #ggtitle(paste0('Cox Propotional Hazard Regression Model')) +
  geom_errorbar(aes(ymin=lower, ymax=upper), size= 1.8,width=.2, position=pd) +
  theme_bw() + theme(panel.grid=element_blank())  +
  geom_line(position=pd, size = 1.8)  + ylab('Hazard Ratio (HR)') +
  geom_hline(yintercept = 1, linetype = 'dashed', color = "blue", size = 1) +
  theme(axis.text.x = element_text(angle = 90, hjust = 0.5, 
                                   vjust = 0.6, size = 15,color= 'black',face='bold'),
        axis.title.y = element_text(angle = 90, hjust = 0.5, 
                                    vjust = 0.6, size = 15,color= 'black',face='bold'),
        axis.text.y = element_text(angle = 90, hjust = 0.5, 
                                   vjust = 0.6, size = 15,color= 'black',face='bold'))  +
  geom_point(position=pd)+scale_colour_manual(values= c('black','red'))+
  theme(legend.title =element_blank())

g
write.table(cox.sum, paste0('TCGA-cox of all (OS,)-20210909', '.txt'), 
            quote = F, sep = '\t', row.names = T)
tiff(paste0('TCGA-Cox Propotional Hazard Regression Model of all (OS,)', '.tiff'),
     width=6,height=4, units="in", compression="lzw", res=150) 
print(g)
dev.off()

#Supplementary Fig. 2c
library(Hmisc) 
library(grid)
library(lattice)
library(Formula)
library(ggplot2)
library(survival)
library(rms)
library(VRPM)

library(survival)
rm(list = ls())
library(vcd)
#######TCGA

Clin2 <- read.csv("clinical_OSCC-pjk-20200805 - revise.csv",header = T,stringsAsFactors = F,sep = ",")
Clin<-Clin2
Clin[Clin == 'Unknown'] <- NA
Clin$OS.5.years[Clin$OS.5.years == "Alive"] <- 0
Clin$OS.5.years[Clin$OS.5.years == "Dead"] <- 1
table(Clin$OS.5.years)#Clin<- na.omit(Clin)
Clin$OS.time.5.years <- as.numeric(Clin$OS.time.5.years)
Clin$OS.5.years<- as.numeric(Clin$OS.5.years)
#sur <- Surv(time = Clin$OS.time.5.years, event = Clin$OS.5.years, type = 'right')
Clin$Age <- as.numeric(Clin$Age)
Clin$grade <-as.factor(Clin$grade)
Clin$Stage <-as.factor (Clin$Stage)
Clin$Stage <-as.numeric(Clin$Stage)
library("VRPM")

library(survival)
table(Clin$grade)

Clin$Grade[Clin$Grade ==1 | Clin$Grade == 2] <- 1

Cox_nomo<-coxph(Surv(Clin$OS.time.5.years,Clin$OS.5.years)~Age+Sex+Grade+Stage+Chemotherapy+Radiation,
                data=Clin,
                model=T)

colplot(Cox_nomo, coloroptions = 3,
        
        time = 60,
        
        risklabel ="Probability of survival-TCGA",
        
        filename="Color Nomogram_5-Yr-TCGA(Grade)-20211018") 
#Supplementary Fig. 2d

Clin1 <- read.csv("clinical_OSCC-pjk-20200805.csv",header = T,stringsAsFactors = F,sep = ",")
Clin<- Clin1
Clin[Clin == 'Unknown'] <- NA
table(Clin$OS.5.years)#Clin<- na.omit(Clin)

Clin$OS.time.5.years <- as.numeric(Clin$OS.time.5.years)
Clin$OS.5.years<- as.numeric(Clin$OS.5.years)
Clin$Age <- as.numeric(Clin$Age)
table(Clin$grade)

Clin$Grade <- Clin$grade

Clin$Grade[Clin$Grade=='Grade I-II'] <-'I-II'
Clin$Grade[Clin$Grade=='Grade III-IV'] <-'III-IV'
prog <- function(gra) {
  title <- 'Overall Survival'
  subs <<- Clin$Grade == gra
  sur.result <<- survfit(sur~Chemotherapy, data=Clin, subset = subs)
  g <- ggsurvplot(sur.result, data=Clin, conf.int=F, pval=T, 
                  pval.method=F,
                  pval.method.size=10, 
                  pval.size = 10, 
                  ylim = c(0.3,1),
                  xlim = c(0,60),
                  pval.coord=c(0,0.4),
                  #font.main = c(16, "bold", "darkblue"),
                  font.main = 40,
                  font.x = 30,
                  font.y = 0,
                  font.tickslab=30,
                  font.legend=35,
                  #pval.method.coord=c(1,0.1),
                  #font.main = c(16, "bold", "darkblue"),
                  ggtheme=theme_bw()+
                    theme(axis.title.x=element_text(size=12),
                          axis.title.y=element_text(size=12),
                          axis.text =element_text(size=12,color ='black'), 
                          axis.text.y=element_text(size=12,color ='black'),
                          legend.text=element_text(size=15),
                          panel.background=element_blank(), 
                          axis.line=element_line(colour='black'),
                          legend.title=element_text(size=18), 
                          #text=element_text(face='bold',size=12),
                          panel.grid.major = element_blank(), 
                          panel.grid.minor = element_blank(),
                          panel.border = element_rect(fill=NA,color="black", size=3, linetype="solid")),
                  censor=F, 
                  censor.shape=3,
                  censor.size=6,
                  risk.table=F, 
                  #palette=c("blue", "dark green", "red"),  
                  palette="lancet",
                  legend.size= 16,
                  size = 4,  
                  legend.labs=c('No', 'Yes'), 
                  legend.title="Chemotherapy",
                  title=paste0(title, ' for OSCC in Grade ', gra), 
                  xlab='Time(months)',
                  legend ='top' ))
return(g)
}

for(g in c('I-II')) {
  tiff(paste0('TCGA-Overall Survival for OSCC of Grade ', g,'(chemo,OS)', '.tiff'),
       width=12,height=8, units="in", compression="lzw", res=150) 
  print(prog(g))
  dev.off()
}

#Supplementary Fig. 2e 
rm(list=ls())
library(survival)
library(survminer)
library(ggplot2)
library(ggpubr)
library(xtable)

Clin1 <- read.csv("clinical_OSCC-pjk-20200805.csv",header = T,stringsAsFactors = F,sep = ",")
Clin<- Clin1
Clin[Clin == 'Unknown'] <- NA
table(Clin$OS.5.years)#Clin<- na.omit(Clin)

Clin$OS.time.5.years <- as.numeric(Clin$OS.time.5.years)
Clin$OS.5.years<- as.numeric(Clin$OS.5.years)
#sur <- Surv(time = Clin$OS.time.5.years, event = Clin$OS.5.years, type = 'right')
Clin$Age <- as.numeric(Clin$Age)


Clin$Grade <- Clin$grade

Clin$Grade[Clin$Grade=='Grade I-II'] <-'I-II'
Clin$Grade[Clin$Grade=='Grade III-IV'] <-'III-IV'
table(Clin$new_age)
sur <- Surv(time = Clin$OS.time.5.years, event = Clin$OS.5.years, type = 'right')
table(Clin$Grade, Clin$Chemotherapy)

sur <- Surv(time = Clin$OS.time.5.years, event = Clin$OS.5.years, type = 'right')

Clin$OS.time.5.years

sur <- Surv(time = Clin$OS.time.5year, event = Clin$OS.5year, type = 'right')
cox.multi <- function(gra) {
  
  cox <- coxph(sur~Age+Sex+stage+Chemotherapy+Radiation, data=Clin,
               subset = Clin$Grade == gra)
  cox.sum1 <- xtable(summary(cox)$coefficient)
  cox.sum2 <- xtable(summary(cox)$conf.int)
  type <-c("Others", "Others","Others",'Chemotherapy',"Others")
  type=data.frame(type)
  rownames(type)<-rownames(cox.sum1)
  cox.sum <- cbind(cox.sum1, cox.sum2, type)
  cox.sum <- cox.sum[c(1:5),]
  rownames(cox.sum) <- c('Age','Male',  'Stage III-IV',
                         'Chemotherapy',  
                         'Radiation')
  colnames(cox.sum)[c(2,8:9,10)] <- c('HR','lower','upper','type')
  cox.sum$var <- rownames(cox.sum)
  write.table(cox.sum, paste0('TCGA-cox of Grade',gra,'(OS 1)-20210909', '.txt'), 
              quote = F, sep = '\t', row.names = T)
  pd <- position_dodge(0.1)
  g <- ggplot(cox.sum, aes(x=var, y=HR,color = type),legend.title="Type") + 
    geom_point(size = 2) + 
    #ggtitle(paste0('Cox Propotional Hazard Regression Model of Grade',gra)) +
    geom_errorbar(aes(ymin=lower, ymax=upper), size= 1.8,width=.2, position=pd) +
    theme_bw() + theme(panel.grid=element_blank())  +
    geom_line(position=pd, size = 1.8) + ylab('Hazard Ratio (HR)') +
    geom_hline(yintercept = 1, linetype = 'dashed', color = "blue", size = 1) +
    theme(axis.text.x = element_text(angle = 90, hjust = 0.5, 
                                     vjust = 0.6, size = 15,color= 'black',face='bold'),
          axis.title.y = element_text(angle = 90, hjust = 0.5, 
                                      vjust = 0.6, size = 15,color= 'black',face='bold'),
          axis.text.y = element_text(angle = 90, hjust = 0.5, 
                                     vjust = 0.6, size = 15,color= 'black',face='bold'))  +
    geom_point(position=pd)+scale_colour_manual(values= c('red', 'black'))
  return(g)
}

for(Grade in c('I-II')) {
  tiff(paste0('TCGA-Cox Propotional Hazard Regression Model of Grade', Grade,' (OS 1,s)-20210909', '.tiff'),
       width=6,height=4, units="in", compression="lzw", res=150)
  print(cox.multi(Grade))
  dev.off()
}

#Supplementary Fig. 2f 
library(Hmisc) 
library(grid)
library(lattice)
library(Formula)
library(ggplot2)
library(survival)
library(rms)
library(VRPM)

library(survival)
rm(list = ls())
library(vcd)
#######TCGA

Clin2 <- read.csv("clinical_OSCC-pjk-20200805 - revise.csv",header = T,stringsAsFactors = F,sep = ",")
Clin<-Clin2
Clin[Clin == 'Unknown'] <- NA
Clin$OS.5.years[Clin$OS.5.years == "Alive"] <- 0
Clin$OS.5.years[Clin$OS.5.years == "Dead"] <- 1
table(Clin$OS.5.years)#Clin<- na.omit(Clin)
Clin$OS.time.5.years <- as.numeric(Clin$OS.time.5.years)
Clin$OS.5.years<- as.numeric(Clin$OS.5.years)
#sur <- Surv(time = Clin$OS.time.5.years, event = Clin$OS.5.years, type = 'right')
Clin$Age <- as.numeric(Clin$Age)

Clin$grade <-as.factor(Clin$grade)
Clin$Stage <-as.factor (Clin$Stage)
Clin$Stage <-as.numeric(Clin$Stage)

library("VRPM")

library(survival)
table(Clin$grade)
Clin1<- subset(Clin, Clin$grade=='Grade I-II')

Cox_nomo<-coxph(Surv(Clin1$OS.time.5.years,Clin1$OS.5.years)~Age+Sex+Stage+Chemotherapy+Radiation,
                data=Clin1,
                model=T)

colplot(Cox_nomo, coloroptions = 3,
        
        time = 60,
        
        risklabel ="Probability of survival-TCGA",
        
        filename="Color Nomogram_5-Yr-TCGA(Grade I-II)-20211018")


#Supplementary Fig. 2g

Clin1 <- read.csv("clinical_OSCC-pjk-20200805.csv",header = T,stringsAsFactors = F,sep = ",")
Clin<- Clin1
Clin[Clin == 'Unknown'] <- NA
table(Clin$OS.5.years)#Clin<- na.omit(Clin)

Clin$OS.time.5.years <- as.numeric(Clin$OS.time.5.years)
Clin$OS.5.years<- as.numeric(Clin$OS.5.years)
Clin$Age <- as.numeric(Clin$Age)

table(Clin$grade)

Clin$Grade <- Clin$grade

Clin$Grade[Clin$Grade=='Grade I-II'] <-'I-II'
Clin$Grade[Clin$Grade=='Grade III-IV'] <-'III-IV'
prog <- function(gra) {
  title <- 'Overall Survival'
  subs <<- Clin$Grade == gra
  sur.result <<- survfit(sur~Chemotherapy, data=Clin, subset = subs)
  g <- ggsurvplot(sur.result, data=Clin, conf.int=F, pval=T, 
                  pval.method=F,
                  pval.method.size=10, 
                  pval.size = 10, 
                  ylim = c(0.3,1),
                  xlim = c(0,60),
                  pval.coord=c(0,0.4),
                  #font.main = c(16, "bold", "darkblue"),
                  font.main = 40,
                  font.x = 30,
                  font.y = 0,
                  font.tickslab=30,
                  font.legend=35,
                  #pval.method.coord=c(1,0.1),
                  #font.main = c(16, "bold", "darkblue"),
                  ggtheme=theme_bw()+
                    theme(axis.title.x=element_text(size=12),
                          axis.title.y=element_text(size=12),
                          axis.text =element_text(size=12,color ='black'), 
                          axis.text.y=element_text(size=12,color ='black'),
                          legend.text=element_text(size=15),
                          panel.background=element_blank(), 
                          axis.line=element_line(colour='black'),
                          legend.title=element_text(size=18), 
                          #text=element_text(face='bold',size=12),
                          panel.grid.major = element_blank(), 
                          panel.grid.minor = element_blank(),
                          panel.border = element_rect(fill=NA,color="black", size=3, linetype="solid")),
                  censor=F, 
                  censor.shape=3,
                  censor.size=6,
                  risk.table=F, 
                  #palette=c("blue", "dark green", "red"),  
                  palette="lancet",
                  legend.size= 16,
                  size = 4,  
                  legend.labs=c('No', 'Yes'), 
                  legend.title="Chemotherapy",
                  title=paste0(title, ' for OSCC in Grade ', gra), 
                  xlab='Time(months)',
                  legend ='top' ))
return(g)
}

for(g in c( 'III-IV')) {
  tiff(paste0('TCGA-Overall Survival for OSCC of Grade ', g,'(chemo,OS)', '.tiff'),
       width=12,height=8, units="in", compression="lzw", res=150) 
  print(prog(g))
  dev.off()
}
#Supplementary Fig. 2h
rm(list=ls())
library(survival)
library(survminer)
library(ggplot2)
library(ggpubr)
library(xtable)


Clin1 <- read.csv("clinical_OSCC-pjk-20200805.csv",header = T,stringsAsFactors = F,sep = ",")
Clin<- Clin1
Clin[Clin == 'Unknown'] <- NA
table(Clin$OS.5.years)#Clin<- na.omit(Clin)

Clin$OS.time.5.years <- as.numeric(Clin$OS.time.5.years)
Clin$OS.5.years<- as.numeric(Clin$OS.5.years)
Clin$Age <- as.numeric(Clin$Age)
Clin$Grade <- Clin$grade

Clin$Grade[Clin$Grade=='Grade I-II'] <-'I-II'
Clin$Grade[Clin$Grade=='Grade III-IV'] <-'III-IV'
table(Clin$new_age)
sur <- Surv(time = Clin$OS.time.5.years, event = Clin$OS.5.years, type = 'right')
table(Clin$Grade, Clin$Chemotherapy)

sur <- Surv(time = Clin$OS.time.5year, event = Clin$OS.5year, type = 'right')
cox.multi <- function(gra) {
  cox <- coxph(sur~Age+Sex+stage+Chemotherapy+Radiation, data=Clin,
               subset = Clin$Grade == gra)
  cox.sum1 <- xtable(summary(cox)$coefficient)
  cox.sum2 <- xtable(summary(cox)$conf.int)
  type <-c("Others", "Others","Others",'Chemotherapy',"Others")
  type=data.frame(type)
  rownames(type)<-rownames(cox.sum1)
  cox.sum <- cbind(cox.sum1, cox.sum2, type)
  cox.sum <- cox.sum[c(1:5),]
  rownames(cox.sum) <- c('Age','Male',  'Stage III-IV',
                         'Chemotherapy',  
                         'Radiation')
  colnames(cox.sum)[c(2,8:9,10)] <- c('HR','lower','upper','type')
  cox.sum$var <- rownames(cox.sum)
  write.table(cox.sum, paste0('TCGA-cox of Grade',gra,'(OS 1)-20210909', '.txt'), 
              quote = F, sep = '\t', row.names = T)
  pd <- position_dodge(0.1)
  g <- ggplot(cox.sum, aes(x=var, y=HR,color = type),legend.title="Type") + 
    geom_point(size = 2) + 
    #ggtitle(paste0('Cox Propotional Hazard Regression Model of Grade',gra)) +
    geom_errorbar(aes(ymin=lower, ymax=upper), size= 1.8,width=.2, position=pd) +
    theme_bw() + theme(panel.grid=element_blank())  +
    geom_line(position=pd, size = 1.8) + ylab('Hazard Ratio (HR)') +
    geom_hline(yintercept = 1, linetype = 'dashed', color = "blue", size = 1) +
    theme(axis.text.x = element_text(angle = 90, hjust = 0.5, 
                                     vjust = 0.6, size = 15,color= 'black',face='bold'),
          axis.title.y = element_text(angle = 90, hjust = 0.5, 
                                      vjust = 0.6, size = 15,color= 'black',face='bold'),
          axis.text.y = element_text(angle = 90, hjust = 0.5, 
                                     vjust = 0.6, size = 15,color= 'black',face='bold'))  +
    geom_point(position=pd)+scale_colour_manual(values= c('red', 'black'))
  return(g)
}

for(Grade in c('III-IV')) {
  tiff(paste0('TCGA-Cox Propotional Hazard Regression Model of Grade', Grade,' (OS 1,s)-20210909', '.tiff'),
       width=6,height=4, units="in", compression="lzw", res=150)
  print(cox.multi(Grade))
  dev.off()
}

#Supplementary Fig. 2i
library(Hmisc) 
library(grid)
library(lattice)
library(Formula)
library(ggplot2)
library(survival)
library(rms)
library(VRPM)

library(survival)
rm(list = ls())
library(vcd)
#######TCGA

Clin2 <- read.csv("clinical_OSCC-pjk-20200805 - revise.csv",header = T,stringsAsFactors = F,sep = ",")
Clin<-Clin2
Clin[Clin == 'Unknown'] <- NA
Clin$OS.5.years[Clin$OS.5.years == "Alive"] <- 0
Clin$OS.5.years[Clin$OS.5.years == "Dead"] <- 1
table(Clin$OS.5.years)#Clin<- na.omit(Clin)
Clin$OS.time.5.years <- as.numeric(Clin$OS.time.5.years)
Clin$OS.5.years<- as.numeric(Clin$OS.5.years)
Clin$Age <- as.numeric(Clin$Age)

Clin$grade <-as.factor(Clin$grade)
Clin$Stage <-as.factor (Clin$Stage)
Clin$Stage <-as.numeric(Clin$Stage)

library("VRPM")

library(survival)
table(Clin$grade)
Clin1<- subset(Clin, Clin$grade=='Grade III-IV')
Cox_nomo<-coxph(Surv(Clin1$OS.time.5.years,Clin1$OS.5.years)~Age+Sex+Stage+Chemotherapy+Radiation,
                data=Clin1,
                model=T)
colplot(Cox_nomo, coloroptions = 3,
        
        time = 60,
        
        risklabel ="Probability of survival-TCGA",
        
        filename="Color Nomogram_5-Yr-TCGA(Grade III-IV)-20211018")
#Supplementary Fig. 3a 
library(maftools)
laml.maf = read.csv("TCGA.LIHC.mutect.maf.csv",header=TRUE)
oncoplot(maf = laml.maf , top = 30, fontSize = 12 ,showTumorSampleBarcodes = F，clinicalFeatures = 'cluster' )
laml.titv = titv(maf = laml, plot = FALSE, useSyn = TRUE)
plotTiTv(res = laml.titv)
#Supplementary Fig. 3b none
#Supplementary Fig. 3c
library(limma)
####TCGA CLUSTER
dat1<- read.csv('OSCC_exp-pjk-20200805.csv',sep=',')
a<-dat1[1:10,]
rownames(dat1) <- dat1$Gene
dat1$Gene <- NULL

Clin<- read.csv("clinical-oscc-PAM-top 50.csv",sep=',')
table(Clin$cluster)

Group = factor(Clin$cluster,levels=c('I','II'))
design = model.matrix(~0+Group)
colnames(design) <- c('I','II')

design

#linear model fitness
fit <- lmFit(dat1, design)

#generate contrast matrix
contrast.matrix <- makeContrasts( II - I, #1
                                  
                                  levels=design)


#constrast model fit 
fit2 <- contrasts.fit(fit, contrast.matrix)

#bayes model 
fit2 <- eBayes(fit2)

#get DEGs
diff = topTable(fit2,adjust.method="fdr",coef=1,p.value=1,
                lfc=log(1,2),number=50000,sort.by = 'logFC')
#View(diff)
diff$gene<- rownames(diff)


write.csv(diff,'DEGs based new cluster(cluster II VS I).csv',row.names=T,quote=F)

#GO enrichment
library(R.utils)
library(BiocManager)
library(clusterProfiler)
library(topGO)
library(Rgraphviz)
library(pathview)
library(org.Hs.eg.db)
library(limma)
library(ggplot2)
setwd("new molecular garde-20210915")
dat2<- read.csv("DEGs based new cluster(cluster II VS I).csv",sep=',')
dat3<- subset(dat2 , dat2$logFC> 1 & dat2$adj.P.Val < 0.05)  # PDMG
dat4<- subset(dat2 , dat2$logFC< -1 & dat2$adj.P.Val < 0.05) ## WDMG

diff23<-rbind(dat3,dat4)
GOKEGG <- function(geneset=NULL, output, filename, trend) {
  if(output == 'GO') {
    ego <- enrichGO(gene          = bitr(geneset,
                                         fromType = 'SYMBOL', toType = 'ENTREZID',
                                         OrgDb = 'org.Hs.eg.db')$ENTREZID,
                    # universe      = names(geneList),
                    OrgDb         = org.Hs.eg.db,
                    ont           = "ALL",
                    pAdjustMethod = "BH",
                    pvalueCutoff  = 0.5,
                    qvalueCutoff  = 0.5,
                    readable      = TRUE)
    GO_ <- as.data.frame(ego)
    write.table(GO_, paste0(filename, '_GO_', trend, '(TCGA-new cluster).txt'), 
                sep = '\t', row.names = F, quote = F)
    return(GO_)}
  if(output == 'KEGG') {
    kk <- enrichKEGG(gene          = bitr(geneset,
                                          fromType = 'SYMBOL', toType = 'ENTREZID',
                                          OrgDb = 'org.Hs.eg.db')$ENTREZID,
                     organism      = 'hsa',
                     pAdjustMethod = "BH",
                     pvalueCutoff  = 0.5,
                     qvalueCutoff  = 0.5)
    KEGG_ <- as.data.frame(setReadable(kk, OrgDb = org.Hs.eg.db, keyType="ENTREZID"))
    write.table(KEGG_, paste0(filename, '_KEGG_', trend, '(TCGA-new cluster).txt'), 
                sep = '\t', row.names = F, quote = F)
    return(KEGG_)}
}

DrawEnrich <- function(dat, type, top.number = 5, col="blue", trend){
  if(type == 'BP') {tit <- 'Biological Process of '}
  if(type == 'CC') {tit <- 'Cellular Component of '}
  if(type == 'MF') {tit <- 'Molecular Function of '}
  if(type == 'KEGG') {tit <- 'KEGG Pathway of '}
  if(type == 'GO') {tit <- 'Gene Ontology Enrichment of '}
  dat1 = dat[c(1:top.number),]
  dat1$Description = capitalize(dat1$Description)
  dat1 = dat1[order(dat1$p.adjust),,drop=F]
  dat1$Description = factor(dat1$Description,levels=dat1$Description[length(dat1$Description):1])
  dat1$PValue = -log10(dat1$p.adjust)
  dat1$GeneRatio <- dat1$Count / as.numeric(gsub('^.*/', '', dat1$GeneRatio))
  p = ggplot(dat1,aes(GeneRatio, Description)) +
    geom_point(aes(size=Count,colour=PValue)) +
    scale_colour_gradient(low=col,high="red") + 
    labs(colour=expression(-log[10]("P Value")),size="Gene counts",  
         x="Gene Ratio",y="",title=paste0(tit, trend, '-regulated Genes(TCGA)')) +
    theme_bw() + theme(axis.text.x = element_text(size = 14), axis.text.y=element_text(size=12), 
                       plot.title=element_text(size=20, hjust=1), 
                       legend.text=element_text(size=12), legend.title=element_text(size=14),
                       axis.title.x=element_text(size=18)) +
    scale_x_continuous(limits = c(0,max(dat1$GeneRatio) * 1.2)) 
  return(p)
}

GO <- GOKEGG(geneset=dat3$gene, output='GO',filename='sqh2021','UP')

#DrawEnrich(GO, 'GO', min(nrow(GO), 5), 'blue', ' UP')

GO <- GOKEGG(geneset=dat4$gene, output='GO',filename='sqh2021','DOWN')
#DrawEnrich(GO, 'GO', min(nrow(GO), 5), 'blue', ' DOWN')

library(ggplot2)
library(ggrepel)
colnames(dat3)

diff1<- read.table('sqh2021_GO_DOWN(TCGA-new cluster,high differ).txt',header = T, sep='\t')
diff11<-diff1[order(diff1$p.adjust,decreasing = F),]
diff11<- diff11[1:5,]

###GO plot 
dat1<-read.table("sqh2021_GO_DOWN(TCGA-new cluster,high differ).txt",sep='\t',header = T)
dat11<-subset(dat1,dat1$p.adjust<0.05)
dat11<-dat11[order(dat11$p.adjust,decreasing = F),]
dat11<-dat11[1:5,]

dat2<-read.table("sqh2021_GO_UP(TCGA-new cluster,poor differ).txt",sep='\t',header = T)
dat22<-subset(dat2,dat2$p.adjust<0.05)
dat22<-dat22[order(dat22$p.adjust,decreasing = F),]
dat22<-dat22[1:5,]

dat12<-rbind(dat11,dat22)
library(PerformanceAnalytics)
library(corrplot)
library(psych)
library(xtable)
library('GOplot')

dat12$geneID<-gsub('/', ', ', dat12$geneID)# Category,ID,term,Genes,adj_pval；
colnames(dat12)
dat12<-dat12[,c(1:3,9,7)]
colnames(dat12)<- c('Category','ID','term','Genes','adj_pval')
diff23<-diff23[,1:2]
colnames(diff23)<-c("ID","logFC") 
dat12$Category<-as.character(dat12$Category)
dat12$ID<-as.character(dat12$ID)
dat12$term<-as.character(dat12$term)
diff23$ID<-as.character(diff23$ID)
dat12<-as.data.frame(dat12)
diff23<-as.data.frame(diff23)
#plot_data=list(DA=dat12, GE= diff23)
#circ_2= data.frame()

#circ2<-circle_dat(plot_data$DA,plot_data$DA) 

circ<-circle_dat(dat12,diff23) 

chord<-chord_dat(data = circ,genes =diff23, process= unique(circ$term) )
#chord<-chord[1:10,1:10]
chord<-as.matrix(chord)
GOChord(chord, gene.order = 'logFC',gene.size = 3
        
)
GOChord(chord, gene.order = 'logFC',gene.size = 3,space=0.1,gene.space = 0.2,
        border.size=0.00001, 
        ribbon.col=brewer.pal(10 ,"Set3"),process.label = 10)

#Supplementary Fig. 4a
setwd("D:/SEER-OSCC-R-1013/AAAA final manuscript and data/original data-20211020")
dat1<-read.csv("20X-s-slide.csv",sep=',',header = T)
colnames(dat1)

dat2<-read.csv("TMA-fina1(340).csv",sep=',',header=T)
table(dat3$Grade)
Clin<-merge(dat1,dat3,by= "ID")
table(Clin$cluster.0.66.)
Clin$cluster.0.66.[Clin$cluster.0.66. == 'PDMG'] <-'ZPDMG'

table(Clin$Grade)
library(ggplot2)
library(survival)
library(survminer)
library(xtable)

Clin$OS_5year <- as.numeric(Clin$OS_5year)
Clin$OS_time_5year <-as.numeric(Clin$OS_time_5year)
Clin$Age <- as.numeric(Clin$Age)
table(Clin$Grade)
sur <- Surv(time = Clin$OS_time_5year, event = Clin$OS_5year, type = 'right')
Clin$Stage[Clin$Stage==1 |Clin$Stage ==2] <-'I-II'
Clin$Stage[Clin$Stage==3 |Clin$Stage ==4] <-'III-IV'

Clin$cluster.0.66.[Clin$cluster.0.66. == 'PDMG'] <-'ZPDMG'
colnames(Clin)
sur <- Surv(time = Clin$OS_time_5year, event = Clin$OS_5year, type = 'right')
cox <- coxph(sur~Age+Gender
             +cluster.0.66.+Stage+Chemotherapy+Radiation, data=Clin)
cox.sum1 <- xtable(summary(cox)$coefficient)
cox.sum2 <- xtable(summary(cox)$conf.int)
type <-c("Others",  "Others","PDMG","Others","Others","Others")
type=data.frame(type)
rownames(type)<-rownames(cox.sum1)
cox.sum <- cbind(cox.sum1, cox.sum2, type)
cox.sum <- cox.sum[c(1:6),]
rownames(cox.sum) <- c('Age','Male', 'PDMG', 'Stage III-IV',
                       'Chemotherapy',  
                       'Radiation')
colnames(cox.sum)[c(2,8:9,10)] <- c('HR','lower','upper','type')
cox.sum$var <- rownames(cox.sum)
write.table(cox.sum, paste0('TMA-cox of cluster 0.66 (OS,340)', '.txt'), 
            quote = F, sep = '\t', row.names = T)
pd <- position_dodge(0.1)
g <- ggplot(cox.sum, aes(x=var, y=HR,color = type),legend.title="Type") + ggtitle( ' ')+
  geom_point(size = 2) + geom_errorbar(aes(ymin=lower, ymax=upper), size= 1.8,width=.2, position=pd) +
  theme_bw() + theme(panel.grid=element_blank())  +
  geom_line(position=pd, size = 1.8) + xlab(' ') + ylab('Hazard Ratio (HR)') +
  geom_hline(yintercept = 1, linetype = 'dashed', color = "blue", size = 1) +
  theme(axis.text.x = element_text(angle = 90, hjust = 0.5, 
                                   vjust = 0.6, size = 15,color= 'black',face='bold'),
        axis.title.y = element_text(angle = 90, hjust = 0.5, 
                                    vjust = 0.6, size = 15,color= 'black',face='bold'),
        axis.text.y = element_text(angle = 90, hjust = 0.5, 
                                   vjust = 0.6, size = 15,color= 'black',face='bold'))  +
  geom_point(position=pd)+scale_colour_manual(values= c('black','red' ))


tiff(paste0('TMA-Cox Model of cluster 0.66 (OS,340)', '.tiff'),
     width=6,height=4, units="in", compression="lzw", res=150)
print(g)
dev.off()
#Supplementary Fig. 4b
setwd("D:/SEER-OSCC-R-1013/AAAA final manuscript and data/original data-20211020")
dat1<-read.csv("20X-s-slide.csv",sep=',',header = T)
colnames(dat1)

dat2<-read.csv("TMA-fina1(340).csv",sep=',',header=T)
table(dat3$Grade)
Clin<-merge(dat1,dat3,by= "ID")
table(Clin$cluster.0.66.)
Clin$cluster.0.66.[Clin$cluster.0.66. == 'PDMG'] <-'ZPDMG'

table(Clin$Grade)
library(ggplot2)
library(survival)
library(survminer)
library(xtable)

Clin$OS_5year <- as.numeric(Clin$OS_5year)
Clin$OS_time_5year <-as.numeric(Clin$OS_time_5year)
Clin$Age <- as.numeric(Clin$Age)
sur <- Surv(time = Clin$OS_time_5year, event = Clin$OS_5year, type = 'right')
Clin$Stage[Clin$Stage==1 |Clin$Stage ==2] <-'I-II'
Clin$Stage[Clin$Stage==3 |Clin$Stage ==4] <-'III-IV'

Clin$cluster.0.66.[Clin$cluster.0.66. == 'PDMG'] <-'ZPDMG'
colnames(Clin)
sur <- Surv(time = Clin$OS_time_5year, event = Clin$OS_5year, type = 'right')

Clin$Grade[Clin$Grade == 1] <- 'I'
Clin$Grade[Clin$Grade == 2] <- 'II'
Clin$Grade[Clin$Grade == 3] <- 'III'

sur <- Surv(time = Clin$OS_time_5year, event = Clin$OS_5year, type = 'right')

cox <- coxph(sur~Age+Gender
             +Grade+Stage+Chemotherapy+Radiation, data=Clin)
cox.sum1 <- xtable(summary(cox)$coefficient)
cox.sum2 <- xtable(summary(cox)$conf.int)
type <-c("Others",  "Others","Grade II",'Grade II',"Others","Others","Others")
type=data.frame(type)
rownames(type)<-rownames(cox.sum1)
cox.sum <- cbind(cox.sum1, cox.sum2, type)
cox.sum <- cox.sum[c(1:7),]
rownames(cox.sum) <- c('Age','Male', "Grade II",'Grade III', 'Stage III-IV',
                       'Chemotherapy',  
                       'Radiation')
colnames(cox.sum)[c(2,8:9,10)] <- c('HR','lower','upper','type')
cox.sum$var <- rownames(cox.sum)
write.table(cox.sum, paste0('TMA-cox of grade-20211002', '.txt'), 
            quote = F, sep = '\t', row.names = T)
pd <- position_dodge(0.1)
cox.sum$var <- factor(cox.sum$var,levels=c('Age','Chemotherapy','Male', "Grade II",'Grade III',
                                           'Stage III-IV',
                                           'Radiation'))

g <- ggplot(cox.sum, aes(x=var, y=HR,color = type),legend.title="Type") + ggtitle( ' ')+
  geom_point(size = 2) + geom_errorbar(aes(ymin=lower, ymax=upper), size= 1.8,width=.2, position=pd) +
  theme_bw() + theme(panel.grid=element_blank())  +
  geom_line(position=pd, size = 1.8) + xlab(' ') + ylab('Hazard Ratio (HR)') +
  geom_hline(yintercept = 1, linetype = 'dashed', color = "blue", size = 1) +
  theme(axis.text.x = element_text(angle = 90, hjust = 0.5, 
                                   vjust = 0.6, size = 15,color= 'black',face='bold'),
        axis.title.y = element_text(angle = 90, hjust = 0.5, 
                                    vjust = 0.6, size = 15,color= 'black',face='bold'),
        axis.text.y = element_text(angle = 90, hjust = 0.5, 
                                   vjust = 0.6, size = 15,color= 'black',face='bold'))  +
  geom_point(position=pd)+scale_colour_manual(values= c('red','black' ))


tiff(paste0('D:/SEER-OSCC-R-1013/AAAA final manuscript and data/TMA/TMA-Cox Model of grade-20211003', '.tiff'),
     width=6,height=4, units="in", compression="lzw", res=150)
print(g)
dev.off()



