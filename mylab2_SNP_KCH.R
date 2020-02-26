setwd("Dropbox/2020_UMASS/Human_Genome_Class_Jeffrey/")

#Vectors
SNPs <- c("AA", "AA", "GG", "AG", "AG", "AA","AG", "AA", "AA", "AA", "AG")

#Factors 
SNPs_cat <- factor(SNPs)
table(SNPs_cat)
plot(SNPs_cat)
as.numeric(SNPs_cat)

#Matrices
Day1 <- c(2,4,6,8)
Day2 <- c(3,6,9,12)
Day3 <- c(1,4,9,16)
A <- cbind(Day1,Day2,Day3)
B <- rbind(Day1,Day2,Day3)

Day4 <- c(5,10,11,20)
C <- rbind(B,Day4)

A * 10
A[1]
A[12]

A[ ,c(1,3)]
A[c(2,4), ]
t(A)

#Data Frames
Gene1 <- c(2,4,6,8)
Gene2 <- c(3,6,9,12)
Gene3 <- c(1,4,9,16)
Gene <- c("Day 1", "Day 2", "Day 3", "Day 4")
RNAseq <- data.frame(Gene1, Gene2, Gene3, row.names = Gene)
plot(RNAseq$Gene1,RNAseq$Gene3)
plot(RNAseq$Gene2,RNAseq$Gene3)

RNAseq$Gene4 <- c(5,10,15,20)
RNAseq[,"Gene5"] <- c(1,2,3,3)
RNAseq["Day 5",] <- rbind(10,14,20,22,3)

#Object types
x <- 1
str(x)
a <- "ATGCCCTGA"
str(a)
str(SNPs)
str(SNPs_cat)
str(B)
str(RNAseq)

SNP_table <- read.table("23andMe_example_cat25.txt", header = T, sep = "\t")
names(SNP_table)
str(SNP_table)
levels(SNP_table$genotype)
dim(SNP_table)
class(SNP_table)
head(SNP_table, n=10)
head(SNP_table, n=5)

SNP_table$chromosome <- as.factor(SNP_table$chromosome)
str(SNP_table)
SNP_table$chromosome <- as.integer(SNP_table$chromosome)
str(SNP_table)

#subset
SNP_table_AG <- subset(SNP_table, genotype == 'AG')
table(SNP_table_AG$chromosome)
SNP_table_CHR <- subset(SNP_table, position >700000 & position <800000)


#Excersise 
#E1

vec.1 <- c(1,3,6,9,12)
vec.2 <- c(1,0,1,0,1)

vec.sum <- vec.1 + vec.2
vec.subs <- vec.1 - vec.2
vec.mult <- vec.1 * vec.2
vec.div <- vec.1 / vec.2

#E2
vec.3 <- c(0,1,2,3)
str(vec.3) #num
vec.4 <- c("aa","bb","cc","dd")
str(vec.4) #chr
vec.5 <- c("aa",1,"bb",2)
str(vec.5) #chr

#E3
genotype1 <- c("AA","AA","AG","GG","GG")
genotype2 <- c("AA", "AA", "GG", "GG", "GG")
genotype <- cbind(genotype1,genotype2)
table(genotype) 
#genotype
#AA AG GG 
#4  1  5

#E4
time <- c(0,2,4,6,8)
treatment1 <- c(0,1,2,3,4)
treatment2 <- c(0,2,4,6,8)
treatment3 <- c(0,3,6,9,12)
treatment <- as.data.frame(cbind(treatment1,treatment2,treatment3))
treatment$Time <- c(0,2,4,6,8)
treatment
plot(treatment$Time,treatment$treatment3, xlab = "Time", ylab = "Treatment 3")

#E5
SNP_23me <- read.table("23andMe_complete.txt", header =  T, sep = "\t")
str(SNP_23me)
str(SNP_23me$chromosome) # Factor 
#chromosome object type in truncated file -> int only one level
#chromosome object type in big file -> Factor has multiple levels 

#E6
SNP_23me_genotype <- table(SNP_23me$genotype)
SNP_23me_genotype

#E7
SNP_23me_genotypeA <- subset(SNP_23me, genotype =='A')
head(SNP_23me_genotypeA, n=10)
str(SNP_23me_genotypeA)
table(SNP_23me_genotypeA$chromosome) #chromosome X, Y, MT are haploid with only one copy of the genome, MT has 732 'A', X has 5782 'A' & Y has 162 'A'
GenotypeA_table <- table(SNP_23me_genotypeA$chromosome)

#Table for Frequency of genotype A
GenotypeA_table <- as.data.frame(GenotypeA_table)
GenotypeA_table <- GenotypeA_table[-c(1:22), ]
names(GenotypeA_table)[1] <- paste("CHR")
