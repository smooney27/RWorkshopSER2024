# Slide 10 -- Note that it's good practice to load all libraries up front
# Note that you'll need to replace /Users/sjm2186/RWorkshopSER2023/ with your path
library(haven)
demo <- read_xpt('/Users/sjm2186/RWorkshopSER2023/DEMO_D.XPT')
phthalates <- read_xpt('/Users/sjm2186/RWorkshopSER2023/PHTHTE_D.XPT')

# Slide 11 -- horizontal merge the two NHANES data frames
nhanes <- merge(demo, phthalates, by='SEQN')

# Slide 12 -- Filter to women between 15 and 45
women <- nhanes[nhanes$RIAGENDR == 2,]
women1545 <- women[women$RIDAGEEX >= 15*12 & women$RIDAGEEX <= 45*12,]
nrow(women1545)

# Slide 13 -- identify columns
colnames(women1545)
phthalate_column_names <- c('URXMHP',
				'URXMHH',
				'URXECP',
				'URXMOH',
				'URXMC1',
				'URXMIB',
				'URXMBP',
				'URXMZP',
				'URXMEP')
phthalate_column_names

# Slide 14 -- bulk log-transform the elegant way
log_phthalate_column_names <- paste('Ln', phthalate_column_names, sep='')
log_phthalate_column_names
women1545[,log_phthalate_column_names] <- log(women1545[,phthalate_column_names])

# Slide 15, how I actually did it the first time before I thought about it
log_phthalate_column_names <- vector(length=length(phthalate_column_names))
for (i in 1:length(phthalate_column_names)) {
	new_column_name <- paste('Ln', phthalate_column_names[i], sep='')
	women1545[,new_column_name] <- log(women1545[,phthalate_column_names[i]])
	log_phthalate_column_names[i] <- new_column_name
}
log_phthalate_column_names

# Slide 15, The brute force way, which also works
women1545$LnURXMHP <- log(women1545$URXMHP)
women1545$LnURXMHH <- log(women1545$URXMHH)
women1545$LnURXECP <- log(women1545$URXECP)
women1545$LnURXMOH <- log(women1545$URXMOH)
women1545$LnURXMC1 <- log(women1545$URXMC1)
women1545$LnURXMIB <- log(women1545$URXMIB)
women1545$LnURXMBP <- log(women1545$URXMBP)
women1545$LnURXMZP <- log(women1545$URXMZP)
women1545$LnURXMEP <- log(women1545$URXMEP)

# Slide 22 -- load FactoMineR and do the PCA
library(FactoMineR)
complete_women1545 <- women1545[complete.cases(women1545[,log_phthalate_column_names]),]
pca.result <- PCA(complete_women1545[,log_phthalate_column_names], 
			            ncp=2, 
                  row.w=complete_women1545$WTMEC2YR)
rotated <- varimax(pca.result$var$coord)
round(matrix(rotated$loadings, ncol=2), 2)

