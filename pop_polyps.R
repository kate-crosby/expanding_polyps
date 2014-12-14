library(doParallel)
## Loading required package: foreach
## Loading required package: iterators
## Loading required package: parallel
detectCores()
cl <- makeCluster(4)  # Use 4 cores
registerDoParallel(cl)

# Run 1000 simulations, 
# on a popsize of 1000 each simulation for 40 generations
# Note there's no selection in this as yet!!!!
# Just doing random drift and outcrossing to start
# Not sure if we should start with some initial tetras or create them from dips
# HMMMMMMMMMMMMMM
totalSims <- 1000
popsize.dip <- 1000
popsize.tetra <- 10
totalGen <- 100

# Define the genotypes that would exist as diploids initially (as homozytoes)
Dip1 <- c(1,1)
Dip2 <- c(0,0)

# Define the genotypes that would be tetraploids initially (as homozygotes)
# Yes, there can be more types - this is just to keep it simple initially
# I'm going to assume that 1s are good and 0s are bad i.e. (1-s)
# Note I may just go with creating a new tetra with the auto rate and store to 
# new array (so I may do away with tetras present in the initial pop)

Tetra1 <- c(1,1,1,1)
Tetra2 <- c(0,0,0,0)

# Specify outcrossing rate (this will be incorporated into both the dips
# and the tetras), but can always split it up or up it or down it
outcrossingrate <- 0.02

# Specify the autopolyploid rate, and an equal or lesser reduction rate per gen?
# Ask Simon how much red. vs. polyploid occurs - I assume more ups than downs
autopolyrate <- 0.001
redpolyrate <- 0.00001

# Outputs defined below
Output = matrix(0,totalGen,10)
OutputSims = matrix (0, totalSims, 11)

# Start simulation loop
#for(isim in 1:totalSims)
#{
  # Keep track of pseudo-random seeds 
  seeder<-round(runif(min=2, max = 80E4, n=1),2)
  set.seed(seeder)
  
  # Start close to the observed frequencies of the ploidy trait
  Dip1 <- matrix(data = Dip1, nrow = popsize.dip*.5, ncol = 2, byrow = T)
  Dip2 <- matrix(data = Dip2, nrow = popsize.dip*.5, ncol = 2, byrow = T)
  
  Tetra1 <- matrix(data = Tetra1, nrow = popsize.tetra*.5, ncol = 4, byrow = T)
  Tetra2 <- matrix(data = Tetra2, nrow = popsize.tetra*.5, ncol = 4, byrow = T)
  # Make the initial generation parents array of the two types
  dip.parents <- rbind(Dip1, Dip2)
  tetra.parents <- rbind(Tetra1, Tetra2)
  
  #Start the generation loop
  #for(igen in 1:totalGen) 
  #{ 
    moms.dip <- dip.parents[sample(nrow(dip.parents), popsize.dip, replace=T),]
    num.dip.outcrossers <- rbinom(1,popsize.dip,outcrossingrate)
    
    selfers.dip <- popsize.dip - num.dip.outcrossers
    
    moms.tetra <- dip.parents[sample(nrow(dip.parents), popsize.dip, replace=T),]
    num.tetra.outcrossers <- rbinom(1,popsize.tetra,outcrossingrate)
    
    selfers.tetra <- popsize.tetra - num.tetra.outcrossers
    
    
    
    Output[igen,1] <- igen
    Output[igen,2] <- popsize.dip
    Output[igen,3] <- popsize.tetra
    Output[igen,4] <- num.dip.outcrossers
    Output[igen,5] <- num.tetra.outcrossers
    Output[igen,6] <- selfers.dip
    Output[igen,7] <- selfers.tetra
    
    sex.dip <- sample(1:popsize.dip, num.dip.outcrossers)
    selfing.dip <- setdiff(1:popsize.dip, sex.dip)      
    
    sex.tetra <- sample(1:popsize.tetra, num.tetra.outcrossers)
    selfing.tetra <- setdiff(1:popsize.tetra, sex.tetra) 
    
    # Identify those rows
    sex.dip <- moms.dip[sex.dip,]
    self.dip <- moms.dip[selfing.dip,]
    sex.tetra <- moms.tetra[sex.tetra,]
    self.tetra <- moms.dip[selfing.tetra,]
    
    # Start with outcrossing diploids - i.e. one locus two possible alleles
    # MOTHERS (i.e. NOT POLLEN), i.e. the maternal allele at thelocus
    
    locus.dip.maternal <- sex.id.dip[,1:2]
    mom.sex.locus1.color.allele1 <- NULL

    locus1.color.maternal <- sex.id[,1:2]
    locus2.color.maternal <- sex.id[,3:4]
    
    mom.sex.locus1.color.allele1 <- NULL
    for(i in 1:nrow(locus1.color.maternal))
    {  
      mom.sex.locus1.color.allele1[i] = sample(locus1.color.maternal[i,],1)
    }
    
    mom.sex.locus2.color.allele2 <- NULL
    for(i in 1:nrow(locus2.color.maternal))
    {
      mom.sex.locus2.color.allele2[i] = sample(locus2.color.maternal[i,],1)
    }
    
    # Pollen  
    dads <- sample(1:popsize,num.outcrossers)
    dads.id <- parents[dads,]
    
    locus1.color.pollen <- dads.id[,1:2]
    locus2.color.pollen <- dads.id[,3:4]
    
    dad.sex.color.locus1allele1 <- NULL
    for(i in 1:nrow(locus1.color.pollen))
    {
      dad.sex.color.locus1allele1[i] = sample(locus1.color.pollen[i,],1)
    }
    
    dad.sex.color.locus2allele2 <- NULL
    for(i in 1:nrow(locus2.color.pollen))
    {
      dad.sex.color.locus2allele2[i] = sample(locus2.color.pollen[i,],1)
    }
    # Then cbind the alleles for locus 1 and locus 2 together making a new dataframe
    
    new.outcrossed.progeny <- data.frame(cbind( mom.sex.locus1.color.allele1,
                                                dad.sex.color.locus1allele1, mom.sex.locus2.color.allele2,
                                                dad.sex.color.locus2allele2))
    
    # For the selfers - use "self.id" array, and define each locus
    locus1.color.selfer <- self.id[,1:2]
    locus1.color.selfer <- as.matrix(locus1.color.selfer)
    locus2.color.selfer <- self.id[,3:4]
    locus2.color.selfer <- as.matrix(locus2.color.selfer)
    
    # Choose alleles
    
    locus1.color.allele1 <- NULL
    for(i in 1:nrow(locus1.color.selfer))
    {
      locus1.color.allele1[i] <- sample(locus1.color.selfer[i,],1,replace =T)
    }
    
    locus1.color.allele2 <- NULL
    for(i in 1:nrow(locus1.color.selfer))
    {
      locus1.color.allele2[i] <- sample(locus1.color.selfer[i],1,replace =T)
    }
    
    locus2.color.allele1 <- NULL
    for(i in 1:nrow(locus2.color.selfer))
    {
      locus2.color.allele1[i]  <- sample(locus2.color.selfer[i,],1, replace =T)
    }
    
    locus2.color.allele2 <- NULL
    for(i in 1:nrow(locus2.color.selfer))
    {
      locus2.color.allele2[i]  <- sample(locus2.color.selfer[i,],1, replace =T)
    }
    
    
    # Make the array of the selfed progeny
    new.selfed.progeny <- data.frame(cbind(locus1.color.allele1,
                                           locus1.color.allele2, 
                                           locus2.color.allele1,
                                           locus2.color.allele2))
    
    # Combine arrays, just use column names from selfed progeny
    new.generation <- rbind(new.selfed.progeny, setNames(new.outcrossed.progeny, names(new.selfed.progeny)))
    new.generation <- as.matrix(new.generation)              
    
    #Get homozygotes
    homs <- subset(new.generation, locus1.color.allele1==locus1.color.allele2 & 
                     locus2.color.allele1==locus2.color.allele2)
    
    dim.homs<-dim(homs)
    
    sum.homs <- dim.homs[1]
    
    #How many heterozygotes?
    hets <- popsize - sum.homs
    
    #Get the morphotypes
    dark <- rowSums(new.generation[,1:4]) > 0
    light <- rowSums(new.generation[,1:4]) == 0
    dark <- sum(dark)
    light <- sum(light)
    
    Output[igen,5] <- sum.homs
    Output[igen,6] <- hets
    Output[igen,7] <- light
    Output[igen,8] <- light/popsize
    Output[igen,9] <- dark
    Output[igen,10] <- dark/popsize
    
    
    parents = new.generation
  }
  
  #Output
  
  # Define outputs for 1000 simulations
  
  OutputSims[isim,1] <- isim
  OutputSims[isim,2] <- seeder
  OutputSims[isim,3] <- Output[igen,2]
  OutputSims[isim,4] <- mean(Output[igen,3])
  OutputSims[isim,5] <- mean(Output[igen,4])
  OutputSims[isim,6] <- mean(Output[igen,5])
  OutputSims[isim,7] <- mean(Output[igen,6])
  OutputSims[isim,8] <- mean(Output[igen,7])
  OutputSims[isim,9] <- mean(Output[igen,8])  
  OutputSims[isim,10] <- mean(Output[igen,9])
  OutputSims[isim,11] <- mean(Output[igen,10])
  
  
}

#OutputSims

# Rename columns
Sim_No <- OutputSims[,1]
Seed_No <- OutputSims[,2]
popsize <- OutputSims[,3] 
mean_no_outcross <- OutputSims[,4]
mean_no_selfers <- OutputSims[,5]
mean_homozygotes <- OutputSims[,6]
mean_heterozygotes <- OutputSims[,7]
mean_light <- OutputSims[,8]
mean_light_freq <- OutputSims[,9]
mean_dark <- OutputSims[,10]
mean_dark_freq <- OutputSims[,11]


simResults <- data.frame(cbind(Sim_No, Seed_No, popsize, mean_no_outcross,
                               mean_no_selfers, mean_homozygotes, mean_heterozygotes,
                               mean_light, mean_light_freq, mean_dark, mean_dark_freq))

simResults

save(simResults, file = "simulationResults.RData")
