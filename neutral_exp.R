#################################################

# Run 1000 simulations,
# on a popsize of 1000 each simulation for x# of generations
# Note there's no selection in this as yet!!!! But actually I was thinking
# We can just calculate off allele distributions at the end?
# Just doing random drift and outcrossing to start
# Not sure if we should start with some initial tetras or create them from dips
# HMMMMMMMMMMMMMM

totalSims <- 2
popsize.dip <- 1000
popsize.tetra <- 1000
totalGen <- 100

# Define the genotypes that would exist as diploids initially (as homozytoes)
# Note this is a single locus with two alleles
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
Output = matrix(0,totalGen,15)
OutputSims = matrix (0, totalSims, 25)

# Start simulation loop - i.e. for each simulation
for(isim in 1:totalSims)
{
  # Keep track of pseudo-random seeds for each simulation
  seeder<-round(runif(min=2, max = 80E4, n=1),2)
  set.seed(seeder)

  # We can alter the nrow arg to change the starting frequencies in each
  # Right now I start the homozygotes in tetras and dips at p = 0.5
  Dip1 <- matrix(data = Dip1, nrow = popsize.dip*.5, ncol = 2, byrow = T)
  Dip2 <- matrix(data = Dip2, nrow = popsize.dip*.5, ncol = 2, byrow = T)

  Tetra1 <- matrix(data = Tetra1, nrow = popsize.tetra*.5, ncol = 4, byrow = T)
  Tetra2 <- matrix(data = Tetra2, nrow = popsize.tetra*.5, ncol = 4, byrow = T)

  # Make the initial generation parents array of the two types
  dip.parents <- rbind(Dip1, Dip2)
  tetra.parents <- rbind(Tetra1, Tetra2)

  #Start the generation loop - i.e. for each generation
  #for(igen in 1:totalGen)
  #{
    moms.dip <- dip.parents[sample(nrow(dip.parents), popsize.dip, replace=T),]
    num.dip.outcrossers <- rbinom(1, popsize.dip, outcrossingrate)
    selfers.dip <- popsize.dip - num.dip.outcrossers

####### HERE IS A HARD PART - we can identify a doubling but then need to double
##### array size mannnnnnnn - actually I think the code in R is easy - gotta think where
##### that line is gonna go
    num.dip2tetra <- rbinom(1, popsize.dip, autopolyrate)
    num.tetra2dip <- rbinom(1, popsize.tetra, redpolyrate)
#################################################################################
    moms.tetra <- tetra.parents[sample(nrow(tetra.parents), popsize.tetra, replace=T),]
    num.tetra.outcrossers <- rbinom(1,popsize.tetra,outcrossingrate)

    selfers.tetra <- popsize.tetra - num.tetra.outcrossers


    Output[igen,1] <- igen
    Output[igen,2] <- popsize.dip
    Output[igen,3] <- popsize.tetra
    Output[igen,4] <- num.dip.outcrossers
    Output[igen,5] <- num.dip2tetra
    Output[igen,6] <- num.tetra2dip
    Output[igen,7] <- num.tetra.outcrossers
    Output[igen,8] <- selfers.dip
    Output[igen,9] <- selfers.tetra


# Section below I want to keep track of who's outcrossing vs. sexing
# And who is changing their stripes from diploid to tetraploid and
# vice versa (note these last two are really low)

    sex.dip <- sample(1:popsize.dip, num.dip.outcrossers)
    selfing.dip <- setdiff(1:popsize.dip, sex.dip)

    sex.tetra <- sample(1:popsize.tetra, num.tetra.outcrossers)
    selfing.tetra <- setdiff(1:popsize.tetra, sex.tetra)

    new.polys <- sample(1:popsize.dip, numdip2tetra)
    stay.dip <- setdiff(1:popsize.dip, new.polys)

    new.dips <- sample(1:popsize.tetra, numtetra2dip)
    stay.tetra <- sample(1:popsize.tetra, new.dips)

    # Identify those rows
    sex.dip <- moms.dip[sex.dip,]
    self.dip <- moms.dip[selfing.dip,]
    sex.tetra <- moms.tetra[sex.tetra,]
    self.tetra <- moms.tetra[selfing.tetra,]

    # Start with outcrossing diploids - i.e. one locus two possible alleles
    # MOTHERS (i.e. NOT POLLEN), i.e. the maternal allele at thelocus

    locus.dip.maternal <- sex.dip[,1:2]
    mom.sex.locus.dip.allele1 <- NULL

    for(i in 1:nrow(locus.dip.maternal))
    {
      mom.sex.locus.dip.allele1[i] = sample(locus.dip.maternal[i,],1)
    }

    # Now bring in the allele from diploid dads - the pollen
    dads.dip <- sample(1:popsize.dip,num.dip.outcrossers)
    dads.id.dip <- dip.parents[dads.dip,]

    locus.dip.pollen <- dads.id.dip[,1:2]
    dad.sex.locus.dip.allele2 <- NULL
    for(i in 1:nrow(locus.dip.pollen))
    {
      dad.sex.locus.dip.allele2[i] = sample(locus.dip.pollen[i,],1)
    }


# Then cbind the alleles for diploid locus 1 together making a new dataframe

new.outcrossed.dip.progeny <- data.frame(cbind(mom.sex.locus.dip.allele1, dad.sex.locus.dip.allele2)

#################################################################################
# Now repeat the whole outcrossing process for the tetraploids - note it's double
# what is above - for every locus there should be 4 alleles in progeny with
# 2 alleles from mom and 2 alleles from pollen/dad
#################################################################################

locus.tetra.maternal <- sex.tetra[,1:4]


  mom.sex.locus.tetra.allele1 <- NULL
  for(i in 1:nrow(locus.tetra.maternal))
  {
  mom.sex.locus.tetra.allele1[i] = sample(locus.tetra.maternal[i,], 1, replace = T)
  }

  mom.sex.locus.tetra.alelle2 <-NULL
  for(i in 1:nrow(locus.tetra.maternal))
  {
    mom.sex.locus.tetra.allele2[i] = sample(locus.tetra.maternal[i,], 1, replace =T)
  }

# Now bring in the 2 alleles from tetraploid dads - the pollen
# Need to sample 2 from tetraploid aaray
  dads.tetra <- sample(1:popsize.tetra, num.tetra.outcrossers)
  dads.id.tetra <- tetra.parents[dads.tetra,]

  locus.tetra.pollen <- dads.id.tetra[,1:4]

  dad.sex.locus.tetra.allele1 <- NULL
  for(i in 1:nrow(locus.tetra.pollen))
  {
  dad.sex.locus.tetra.allele1[i] = sample(locus.tetra.pollen[i,],1, replace =T)
  }

  dad.sex.locus.tetra.allele2 <- NULL
  for(i in 1:nrow(locus.tetra.pollen))
  {
    dad.sex.locus.tetra.allele2[i] = sample(locus.tetra.pollen[i,],1, replace =T)
  }

# Then cbind the alleles for diploid locus 1 together making a new dataframe

new.outcrossed.tetraploid.progeny <- data.frame(cbind(mom.sex.locus.tetra.allele1,
  mom.sex.locus.tetra.allele2, dad.sex.locus.tetra.allele1,
  dad.sex.locus.tetra.allele2)

#################### SELFING ###################################################
    # Now do the selfers
    # For the selfers - use "self.id" array, and define each locus
    locus.dip.selfer <- self.dip[,1:2]
    locus.dip.selfer <- as.matrix(locus.dip.selfer)


    # Choose alleles for the diploids - sample one allele at a locus

    locus.dip.allele1 <- NULL
    for(i in 1:nrow(locus.dip.selfer))
    {
      locus.dip.allele1[i] <- sample(locus.dip.selfer[i,],1,replace =T)
    }

    locus.dip.allele2 <- NULL
    for(i in 1:nrow(locus.dip.selfer))
    {
      locus.dip.allele2[i] <- sample(locus.dip.selfer[i],1,replace =T)
    }

new.selfed.dip.progeny <- data.frame(cbind(locus.dip.allele1, locus.dip.allele2))

##### SELFING for the tetras - choose one allele from selfing tetra matrix
##### this matrix is defined above - choose one allele
    locus.tetra.selfer <- self.tetra[,1:4]

    locus.tetra.alleles.set1 <- NULL
    for(i in 1:nrow(locus.tetra.selfer))
    {
      locus.tetra.alleles.set1[i] <- sample(locus.tetra.selfer[i,],2,replace =T)
    }

    locus.tetra.alleles.set2 <- NULL
    for(i in 1:nrow(locus.tetra.selfer))
    {
      locus.tetra.allele.set2[i] <- sample(locus.tetra.selfer[i],2,replace =T)
    }


    # Make the array of the selfed tetra progeny
    new.selfed.tetra.progeny <- data.frame(cbind(locus.tetra.alleles.set1,
      locus.tetra.alleles.set2))



######## New generation arrays, i.e. self + sex progeny ###########################
# Combine diploid new gen arrays, just use column names from selfed diploid progeny
    new.dip.generation <- rbind(new.selfed.dip.progeny, setNames(new.outcrossed.dip.progeny,
      names(new.selfed.dip.progeny)))
    new.dip.generation <- as.matrix(new.dip.generation)


    new.tetra.generation <-rbind(new.selfed.tetra.progeny, setNames(new.outcrossed.tetraploid.progeny,
      names(new.selfed.tetra.progeny)))
    new.tetra.generation <- as.matrix(new.tetra.generation)

    # Get homozygotes for each dip and tetra
    # Note at this point I am not differentiating from aa or AA, same for tetra
    dip.homs <- subset(new.dip.generation, locus.dip.allele1==locus.dip.allele2)

    dim.dip.homs<-dim(dip.homs)

    sum.dip.homs <- dim.dip.homs[1]

    tetra.homs <- subset(new.tetra.generation, locus.tetra.allele1 ==
      locus.dip.allele2 == locus.tetra.allele3 == locus.tetra.allele4)

    dim.tetra.homs<-dim(tetra.homs)

    sum.tetra.homs <- dim.tetra.homs[1]

    #How many heterozygotes?
    dip.hets <- popsize.dip - sum.dip.homs
    tetra.hets <- popsize.tetra - sum.tetra.homs

    #Get the other genotypes

    dip.doms <- rowSums(new.dip.generation[,1:2]) == 2
    dip.rec <- rowSums(new.dip.generation[,1:2]) == 0
    dip.other <- rowSums(new.dip.generation[,1:2]) > 0
    dip.doms <- sum(dip.doms)
    dip.rec <- sum(dip.rec)
    dip.other <- sum(dip.other)

    tetra.doms <- rowSums(new.tetra.generation[,1:4]) == 4
    tetra.rec <- rowSums(new.tetra.generation[,1:4]) == 0
    tetra.other <- rowSums(new.tetra.generation[,1:4]) > 0
    tetra.doms <- sum(tetra.doms)
    tetra.rec <- sum(tetra.rec)
    tetra.other <- sum(tetra.other)


    Output[igen,10] <- sum.dip.homs
    Output[igen,11] <- sum.tetra.homs
    Output[igen,12] <- dip.hets
    Output[igen,13] <- tetra.hets
    Output[igen,14] <- dip.doms
    Output[igen,15] <- dip.doms/popsize.dip
    Output[igen,16] <- dip.rec
    Output[igen,17] <- dip.rec/popsize.dip
    Output[igen,18] <- dip.other
    Output[igen,19] <- dip.other/popsize.dip
    Output[igen,20] <- tetra.doms
    Output[igen,21] <- tetra.doms/popsize.tetra
    Output[igen,22] <- tetra.rec
    Output[igen,23] <- tetra.rec/popsize.tetra
    Output[igen,24] <- tetra.other
    Output[igen,25] <- tetra.other/popsize.tetra

    dip.parents = new.dip.generation
    tetra.parents = new.tetra.generation
#  }

  #Output

  # Define outputs for 1000 simulations

  OutputSims[isim,1] <- isim
  OutputSims[isim,2] <- seeder
  OutputSims[isim,3] <- Output[igen,2]
  OutputSims[isim,4] <- Output[igen,3]
  OutputSims[isim,5] <- mean(Output[igen,4])
  OutputSims[isim,6] <- mean(Output[igen,5])
  OutputSims[isim,7] <- mean(Output[igen,6])
  OutputSims[isim,8] <- mean(Output[igen,7])
  OutputSims[isim,9] <- mean(Output[igen,8])
  OutputSims[isim,10] <- mean(Output[igen,9])
  OutputSims[isim,11] <- mean(Output[igen,10])
  OutputSims[isim,12] <- mean(Output[igen,11])
  OutputSims[isim,13] <- mean(Output[igen,12])
  OutputSims[isim,14] <- mean(Output[igen,13])
  OutputSims[isim,15] <- mean(Output[igen,14])
  OutputSims[isim,16] <- mean(Output[igen,15])
  OutputSims[isim,17] <- mean(Output[igen,16])
  OutputSims[isim,18] <- mean(Output[igen,17])
  OutputSims[isim,19] <- mean(Output[igen,18])
  OutputSims[isim,20] <- mean(Output[igen,19])
  OutputSims[isim,21] <- mean(Output[igen,20])
  OutputSims[isim,22] <- mean(Output[igen,21])
  OutputSims[isim,23] <- mean(Output[igen,22])
  OutputSims[isim,24] <- mean(Output[igen,23])
  OutputSims[isim,25] <- mean(Output[igen,24])
  OutputSims[isim,26] <- mean(Output[igen,25])


#}

#OutputSims

# Rename columns
Sim_No <- OutputSims[,1]
Seed_No <- OutputSims[,2]
popsize.dip <- OutputSims[,3]
popsize.tetra <- OutputSims[,4]
mean.no.dip.outcross <- OutputSims[,5]
mean.no.dip2tetra <- OutputSims[,6]
mean.no.tetra2dip <- OutputSims[,7]
mean.no.tetra.outcross <- OutputSims[,8]
mean.no.self.dip <- OutputSims[,9]
mean.no.self.tetra <- OutputSims[,10]
mean.sum.dip.homs <- OutputSims[,11]
mean.sum.tetra.homs <- OutputSims[,12]
mean.dip.hets <- OutputSims[,13]
mean.tetra.hets <- OutputSims[,14]
mean.dip.doms <- OutputSims[,15]
mean.freq.dip.doms <- OutputSims[,16]
mean.dip.rec <- OutputSims[,17]
mean.freq.dip.rec <- OutputSims[,18]
mean.dip.other <- OutputSims[,19]
mean.freq.dip.other <- OutputSims[,20]
mean.tetra.doms <- OutputSims[,21]
mean.freq.tetra.doms <- OutputSims[,22]
mean.tetra.rec <- OutputSims[,23]
mean.freq.tetra.rec <- OutputSims[,24]
mean.tetra.other <- OutputSims[,25]
mean.freq.tetra.other <- OutputSims[,26]

simResults <- data.frame(cbind(Sim_No, Seed_No,popsize.dip, popsize.tetra,
  mean.no.dip.outcross, mean.no.dip2tetra, mean.no.tetra2dip, mean.no.tetra.outcross,
  mean.no.self.dip,mean.no.self.tetra,mean.sum.dip.homs, mean.sum.tetra.homs,
  mean.dip.hets, mean.tetra.hets, mean.dip.doms, mean.freq.dip.doms, mean.dip.rec,
  mean.freq.dip.rec,mean.dip.other,mean.freq.dip.other, mean.tetra.doms,
  mean.freq.tetra.doms, mean.tetra.rec, mean.freq.tetra.rec, mean.tetra.other,
  mean.freq.tetra.other))

simResults

save(simResults, file = "simulationResults.RData")
