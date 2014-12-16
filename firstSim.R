#########################
##### A NOVICE simulation program
#########################

#Simon Renny-Byfield, UC Davis, Dec '14 version 1.0

#I am going to start at the centre of this program and work outwards.
#i.e. first learb how to produce gametes, then how to do this for 
#every individual, then per generation, then per simulation etc etc.

#This is based heavily on Kate Crosby script and will follow the same
#general pattern. It's just my attempt at trying to understand what is
#going on. Where possible I will use the same variable names..
#There is a lot of cutting and pasting here..

#First start with some global paramters, the same as in Kate's script.
totalSims <- 2
popsize<-100
totalGen <- 100
ploidy<-2

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

#make a population of 100 individuals, 1/2 clones od Dip1, 1/2 of Dip2
Dip1 <- matrix(data = Dip1, nrow = popsize*.5, ncol = 2, byrow = T)
Dip2 <- matrix(data = Dip2, nrow = popsize*.5, ncol = 2, byrow = T)
diploids<-rbind(Dip1,Dip2)
#now do the same for tetraploids
Tetra1 <- matrix(data = Tetra1, nrow = popsize*.5, ncol = 4, byrow = T)
Tetra2 <- matrix(data = Tetra2, nrow = popsize*.5, ncol = 4, byrow = T)
tetraploids<-rbind(Tetra1,Tetra2)

####
# Set up some space to save the analysis
####

###The multi-dim array is all wrong at the moment.
###

#set up a multi-dimensional array for the DIPLODS
diploids.df<-array(0,dim=c(totalSims,popsize,totalGen+1,2))
#populate the first generation
diploids.df[,,1,1]<-diploids[,1]
diploids.df[,,1,2]<-diploids[,2]

#set up a multi-dimensional array for the TETRAPLOIDS
tetras.df<-array(0,dim=c(totalSims,popsize,totalGen+1,4))
#populate the first generation, four alleles this time
tetras.df[,1,1]<-tetraploids[,1]
tetras.df[,1,2]<-tetraploids[,2]
tetras.df[,1,3]<-tetraploids[,3]
tetras.df[,1,4]<-tetraploids[,4]

for ( j in 1: totalSims) {
	print(paste("Simulations ", j))
	for ( i in 1:totalGen ) {
		print(paste("generation ", i))
		####
		# for diploids!!!
		####
		#grab the appropriate generation, 1 is the starting generation, see * later
		pop<-diploids.df[j,1:2,i]
		##generate a new bunch of 1000 individuals using the function generateOffpsring(). 
		currentGen<-generateOffpsring(pop)
		#add the new generation to the appropriate section of the 3D array
		#insert into the ith+1 generation (i.e) move one generarion on
		diploids.df[j,i+1,1]<-currentGen[,1]	#this is the first allele per individual
		diploids.df[j,i+1,2]<-currentGen[,2]	#this is the second allele per individual
		
		####
		# for tetraploids!!
		####
	
		#grab the appropriate generation
		popTet<-tetras.df[,i,1:4]
		currentGen<-generateOffpsring(popTet)
		# add the new generation to the appropriate section of the 3D array
		tetras.df[j,i+1,1]<-currentGen[1]	#this is the first allele per individual
		tetras.df[j,i+1,2]<-currentGen[2]	#this is the second allele per individual
		tetras.df[j,i+1,3]<-currentGen[3]	#this is the third allele per individual
		tetras.df[j,i+1,4]<-currentGen[4]	#dito
	}#for i in totlaGens
}#for i in totalSims
#Now make a table for the abundance of the 0 allele over the generations

gensTable<-data.frame("prop"=(apply(diploids.df,2,table)[1,])/(popsize*2))
plot(1:101,gensTable$prop, type = "n", ylim=c(0,1))
lines(1:101,gensTable$prop)


######
# Functions
######

generateOffpsring<-function(pop, popsize) {
	
	seeder<-round(runif(min=2, max = 80E4, n=1),2)
  	set.seed(seeder)
  	
  	##find the mothers for each generation
  	moms <- pop[sample(nrow(pop), popsize, replace=T),]
  	##find the fathers for each generation
  	dads <- pop[sample(nrow(pop), popsize, replace=T),]
  	#these choices shouold essentially be random, so no need to jumble them up any more
	
	#sample half the alleles on an individual, i.e. 1 in diploids, two in tetraploids
	gamete1<-apply(moms,1,function(x) sample(x,length(x)/2,replace = T))
	gamete2<-apply(dads,1,function(x) sample(x,length(x)/2,replace = T))
	
	#unify the gamtes to produce a new individual
	newInds<-cbind(gamete1,gamete2)
	
	#return the object
	return(newInds)
}#generateOffpsring
