/* 
 * File:   main.cpp
 * Author: stephan peischl
 *
 * Created on February 27, 2012, 5:37 PM
 */

#include <cstdlib>
#include <iostream> // For input/output
#include <fstream>
#include <string>

#include "range_expansion.h"
//#include "rng.h"
#include "rng2.h"

#ifdef  _GCC_
#include <sys/time.h>
#else
#include <time.h>
#endif

#include <unistd.h>

//#include <boost\math\special_functions\binomial.hpp>
//#include <boost\random\linear_congruential.hpp>
//#include <boost\random\uniform_real.hpp>
//#include <boost\random\variate_generator.hpp>
//#include <boost\random\mersenne_twister.hpp>

using namespace std;
//using namespace boost::math;

// Global declarations:
//boost::mt19937 gen;		// Random generator recommended by Boost; declared globally so that it can be 
							//	 seeded in main.cpp and then used elsewhere

using namespace std;

int main() {
    
    // A static seed, useful for debuggin:
    //
    //gen.seed(42U);	
	
    // In real use, seed the random number generator from the system clock -- don't forget to do this!:
    //
    //gen.seed(static_cast<unsigned int>(time(NULL)));	
   
   //Loro_09_04_13 Random number initialization
   long randSeed;
   #ifdef  _GCC_
   srand(1);
   struct timeval tv;
   struct timezone tz;
   gettimeofday(&tv, &tz);
   randSeed = ((tv.tv_sec ^ tv.tv_usec) ^ getpid()) % 1000000; //Implies only  one million possible seeds!

   #else
   randomize();
   long curRand = rand() % 1000;
   long curTime = time(NULL);
   randSeed = (long) (1.0 * curTime + curClock * curRand) % (200000 - curRand);
   #endif

   //Could be good to have the possibility to use an external seed for random number generator
   long manualSeed=0;
   bool seedFound=false;
   long curSeed;
   if (seedFound) curSeed=abs(manualSeed);
   else curSeed = randSeed;
   
   
   
   initializeRan3(curSeed);

    //srand(1);
    //long curRand = rand() % 1000;
 
    //long randSeed = ( getpid()) % 1000000;
    //initializeRan3(randSeed);
       
    // parameters 
    
    int anc_pop_size = 1000;                                                    // size of ancestral population
    int capacity = 200;                                                         // carrying capacity of a deme
    int generations = 500;                                                      // number of generations that the expansion lasts
    int burnin_time = 1;                                                        // number of burn in phase of the ancestral population
    int IndSample = 100;                                                        // number of individuals that are sampled at the end of the simulation
    double wf;
    double s = 0.01;                                                            // selection coefficient
    double m = 0.1;                                                             // migration rate
    double mu = 0.05;
    int snapshot = 10;                                                          // number of generations between two snapshots of the whole metapopulation
    int m1,m2;                                                                  // size of the 2D grid
    int replicates = 2;                                                         // number of replicates of the simulation
    int expansion_start = 100;
    
    int selectionMode = 0;                                                      // SS = 0, HS = 1
    int expansionMode = 1;                                                      // 0 = linear expansion, 1 = radial expansion, 2 linear - starting from both ends of the habitat, 
                                                                                // 3 first and last deme are colonized (at opposite edges), 4 = four corners of the habitat are colonized
    

    const char base[] = "output_";

   
    char filename[150]; 
    char filename2[150]; 
    char filename3[150];  
    char filename4[150];  
    
    const char filename_log[] = "log_";

    
    int rep = 30;                                                                
    int i,j,k;
    int loci = 100;
                                                           
                                            
    ofstream outputfile,outputfile2,outputfile3,outputfile4,logfile;                        // streams to outputfiles
    logfile.open(filename_log);
    
    logfile << "Random number generator initialized with seed " << curSeed << "\n";
    
    m1=5;                                                                       // size of the grid
    m2=200;
    
    int initial_colonized = 10;                                                 // number of initially colonized demes (location of demes is determined via mode)
    
    int tot_demes = m1*m2;                                                      // total number of demes in the world
    
    ifstream infile;

    
    infile.open ("parameterfile.txt", ifstream::in);                            
    
    
    double par;
    vector<double> params;
    
    while (infile >> par){
                
        params.push_back(par);
    
    }
    
 
    
    if (params.size() > 11)
    {
        m1 = params[0];
        m2 = params[1];
        tot_demes = m1*m2;
        expansionMode = params[2];
        selectionMode = params[3];
        anc_pop_size = params[4];
        burnin_time = params[5];
        capacity = params[6];
        mu = params[7];
        m = params[8];
        s = params[9];
        expansion_start = params[10];
        generations = params[11];
        replicates = params[12];

        initial_colonized = 10*m1; 
        
    }
    
    else 
    {
        logfile << "\n NO VALID PARAMETER FILE FOUND! USING DEFAULT PARAMETERS. \n";
    }
    
    logfile << "Simulating an expansion on a " << m1 << "x"<<m2<<" grid. \n";
    logfile << "Selection is ";
    if(selectionMode == 0)
        logfile << "soft.\n";
    else
        logfile << "hard.\n";
    
    logfile << "\nParameters: \n   Carrying capacity: "     << capacity << 
                             "\n   Migration rate: "        << m << 
                             "\n   Selection coefficient: " << s << 
                             "\n   Mutation rate: "         << mu << 
                             "\n   Burnin time: "           << burnin_time <<
                             "\n   Expansion start: "       << expansion_start;
    logfile << "\n Ancestral population size: " << anc_pop_size << "\n";
    logfile << "\n Expansion Mode: " << expansionMode << "\n";
    logfile << "\n Number of replicates: " << replicates<< "\n";
    
    logfile << endl;
    
    cout << "Simulating an expansion on a " << m1 << "x"<<m2<<" grid. \n";
    cout << "Selection is ";
    if(selectionMode == 0)
        cout << "soft.\n";
    else
        cout << "hard.\n";
    
    cout << "\nParameters: \n   Carrying capacity: "     << capacity << 
                             "\n   Migration rate: "        << m << 
                             "\n   Selection coefficient: " << s << 
                             "\n   Mutation rate: "         << mu << 
                             "\n   Burnin time: "           << burnin_time <<
                             "\n   Expansion start: "       << expansion_start;
    cout << "\n  Ancestral population size: " << anc_pop_size << "\n";
    cout << "\n  Expansion Mode: " << expansionMode;
    cout << "\n  Number of replicates: " << replicates;
    
    cout << endl;
 
    vector<double> outdata(tot_demes);  
    
    World Grid2D(m1,m2,initial_colonized,anc_pop_size,burnin_time,capacity,expansionMode,mu,s,m);               // initialize world: grid size (m1,m2), number of initially colonized demes, 
                                                                                                                // size of original population, burn in time of original population, capacity of demes, mode of intial colonization   
    
    srand(time(NULL));
    
    
    
    for (rep = 0;rep<replicates;rep++)                                         // loop that simulates replicates for the same set of parameters and initial conditions
    {
        Grid2D.setParams(capacity,mu,s,m);
        sprintf(filename,"%s%d",base,rep);
        sprintf(filename2,"%s%s%d",base,"hom_wt_",rep);
        sprintf(filename3,"%s%s%d",base,"het_",rep);

        
        outputfile.open(filename);
//        outputfile2.open(filename2);
//        outputfile3.open(filename3);
        
         // migration barrier along expansion axis:
        
//        for(i = 10; i < m2; i++)
//        {
//                Grid2D.startExpansion((m1/2)*m2+i,0);
//
//        }
//        
       for (i = 0;i < m1; i++)
        {
//             Grid2D.startExpansion((i)*m2+(initial_colonized/m1)+1,0);     //  Migration-barrier for burn in
        }
     

        for (k = 0;k<expansion_start;k++)                                      
        {                        
            Grid2D.migrate(tot_demes);                                       // migration        
            Grid2D.reproduce(selectionMode);                                // reproduction and selection     
        }  
    
        cout << "\n Burn-in finished, expansion into new territory starts.";

        for (i = 0;i < m1; i++)
        {
             Grid2D.startExpansion((i)*m2+(initial_colonized/m1)+1,capacity);     // remove Migration-barrier after burn in
        }
        

        
        for(i = 0; i< (generations)/snapshot;i++)                                 
        {

                outdata = Grid2D.getMeanFit();                                  // get mean fitness of the whole population
    
                for (j = 0;j<tot_demes;j++)                                     // write it to file
                { 
        
                        outputfile << outdata[j] << " ";
                
                }
                outputfile << "\n";
                                
                outdata = Grid2D.getGenotypeFrequencies(0,loci,0);              // get ancestral homozygotes
                       
                sprintf(filename4,"%s%s%d",filename2,"_gen_",(i*snapshot));
                outputfile3.open(filename4);
                for (j = 0;j<(tot_demes);j++)                                     
                { 
                    for ( k = 0;k< loci;k++)
                    {
                        outputfile3 << outdata[j*loci+k] << " ";

                    }
                    outputfile3 << "\n";
                }
                outputfile3.close();
                
                outdata = Grid2D.getGenotypeFrequencies(0,loci,1);              // get  heterozygotes
                
                sprintf(filename4,"%s%s%d",filename3,"_gen_",(i*snapshot));
                outputfile3.open(filename4);
                for (j = 0;j<(tot_demes);j++)                                     
                { 
                    for ( k = 0;k< loci;k++)
                    {
                        outputfile3 << outdata[j*loci+k] << " ";

                    }
                    outputfile3 << "\n";
                }
                outputfile3.close();
                
                for (k = 0;k<snapshot;k++)                                      
                {
                
                                                         
                        Grid2D.migrate(tot_demes);                              // migration        
                        Grid2D.reproduce(selectionMode);                        // reproduction and selection     
               
                }  
    
        }
    
        outdata = Grid2D.getMeanFit();                                          // write data to output file
    
        
    
        for (j = 0;j<tot_demes;j++) 
        { 
        
                outputfile << outdata[j] << " ";
                //finaloutputfile << outdata[j] << " ";
                
        }
        
        
        outputfile << "\n";
             
        outdata = Grid2D.getGenotypeFrequencies(0,loci,0);              // get ancestral homozygotes


        sprintf(filename4,"%s%s%d",filename2,"_gen_",(i*snapshot));
        outputfile3.open(filename4);
        for (j = 0;j<(tot_demes);j++)                                     
        { 
            for ( k = 0;k< loci;k++)
            {
                outputfile3 << outdata[j*loci+k] << " ";

            }
            outputfile3 << "\n";
        }
        outputfile3.close();

        outdata = Grid2D.getGenotypeFrequencies(0,loci,1);              // get  heterozygotes

        sprintf(filename4,"%s%s%d",filename3,"_gen_",(i*snapshot));
        outputfile3.open(filename4);
        for (j = 0;j<(tot_demes);j++)                                     
        { 
            for ( k = 0;k< loci;k++)
            {
                outputfile3 << outdata[j*loci+k] << " ";

            }
            outputfile3 << "\n";
        }
        outputfile3.close();

        outputfile.close();
        outputfile2.close();
        //outputfile3.close();
       // outputfile4.close();
     
        Grid2D.clear(initial_colonized,anc_pop_size,capacity,expansionMode);                          // clear world and reinitialize it 

    }
    
    
    
    
    return 0;
   

}



