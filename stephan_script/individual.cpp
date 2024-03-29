#include <iostream> // For input/output
#include <fstream> // For file input/output
#include <string>  // For strcpy
#include <time.h>  // For time
#include <stdlib.h>  // For toupper and tolower
#include <math.h>
#include <vector>
#include <list>
#include "range_expansion.h"
//#include "rng.h"
#include "rng2.h"

using namespace std;

int Individual::loci = 100;
double Individual::rrate = 0.01; 
vector<int> Individual::used_loci;
vector<float> Individual::s_coeff;
//unsigned long long Individual::counter = 0;

//inline double rand_unif(double x0, double x1)
//{
//return x0 + (x1 - x0) * rand() / ((double) RAND_MAX);
//}

inline int max(int a, int b) { return (a < b) ? b : a; }
inline int min(int a, int b) { return (a < b) ? a : b; }


Individual::Individual()
{
    
    haplotypes.resize(2);
    //mutations.resize(2);
    
    used_loci.resize(loci);
    
    for(int i = 0; i < loci; i++)
    {
        used_loci[i] = i;
        
    
    }
    
    wf_ID = 1;
    
    ancestors = 0;
    


    /*mutations_b.resize(2);
    mb_front.resize(2);
    mutations_d.resize(2);
    md_front.resize(2);*/
    
    
    haplotypes[0].resize(loci);
    haplotypes[1].resize(loci);
    
    //mutations[0].resize(loci);
    //mutations[1].resize(loci);
    
    /*mutations_b[0].resize(loci);
    mb_front[0].resize(loci);
    mutations_d[0].resize(loci);
    md_front[0].resize(loci);
 
    mutations_b[1].resize(loci);
    mb_front[1].resize(loci);
    mutations_d[1].resize(loci);
    md_front[1].resize(loci); */
    
    fill_n(haplotypes[0].begin(),loci,0);
    fill_n(haplotypes[1].begin(),loci,0);

    
   /* fill_n(mutations_d[0].begin(),loci,0);
    fill_n(md_front[0].begin(),loci,0);
    fill_n(mutations_b[0].begin(),loci,0);
    fill_n(mb_front[0].begin(),loci,0);
    
    fill_n(mutations_d[1].begin(),loci,0);
    fill_n(md_front[1].begin(),loci,0);
    fill_n(mutations_b[1].begin(),loci,0);
    fill_n(mb_front[1].begin(),loci,0);*/
    
    
    
}


Individual::~Individual()
{
    
}


heritableUnit Individual::getNewGamete(double mu,double s,bool front)           // this function adds mutations to the genome. mutations and backmutations occur at the same rate. (this will ahve an effect on the DFE, need to investigate this)
{
   
    Loci hap_new;

    
    heritableUnit gam_new;

    
   int i;
    
   int nmutations;
   int site = 0;
   
   
    
   site = randint(0,1);
   hap_new = haplotypes[site];
   
   for (i = 0; i < loci;i++)   // recombination
   {
       if(randreal(0,1) < rrate)
       {
           site = (site+1)%2;
       }
       
       hap_new[i]=haplotypes[site][i]; 
       

   } 
   
  

    nmutations = 0;
    if (mu > 0) {nmutations = randpois(mu); }
    
    
    for(i =0; i < nmutations; i++)
    {   
        
        
        site = randint(0,loci-1); 
        hap_new[site] = !hap_new[site];
       
    } 
    
    
    
    gam_new.haplotype = hap_new;
    
    
    return(gam_new);
    
    
}

heritableUnit Individual::getNewGameteMM2(double mu1,double mu2,double s)
{
   
    Loci hap_new;
    vector<int>::iterator it;
    heritableUnit gam_new;
    
    int i;
   
    int site = 0;
    site = randint(0,1);
    
    hap_new = haplotypes[0];
      
    for (i = 0; i < loci;i++)   // recombination
    {
        
        if(randint(0,1)<rrate)   
        {
            site = (site+1)%2;
        }
        
        
        
        hap_new[i]=haplotypes[site][i]; 
        
//         if (hap_new[i]==0)
//        {
//                if(randreal(0,1)<mu1) 
//                { 
//                        hap_new[i]=1;
//                        
//                }
//
//        }
        
        
    }
    
    for (it = used_loci.begin(); it < used_loci.end();)
    {

            if(randreal(0,1)<mu1) 
            { 
                    hap_new[*it]=1;

                    it = used_loci.erase(it); 

            }
            else    {it++; }

    }
//    
//        if (hap_new[i]==0)
//        {
//                if(randreal(0,1)<mu1) 
//                { 
//                        hap_new[i]=1;
//                        
//                }
//
//        }
//        
//        if (hap_new[i]==1)                                                    //back mutation
//        {
//            if(randreal(0,1)<(mu2))
//                { 
//                        hap_new[i]=0;
//                }
//        }



    
    
    
    gam_new.haplotype = hap_new;

    
    return(gam_new);
    
    
}



void Individual::setGenotype(heritableUnit g1,heritableUnit g2)
{
    
    haplotypes[0] = g1.haplotype;
    haplotypes[1] = g2.haplotype;
    
    
//    mutations[0] = g1.muts;
//    mutations[1] = g2.muts;
    
    /*mutations_d[0] = g1.m_d;
    md_front[0] = g1.md_front;
    mutations_b[0] = g1.m_b;
    mb_front[0] = g1.mb_front;
    
    mutations_d[1] = g2.m_d;
    md_front[1] = g2.md_front;
    mutations_b[1] = g2.m_b;
    mb_front[1] = g2.mb_front;*/
    
    
     
}
 
 
 
double Individual::getFitness(double s)                                         // this is no longer used
{
    
    double w = 1;
    int i;
    
    // calculate fitness from genotype
    
    for (i=0;i<(loci-1);i++) 
    {
       //w = w*pow((1-s),haplotypes[0][i])*pow((1-s),haplotypes[1][i]);      //multiplicative   
       //w = w-haplotype_1[i]*s - haplotype_2[i]*s;                       //additive        
       //if(w < 0) {w = 0;}
       //w = 0.00001;
       //w = w * haplotypes[0][i] * haplotypes[1][i];    //additive
       //w = w * pow(max(haplotypes[0][i],haplotypes[1][i]),2);    //recessive-like 
       //w = w * (1 - (abs(haplotypes[0][i]-haplotypes[1][i])));    //underdominant-like 

    }
    
    return(w);
    
 
}

double Individual::getRelativeFitness(double s)
{
    
    double w = 1;
    int i;
    
    // calculate fitness from genotype
    
    for (i=0;i<loci-1;i++)                                                    // add up the effects of deleterious mutations        
    {

       if (haplotypes[0][i] && haplotypes[1][i])                              // completely recessive mutations
       { 
           w *= (1 - s_coeff[i]);  
       }
////                                                                                // mutliplicative effects  
//        if(haplotypes[0][i]) w *= (1-s);
//        if(haplotypes[1][i]) w *= (1-s);
        
        
//         w *= (1-( (haplotypes[1][i]+haplotypes[0][i])* s_coeff[i]/2 ) );                       // additive effects
    }
                                                                                // remove this if you want neutral mutations
//    for (i=loci/3;i<(2*loci/3);i++)                                                 // add up the effects of beneficial mutations   
//    {
//
////       if (haplotypes[0][i] && haplotypes[1][i])                              // completely recessive mutations
////       { 
////           w *= (1 + s);  
////       }
//                                                                                // mutliplicative effects        
//        if(haplotypes[0][i]) w *= (1+s);
//        if(haplotypes[1][i]) w *= (1+s);
//      
//    }
    

    
    return(w);
    
 
}



double Individual::getMaxFitness(double s)
{
    
    double w = 1;
    int i;
    return(1);
    // calculate fitness from genotype
    
    for (i=0;i<(2*loci/3);i++) 
    {
       if (haplotypes[0][i] ||  haplotypes[1][i])                                
       { 
           w *= (1 - s);  
       }
    }
    
    return(w*w);
    
 
}

void Individual::print()
{
    cout << "\n h1:";
    for(int i=0;i<haplotypes[0].size();i++) { cout << haplotypes[0][i] << " ";}
    cout << "\n h2:";
    for(int i=0;i<haplotypes[1].size();i++) { cout << haplotypes[1][i] << " ";}
}

void Individual::setParams(int number_loci)
{
    loci = number_loci;
    
    haplotypes[0].resize(loci);
    haplotypes[1].resize(loci);
    
 
    fill_n(haplotypes[0].begin(),loci,0);      // initialize haplotype 
    fill_n(haplotypes[1].begin(),loci,0);  
}


 

/*Count Individual::getMutationCount()
{
    
    int i,j;
    Count c;
    c.resize(4);
    fill_n(c.begin(),4,0);
    
    // calculate fitness from genotype
    for (j = 0;j<2;j++)
    {
    
        for (i=0;i<loci;i++) 
    
        {
               
            c[0] += mutations_d[j][i];
            c[1] += md_front[j][i];
            c[2] += mutations_b[j][i];
            c[3] += mb_front[j][i];
            
        }
    }
    
    return(c);
    
 
} 
*/

/*void Individual::ResetMutationOrigin()
{
    
    
    fill_n(md_front[0].begin(),loci,0);
   
    fill_n(mb_front[0].begin(),loci,0);
    
    fill_n(md_front[1].begin(),loci,0);
   
    fill_n(mb_front[1].begin(),loci,0);
    

}*/

void Individual::setAncestors(int a)
{
    ancestors = a;
}
        

void Individual::setWFID(double id)
{
    wf_ID = id;
}


int Individual::getAncestors()
{
    return(ancestors);
}

double Individual::getWFID()
{
    return(wf_ID);
}


vector<double> Individual::getSumAlleles(int loci_begin,int loci_end)
{
    vector<double> p;
    int start = 0;
    
    
    p.resize(loci_end);
        
    fill_n(p.begin(),loci_end,0);
    
    for (int i = loci_begin;i<loci_end;i++)
    {
        p[i] += haplotypes[0][i]+haplotypes[1][i];
        
    }
       
      
    return(p);
}


vector<double> Individual::getSumGenotypes(int loci_begin,int loci_end,int genotype)
{
    vector<double> p;
    int start = 0;
    
    
    p.resize(loci_end);
        
    fill_n(p.begin(),loci_end,0);
  
    for (int i = loci_begin;i<loci_end;i++)
    {
        if(haplotypes[0][i]+haplotypes[1][i]==genotype)
            p[i] += 1;
    }

    return(p);
}

double Individual::getSquareSumAlleles()                                        // not used anymore
{

}

void Individual::normalizeFitness(double mean_fit)                              // not used anymore
{
    
    //double co = pow(mean_fit,1/(2*(double)loci));
    
    //cout << mean_fit << "   " << co <<  " \n";
    int i;
    
    // calculate fitness from genotype
    
    //for (i=0;i<loci;i++) 
    //{
    //    haplotypes[0][i]/=co;
    //    haplotypes[1][i]/=co;  
    //}
    
    
    
 
}

unsigned long Individual::getNumberMutations()
{
    int i;
    unsigned long n_mut = 0;
    for (i = 0;i<loci;i++)
    {
//        n_mut += mutations[0][i].size();
//        n_mut += mutations[1][i].size();
    }
    
    return(n_mut);
        
}

void Individual::set_selection_dist(double s)
{       
    Individual::s_coeff.resize(loci);
    int i;
    for (i = 1;i<loci;i++)
    {
        s_coeff[i-1] = s*(-log(1-((float)i-1)/loci));
        
    }
    
    s_coeff[loci-1] = s*(-log(1-((float)loci-1)/loci));
    
    

}