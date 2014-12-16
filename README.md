Polyploid expansion
==================

A initial repo for the project of polyploid and standing genetic variation in an expanding wave built off initial idea of Peischl and Excoffier in [Expansion load: recessive mutations and the role of standing genetic variation] (http://biorxiv.org/content/early/2014/11/18/011593).

## Main question:
If mutations are fully (or perhaps even partially) recessive for which there is evidence (see Agrawal 2011) - then are polyploids at an advantage for escaping both hard and soft selection?

**Hypothesis/Prediction**:

Under recessive model: aaaa genotype is bad for tetraploids, but more ways (genotypically) to escape selection with aaaA, aaAA, aAAA, and AAAA.

Whereas diploids have only two ways to escape selection with: aA and AA.

Will need to update with some additional scripts (only one now named neutral_exp - for expectations under no selection - just checking the distribution of genotypes under drift and occasional outcrossing). Will also need to incorporate demes as a grid.

### Some ideas:
Incorporate Iain Mathieson's spatial grid lattice simulation in R [forked here] (https://github.com/kate-crosby/s_lattice) so that we don't have to trudge through C++ code?


