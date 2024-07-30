# online_e-BH

This repository contains the R code for reproducing the results in the paper "An online generalization of the e-BH procedure". In the aforementioned paper, we introduced the concept of online ARC (online with acceptance-to-rejection changes) procedures and propose the online e-BH procedure as concrete algorithm. We prove that the online e-BH procedure provides SupFDR control and therefore controls the FDR at arbitrary stopping times.

Files:

boosted_e_vals.R:            Contains the functions for boosting e-values. 

procedures.R:                Contains the functions for the online e-BH procedure and other procedures used in simulations.

data_generator.R:            Generates the simulated data and saves it in the files "weak_signal.rda" and "strong_signal.rda" in the results folder.

plot_generator.R:            Uses the data in "results/weak_signal.rda" and "results/strong_signal.rda" to create plots 1-3 of the paper.
