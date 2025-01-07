# online_e-BH

This repository contains the R code for reproducing the results in the paper "An online generalization of the
(e-)Benjamini-Hochberg procedure" (https://arxiv.org/abs/2407.20683). In the aforementioned paper, we introduced the concept of online ARC (online with acceptance-to-rejection changes) procedures and propose the online e-BH and online BH procedures as concrete algorithms. We prove that the online e-BH procedure provides SupFDR control and therefore controls the FDR at arbitrary stopping times. The online BH procedure controls the OnlineFDR under PRDS at level \alpha and the SupFDR under PRDN/WNDN at level \alpha(1+\log(1/\alpha)).

Files:

boosted_e_vals.R:                     Contains the functions for boosting e-values. 

procedures.R:                         Contains the functions for the online e-BH procedure and other procedures used in simulations.

data_generator_e-values.R:            Generates the data for the e-value based simulations and saves it in the files "e-val_weak_signal.rda" and "e-val_strong_signal.rda" in the results folder.

data_generator_p-values.R:            Generates the data for the p-value based simulations and saves it in the files "p-val_large_q.rda" and "p-val_small_q.rda" in the results folder.

plot_generator.R:                     Uses the data in "results/weak_signal.rda" and "results/strong_signal.rda" to create plots 1-3 of the paper.
