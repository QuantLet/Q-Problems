<div style="margin: 0; padding: 0; text-align: center; border: none;">
<a href="https://quantlet.com" target="_blank" style="text-decoration: none; border: none;">
<img src="https://github.com/StefanGam/test-repo/blob/main/quantlet_design.png?raw=true" alt="Header Image" width="100%" style="margin: 0; padding: 0; display: block; border: none;" />
</a>
</div>

```
Name of QuantLet: SFStailGPareto

Published in: Statistics of Financial Markets : Exercises and Solutions

Description: Estimates the parameters of a Generalized Pareto Distribution for the negative log-returns of a portfolio (Bayer, BMW, Siemens) for the time period from 1992-01-01 to 2006-09-21 and produces a QQ-plot and PP-plot. Corresponds to exercise 16.5 in SFS.

Keywords: GEV, distribution, pareto, returns

See also: SFEgpdist, SFEtailGEV_pp, SFSevt2, SFSmeanExcessFun, SFSportfolio, SFStailGEV, SFStailport, SFSvar_block_max_backtesting, SFSvar_block_max_params, SFSvar_pot_backtesting, SFSvar_pot_params, SFSvarblockmaxbacktesting, SFSvarpotbacktesting

Author: Lasse Groth

Submitted: Sat, October 01 2011 by Awdesch Melzer

Datafiles: Bay9906_close_2kPoints.txt, Bmw9906_close_2kPoints.txt, Sie9906_close_2kPoints.txt

Input: 
- Please change working directory.

Output: 
- QQ-plot and PP-plot with Generalized Pareto Distribution.

Example: PP plot of 100 tail values of daily log-returns of portfolio (Bayer, BMW, Siemens) from 1992-01-01 to 2006-09-01 against Generalized Pareto Distribution with parameter = 0.0768 globally estimated with POT method. QQ plot of 100 tail values of daily log-returns of portfolio (Bayer, BMW, Siemens) from 1992-01-01 to 2006-09-01 against Generalized Pareto Distribution with a global parameter = 0.0768 estimated with POT method.

```
