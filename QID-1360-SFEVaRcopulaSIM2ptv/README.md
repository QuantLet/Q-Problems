<div style="margin: 0; padding: 0; text-align: center; border: none;">
<a href="https://quantlet.com" target="_blank" style="text-decoration: none; border: none;">
<img src="https://github.com/StefanGam/test-repo/blob/main/quantlet_design.png?raw=true" alt="Header Image" width="100%" style="margin: 0; padding: 0; display: block; border: none;" />
</a>
</div>

```
Name of QuantLet: SFEVaRcopulaSIM2ptv

Published in: Statistics of Financial Markets

Description: 'According to the VaR methodology the profit and loss (P&L) is plotted against the time. The dots represent the empirical P&L stated by the data. The yellow plot indicates the lower alpha quantile. The residuals of an estimated GARCH(1,1) model are used to estimate the marginal distributions of the different series. The GARCH model is based on a window of 250 days. Based on the marginal distribution a copula is calculated. Corresponding to the previous results a one step ahead forecast is computed and the procedure starts again. Finally 12 plots are produced. Each plot contents a graph with regard to one copula (Gumbel, Clayton and Normal) combined with one quantile (5%, 1%, 0.5% and 0.1%).'

Keywords: copula, plot, graphical representation, distribution, estimation, gumbel, clayton, normal, garch, autoregressive, financial, Loss-Profit function

See also: SFEclaytonMC, SFEstaticCop, SFEplotCop, SFEtCop, SFEArchCopDensity, BCS_ClaytonMC, SFEfrechet, SFEgaussCop, SFEresDens, SFEtMC, SFScontourgumbel, SFEgaussCop, SFEVaRHAC

Author: Ostap Okhrin

Submitted: Mon, October 17 2011 by Awdesch Melzer

Datafiles: Representative_Data.txt, Stocks_Series.txt

Example: 

```
