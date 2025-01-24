# Impact of Climate Variability on Planktic Foraminifera: Unveiling Distinct Ecological Behaviors across the Pliocene-early Pleistocene

**Team members:**
Ekaterina Larina<sup>1</sup>, Adam Woodhouse<sup>2,3</sup>, Anshuman Swain<sup>4,5</sup>, Christopher M. Lowery<sup>2</sup>, Rowan C. Martindale<sup>1</sup>, and Corinne E. Myers<sup>6</sup>

<sup>1</sup> Jackson School of Geosciences, University of Texas at Austin, Austin, TX, 78712, USA.
<sup>2</sup> University of Texas Institute for Geophysics, Jackson School of Geosciences, University of   Texas at Austin, Austin, TX, 78758, U.S.A.
<sup>3</sup> School of Earth Sciences, University of Bristol, Bristol, BS8 1 RJ, United Kingdom.
<sup>4</sup> Department of Organismic and Evolutionary Biology, Harvard University, Cambridge, MA 02138.
<sup>5</sup>  Museum of Comparative Zoology, Harvard University, Cambridge, MA 02138.
<sup>6</sup> Department of Earth and Planetary Sciences, University of New Mexico, Albuquerque, NM 87131, U.S.A.

## Project description
Current global warming is already disproportionately affecting marine zooplankton communities due to the spatial variability of climate change. Despite ongoing efforts, comprehensive high-resolution studies of zooplankton across broad geographical scales and geological times remain scarce. The newly compiled Triton dataset addresses this gap by offering a detailed record of global fossil occurrences of planktonic foraminifera. This dataset enables the exploration of species diversity and functional dynamics with unmatched resolution, shedding light on changes in the vertical temperature structure of oceans and the associated responses of planktonic foraminifera communities at a regional scale. The importance of this work lies in its comprehensive analysis of planktonic foraminifera during the Pliocene-early Pleistocene period, a time marked by significant climatic fluctuations that are analogous to current and projected climate changes. By using a high-resolution, basin-wide approach, our findings highlight the role of enhanced water column stratification and nutrient delivery, associated with bipolar ice sheet development, in restructuring species diversity and ecogroup latitudinal equitability gradients towards modern patterns.  

## Brief file descriptions for this repository
### Data
**Triton_df_Oceans.RData** This file contains three dataframes with planktic foraminifera data for the Atlantic, Pacific, and Indian Oceans, derived from the Triton database used in this study.
### Base scripts in R
**Ecogroups_Relative_Abundance.R** This script calculates ecogroup relative abundance and produces the raw plots for Figure 3 of the manuscript, as well as Figures 2 and 8 of the supplementary material. 

**Fitted_Linear_Models.R** This script calculates fitted linear models of the relative abundances of five ecogroups by hemisphere across the Atlantic, Pacific, and Indian Oceans.

**Network_Properties.R** This script calculates network properties at the node level using the 'bipartite' package. This script also produces the raw plots for Figures 2B, 2D, and 2F of the manuscript.

**Null_Models.R** This script uses 'econullnetr' package to run null models and produces the raw plots for Figures 10, 11, and 12 of the supplementary material. 

**SQS_analysis.R** This script calculates the corrected sample-in-bin metric, utilizing the shareholder quorum subsampling (SQS) method. This script also produces the raw plots for Figure 4 of the manuscript.

**Sample Completeness iNEXT.R** This script assesses dataset completeness. It examines the sampling coverage and frequency of each unique ecogroup within a paleolatitudinal band during each time interval, utilizing the iNEXT package.

**Subsampling.R** This script performs resampling of ecogroup distribution data without replacement and produces the raw plots for Figure 9 of the supplementary material.
