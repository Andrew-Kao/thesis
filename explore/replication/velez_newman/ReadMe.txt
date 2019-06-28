Tuning In, Not Turning: 
Evaluating the Impact of Ethnic Television on Political Participation 

*All data analyses in this article (and its Supplemental Appendix) were carried out using R version 3.5.0 on MacOS.

Notes:  Code to reproduce analyses, results, and figures in the main text is located in the “main_text_replication_code.R” file.  

Description of files in the replication package:

codebook.pdf = PDF file containing variable names and descriptions in each data set. Note: all ordinal and continuous variables are rescaled to range from a minimum of 0 to a maximum of 1. 

main_text_replication_code.R = reproduces figures and tables presented in the main text

table1.R = estimates models and reproduces Table 1 in the main text
table2.R = estimates models and reproduces Table 2 in the main text
figure3.R = estimates models and reproduces Figure 3 in the main text
figure5.R = reproduces Figure 5 in the main text (Note: figure5.R uses saved output, rather than estimating the models directly. This is done to protect the privacy of voters in the voter file. See appendixD_models.R for the script that was used to estimate the GRD models.)
figure5_models.R = script to estimate GRD models for Figure 5 (Note: the script will not execute without coordinate data. Coordinates were redacted due to privacy concerns.) 
figure6.R = estimates models and reproduces Figure 6 in the main text
figure7.R = estimates models and reproduces Figure 7 in the main text

wbwp_fl_distance-functions.R = GRD functions  
national_lns_data.csv = 2006 Latino National Survey data set
national_nsl_data.csv = 2012 National Survey of Latinos data set
national_ldees_data.csv = 2012 LD Election Eve survey data set
wuvc_nc_voting_data.csv = L2 voter file data for Latinos residing in Wayne County
wbwp_fl_boundary_points.csv = Points along WBWP reception boundary for Florida GRD
wbwp_fl_voting_data.csv = Florida Division of Elections voter file data for Latinos residing in West Palm Beach County 
wuvc_nc_balance_data.csv = L2 voter file data with block group data for balance tests
wuvc_nc_aux_data.csv = Nation Builder voter file data for registered voters in Wayne County 

appendixA.R = estimates chi-sq values across different buffer sizes from boundary to examine whether balance improves with proximity to the reception boundary and generates figures for Districts 1 & 3
appendixB.R = estimates treatment effect across different buffer sizes
appendixC.R = examines differential registration bias and produces figures
appendixD.R = examines balance in Florida (Note: appendixD.R uses saved output, rather than estimating the models directly. This is done to protect the privacy of voters in the voter file. See appendixD_models.R for the script that was used to estimate the GRD models.)
appendixD_models.R = script to estimate GRD models for pre-treatment variables (Note: the script will not execute without coordinate data. Coordinates were redacted due to privacy concerns.)
appendixE-G.R = estimates conditional effects based on media language usage and generates figures
appendixJ.R = explores conditional effects across all three surveys
appendixI.R = examines Latino self-identification at different distances from the reception boundary

wbwp_fl_estimates_2000.csv = saved FL GRD results in 2000
wbwp_fl_estimates_2004.csv = saved FL GRD results in 2004
wbwp_fl_estimates_2008.csv = saved FL GRD results in 2008
wbwp_fl_estimates_2012.csv = saved FL GRD results in 2012
placebo_fl.csv = saved FL placebo results

nc_grd.pdf = plot of main findings in Wayne County, North Carolina
fl_grd.pdf = plot of main findings in Palm Beach County, Florida
national_replication_test.pdf = plot of LNS/LDEES findings
national_intermediate_outcomes.pdf = plot of intermediate outcome findings


