*Methodology for grad student expenses*
 
The public-use microdata from the Interview version of the Consumer Expenditure Survey (http://www.bls.gov/cex/pumdhome.htm) was downloaded for years 2010-2012. Consumer units with the following characteristics were downloaded:
- Annual income between $5,000 and $20,000.
- Average age of the referencee person and spouse (if applicable): 22-40 years
- Highest education obtained: Bachelor's or Master's degree

With this subsample, the medians of the following variables were then calculated:
houscq, fdhomecq, apparcq, transcq
These represent housing, grocery, clothing, and transportation expenses, respectively.

For the code that computes these values, go here:
https://github.com/tdmcarthur/misc/blob/master/TAA/consumer-exp.r