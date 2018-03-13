# Heart Failure vs All Case Reports - Geographic

## Background
This project compares heart failure case reports (approximately 18000) to all case reports (approximately 1.4 million) to determine which states/countries are significant in publishing heart failure case reports as well as which are significant in not publishing heart failure case reports. These findings may or may not correlate to actual heart failure cases.

## Contents
The R script includes a few stepes: 
1. Reading in the data and cleaning it
2. Extracting geographic location
3. Comparing proportions using statistical testing
4. Graphing the significant regions

The image enclosed is on a scale from blue to red and only includes those countries that are significant in the above comparison; the more red the country is, the more significant the country is in producing heart failure case reports. The more blue it is, the more significant the country is to not produce heart failure case reports, as compared to its expected case report publishing from all case reports (1.4 million case reports).

For instance, if India shares 10% of the total case reports and only 5% of the heart failure case reports, this map will map the country if the proportion is statistically significantly different from what it is expected. The scaling is so that we assume that each country is producing 1 case report. Any number greater than 1 means that the country is producing more heart failure case reports than expected. Any number less than 1 means that the country is producing much fewer heart failure case reports than expected.

## Findings
It's extremely interesting to note that those countries that significantly produce heart failure case reports are clustered together (South America and Europe). It's also intriguing to see that those countries that significantly don't produce heart failure case reports are also somewhat clustered (Asia). India and China are very populous countries, so it begs the question of whether doctors there see too many heart failure patients to write up case reports on them? More analyses will be needed.

## Future Steps
- Will compare all case reports to cancer to see if more populous countries (India, China) will publish significantly.
- Will compare localized diseases (malaria) to see if countries affected with it are more likely to publish case reports. 
- Will convert this into an R Shiny app in which one can choose a disease and see the geographic distribution. Will not have searching features since querying 1.4 million case reports will be extremely slow.
- These insights will help us better understand why one chooses to publish a case report as well as which regions are popular for certain diseases to publish case reports.
