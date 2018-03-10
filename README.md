# Heart Failure vs All Case Reports - Geographic

## Background
This project compares heart failure case reports (approximately 18000) to all case reports (approximately 1.4 million) to determine which states/countries are significant in publishing heart failure case reports as well as which are significant in not publishing heart failure case reports. These findings may or may not correlate to actual heart failure cases.

## Contents
The R script includes a few stepes: 
1. Reading in the data and cleaning it
2. Extracting geographic location
3. Comparing proportions using statistical testing
4. Graphing the significant regions

The image enclosed is on a scale from blue to red and only includes those countries that are significant in the above comparison; the more blue the country is, the more significant the country is in producing heart failure case reports. The more red it is, the more significant the country is to not produce heart failure case reports, as compared to its expected case report publishing from all case reports (1.4 million case reports).

## Findings
It's extremely interesting to note that those countries that significantly produce heart failure case reports are clustered together (South America and Europe). It's also intriguing to see that those countries that significantly don't produce heart failure case reports are also somewhat clustered (Asia). India and China are very populous countries, so it begs the question of whether doctors there see too many heart failure patients to write up case reports on them? More analyses will be needed.

## Future Steps
- Will be adding in the US states to see if there are drastic differences or an overall same scale.
- Will compare all case reports to cancer to see if more populous countries (India, China) will publish significantly.
- Will compare localized diseases (malaria) to see if countries affected with it are more likely to publish case reports. 
- Will convert this into an R Shiny app in which one can choose a disease and see the geographic distribution. Will not have searching features since querying 1.4 million case reports will be extremely slow.
- These insights will help us better understand why one chooses to publish a case report as well as which regions are popular for certain diseases to publish case reports.
