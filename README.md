# RiskBudgeting

I have developed several tools to aid me in my career. Here, I will present one such tool, which was useful not only to me but to my whole team.

In this project, I have constructed the risk budgeting engine. The engine allows us to view and simulate risk exposures of chosen fixed income 
investment strategies. This application helps users simulate new trades and incorporate them into the existing portfolio. Users can analyze 
sensitivity (i.e. Duration) of the portfolio to changes in each point (i.e. Key Rate) on the yield curve. Moreover, using principal component 
analysis, key rates are reduced to three factors. These factors explain 99% of yield curve movement and closely resemble the level, slope, and 
curvature of a yield curve. Users are then able to view the sensitivity of the portfolio and simulated strategies to given principal components.

Using three principal components over the traditional key rates yields several benefits. First, multiple key rates are reduced to three orthogonal 
factors, which can be treated independently. Second, measures like key rate duration of fixed-income security can be easily transformed into principal 
component duration, by multiplying a vector of key rate durations by factor loading. Three variables are sufficient to explain most of the variation 
in key rate changes. Factors are traditionally considered level, curviture and slope of a yield curve, explain 79%, 14%, and 5% of the variation.

R Shiny package allows my team to use the application without any prior programming knowledge. While the traditional Bloomberg interface has the 
option of viewing key rate duration, my application has several advantages. First, the application is significantly faster at simulating new 
strategies compared to Bloomberg's PORT. Second, the addition of principal component duration (i.e. level, slope, and curvature sensitivity) 
gives more useful insight into the portfolio. Finally, the application is flexible and can be tailored to the team's needs.

NOTE: Running the application requires a working Bloomberg terminal
