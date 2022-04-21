# SerieA-salary-and-players-classification
The starting point of the analysis is the observation of the increasing salaries of football
players in the last years. The main goal of this project is to provide a statistical analysis
on football players’ salaries and performance, an attempt to evaluate the ability of teams’
management to correctly evaluate the performance of players and the correct level of
the relative salaries and an approach to decrease the number of overpaid players, hiring
athletes with the same characteristics but with low salary. Random forest and forward
stepwise regression are used to select most relevant variables. Linear regression is applied
on the selected variables to understand the impact of variables on gross salary. Residuals
are considered as a measure to comprehend if a player is underpaid or overpaid, and a
logistic regression is applied to compute the probability of being overpaid if an athletes
had signed to a particular team. Clustering is applied to group players with the same
characteristics and to provide meaningful substitute to overpaid players.

### Conclusion
The project results allow to conclude that the most relevant variables that determine the salary of a
football player are the team for which he had signed, age, the ability to shoot on target, the number
of completed passes during the season and the relative precision.
Grouping the residuals (difference between real and predicted salaries with regression) of players for
each team, it is possible to understand if the teams’ managers successfully managed the clubs and
correctly evaluating the players’ wages based on their expected performance. Moreover, a logistic
regression is applied to data to compute the probability of finding a player in the 20 % of overpaid
athletes for the season 2020/21, given the team for which that player had signed. U.S. Sassuolo and
S.S. Lazio were the best managed clubs of the previous seasuon, while Cagliari Calcio was the worst
one.
Hierarchical clustering is applied to create groups of similar players. This algorithm could provide a
solution for the overpaid players and for poorly managed clubs. In fact, given the information extracted
by clustering, a football club can easily find players that are very similar to overpaid players of the
team, but with lower salaries.
The 8 clusters found are: centre backs/full backs, strikers, wing backs, low used offensive players, very
low used players, attacking midfielder, defensive/box-to-box midfielder and centre-forward.
The accuracy of regression could be improved by considering also the number of individual trophies and
team trophies won by a player. A winning/experienced player is, obviously, more paid. Furthermore,
football players have high influence on the audience, and for this reason variables such as followers on
social media must be introduced in the analysis.
To discover some needed players, clustering algorithms could be used, as in the previous analysis. The
analysis done in this paper could be improved considering data of all European players and increasing
the number of clusters, improving the quality of the research of players.
