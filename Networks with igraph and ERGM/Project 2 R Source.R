# Data and summary found at:
# http://moreno.ss.uci.edu/data.html#ckm

library(statnet)
require(stringr)
library(ggplot2)
library(RColorBrewer)

# Read and parse file of doctors' attributes. 
attr_info <- read.csv("attributes.dat.txt", skip = 4, nrows = 13, header = FALSE, stringsAsFactors = FALSE, sep = '')
attr_vec <- as.vector(as.matrix(attr_info, nrow = 1, ncol = 13))

# Ensure column names have similar format
attr_vec <- str_replace_all(attr_vec, " ", "_")

# Ensure vector attributes are each set to an appropriate class type. Set values
# to Factor that describe attributes that seem nominal in nature.
colClassTypes <- rep("character",13)
doc_attributes <- read.csv("attributes.dat.txt", skip = 18, header = FALSE, stringsAsFactors = TRUE, sep = '', 
              col.names = attr_vec, colClasses = colClassTypes)

# replace city codes with city names:
doc_attributes$city[doc_attributes$city == 1] = "Peoria"
doc_attributes$city[doc_attributes$city == 2] = "Bloomington"
doc_attributes$city[doc_attributes$city == 3] = "Quincy"
doc_attributes$city[doc_attributes$city == 4] = "Galesburg"

# read in and parse sociomatrix data.
all_socio <- read.csv("ckm.dat.txt", skip = 9, header = FALSE, stringsAsFactors = FALSE, sep = '')
advice <- all_socio[1:246,]
discussion <- all_socio[247:492,]
friend <- all_socio[493:738,]

#################################################
# Create network objects from adjacency sociomatrices. Networks are directional.
#################################################
# Advice: When you need information or advice about questions of therapy where do you usually turn?
# Discuss: And who are the three or four physicians with whom you most often find yourself 
#          discussing cases or therapy in the course of an ordinary week -- last week for instance?
# Friend: Would you tell me the first names of your three friends whom you see most often socially?
#################################################

advice_net <- network(advice, type = "adjacency")
discuss_net <- network(discussion, type = "adjacency")
friend_net <- network(friend, type = "adjacency")

# names of vertices are simple row numbers, corresponding to each (unnamed) doctor.
network.vertex.names(advice_net) <- as.character(1:246)
network.vertex.names(discuss_net) <- as.character(1:246)
network.vertex.names(friend_net) <- as.character(1:246)

#Put all of the doctors' attributes into the three networks.
for (i in 1:13) {
  set.vertex.attribute(advice_net, attr_vec[i], doc_attributes[,i])
  set.vertex.attribute(discuss_net, attr_vec[i], doc_attributes[,i])
  set.vertex.attribute(friend_net, attr_vec[i], doc_attributes[,i])
}

# How many isolates exist in each of the three networks, prior to removal?
length(isolates(advice_net))
length(isolates(discuss_net))
length(isolates(friend_net))

# Next step - remove the isolates from each network
delete.vertices(advice_net, isolates(advice_net))
delete.vertices(discuss_net, isolates(discuss_net))
delete.vertices(friend_net, isolates(friend_net))

# Communities in each of the three networks are grouped by city of residence.
# Even considering that the time period is before the Internet and before the
# start of the Interstate Highway System (legislation signed 1956), it's odd
# that there are NO friendship, advice, or discussion network links spanning the
# four cities.

  # Two city combinations - Galesburg-Peoria and Bloomington-Peoria - are < 50
# miles apart under today's transit infrastructure. You would think at least one
# mentor-student relationship would be facilitated by telephone contact and
# would survive what is otherwise an insular set of structures.
my_pal <- brewer.pal(4, "Dark2")
city_adv <- as.factor(advice_net %v% "city")
city_disc <- as.factor(discuss_net %v% "city")
city_friend <- as.factor(friend_net %v% "city")

jpeg(file = "Plot1.jpg", width = 1600, height = 708, quality = 100)
plot(advice_net, vertex.col = my_pal[city_adv], main = "Advice Network (1953-1955) by City of Residence")
dev.off()

jpeg(file = "Plot2.jpg", width = 1600, height = 708, quality = 100)
plot(discuss_net, vertex.col = my_pal[city_disc], main = "Discussion Network (1953-1955) by City of Residence")
dev.off()

jpeg(file = "Plot3.jpg", width = 1600, height = 708, quality = 100)
plot(friend_net, vertex.col = my_pal[city_friend], main = "Friend Network (1953-1955) by City of Residence")
legend(x=80, y=0, legend = c("Peoria", "Bloomington", "Quincy", "Galesburg"), 
       col = c(my_pal[3],my_pal[1],my_pal[4],my_pal[2]), 
       pch = 19, pt.cex = 2, bty = "n")
dev.off()

components(advice_net, connected = "weak")
components(friend_net, connected = "weak")
components(discuss_net, connected = "weak")

# Prestige for a directed network
caption <-  "http://moreno.ss.uci.edu/data.html#ckm"
ad_degree <- as.data.frame(table(degree(advice_net, cmode = "indegree")), stringsAsFactors = FALSE)
ad_degree$Var1 <- as.integer(ad_degree$Var1)

jpeg(file = "Plot4.jpg", width = 960, height = 425, quality = 100)
plot4 <- ggplot(ad_degree, aes(x = Var1, y = Freq)) + geom_point() + geom_line() + 
          annotate("text", label= "102 doctors were not asked for advice", x=3.1,y=102, size=3.5) +          
          annotate("text", label= "2 doctors' advice \nsought by 20 others", x=19.5,y=10, size=3.5) +
          labs(x = "Count of Doctors Advised", y = "Count of Doctors Advising", 
            title = "InDegree incidence of Professional Advice Given",
            subtitle = "Across All Cities in the Study",
            caption = caption)

print(plot4)
dev.off()

#doc_attributes[c(which(degree(advice_net, cmode = "indegree") > 9)),]

# Subgraphs of cities - advice_net
detach(package:statnet)
detach(package:sna)
library(igraph)
library(intergraph)

advice_net_i <- asIgraph(advice_net)
advice_net_i1 <- induced.subgraph(advice_net_i, vids = which(V(advice_net_i)$city == "Peoria"))
advice_net_i2 <- induced.subgraph(advice_net_i, vids = which(V(advice_net_i)$city == "Bloomington"))
advice_net_i3 <- induced.subgraph(advice_net_i, vids = which(V(advice_net_i)$city == "Quincy"))
advice_net_i4 <- induced.subgraph(advice_net_i, vids = which(V(advice_net_i)$city == "Galesburg"))

#Community measurements using cluster_edge_betweenness (directed)
ceb_advice_i1 <- cluster_edge_betweenness(advice_net_i1)
ceb_advice_i2 <- cluster_edge_betweenness(advice_net_i2)
ceb_advice_i3 <- cluster_edge_betweenness(advice_net_i3)
ceb_advice_i4 <- cluster_edge_betweenness(advice_net_i4)

#Community measurements using cluster_infomap (directed)
inf_advice_i1 <- cluster_infomap(advice_net_i1)
inf_advice_i2 <- cluster_infomap(advice_net_i2)
inf_advice_i3 <- cluster_infomap(advice_net_i3)
inf_advice_i4 <- cluster_infomap(advice_net_i4)

ceb = c(length(table(membership(ceb_advice_i1))),length(table(membership(ceb_advice_i2))),
        length(table(membership(ceb_advice_i3))),length(table(membership(ceb_advice_i4))))
                                    
inf = c(length(table(membership(inf_advice_i1))),length(table(membership(inf_advice_i2))),
        length(table(membership(inf_advice_i3))),length(table(membership(inf_advice_i4))))

communityAdvice = as.data.frame(rbind(ceb,inf))
colnames(communityAdvice) = c("Peoria", "Bloomington", "Quincy", "Galesburg")
rownames(communityAdvice) = c("Cluster_Edge_Betweeness","InfoMap")
print(communityAdvice)

plot(inf_advice_i1, advice_net_i1, vertex.label = degree(advice_net_i1, mode = "in"), 
     vertex.size = 6+(.4*degree(advice_net_i1, mode = "in")), edge.arrow.size = .12, 
     layout = layout_with_dh,
     main = "Cluster_infomap() Community Detection - Advice Network, Peoria\nInDegrees Labeled")

plot(inf_advice_i2, advice_net_i2, vertex.label = degree(advice_net_i2, mode = "in"), 
     vertex.size = 6+(.4*degree(advice_net_i2, mode = "in")), edge.arrow.size = .12, 
     main = "Cluster_infomap() Community Detection - Advice Network, Bloomington\nInDegrees Labeled")

plot(inf_advice_i3, advice_net_i3, vertex.label = degree(advice_net_i3, mode = "in"), 
     vertex.size = 6+(.4*degree(advice_net_i3, mode = "in")), edge.arrow.size = .12, 
     main = "Cluster_infomap() Community Detection - Advice Network, Quincy\nInDegrees Labeled")

# Characteristics of the dataset that we will run ERGM against for Peoria/Advice
table(V(advice_net_i1)$adoption_date)
table(V(advice_net_i1)$clubs)
table(V(advice_net_i1)$community)
table(V(advice_net_i1)$discuss)
table(V(advice_net_i1)$free_time)
table(V(advice_net_i1)$friends)
table(V(advice_net_i1)$jours)
table(V(advice_net_i1)$med_sch_yr)
table(V(advice_net_i1)$meetings)
table(V(advice_net_i1)$patients)
table(V(advice_net_i1)$proximity)
table(V(advice_net_i1)$specialty)

detach(package:igraph)
detach(package:intergraph)
library(ergm)
library(network)

# Make a network obect out of just the Peoria data (like the igraph/induced.subgraph advice_net_i1)
advicePeoria_net <- advice_net
delete.vertices(advicePeoria_net, which(advicePeoria_net %v% "city" != "Peoria"))
nullAdvice <- ergm(advicePeoria_net ~ edges, control = control.ergm(seed = 2112))
AICnull = nullAdvice$glm$aic
print(AICnull)

fullAdvice <- ergm(advicePeoria_net ~ edges + nodefactor('proximity') + nodefactor('med_sch_yr')
                + nodefactor('community') + nodefactor('adoption_date') + nodefactor('clubs')
                + nodefactor('discuss') + nodefactor('free_time') + nodefactor('friends')
                + nodefactor('jours') + nodefactor('meetings') + nodefactor('patients') 
                + nodefactor('specialty'), control = control.ergm(seed = 2112))
AICfull = fullAdvice$glm$aic
print(AICfull)

# Remove 'patients' from consideration, does not look to contribute.
Advice1 <- ergm(advicePeoria_net ~ edges + nodefactor('proximity') + nodefactor('med_sch_yr')
                   + nodefactor('community') + nodefactor('adoption_date') + nodefactor('clubs')
                   + nodefactor('discuss') + nodefactor('free_time') + nodefactor('friends')
                   + nodefactor('jours') + nodefactor('meetings') 
                   + nodefactor('specialty'), control = control.ergm(seed = 2112))
AICAdvice1 = Advice1$glm$aic

modelResults <- c(AICNull = AICnull, AICAdvice1 = AICAdvice1, AICFull = AICfull)
print(modelResults)
# AIC improves without 'patients'.

# Remove 'jours' from consideration, does not look to contribute.
Advice2 <- ergm(advicePeoria_net ~ edges + nodefactor('proximity') + nodefactor('med_sch_yr')
                + nodefactor('community') + nodefactor('adoption_date') + nodefactor('clubs')
                + nodefactor('discuss') + nodefactor('free_time') + nodefactor('friends')
                + nodefactor('meetings') 
                + nodefactor('specialty'), control = control.ergm(seed = 2112))
AICAdvice2 = Advice2$glm$aic

modelResults <- c(AICNull = AICnull, AICFull = AICfull, AICAdvice1 = AICAdvice1, 
                  AICAdvice2 = AICAdvice2)
print(modelResults)
# AIC improves without 'jours'.

# Remove 'meetings' from consideration, does not look to contribute.
Advice3 <- ergm(advicePeoria_net ~ edges + nodefactor('proximity') + nodefactor('med_sch_yr')
                + nodefactor('community') + nodefactor('adoption_date') + nodefactor('clubs')
                + nodefactor('discuss') + nodefactor('free_time') + nodefactor('friends')
                + nodefactor('specialty'), control = control.ergm(seed = 2112))
AICAdvice3 = Advice3$glm$aic

modelResults <- c(AICNull = AICnull, AICFull = AICfull, AICAdvice1 = AICAdvice1, 
                  AICAdvice2 = AICAdvice2, AICAdvice3 = AICAdvice3)
print(modelResults)
# AIC improves without 'meetings'.

# Remove 'proxixmity' from consideration, does not look to contribute.
Advice4 <- ergm(advicePeoria_net ~ edges + nodefactor('med_sch_yr')
                + nodefactor('community') + nodefactor('adoption_date') + nodefactor('clubs')
                + nodefactor('discuss') + nodefactor('free_time') + nodefactor('friends')
                + nodefactor('specialty'), control = control.ergm(seed = 2112))
AICAdvice4 = Advice4$glm$aic

modelResults <- c(AICNull = AICnull, AICFull = AICfull, AICAdvice1 = AICAdvice1, 
                  AICAdvice2 = AICAdvice2, AICAdvice3 = AICAdvice3, AICAdvice4 = AICAdvice4)
print(modelResults)
# AIC improves without 'proximity'.

# Remove 'discuss' from consideration, does not look to contribute.
Advice5 <- ergm(advicePeoria_net ~ edges + nodefactor('med_sch_yr')
                + nodefactor('community') + nodefactor('adoption_date') + nodefactor('clubs')
                + nodefactor('free_time') + nodefactor('friends')
                + nodefactor('specialty'), control = control.ergm(seed = 2112))
AICAdvice5 = Advice5$glm$aic

modelResults <- c(AICNull = AICnull, AICFull = AICfull, AICAdvice1 = AICAdvice1, 
                  AICAdvice2 = AICAdvice2, AICAdvice3 = AICAdvice3, AICAdvice4 = AICAdvice4,
                  AICAdvice5 = AICAdvice5)
print(modelResults)
# AIC improves without 'discuss'.

# Treating the higher-contributing 'clubs' as nodematch rather than nodefactor yields a better AIC
Advice6 <- ergm(advicePeoria_net ~ edges + nodefactor('med_sch_yr')
                + nodefactor('community') + nodefactor('adoption_date') + nodematch('clubs', diff = T)
                + nodefactor('free_time') + nodefactor('friends')
                + nodefactor('specialty'), control = control.ergm(seed = 2112))
AICAdvice6 = Advice6$glm$aic

modelResults <- c(AICNull = AICnull, AICFull = AICfull, AICAdvice1 = AICAdvice1, 
                  AICAdvice2 = AICAdvice2, AICAdvice3 = AICAdvice3, AICAdvice4 = AICAdvice4,
                  AICAdvice5 = AICAdvice5, AICAdvice6 = AICAdvice6)
print(modelResults)

# Treating the higher-contributing 'specialty' as nodematch rather than nodefactor yields a better AIC
Advice7 <- ergm(advicePeoria_net ~ edges + nodefactor('med_sch_yr')
                + nodefactor('community') + nodefactor('adoption_date') + nodematch('clubs', diff = T)
                + nodefactor('free_time') + nodefactor('friends')
                + nodematch('specialty', diff = T), control = control.ergm(seed = 2112))
AICAdvice7 = Advice7$glm$aic

modelResults <- c(AICNull = AICnull, AICFull = AICfull, AICAdvice1 = AICAdvice1, 
                  AICAdvice2 = AICAdvice2, AICAdvice3 = AICAdvice3, AICAdvice4 = AICAdvice4,
                  AICAdvice5 = AICAdvice5, AICAdvice6 = AICAdvice6, AICAdvice7 = AICAdvice7)
print(modelResults)
summary(Advice7)

# Treating the higher-contributing 'free_time' as nodematch rather than nodefactor 
# DOES NOT yield a better AIC. We'll stop there with Advice7 being our final model.
Advice8 <- ergm(advicePeoria_net ~ edges + nodefactor('med_sch_yr')
                + nodefactor('community') + nodefactor('adoption_date') + nodematch('clubs', diff = T)
                + nodematch('free_time', diff = T) + nodefactor('friends')
                + nodematch('specialty', diff = T), control = control.ergm(seed = 2112))
AICAdvice8 = Advice8$glm$aic

modelResults <- c(AICNull = AICnull, AICFull = AICfull, AICAdvice1 = AICAdvice1, AICAdvice2 = AICAdvice2,
                  AICAdvice3 = AICAdvice3, AICAdvice4 = AICAdvice4, AICAdvice5 = AICAdvice5, 
                  AICAdvice6 = AICAdvice6, AICAdvice7 = AICAdvice7, AICAdvice8 = AICAdvice8)
print(modelResults)

## Odds calculations with Advice7
or <- exp( Advice7$coef )
or #odds ratio
ste <- sqrt( diag( Advice7$covar ))
lci <- exp( Advice7$coef-1.96*ste )
uci <- exp( Advice7$coef+1.96*ste )
oddsratios <- rbind( round( lci,digits = 4 ),round(
  or,digits = 4 ),round( uci, digits = 4 ))
oddsratios <- t( oddsratios )
colnames( oddsratios ) <- c( "Lower","OR","Upper" )

# Leave out oddsratios for nodematches
oddsratios[c(2:30,34:40),]

# Probability calculated at end of document
sampleProb <- plogis(Advice7$coef[1] + Advice7$coef[2] + Advice7$coef[3] + 
                    Advice7$coef[8] + Advice7$coef[11] + Advice7$coef[28] +
                    Advice7$coef[32] + Advice7$coef[34] + Advice7$coef[35] + 
                    Advice7$coef[37] + Advice7$coef[38])

# As compared to null model
nullProb <- plogis(Advice7$coef[1])
sprintf("Null probability of tie = %.2f, Hypothesized probability of tie = %.2f", 
        nullProb, sampleProb)
