Visualizing Dolphin Social Networks
===================================

Kevin Cullen

This project fulfilled a requirement for CIS576: Data Visualization and Manipulation. We were to find a "reasonably large" network data set and then create both a network and hive (or circular/arc) visualization, using any tool we wished.

Data set
--------
I searched for a “medium” sized data set (roughly 62 nodes with about 1000 edges) which would allow me to tell a story via visualization without haphazardly filtering or aggregating. I stumbled upon a data set at <http://networkrepository.com/soc-dolphins.php> drawn from this research article:

*The bottlenose dolphin community of Doubtful Sound features a large proportion of long-lasting associations.* Behav Ecol Sociobiol (2003) 54:396–405 DOI 10.1007/s00265-003-0651-y

<https://abdn.pure.elsevier.com/en/publications/the-bottlenose-dolphin-community-of-doubtful-sound-features-a-lar>

It is "a list of all of links, where a link represents frequent associations between dolphins."

From the abstract:

> We describe a small, closed population of bottlenose dolphins living at the southern extreme of the species’ range. Individuals live in large, mixed-sex groups in which no permanent emigration/immigration has been observed over the past 7 years...

> The community structure is temporally stable, compared to other bottlenose dolphin populations, and constant companionship seems to be prevalent in the temporal association pattern. Such high degrees of stability are unprecedented in studies of bottlenose dolphins and may be related to the ecological constraints of Doubtful Sound.

I was already interested in the community detection feature after using the tutorial at <https://kateto.net/network-visualization>.

Community detection only required a couple of lines of code...

    clp <- cluster_optimal(igraph.net)
    class(clp)


Network Plot
------------
Community detection returns an object of class "communities" with its own built-in plotting function. I added the community information to the `igraph` object and used igraph’s plotting features.

I used R to generate/assign colors (node and background) for each community in the network and tweaked region/community markings.

I chose the `layout_with_fr` option and used `charge`, `max.sa.movement`, `spring.length` to improve separation of the groups.

The visualization here demonstrates the article's conclusions: “Fjords are low-productivity systems in which survival may easily require a greater level of co-operation, and hence group stability.” Unlike dolphins who live in the open ocean, these groups maintain long-term relationships with individuals in other nearby communities/pods because life is tough there.

### Network Plot - Dolphin Social Networks Colored by Community

![Dolphin Social Networks Colored by Community](plots/network-plot.png "Dolphin Social Networks Colored by Community")

Nodes = Dolphins. Communities detected with `cluster_optimal()`

Circle plots/chord diagrams
---------------------------
My chord diagrams gave the best results and required much more time to arrange data correctly. The final results are visually stunning, easy to interpret and told the tale well.

Not all was perfect with `circlize`. Legends were difficult. Documentation was sparse and I never truly understood modifying the tracks, in spite of getting it to work. I added the ids as labels because I didn't have the dolphin's names as used in the article.

### Circle Plot - Dolphin Social Networks Colored by Community

![Circle Plot - Dolphin Social Networks Colored by Community](plots/circle-plot.png "Circle Plot - Dolphin Social Networks Colored by Community")

Each id number = 1 dolphin  
Colors = communities


Hive Plots
----------
“*By the way, there is no guarantee that any data set can be made into a hive plot, but there are certainly a number of data sets that will give a very useful hive plot after some thought*.”
- Bryan A. Hanson, *The HiveR Package* (July27,2017)
<https://cran.r-project.org/web/packages/HiveR/vignettes/HiveR.pdf>

I was never satisfied with my hive plots. When I put each of the five communities/pods on a separate axis, many edges/relationships did not appear on the plot. Perhaps because they would have crossed axes.

The other two hive plots showed all relationships, so I know my data was good. My source-man-sink plot looked like gibberish. I then figured out how to arbitrarily assign communities/pods to axes and set the radius so that all members of each group appeared together.


### Hive Plot: source / man / sink (useless in this case)

![Hive Plot - source / man / sink](plots/hive-plot-1.png "Hive Plot - source / man / sink")

Nodes = Dolphins  
Colors = communities

### Hive Plot: communities assigned to/sorted on arbitrary axes

![Hive Plot - arbitrary axes](plots/hive-plot-2.png "Hive Plot - arbitrary axes")

Nodes = Dolphins  
Colors = communities

### Hive Plot: one community per axis (but relationships/edges were omitted)

![Hive Plot - one community per axis](plots/hive-plot-3.png "Hive Plot - one community per axis")

Nodes = Dolphins  


Communities class built-in plot
-------------------------------
Straight out of the box, the `community` class’s built-in plotting function did an interesting job, but I couldn't find documentation for modifying the output.

### Network Plot - Dolphin Social Networks Colored by Community

![Dolphin Social Networks Colored by Community](plots/communities-class-plot.png "Dolphin Social Networks Colored by Community")

Nodes = Dolphins. Communities detected with `cluster_optimal()`


Graphic from the original article
---------------------------------
I wonder what these folks used in 2003. I think my plots look better.

![Sociogram of the community for groups](plots/dolphin-network-from-article.png "Sociogram of the community for groups")

*The bottlenose dolphin community of Doubtful Sound features a large proportion of long- lasting associations*. Behav Ecol Sociobiol (2003) 54:396–405 DOI 10.1007/s00265-003-0651-y