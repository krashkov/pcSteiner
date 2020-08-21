# pcSteiner

![](https://travis-ci.org/krashkov/pcSteiner.svg?branch=master)
![](http://www.r-pkg.org/badges/version-ago/pcSteiner)
![](http://cranlogs.r-pkg.org/badges/grand-total/pcSteiner)

The Steiner tree problem is a well-known combinatorial optimization problem which asks a minimum spanning subtree containing a given subset of vertices (terminals). However, in many practical applications nodes and edges have an additional numerical value which represents their significance. That is where the Prize-Collecting Steiner Tree problem arises: the goal is to find a subgraph connecting all the terminals with the most expensive nodes and least expensive edges. Since this problem is proven to be NP-hard, exact and efficient algorithm does not exist. Loopy belief propagation can be utilised to obtain an approximate solution to this problem [1,2].

## Installation

To get the latest version of the package and install it from CRAN run the following command:

    install.packages("pcSteiner")

## References

1. M. Bayati, C. Borgs, A. Braunstein, J. Chayes, A. Ramezanpour, and R. Zecchina, "Statistical Mechanics of Steiner Trees". PRL, 2008.
2. I. Biazzo, A. Braunstein and R. Zecchina, "Performance of a cavity-method-based algorithm for the prize-collecting Steiner tree problem on graphs". PRL, 2012.
