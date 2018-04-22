## Turning back issues of PC Magazine into Economics

Follow me as I try to extract information on the prices, features and sales of home laser printers from [PC Magazine](https://books.google.com.au/books?id=N09JwLeo7s4C&source=gbs_all_issues_r&cad=1)'s Annual Printer Editions.

I will be documenting my experimentation with various data wrangling tools and methods. This may include: 
- optical character recognition (OCR) on tables, and text in columns
- mapping various data structures into a unified dataset

Click here for updates: [@cynthiahqy](https://twitter.com/cynthiahqy)

### About the Project

**TASK**: Collect historical data about desktop laser printers from their introduction in 1984, up to the late 1990s. 

**PURPOSE**: In a [Damaged Goods](https://en.wikipedia.org/wiki/Crippleware) market, manufacturers may intentionally restrict functionality of their product in order to price discriminate. 
Home laser printers can be considered damaged goods as explained in the excerpt below. This data set will be used to examine and describe the evolution and dynamics of the market from entry to diffusion.

**METHOD** From 1984 to 1996, PC Magazine published reviews of every new printer on the market in their annual printer edition. This includes details on price, printer performance and features, as well as general commentary on market trends. 

### Related Reading

#### [Deneckere, Raymond J., and R. Preston McAfee. "Damaged goods." Journal of Economics & Management Strategy 5.2 (1996): 149-174.](http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.476.6124&rep=rep1&type=pdf)

> 2.2 IBM LASERPRINTER E

> In May 1990, IBM announced the introduction of the Laserprinter E,
a lower cost alternative to its popular Laserprinter. The Laserprinter
E was virtually identical to the original Laserprinter, except that the
E model printed text at 5 pages per minute (ppm), as opposed to 10
ppm for the Laserprinter. According to Jones (1990), the Laserprinter
E uses the same ”engine” and virtually identical parts, with one exception:

> The controllers in our evaluation unit differed only by virtue
of four socketed firmware chips and one surface
mounted chip. PC Labs’ testing of numerous evaluation
units indicated that the Laserprinter E firmware in effect
inserts wait states to slow print speed. . . . IBM has gone
to some expense to slow the Laserprinter in firmware so
that it can market it at a lower price.
That is, IBM has added chips to the LaserPrinter E that serve as
counters or idlers, chips that perform no function other than to make
the machine pause and hence print more slowly. Moreover, this is 
154 Journal of Economics & Management Strategy
the only difference in the two machines. In particular, the idling only
applies to text printing, so that graphics comes out at the same speed.
It is interesting that PC Magazine (Jones, 1990) gave a good review
of the Laserprinter E, calling it “the obvious choice” over the HewlettPackard
IIP. For an additional $1099, one can upgrade the LaserPrinter
E to identical performance with the Laserprinter, bringing the total
cost of the upgraded Laserprinter E to $200 more than the original
Laserprinter.



