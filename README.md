# web-scraping-engine

Small web scraping engine implemented in Haskell using the Scalpel library: https://hackage.haskell.org/package/scalpel-0.6.2/docs/Text-HTML-Scalpel.html

There source code can be found on: 

app/Main.hs -> Where the action happens: 
	1. Scraps the url https://news.ycombinator.com/ 
	2. Show the data extracted
	3. Do some filtering and sorting

src/Lib.hs -> Contains the scraping rules, formatting data functions and the filtering and sorting functions
