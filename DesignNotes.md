# Design Notes

This file keeps track of some of our decision made throughout the development, 
this should be only of interest if you are considering to contribute. Which is greatly welcome!

## Writing our own Genetic Algorithms

We are aware that there are excellent genetic programming libraries in Haskell available (see some of the comments in #9) - yet we decided to make our own.

There are three primary reasons for this: 

- Genetic algorithms in Haskell usually work on binary arrays, and encoding our patches in that way would be a very complex thing, making a similar overhead than the current search
- We need performance as we have very expensive evaluations. Writing our own search enables us to tweak every part easily, enabling caching or logical tricks. 
- We might want to introduce new elements to genetic search (in general) and third-wheeling over another party might be hurtful towards the development.

Also, we must admit that it is fun to design and write algorithms.

## Why is there random and exhaustive search ? they are so stupid??

Yes, random and exhaustive search are not very sophisticated. 

But: They are important as performance-baselines. 

1. If your search-algorithm performs better than random search, the search is actually "searching" and not just guessing.
2. If your search-algorithm performs better than exhaustive search, the search is actually improving over brute force.

These are two important attributes that your tool must show - atleast when you want to make a scientific point.
As the maintainers are trying to get a PhD, we are somewhat in need to do so. 
As a user, you likely will encounter them only if you do not understand what's happening with your genetic search,
e.g. your search takes long, so you want to see if you had bad parameters and random search would be faster. 
Or, you want to know how complex the problem is in general, and check if anything is found with 10 minutes of exhaustive search.

We do not actually recommend to use anything but genetic search, except for information purposes.

## Search Class 

We introduced a toplevel interface for search classes which is very simple: 

Every search algorithm just requires a Problem (Program+Properties+Dependencies), it's configuration and then it will return the fixes. 
This hides all complexity within the search - making them independent. 

We choose this as it gives low coupling and makes extending searches easier (we proved so by quickly adding exhaustive and random search).
The drawback is, that every search can grow pretty distinct and people might be shocked if they open up genetic search for the first time.

We hope to sufficiently address this by automated testing and proper documentation.

## Check-Helpers extraction 

We extracted some of the utilities into a separate library called "Check-Helpers". 

There were two reasons for this: 

- It seals certain versions of the required and supported dependencies, hiding the needed functionality between interfaces (adapter pattern)
- It helps us to be used in an anonymized publication, where we provide a "anon.exe" + the non-revealing check-helpers.cabal

Potentially, we can merge them back, or utilize other helpers. 
If you think so coming here from *the future*, then feel free to contact us about it.

## Why no Windows?

We are using OS-caching for speeding up the compilation (you really want that).
The libraries used for that are Linux-Only.

We have no other concerns against Windows, but for us it was good enough to cover windows by making a proper docker image. 
If you happen to be well-educated in caching and think you can contribute here, we are very happy if we can welcome windows-users :) 

But: We must admit, speed is more important to us than windows support. 
