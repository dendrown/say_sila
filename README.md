say_sila
=====

Analysis of Sentiment in Tweets on Climate Change.  This project is the core of the research portion
of my Ph.D. studies in _Informatique Cognitive_ at the _Université du Québec à Montréal_.

Build
-----

We're not at the point where we're handling releases.  For the time being, build and run the main Erlang
node from the repo:

    $ rebar3 compile
    $ ERL_LIBS=_build/default/lib  erl -smp -sname sila -config ./say_sila.config

    (sila@devbox)1> sila:go().

Or for a node that will be collecting the tweet stream, we can shortcut the Twitter authentication
process:

    (sila@devbox)1> sila:go(twitter).


TODO
-----
- Compare tracking reports using different lexicons (allow combinatiions)
- Allow **raven** to handle both hashtags at the same time (or distribute across procs/nodes)
- Automate track reporting for daily updates
- Keep track report history
- Make ecsv behave when it encounters bad CSV data
- ~~Reformat WUI as GRAPH:HITS, scrolling vertically for the four emotions~~
- ~~In/exclude RTs in runs~~
- ~~Check language tag in tweet metadata~~

To Ponder
---------
`#climatechange` Big Player tweet/retweet percentages for Oct. 1-27.
```
EHFoundation237: full[5272]  tt[  0]  rt[100]
TrumpvsScience:  full[2728]  tt[  0]  rt[100]
NRGrenaissance:  full[2199]  tt[  0]  rt[100]
eljmkt_daily:    full[2066]  tt[100]  rt[  0]
Nicolasgwet:     full[1761]  tt[  0]  rt[100]
eljmkt_climate:  full[1649]  tt[100]  rt[  0]
green_conc:      full[1442]  tt[  5]  rt[ 95]
Climate_ch:      full[1374]  tt[  6]  rt[ 94]
eco_ec:          full[1209]  tt[  7]  rt[ 93]
natural_sci:     full[1168]  tt[  7]  rt[ 93]
world_dang:      full[1143]  tt[  7]  rt[ 93]
MassCreativity:  full[1143]  tt[ 99]  rt[  1]
annemariayritys: full[1102]  tt[ 94]  rt[  6]
LeadingWPassion: full[ 954]  tt[100]  rt[  0]
AroundOMedia:    full[ 923]  tt[100]  rt[  0]
mabhishek45:     full[ 866]  tt[  0]  rt[100]
BeingFarhad:     full[ 860]  tt[100]  rt[  0]
DrCnfzd:         full[ 606]  tt[ 92]  rt[  8]
latinamerica_ac: full[ 569]  tt[  0]  rt[100]
GCCThinkActTank: full[ 473]  tt[ 14]  rt[ 86]
ENCISIN:         full[ 379]  tt[100]  rt[  0]
KwameGilbert:    full[ 369]  tt[100]  rt[  0]
Hurshal:         full[ 359]  tt[100]  rt[  0]
StopAdaniCairns: full[ 326]  tt[ 28]  rt[ 72]
ZEROCO2_:        full[ 288]  tt[ 98]  rt[  2]
Jackthelad1947:  full[ 284]  tt[ 30]  rt[ 70]
CLIME_IT:        full[ 282]  tt[100]  rt[  0]
rockykistner1:   full[ 277]  tt[ 97]  rt[  3]
1o5CleanEnergy:  full[ 277]  tt[  3]  rt[ 97]
ClimateChangeTT: full[ 269]  tt[100]  rt[  0]
belugasolar:     full[ 259]  tt[  1]  rt[ 99]
6esm:            full[ 243]  tt[ 96]  rt[  4]
LetHumanismRing: full[ 243]  tt[100]  rt[  0]
maggiesmumbles:  full[ 229]  tt[  0]  rt[100]
```
