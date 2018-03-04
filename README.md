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

    (sila@devbox)1> sila:start().

Or for a node that will be collecting the tweet stream, we can shortcut the Twitter authentication
process:

    (sila@devbox)1> sila:start(twitter).


TODO
-----
- Don't reprocess (psql/emote) tweets for days we've already processed
- Compare tracking reports using different lexicons (allow combinatiions)
- Automate track reporting for daily updates
- Keep track report history
- Make ecsv behave when it encounters bad CSV data
- ~~Allow **raven** to handle both hashtags at the same time (or distribute across procs/nodes)~~
- ~~Reformat WUI as GRAPH:HITS, scrolling vertically for the four emotions~~
- ~~In/exclude RTs in runs~~
- ~~Check language tag in tweet metadata~~

Questions
-----
- Who are the RT-100 big players retweeting?
- Are people retweeting the non TT big players?
