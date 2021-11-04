say_sila
=====

Analysis of Sentiment in Tweets on Climate Change.  This project is the core of the research portion
of my PhD studies in _Informatique Cognitive_ at the _Université du Québec à Montréal_.

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

