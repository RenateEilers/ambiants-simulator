feeding-haskell-to-ambiants
===========================

Simulator for the 2004 ICFP contest "Dinner with Ambiants", started at
London Haskell meetup group.

Problem description: https://alliance.seas.upenn.edu/~plclub/cgi-bin/contest/task.php

Meetup details: http://www.meetup.com/London-Haskell/events/182632022/

== Runners

There are two fronteds now (served by the same simulator backend), one
prints the steps to the console, the other using Gloss to render the
field.

In the data folder are a few world files created by the ICFP judges,
and also ant files of some of the topmost contest submissions.

=== Build / run

    $ git clone <clone repository>; cd <repositor>

    $ cabal sandbox init
    $ cabal configure
    $ cabal install

    $ ./.cabal-sandbox/bin/ambiants-console <world file> <red ant file> <black ant file> inf|<number of rounds>
    $ ./.cabal-sandbox/bin/ambiants-gloss   <world file> <red ant file> <black ant file> inf|<number of rounds>