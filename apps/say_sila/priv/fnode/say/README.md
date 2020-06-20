# say
A Clojure bridge from say_sila on the BEAM over to the JVM:

&nbsp;&nbsp;&nbsp;&nbsp; **say.core, say.weka, say.sila!**


## Usage
The **say** project runs as an fnode (foreign node) for **say_sila** on Erlang.
Here in the development stage, it still needs a manual startup:

    $ lein repl
    nREPL server started on port 41919 on host 127.0.0.1 - nrepl://127.0.0.1:41919
    REPL-y 0.4.3, nREPL 0.6.0
    Clojure 1.10.1

    say.core=> (start)

## Libraries
Note: We're updating this list as we touch the associated libraries in the code
- [Tawny OWL](https://github.com/phillord/tawny-owl): OWL Ontologies in a Clojure Environment 
- [HermiT](https://github.com/owlcs/hermit-reasoner): Conformant OWL 2 DL reasoner that uses the direct semantics
- [Tweebo Parser](https://github.com/ikekonglp/TweeboParser): A Dependency Parser for Tweets


## License
Distributed under the BSD 3-clause License.

Copyright © 2017-2020 Dennis Drown et l'Université du Québec à Montréal
