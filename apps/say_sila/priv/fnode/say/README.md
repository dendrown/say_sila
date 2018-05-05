# say
A Clojure bridge from say_sila on the BEAM over to the JVM:

&nbsp;&nbsp;&nbsp;&nbsp; **say.core, say.weka, say.sila!**


## Usage
The **say** project runs as an fnode (foreign node) for **say_sila** on Erlang.
Here in the development stage, it still needs a manual startup:

    $ lein repl
    nREPL server started on port 41919 on host 127.0.0.1 - nrepl://127.0.0.1:41919
    REPL-y 0.3.7, nREPL 0.2.12
    Clojure 1.9.0

    say.core=> (start)

## License
Distributed under the BSD 3-clause License.

Copyright © 2017-2018 Dennis Drown et l'Université du Québec à Montréal
