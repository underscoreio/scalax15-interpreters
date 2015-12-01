Interpreters Workshop for Scala Exchange 2015
=============================================

Slides and companion material for the Scala Interpreters workshop from [Scala Exchange 2015].

Copyright 2014 [Noel Welsh] of [Underscore]. Licensed [CC-BY-NC-SA 4.0].

**IMPORTANT! Please follow the QUICK START below PRIOR TO THE CONFERENCE.**

[![Join the chat at https://gitter.im/underscoreio/scalax15-interpreters](https://badges.gitter.im/Join%20Chat.svg)](https://gitter.im/underscoreio/scalax15-interpreters?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)

# Quick Start

Follow the instructions below prior to the conference to get started.
Doing so will prevent you having to download the internet on conference wifi!
You will need a Java 8 compatible JVM and a familiar programmer's text editor or IDE.
If you have any problems please let me know on the [Gitter channel].

1. Clone this repo and switch to the root directory:

    ~~~ bash
    $ git clone https://github.com/underscoreio/scalax15-interpreters.git

    $ cd scalax15-interpreters
    ~~~

2. Run the example code using SBT.
   This will take a few minutes to run the first time.
   You'll need an internet connection to download dependencies:

    ~~~ bash
    $ ./sbt.sh run     # "./sbt.bat run" on Windows
    ~~~

3. If you see the message "Hello world!", you're good.
   If not, let us know on the [Gitter channel].

4. If you use an IDE that requires further setup, do that now.
   I've included the `sbteclipse` and `ensime-sbt` plugins in the build.

I'll post the complete workshop code and slides immediately prior to the conference.
All you'll have to do on the day is `git pull` and start coding.
Looking forward to seeing you there!

# Further Reading

Sign up to the [Underscore newsletter] to receive news, views,
and code samples about Scala, interpreters, and functional programming.

[Scala Exchange 2015]: http://scala.exchange
[Noel Welsh]: http://twitter.com/noelwelsh
[Underscore]: http://underscore.io
[CC-BY-NC-SA 4.0]: http://creativecommons.org/licenses/by-nc-sa/4.0/
[Underscore newsletter]: http://underscore.io/newsletter.html
[Gitter channel]: https://gitter.im/underscoreio/scalax15-interpreters
