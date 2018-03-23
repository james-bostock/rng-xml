# rng-xml
RELAX NG parsing for Emacs' nxml-mode

Emacs' nxml-mode only allows grammars to be specified using RELAX NG's
compact syntax (see [RELAX NG Compact
Syntax](https://www.oasis-open.org/committees/relax-ng/compact-20021121.html)). Rng-xml
will, one day, allow nxml-mode to also use the standard, XML, RELAX NG
syntax (see [RELAX
NG](https://www.oasis-open.org/committees/relax-ng/spec.html)).

Fortunately, Emacs comes with a compact syntax schema for the RELAX NG
syntax meaning that, by the time we get the parsed XML, we know that
it is structurally valid. All that is needed is to simplify it and
convert it to a pattern (as defined in rng-pttrn.el).

The test suite is adapted from examples in the [RELAX NG Tutorial](http://relaxng.org/tutorial-20011203.html). The RNC equivalents were generated using [trang](http://www.thaiopensource.com/relaxng/trang.html).

