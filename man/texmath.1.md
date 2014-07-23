---
title: texmath
section: 1
footer: texmath manual
date: July 22, 2014
...

# NAME

texmath - convert between math formats.

# SYNOPSIS

texmath [*options*]

# DESCRIPTION

`texmath` acts as a pipe, reading from stdin and writing to stdout.
It converts math in the specified input format (see `-f` below)
to math in the specified output format (see `-t` below).

# OPTIONS

`-f` *FORMAT*
:   Specify input ("from") format:  `tex`, `mathml`.  Defaults to `tex`.

`-t` *FORMAT*
:   Specify output ("to") format:  `native`, `tex`, `omml`,
    `xhtml`, `mathml`, `pandoc`.  Defaults to `mathml`.

`--inline`
:   Use the inline display style.

`-h, --help`
:   Print usage information.

`-V, --version`
:   Print version.

# AUTHORS

John MacFarlane, Matthew Pickering.

# SEE ALSO

The `texmath` source code and all documentation may be downloaded
from <http://github.com/jgm/texmath/>.
