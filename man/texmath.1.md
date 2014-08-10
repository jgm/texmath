---
title: texmath
section: 1
footer: texmath manual
date: July 22, 2014
...

# NAME

texmath - convert between math formats.

# SYNOPSIS

texmath [*options*] [file\*]

# DESCRIPTION

`texmath` acts as a pipe, reading from stdin or from the specified
files and writing to stdout.  It converts math in the specified input
format (see `-f` below) to math in the specified output format (see `-t`
below).

If `texmath` is invoked under the name `texmath-cgi` (via a symbolic
link, or through simple renaming), it will act as a CGI script.  It will
expect query parameters for `from`, `to`, `input`, and optionally
`inline`, and return a JSON object with either `error` and a message or
`success` and the converted result.

# OPTIONS

`-f` *FORMAT*
:   Specify input ("from") format:  `tex`, `mathml`, `omml`, `native`.
    Defaults to `tex`.

`-t` *FORMAT*
:   Specify output ("to") format:  `tex`, `omml`,
    `xhtml`, `mathml`, `pandoc`, `native`.  Defaults to `mathml`.

`--inline`
:   Use the inline display style.

`-h, --help`
:   Print usage information.

`-V, --version`
:   Print version.

# AUTHORS

John MacFarlane, Matthew Pickering, Jesse Rosenthal.

# SEE ALSO

The `texmath` source code and all documentation may be downloaded
from <http://github.com/jgm/texmath/>.
