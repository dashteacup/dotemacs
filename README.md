dotemacs
=======

My personal Emacs configuration files.

I don't use Emacs as much these days given the improvement in modern
IDE's capabilities, but it's still my preferred tool when I'm going to
be shuffling around a lot of text. Or when a programming language has
poor IDE support.

These files are in version control primarily for my own benefit, but
anyone may use them if they wish.  I've learned many things by reading
other people's `.emacs` files.  I didn't worry much about portability,
so simply dropping these files in place may not work for you without
manual editing.  If you want a ready-to-go Emacs setup you might want
to look at something like the
[emacs-starter-kit](https://github.com/technomancy/emacs-starter-kit)
instead.

History
======

Many people stick all their customizations in a single `.emacs` file.
After mine became several hundred lines long, I decided to split it up
into multiple files for readability/understandability. These are the
results. I found having the different sections version controlled in
different files helpful when I go months (or more) between edits. My
actual `~/.emacs.d/init.el` does little more than load all the other
files.

Installation
========

If you want to install this, just clone this repository into
`~/.emacs.d`.  Emacs version 22 and later will find your
`~/.emacs.d/init.el` automatically. Although, for anyone other than
me, this is not recommended.
