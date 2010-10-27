# Vectro: efficient, dense, high-fanout immutable vectors

This package provides the vectro module, a Haskell library for working
with immutable vectors that provide an efficient mutation interface
(no, really!).

The vector type is represented internally as a tree with high fanout,
so its depth is bounded and accesses are very cheap.  Updates are also
cheap, as they only need to copy those parts of a tree whose structures
need modifying.


# Get involved!

Please report bugs via the
[bitbucket issue tracker](http://bitbucket.org/bos/vectro/issues).

Master [Mercurial repository](http://bitbucket.org/bos/vectro):

* `hg clone http://bitbucket.org/bos/vectro`

There's also a [git mirror](http://github.com/bos/vectro):

* `git clone git://github.com/bos/vectro.git`

(You can create and contribute changes using either Mercurial or git.)


# Authors

This library is written and maintained by Bryan O'Sullivan,
<bos@serpentine.com>.
