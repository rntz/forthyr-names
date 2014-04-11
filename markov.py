#!/usr/bin/env python
import random

# TODO: make this work on generators
def ngrams(gen, n=1, eof=None):
    last = ()
    for out in gen:
        yield (last, out)
        last = last + (out,)
        if len(last) > n: last = last[1:]
    yield (last, eof)

class Markov(object):
    # self.map is a dict from ngrams to (mass, dist) pairs. mass is a count of
    # the (weighted) number of occurrences of this ngram we've seen so far.
    #
    # `dist' is a dict from outputs to a weighted "likelihood" of that output
    # occurring after the given ngram. this "likelihood" must be divided by
    # `num' to yield a probability.
    def __init__(self, n=1, eof=None):
        self.n = n              # length of n-grams
        self.eof = eof          # end-of-sequence symbol
        self.map = {}

    # Converting to/from plain python types.
    @staticmethod
    def load(v, loader=lambda x: x, eofloader=lambda x: x):
        ngramload = lambda x: tuple(map(loader, x))
        n, eof, dct = v
        self = Markov(n, eofloader(eof))
        self.map = {ngramload(k): (n, {loader(o): l
                                       for o,l in d.iteritems()})
                    for k,(n,d) in map.iteritems()}
        return self

    def dump(self, dumper=lambda x: x, eofdumper=lambda x: x):
        ngramdump = lambda x: tuple(map(dumper, x))
        dct = {ngramdump(k): (n, {dumper(o): l
                                  for o,l in d.iteritems()})
               for k,(n,d) in self.map}
        return (n, eofdumper(eof), dct)

    # Returns predicted likelihood of `output' given `ngram' has just occurred.
    def predict(self, ngram, output):
        assert isinstance(ngram, tuple) and len(ngram) <= self.n
        if ngram not in self.map:
            raise Exception("no instances of that ngram yet recorded")
        mass, dist = self.map[ngram]
        return dist.get(output, 0) / float(mass)

    # Updating based on inputs.
    def learn_one(self, ngram, output, weight=1.0):
        assert isinstance(ngram, tuple) and len(ngram) <= self.n
        mass, dist = self.map.get(ngram, (0,{}))
        dist[output] = weight + dist.get(output,0)
        self.map[ngram] = (mass+weight, dist)

    # automatically inserts `eof' at end of sequences
    def learn_seq(self, seq):
        for (ngram, out) in ngrams(seq, n=self.n, eof=self.eof):
            self.learn_one(ngram, out)

    # NB: if you want to learn lines from a file, with tokens separated by
    # whitespace, do:
    #
    #   with open(filename, 'r') as f:
    #       markov.learn_seqs(x.split() for x in f)
    #
    # which works because iterating a file gives you the lines in it.
    def learn_seqs(self, seqs):
        for s in seqs: self.learn_seq(s)

    # Randomly chooses a next-output assuming `ngram' has just occurred.
    def generate_one(self, ngram):
        assert isinstance(ngram, tuple) and len(ngram) <= self.n
        if ngram not in self.map:
            raise Exception("no instances of that ngram yet recorded")
        mass, dist = self.map[ngram]
        assert mass > 0
        p = random.uniform(0, mass)
        sofar = 0
        for out,n in dist.iteritems():
            sofar += n
            if p <= sofar: return out
        assert False            # impossible w/o invariant violation

    # make this a generator
    # TODO: a version that chooses init randomly somehow
    def generate_seq(self, init=()):
        prev = tuple(init)
        while True:
            prev = prev[max(0, len(prev) - self.n):]
            out = self.generate_one(prev)
            if out == self.eof: return
            yield out
            prev = prev + (out,)

def take(n, g):
    s = []
    g = iter(g)
    for i in xrange(n):
        try: s.append(g.next())
        except StopIteration: break
    return s

def from_file_lines(filename, n=1):
    m = Markov(n=n)
    with open(filename, 'r') as f:
        m.learn_seqs(x.split() for x in f)
    return m

def from_file(filename, n=1):
    m = Markov(n=n)
    with open(filename, 'r') as f:
        def mkit():
            for line in f:
                for word in line.split():
                    yield word
        m.learn_seq(mkit())
    return m

print 'loaded markov.py'        # FIXME
