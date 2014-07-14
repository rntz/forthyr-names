import markov

consonants = tuple('f h k l n s t th w x'.split())
vowels = tuple('aeiou')

def tt(n):
    for i in xrange(2 ** n):
        yield tuple(int(bool(i & (1 << j))) for j in xrange(n))

def subseqs(s):
    for t in tt(len(s)):
        yield tuple(x for (x,b) in zip(s,t) if b)

# A syllable is represented as a 4-tuple (c,v,r,y).
# - c is the consonant
# - v is the vowel, or None
# - r and y represent the "r" and "y" modifiers. they are each either None (no
#   modifier), 'pre' (vowel pre-modified), or 'post' (vowel post-modified).
def representable_syllables():
    return representable_syllables_on()

# syllables representable for a given consonant
def representable_syllables_on(c=None):
    # TODO: generate in "canonical order"
    # eg: f, fa, fra, far, fya, fay, frya, fray, fyar, fayr
    return ((c_, v_, r_, y_)
            for c_ in (consonants if c is None else (c,))
            for v_ in (None,) + vowels
            for y_ in ((None,'pre','post') if v_ else (None,))
            for r_ in ((None,'pre','post') if v_ else (None,)))

def show_syllable(syl):
    c,v,r,y = syl
    assert not v or not (r or y)
    return ''.join(
        [c,
         'r' if r == 'pre' else '',
         'y' if y == 'pre' else '',
         v or ('_' if r or y else ''),
         'y' if y == 'post' else '',
         'r' if r == 'post' else ''])

def syllables():
    for s in representable_syllables():
        if legit(s): yield s

def legit(syl):
    c,v,r,y = syl
    # no modifiers without vowel
    if not v and (r or y): return False
    # h and w can't be used w/o vowel
    if c in 'hw' and not v: return False
    return True

def dump_syllables(fd):
    for c in consonants:
        fd.write(' '.join(show_syllable(syl)
                          for syl in representable_syllables_on(c)
                          if legit(syl))
                 + '\n')

print('loaded forthyr.py')       # FIXME
