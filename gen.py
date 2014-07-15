# Hacked-up generator not even using Markov chains because I just want something
# to play around with.
import re
from random import randint, choice, random

# coin flip which returns True w/ probability p
def heads(p): return random() <= p
def tails(p): return random() > p

def matches(r, string):
    return re.fullmatch(r, string) is not None

def endswith(r, string):
    if not isinstance(r, str): r = r.pattern
    return re.search('(?:' + r + ')$', string) is not None

consonants = tuple('tknlpwfshxθ')
vowels = tuple('aeiou')
modifiers = tuple('ry')

r_consonant = re.compile('[tknlpwfshxθ]')
r_vowel = re.compile('[aeiou]')
r_modifier = re.compile('[ry]')
r_pre_modifiers = re.compile('r?y?')
r_post_modifiers = re.compile('y?r?')
r_mora = re.compile(r'([tknlpwfshxθ])(?:(r?y?)([aeiou])(y?r?))?')
r_word = re.compile('(?:' + r_mora.pattern + ')+')

# syllables are represented as strings.
def is_word(s): return matches(r_word, s)
def is_mora(s): return matches(r_mora, s)


# # generates random words, syllable at a time
# nmoras_range = (3,7)        # inclusive
# vowel_prob = 0.8                # probability of a vowel
# modifier_prob = 0.15            # probability of adding a modifier

# def mk_word():
#     nmoras = randint(*nmoras_range)
#     return ''.join(mk_mora() for i in range(nmoras))

# def mk_mora():
#     c = choice(consonants)
#     if tails(vowel_prob): return c
#     v = choice(vowels)
#     # pre- & post-modifiers equally likely
#     pre = heads(modifier_prob)
#     post = heads(modifier_prob)
#     m1 = choice(modifiers) if pre else ''
#     m2 = choice(modifiers) if post else ''
#     return c+m1+v+m2


# A different way of doing things, based on phonemes
r_pvowel = re.compile('(?:y?[aeiou]|[aeiou]y)r?')
r_pconsonant = re.compile(
    r'[tknlpwfshxθr]|nk|sr|lh'
    # affricates
    + r'|tsr?|tθ|tlh|kx|pf')

r_stop = re.compile('[tkpn]')
# h acts like a fricative
r_fricative = re.compile('[fθshx]|sr|lh')

def is_pvowel(s): return matches(r_pvowel, s)
def is_pconsonant(s): return matches(r_pconsonant, s)
def is_pstop(s): return endswith(r_stop, s)
def is_pfricative(s): return endswith(r_fricative, s)

# Some parameters.
nsyllables_range = (1,5)        # inclusive

p_cluster = 0.2      # roughly, probability of a consonant following a consonant
p_vowel_y = 0.12     # probability of a y-modified vowel
p_pre_y = 0.7        # probability that y is a pre- not post-modification
p_vowel_r = 0.15     # probability of an r-post-modified vowel

# probability of a "compound consonant", a single logical phoneme represented by
# multiple consonants in a cluster. e.g. "ch" in English.
p_compound = 0.2

# probability of a second initial consonant in a syllable
p_second_consonant = 0.05
# probability of a terminal consonant in a syllable
p_terminal_consonant = 0.3

# the consonants that can "follow" a consonant to make a compound consonantal
# phoneme, usually but not always an affricate (e.g. "sr" -> English "sh" sound;
# "nk" -> English "ng" sound).
compounds = {
    't': tuple('θ s sr lh'.split()),
    's': tuple('r'),
    'l': tuple('h'),
    'k': tuple('x'),
    'n': tuple('k'),
}

def needs_vowel(prev):
    # some constructions need to be followed by a vowel
    return not (
        prev
        # a vowel must follow a consonant phoneme ending in 'w' or 'r'
        and not prev.endswith('w')
        and not prev.endswith('h')
        and (not prev.endswith('r') or is_pvowel(prev)))


def mk_phoneme(prev=None):
    if prev is None:
        # must make consonant at beginning of word
        return mk_consonant()
    if matches(r_pvowel, prev):
        # no multiple vowels in a row.
        return mk_consonant(prev)
    if not needs_vowel(prev):
        # some things force the next phoneme to be a vowel
        return mk_vowel(prev)
    # Previous phoneme was a consonant not ending in w.
    return mk_consonant(prev) if heads(p_cluster) else mk_vowel(prev)

def mk_vowel(prev=None):
    assert not (prev and matches(r_pvowel, prev))
    v = choice(vowels)
    if heads(p_vowel_y):
        v = 'y'+v if heads(p_pre_y) else v+'y'
    if heads(p_vowel_r):
        v += 'r'
    return v

def mk_consonant(prev=None):
    assert not (prev and needs_vowel(prev))
    choices = ['r'] + list(consonants)
    if prev and not is_pvowel(prev):
        # apply rules disallowing certain consonant clusters
        # first, no doubled consonants
        choices = [x for x in choices if x != prev]
        # no double-stops, no fricative-stops
        if is_pstop(prev) or is_pfricative(prev):
            choices = [x for x in choices if not is_pstop(x)]
        # no stops or fricatives after x
        if prev[-1] in 'x':
            choices = [x for x in choices
                       if not (is_pstop(x) or is_pfricative(x))]
        ## Not sure I like this rule.
        # # no 's' after fricatives
        # if is_pfricative(prev):
        #     choices = [x for x in choices if x != 's']

        # avoid creating a compound (that's handled later in this function,
        # don't want to make compounds more likely than `p_compound`)
        if prev[-1] in compounds:
            choices = [x for x in choices if x not in compounds[prev[-1]]]
    else:
        # can't have a lone "r"
        choices = [x for x in choices if x != 'r']
    c = choice(choices)
    if c in compounds and heads(p_compound):
        c += choice(compounds[c])
    return c

def mk_phonemes(nphonemes=10):
    if not nphonemes: return ''
    prev = None
    phonemes = []
    for _ in range(nphonemes):
        prev = mk_phoneme(prev)
        phonemes.append(prev)
    return ''.join(phonemes)

def mk_syllable(prev=None):
    prev = mk_consonant(prev)
    syl = prev
    if not needs_vowel(prev) and heads(p_second_consonant):
        prev = mk_consonant(prev)
        syl += prev
    prev = mk_vowel(prev)
    syl += prev
    if heads(p_terminal_consonant):
        while True:
            temp = mk_consonant(prev)
            # can't end in a consonant that needs a vowel
            if needs_vowel(temp): continue
            prev = temp
            syl += prev
            break
    return syl, prev

def mk_word(nsyllables=None):
    if nsyllables is None:
        nsyllables = randint(*nsyllables_range)
    prev = None
    syllables = []
    for _ in range(nsyllables):
        syl, prev = mk_syllable(prev)
        syllables.append(syl)
    return ''.join(syllables)
    # prev = None
    # phonemes = []
    # for _ in range(nsyllables):
    #     while True:
    #         prev = mk_phoneme(prev)
    #         phonemes.append(prev)
    #         if is_pvowel(prev): break
    # return ''.join(phonemes)


# Makes it more obvious how to pronounce a word
trans_table = {
    'lh': 'ɬ',
    'tsr': 'ch',
    'sr': 'sh',
    'θ': 'th',
    'nk': 'ng',
    'tθ': 'tθ',                 # keep this
    'iy': 'í',
}
for vowel in 'aeou':
    trans_table[vowel+'y'] = vowel+'i'

ipa_table = {
    'w': 'ʍ',
    # compounds
    'lh': 'ɬ', 'sr': 'ʃ', 'nk': 'ŋ',
    # vowels
    'a': 'ɑ', 'e': 'ɛ', 'i': 'ɪ', 'u': 'ɯ',
}

def transcribe(word, table=trans_table):
    r_key = re.compile('|'.join(table.keys()))
    parts = r_key.split(word)
    weirds = r_key.findall(word)
    parts.reverse()
    weirds.reverse()
    s = []
    while weirds:
        s.append(parts.pop())
        s.append(table[weirds.pop()])
    s.append(parts.pop())
    return ''.join(s)

def sample(n=5, table=trans_table):
    while n:
        w = mk_word()
        t = transcribe(w, table)
        if w != t:
            print(t)
            n -= 1
