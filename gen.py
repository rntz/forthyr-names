# Hacked-up generator not even using Markov chains because I just want something
# to play around with.
from random import randint, choice, random
import itertools
import re
import bisect

# coin flip which returns True w/ probability p
def heads(p): return random() <= p
def tails(p): return random() > p

def matches(r, string):
    return re.fullmatch(r, string) is not None

def endswith(r, string):
    if not isinstance(r, str): r = r.pattern
    return re.search('(?:' + r + ')$', string) is not None

def weighted(weighted_choices):
    # Taken from https://docs.python.org/3/library/random.html
    choices, weights = zip(*weighted_choices)
    cumdist = list(itertools.accumulate(weights))
    x = random() * cumdist[-1]
    return choices[bisect.bisect(cumdist,x)]

def run_weighted(weighted_functions):
    return weighted(weighted_functions)()

def weight(weighted_list, target=1):
    # normalizes to an average of `target`.
    average = sum(w for x,w in weighted_list) / len(weighted_list)
    return [(x, target * w / average) for x,w in weighted_list]


# prefixed with 's' for "Sikandan Abugida"
s_consonants = tuple('tknlpwfshxθ')
s_vowels = tuple('aeiou')
s_modifiers = tuple('ry')

# prefixed with 'r' for regex
r_consonant = re.compile('[tknlpwfshxθ]')
r_vowel = re.compile('[aeiou]')
r_modifier = re.compile('[ry]')
r_pre_modifiers = re.compile('r?y?')
r_post_modifiers = re.compile('y?r?')
r_mora = re.compile(r'([tknlpwfshxθ])(?:(r?y?)([aeiou])(y?r?))?')
r_word = re.compile('(?:' + r_mora.pattern + ')+')

# syllables are represented as strings.
def is_mora(s): return matches(r_mora, s)
def is_word(s): return matches(r_word, s)


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


# Prefixed with 'w' for "weighted list"
w_stops = [('t', 8.5), ('k', 5), ('n', 3), ('p', 1)]
w_fricatives = [('f',1), ('s',2), ('sr',1), ('x',0.5), ('θ',0.8), ('lh',2)]
w_affricates = [('ts',1), ('tsr',1), ('tθ',1), ('tlh',1), ('kx',1), ('klh',1)]
w_approximants = [('l',1), ('w',1), ('r',1), ('h',1)]
w_consonants = sum([weight(w_stops, 3 * len(w_stops)),
                    weight(w_fricatives, 1 * len(w_fricatives)),
                    weight(w_affricates, 0.25 * len(w_affricates)),
                    weight(w_approximants, 0.25 * len(w_approximants))],
                   [])

w_vowels = [('a',5), ('e',1), ('i',0.7), ('o',1.2), ('u',0.4)]


# A different way of doing things, based on phonemes
r_pvowel = re.compile('(?:y?[aeiou]|[aeiou]y)r?')
r_pconsonant = re.compile(
    r'[tknlpwfshxθr]|nk|sr|lh'
    # affricates
    + r'|tsr?|tθ|tlh|kx|pf')

r_stop = re.compile('[tkpn]')
# h acts like a fricative
r_fricative = re.compile('[fθshx]|sr|lh')

def is_vowel(s): return matches(r_pvowel, s)
def is_consonant(s): return matches(r_pconsonant, s)
def is_stop(s): return endswith(r_stop, s)
def is_fricative(s): return endswith(r_fricative, s)

# Some parameters.
nsyllables_range = (2,5)        # inclusive

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
        and (not prev.endswith('r') or is_vowel(prev)))

def is_repeat(a,b):
    if a == b: return True
    if b.startswith(a): return True
    return False


def g_vowel(prev=None):
    assert not (prev and matches(r_pvowel, prev))
    v = weighted(w_vowels)
    if heads(p_vowel_y): v = 'y'+v if heads(p_pre_y) else v+'y'
    if heads(p_vowel_r): v += 'r'
    return v

def g_consonant(prev=None):
    assert not (prev and needs_vowel(prev))
    choices = w_consonants
    if prev and not is_vowel(prev):
        # apply rules disallowing certain consonant clusters
        # first, no doubled consonants
        choices = [(x,w) for x,w in choices if not is_repeat(prev, x)]
        # no double-stops, no fricative-stops
        if is_stop(prev) or is_fricative(prev):
            choices = [(x,w) for x,w in choices if not is_stop(x)]
        # no stops or fricatives after x
        if prev[-1] in 'x':
            choices = [(x,w) for x,w in choices
                       if not (is_stop(x) or is_fricative(x))]
        ## Not sure I like this rule.
        # # no 's' after fricatives
        # if is_fricative(prev):
        #     choices = [x for x in choices if x != 's']

        # avoid creating a compound (that's handled later in this function,
        # don't want to make compounds more likely than `p_compound`)
        if prev[-1] in compounds:
            choices = [(x,w) for x,w in choices if x not in compounds[prev[-1]]]
    else:
        # can't have a lone "r"
        choices = [(x,w) for x,w in choices if x != 'r']
    c = weighted(choices)
    # if c in compounds and heads(p_compound):
    #     c += choice(compounds[c])
    return c


def g_nucleus(prev):
    return weighted([(g_nucleus_strong, 10), (g_nucleus_weak, 2)])(prev)

def g_nucleus_strong(prev):
    return weighted([(g_nucleus_stop, 3 * len(w_stops)),
                     (g_nucleus_fricative, 1 * len(w_fricatives)),
                     (g_nucleus_affricate, 0.25 * len(w_affricates))])(prev)

def g_nucleus_weak(prev):
    c = choice(x for x in ['h','l','w'] if allowed(prev,x))
    if c == 'h':
    elif c == 'l':
    else: return c

def g_syllable(prev=None):
    prev = g_consonant(prev)
    syl = prev
    if not needs_vowel(prev) and heads(p_second_consonant):
        prev = g_consonant(prev)
        syl += prev
    prev = g_vowel(prev)
    syl += prev
    if heads(p_terminal_consonant):
        while True:
            temp = g_consonant(prev)
            # can't end in a consonant that needs a vowel
            if needs_vowel(temp): continue
            prev = temp
            syl += prev
            break
    return syl, prev

def g_word(nsyllables=None):
    if nsyllables is None:
        nsyllables = randint(*nsyllables_range)
    prev = None
    syllables = []
    for _ in range(nsyllables):
        syl, prev = g_syllable(prev)
        syllables.append(syl)
    return ''.join(syllables)


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
    # compounds
    'lh': 'ɬ', 'sr': 'ʃ', 'nk': 'ŋ',
    # vowels
    'a': 'ɑ', 'e': 'ɛ', 'i': 'ɪ', 'u': 'ɯ',
}

def transcribe(word, table=trans_table):
    if not table: return word
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
    for _ in range(n):
        print(transcribe(g_word(), table))
