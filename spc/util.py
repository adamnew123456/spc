"""
Various utility functions.
"""
import itertools
import string

def make_label_maker():
    """
    Returns an infinite generator which produces label names.
    """
    return ('LM_{}'.format(value) for value in itertools.count(1))

# Note that _ is not included, to avoid creating ambiguities - _ is always
# encoded as '_95'
LABEL_CHARS = string.ascii_letters + string.digits
def mangle_label(label):
    """
    Converts an identifier into a label name which most assemblers should
    accept.

    This outputs labels which start with 'L_' and contain
    only alphanumerics and underscores.
    """
    chunks = []
    for char in label:
        if char not in LABEL_CHARS:
            char = '_' + str(ord(char)) + '_'
        
        chunks.append(char)

    return 'L_' + ''.join(chunks)

def unescape_bytes(bytestr):
    """
    Returns a variant of the given bytestring that has C escapes replaced
    with their ASCII values.

    >>> unescape_bytes(b'\\0')
    b'\x00'
    """
    return bytestr.decode('unicode_escape')

