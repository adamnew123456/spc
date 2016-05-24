"""
Lexes Lisp-like S-expression languages into one of a few tokens:

 - LEFT_PAREN
 - RIGHT_PAREN
 - NUM_INTEGER
 - IDENTIFIER
 - STRING
"""
from collections import deque, namedtuple
import logging

LOGGER = logging.getLogger('spc.lexer')

(LEFT_PAREN, RIGHT_PAREN,
 INTEGER, IDENTIFIER, STRING) = range(5)

Token = namedtuple('Token', ['type', 'content', 'line', 'column'])

def to_list(lexer_stream):
    """
    Takes the tokens from a lexer and generates a series of nested lists.
    """
    for token in lexer_stream:
        if token.type == LEFT_PAREN:
            yield list(to_list(lexer_stream))
        elif token.type == RIGHT_PAREN:
            return
        else:
            yield token

def print_list(chunk):
    """
    Converts tokens in list form back into the user-facing syntax
    """
    buffer = ''
    if isinstance(chunk, Token):
        buffer += str(chunk.content) + ' '
    elif isinstance(chunk, list):
        buffer += '('
        for elt in chunk:
            print_list(elt)

        buffer += ')'
    else:
        assert False

    return buffer

class Lexer:
    """
    Takes a raw character stream, and generates a series of Tokens which
    describe the basic syntax of that stream. Supports nesting of ( and ),
    and distinguishes between integers and identifiers.
    """
    def __init__(self, stream):
        self.stream = stream
        self.buffer = deque([], 2048)

        self.line = 1
        self.column = 0
        self.stored_chars = deque([])

    def read_word(self):
        """
        Reads in a word of characters, and distinguishes integers, decimals,
        and identifiers.
        """
        line = self.line
        column = self.column + 1 # Adjust since we haven't read anything yet
        word = ''

        while True:
            char = self.get()
            if char is None:
                break

            if char.isspace():
                break
            if char == ')':
                self.unget(char)
                break
            else:
                word += char

        try:
            return self.get_token(INTEGER, int(word, 0), line, column)
        except ValueError:
            return self.get_token(IDENTIFIER, word, line, column)

    def get_token(self, tok_type, tok_text, line=None, column=None):
        """
        Generates a Token using the Lexer's current position (or, a given
        position).
        """
        if line is None:
            line = self.line
        if column is None:
            column = self.column

        return Token(tok_type, tok_text, line, column)

    def get(self):
        """
        Gets the next character, doing the bookkeeping for the line and column.

        Returns None if no more characters are available.
        """
        if self.stored_chars:
            char = self.stored_chars.pop()
            self.update_position(char)
            return char
        elif self.buffer:
            char = self.buffer.popleft()
            self.update_position(char)
            return char
        else:
            data = self.stream.read(self.buffer.maxlen)
            self.buffer.extend(data)
            if not self.buffer:
                return None
            else:
                return self.get()

    def update_position(self, char):
        """
        Updates the current position given a character read from the buffer.
        """
        if char == '\n':
            self.line += 1
            self.column = 0
        else:
            self.column += 1
        return char

    def unget(self, char):
        """
        Puts a character back onto the input stream.
        """
        if char == '\n':
            assert False, 'Cannot put a newline back on the input stream!'
        else:
            self.column -= 1

        self.stored_chars.appendleft(char)

    def read_comment(self):
        """
        Reads in a comment from the input stream.
        """
        while True:
            char = self.get()
            if char == '\n':
                break
            else:
                if char is None:
                    break

    def read_string(self):
        """
        Reads in a double-quoted string.

        Note that this actually preserves C-style escapes; since the assembler
        will also likely support C-style escapes, there's no need to make the
        backend do extra work to undo them later.
        """
        buffer = ''
        escaped = False

        while True:
            char = self.get()
            if escaped:
                buffer += "\\" + char
                escaped = False
            elif char == '"':
                break
            else:
                buffer += char

        return self.get_token(STRING, buffer.encode('ascii'))

    def lex(self):
        """
        Lexes an input stream, producing a generator of Token objects.
        """
        while True:
            char = self.get()
            if char is None:
                break
            elif char.isspace():
                continue
            elif char == ';':
                self.unget(char)
                self.read_comment()
            elif char == '(':
                token = self.get_token(LEFT_PAREN, char)
                LOGGER.info('Token: %s', token)
                yield token
            elif char == ')':
                token = self.get_token(RIGHT_PAREN, char)
                LOGGER.info('Token: %s', token)
                yield token
            elif char == '"':
                token = self.read_string()
                LOGGER.info('Token: %s', token)
                yield token
            else:
                self.unget(char)
                token = self.read_word()
                if token is None:
                    break

                LOGGER.info('Token: %s', token)
                yield token
