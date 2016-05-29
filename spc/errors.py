"""
The different types of errors the compiler can raise.
"""
class CompilerError(Exception):
    """
    A general-purpose compiler error, which contains the position of the error
    as well as a formatted message.
    """
    def __init__(self, filename, line, col, fmt, *args, **kwargs):
        super().__init__()
        self.filename = filename
        self.line = line
        self.column = col

        if line > 0 and col > 0:
            self.message = (
                "At {}:{}:{}: ".format(filename, line, col) +
                fmt.format(*args, **kwargs))
        else:
            self.message = ('In {}: '.format(filename) + 
                fmt.format(*args, **kwargs))

    @staticmethod
    def from_token(tok, fmt, *args, **kwargs):
        """
        Makes a new CompilerError, taking the position from the given token.
        """
        return CompilerError(tok.filename, tok.line, tok.column, 
                fmt, *args, **kwargs)
