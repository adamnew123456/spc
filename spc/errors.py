"""
The different types of errors the compiler can raise.
"""
class CompilerError(Exception):
    """
    A general-purpose compiler error, which contains the position of the error
    as well as a formatted message.
    """
    def __init__(self, line, col, fmt, *args, **kwargs):
        self.line = line
        self.column = col

        if line > 0 and col > 0:
            self.message = (
                "At {}:{}: ".format(line, col) +
                fmt.format(*args, **kwargs))
        else:
            self.message = fmt.format(*args, **kwargs)

    @staticmethod
    def from_token(tok, fmt, *args, **kwargs):
        """
        Makes a new CompilerError, taking the position from the given token.
        """
        return CompilerError(tok.line, tok.column, fmt, *args, **kwargs)
