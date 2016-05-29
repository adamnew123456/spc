"""
This contains classes which are related to looking up names.
"""
class SymbolTable:
    """
    A lookup table which binds variables in a nested way, where access goes
    from the current symbol table up to the parent.
    """
    def __init__(self, parent=None, is_global=False, is_builtin=False):
        self.parent = parent
        self.is_builtin = is_builtin
        self.is_global = is_global
        self.bindings = {}

    def __iter__(self):
        for name, value in self.bindings.items():
            yield name, value

        if self.parent:
            yield from self.parent
            
    def find(self, key):
        """
        Finds the symbol table in which the given key is bound, or returns
        None if the symbol is not bound.
        """
        table = self
        while table is not None:
            if key in table.bindings:
                return table
            else:
                table = table.parent

        return None

    def __getitem__(self, key):
        table = self
        while table is not None:
            if key in table.bindings:
                return table.bindings[key]
            else:
                table = table.parent

        raise KeyError('Could not resolve symbol "{}"'.format(key))

    def __setitem__(self, key, value):
        if key in self:
            raise KeyError('Cannot overwrite existing definition of "{}"'.format(key))

        self.bindings[key] = value

    def __contains__(self, key):
        table = self
        while table is not None:
            if key in table.bindings:
                return True
            else:
                table = table.parent

        return False
