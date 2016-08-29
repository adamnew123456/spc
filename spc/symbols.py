"""
This contains all the defined values and types within this compile run.
"""
from itertools import count

from . import types

#
# This module comes out of the realization that namespaces are fundamentally
# global objects.
#
# The compiler doesn't attach symbol tables to any given file or function -
# instead, it attaches a namespace to each of those things. Files derive
# their namespace from the (namespace ...) declaration, while functions
# have their namespaces generated in such a way that they don't conflict
# with anything else. Since each value and type is namespaced, the whole
# body of declarations can be shared without causing conflicts.
#
# What is local to each file and function is its namespace search list,
# which is embodied by the namespace context. The namespace context allows for
# lookups local to each file/function/etc. without specifying the full
# namespace for search. It can also create derived namespaces, which may or
# may not have actual names (in reality, they all have names, but not all of
# those names are accessible)
#
# Symbols can also have metadata attached to them:
#
#  - Visible: this is the list of namespaces from which a particular name is
#    allowed to be accessed. This ensures that 'export' and 'require' work
#    in tandem to (a) allow access to remote namespaces and (b) allow for
#    hiding so that namespaces can't see exports without a specific require
#
#  - Global: this marks variables that are stored out in main memory
#   (e.g. inside of a .data section) - others are located on the stack.
#
#  - Array: this marks variables that are arrays, which immediately degrade
#    into pointers, but which cannot be assigned to
#
VISIBLE_ALL = object()

values = {}
values_meta = {}

types = {
    'lang:int': types.Integer,
    'lang:byte': types.Byte,
    'lang:string': types.PointerTo(types.Byte),
}

types_meta = {
    'lang:int': {
        'visible': VISIBLE_ALL,
    },
    
    'lang:byte': {
        'visible': VISIBLE_ALL,
    },

    'lang:string': {
        'visible': VISIBLE_ALL,
    },
}

registered_namespaces = set()

def join_namespace(namespace, ident):
    """
    Joins a namespace and a bare identifier into a full identifier.
    
    >>> join_namespace('a', 'b')
    'a:b'
    >>> join_namespace('', 'b')
    ':b'
    """
    return ':'.join([namespace, ident])

def split_namespace(ident):
    """
    Splits an identifier into its namespace and its bare identifier.

    >>> split_namespace('foo')
    (None, 'foo')
    >>> split_namespace('foo:bar')
    ('foo', 'bar')
    """
    if ':' not in ident:
        return (None, ident)
    else:
        result = ident.split(':', 1)
        if len(result) == 1:
            return ('', ident)
        else:
            return tuple(result)

def has_namespace(ident):
    """
    Returns True if a namespace is given with the identifier, False otherwise.

    >>> has_namespace('foo')
    False
    >>> has_namespace('foo:bar')
    True
    """
    return ':' in ident

def namespace_func_decl(func_decl, namespace):
    """
    Adds a namespace to a function declaration's name.

    This is necessary since FunctionDecl objects come out of the driver
    without a namespace attached (it would be too much to ask of the
    parser to do namespace resolution), so we have to do it later on.
    """
    full_name = join_namespace(namespace, func_decl.name)
    return func_decl._replace(name=full_name)


# Note the space - this isn't something that can be represented in the language,
# so it makes a good escape
NS_GENERATOR = (' compiler_{}'.format(value) for counter in itertools.count())

class SearchProxy:
    """
    A proxy over a dictionary which uses a search path for 
    """
    def __init__(self, names, meta, search_path):
        self.search_path = tuple(search_path)
        self.names = names
        self.meta = meta

    def resolve(self, name):
        """
        Converts a name into a fully namespaced identifier, by searching
        through the search path.
        """
        ns, ident = split_namespace(name)
        if ns is None:
            for namespace in self.search_path:
                full_ident = join_namespace(namespace, ident)
                if full_ident in self.names:
                    return full_ident
            raise KeyError(name)
        else:
            return name

    def is_visible(self, name):
        """
        Returns True if the current context can resolve the given name 
        (based upon its 'visible' list), or False if the current context
        cannot resolve it.
        """
        visible_list = self.meta_get(name, 'visible', set())

        if visible_list is VISIBLE_ALL:
            return True

        for namespace in self.search_path:
            if namespace in visible_list:
                return True

        return False

    def __getitem__(self, name):
        full_ident = self.resolve(name)
        return self.names[full_ident]

    def __setitem__(self, name, value):
        ns, ident = split_namespace(name)
        if ns is None:
            full_ident = join_namespace(self.search_path[0], ident)
        else:
            full_ident = name

        self.names[full_ident] = value

    def __contains__(self, name):
        full_ident = self.resolve(name)
        return full_ident in self.names

    def meta_get(self, name, attr, default=None):
        """
        Retrieves the metadata associated with an identifier.
        """
        full_ident = self.resolve(name)
        return self.meta.get(full_ident, {}).get(attr, default)

    def meta_set(self, name, attr, value):
        """
        Assigns a piece of metadata to the given identifier.
        """
        full_ident = self.resolve(name)

        if full_ident not in self.meta:
            self.meta[full_ident] = {}

        self.meta[full_ident][attr] = value

    def meta_default(self, name, attr, value):
        """
        Sets the value for the given identifier and attribute, but only if
        the attribute is not already assigned.
        """
        full_ident = self.resolve(name)

        if full_ident not in self.meta:
            self.meta[full_ident] = value

class Context:
    """
    This contains the namespace context, which can be used to search for defined
    values and types and manage metadata attached to them.
    """
    def __init__(self, search_path=None):
        if not search_path:
            self.search_path = ['lang']
        else:
            self.search_path = search_path

        self.values = SearchProxy(values, values_meta, self.search_path)
        self.types = SearchProxy(types, types_meta, self.search_path)

    def register(self, name):
        """
        Registers the given named namespace, and then returns the result of
        'enter'ing it.
        
        This ensure that all namespaces used in the program are unique - if
        this namespace is not, this function will raise a ValueError.
        """
        if name in self.registered_namespaces:
            raise ValueError(name)

        return self.enter(name)

    def enter(self, name=None):
        """
        Returns a new namespace context, which has the given namespace 
        attached to it (if name is given) or a new, private namespace attached
        to it (if name is not given).
        """
        if name is None:
            name = next(NS_GENERATOR)

        return Context([name] + self.search_path)
