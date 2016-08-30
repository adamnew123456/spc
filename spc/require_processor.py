"""
This is meant for loading the definitions from an external file.
"""
import os.path

from .backend import EmptyBackend
from .driver import Driver
from .errors import CompilerError
from .lexer import Lexer
from . import symbols
from . import types

# Since a file isn't going to change in the middle of our run, there's no
# point in processing it more than once
IMPORT_CACHE = {}

class RequireProcessor(EmptyBackend):
    """
    This is a limited kind of backend, which only stores types which are
    defined in other files.
    """
    @staticmethod
    def require(filename, backend):
        """
        Returns a RequireProcessor which has processed the given filename,
        or None if this import has already been processed.
        """
        abs_filename = os.path.abspath(filename)
        if abs_filename in IMPORT_CACHE:
            return IMPORT_CACHE[abs_filename]

        # This has to be set to None, so that circular imports are avoided. They
        # shouldn't happen anyway, but this makes for an easy additional level
        # of safety
        IMPORT_CACHE[abs_filename] = None
        with open(filename) as require_stream:
            req_processor = RequireProcessor(filename, backend)
            lex = Lexer(require_stream, filename)
            drv = Driver(lex, req_processor)

            drv.compile()

            IMPORT_CACHE[abs_filename] = req_processor
            return req_processor

    def __init__(self, filename, real_backend):
        self.real_backend = real_backend
        self.in_function = False
        self.import_list = set()

        self.exported_values = set()
        self.exported_types = set()

        self.file_namespace = None
        self.context = symbols.Context()
        self.filename = filename
        self.line = 0
        self.col = 0

    def _value_is_defined(self, name):
        """
        Returns True if the given variable is defined in the current scope, or
        False otherwise.

        This is for the static expression processor function, var-def?
        """
        return (name in self.context.values and
                self.context.values.is_visible(name))

    def _type_is_defined(self, name):
        """
        Returns True if the given type is defined in the current scope, or
        False otherwise.

        This is for the static expression processor function, var-def?
        """
        return (name in self.context.types and
                self.context.types.is_visible(name))

    def _platform(self):
        """
        Returns the (OS, architecture) pair of the underlying backend.
        """
        return self.real_backend._platform()

    def update_position(self, line, col):
        """
        Updates the processor with the current location in the input file.
        """
        self.line = line
        self.col = col

    def _register_require(self, filename):
        """
        Registers that the given file has been required. Raises a ValueError
        if the filename has already been imported.
        """
        abs_filename = os.path.abspath(filename)

        if abs_filename in self.import_list:
            raise ValueError('Circular import')

        self.import_list.add(abs_filename)
        return abs_filename

    def _write_comment(self, comment, *args, **kwargs):
        """
        Passes a comment back to the backend. Needed for static conditionals.
        """
        self.real_backend._write_comment(comment, *args, **kwargs)

    def handle_func_def_start(self, *_):
        """
        Ignore any definitions restricted to functions.
        """
        self.in_function = True

    def handle_func_def_end(self):
        """
        Stop ignoring the next declaration block.
        """
        self.in_function = False

    def handle_namespace(self, namespace):
        """
        Sets the current namespace, if one is not defined.
        """
        if self.file_namespace is not None:
            raise CompilerError(self.filename, self.line, self.col,
                "Namespace already assigned")

        self.file_namespace = namespace
        self.context = self.context.register(namespace)

    def handle_require(self, filename):
        """
        This invokes itself recursively, as long as the require would not be
        circular.
        """
        if self.file_namespace is None:
            raise CompilerError(self.filename, self.line, self.col,
                "Must define a file namespace before executing a require")

        try:
            filename = self._register_require(filename)
        except ValueError:
            raise CompilerError(self.filename, self.line, self.col,
                "Circular require detected: '{}'", filename)

        try:
            req_processor = RequireProcessor.require(filename, self.real_backend)
            if req_processor is None:
                return

            for val_name in req_processor.exported_values:
                self.context.values.meta_get(val_name, 'visible').add(self.file_namespace)

            for type_name in req_processor.exported_types:
                self.context.types.meta_get(type_name, 'visible').add(self.file_namespace)
        except OSError:
            raise CompilerError(self.filename, self.line, self.col,
                "Could not open file '{}' for reading", filename)

    def handle_decl(self, name, decl_type):
        """
        Records the declaration in the external store.
        """
        if self.in_function:
            return

        if self.file_namespace is None:
            raise CompilerError(self.filename, self.line, self.col,
                "Must define a file namespace before executing a declare")

        was_type_name = isinstance(decl_type, types.TypeName)
        decl_type = types.resolve_name(decl_type, self.context.types)

        if isinstance(decl_type, types.StringLiteral):
            self.context.values[name] = types.PointerTo(types.Byte)
            self.context.values.meta_set(name, 'visible', {self.file_namespace})
            self.context.values.meta_set(name, 'array', True)
            self.context.values.meta_set(name, 'global', True)
        elif was_type_name or isinstance(decl_type, types.RAW_TYPES):
            was_array = isinstance(decl_type, types.ArrayOf)
            self.context.values[name] = types.decay_if_array(decl_type)
            self.context.values.meta_set(name, 'visible', {self.file_namespace})
            self.context.values.meta_set(name, 'global', True)

            if was_array:
                self.context.values.meta_set(name, 'array', True)
        elif isinstance(decl_type, types.Struct):
            self.context.types[name] = decl_type
            self.context.types.meta_set(name, 'visible', {self.file_namespace})
        elif isinstance(decl_type, types.FunctionDecl):
            full_decl_type = symbols.namespace_func_decl(
                    decl_type, 
                    self.file_namespace)
            self.context.values[name] = full_decl_type

            self.context.values.meta_set(name, 'visible', {self.file_namespace})
            self.context.values.meta_set(name, 'global', True)
        elif isinstance(decl_type, types.AliasDef):
            self.context.types[name] = decl_type
            self.context.types.meta_set(name, 'visible', {self.file_namespace})

    def handle_exports(self, names):
        """
        Moves the exported names into the export list, so that they are
        visible to the main backend.
        """
        def check_non_foreign(name, context):
            """
            Ensures that the given name doesn't resolve to an identifier
            that belongs to a foreign namespace.

            Allowing these to be re-exported would lead to 'origination 
            issues', since moving them from one namespace to another would
            lose the original name. Since this is required for globals,
            that would have to be stored somewhere, which complicates 
            things.
            """
            namespace, _ = symbols.split_namespace(context.resolve(name))
            if namespace != self.file_namespace:
                raise CompilerError(self.filename, self.line, self.col,
                    'Cannot re-export foreign value or type "{}"', name)

        for name in names:
            if name[0] == "'":
                name = name[1:]
                check_non_foreign(name, self.context.values)

                try:
                    type_obj = self.context.values[name]
                except KeyError:
                    raise CompilerError(self.filename, self.line, self.col,
                        'Cannot export undefined value "{}"')

                self.exported_values.add(self.context.values.resolve(name))
            elif name[0] == '*':
                name = name[1:]
                check_non_foreign(name, self.context.types)

                try:
                    type_decl = self.context.types[name]
                except KeyError:
                    raise CompilerError(self.filename, self.line, self.col,
                        'Cannot export undefined type "{}"', name)

                self.exported_types.add(self.context.types.resolve(name))
            else:
                raise CompilerError(self.filename, self.line, self.col,
                    "Exported name must be prefixed with ' or *")
