"""
This is meant for loading the definitions from an external file.
"""
import os.path

from .backend import EmptyBackend
from .driver import Driver
from .errors import CompilerError
from .lexer import Lexer
from .symbols import SymbolTable
from . import types

# Since a file isn't going to change in the middle of our run, there's no
# point in processing it more than once
IMPORT_CACHE = set()

class RequireProcessor(EmptyBackend):
    """
    This is a limited kind of backend, which only stores types which are
    defined in other files.
    """
    @staticmethod
    def require(filename):
        """
        Returns a RequireProcessor which has processed the given filename,
        or None if this import has already been processed.
        """
        abs_filename = os.path.abspath(filename)
        if filename in IMPORT_CACHE:
            return None

        IMPORT_CACHE.add(abs_filename)
        with open(filename) as require_stream:
            req_processor = RequireProcessor(filename)
            lex = Lexer(require_stream, filename)
            drv = Driver(lex, req_processor)

            drv.compile()

            return req_processor

    def __init__(self, filename):
        self.exported_types = SymbolTable()
        self.exported_values = SymbolTable()
        self.exported_arrays = SymbolTable()

        self.all_values = {}
        self.all_arrays = {}

        self.import_list = set()

        self.filename = filename
        self.line = 0
        self.col = 0

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

    def handle_require(self, filename):
        """
        This invokes itself recursively, as long as the require would not be
        circular.
        """
        try:
            filename = self._register_require(filename)
        except ValueError:
            raise CompilerError(self.filename, self.line, self.col,
                "Circular require detected: '{}'", filename)

        try:
            req_processor = RequireProcessor.require(filename)
            if req_processor is None:
                return

            for type_name, type_obj in req_processor.exported_types:
                self.exported_types[type_name] = type_obj

            for val_name, val_obj in req_processor.exported_values:
                self.exported_values[val_name] = val_obj

            for arr_name, arr_flag in req_processor.exported_arrays:
                self.exported_arrays[arr_name] = arr_flag
        except OSError:
            raise CompilerError(self.filename, self.line, self.col,
                "Could not open file '{}' for reading", filename)

    def handle_decl(self, name, decl_type):
        """
        Records the declaration in the external store.
        """
        was_type_name = isinstance(decl_type, types.TypeName)
        decl_type = types.resolve_name(decl_type, self.exported_types)

        if isinstance(decl_type, types.StringLiteral):
            self.all_values[name] = types.PointerTo(types.Byte)
            self.all_arrays[name] = True
        elif was_type_name or isinstance(decl_type, types.RAW_TYPES):
            was_array = isinstance(decl_type, types.ArrayOf)
            self.all_values[name] = types.decay_if_array(decl_type)

            if was_array:
                self.all_arrays[name] = True
        elif isinstance(decl_type, types.Struct):
            # NOTE: This is a hack around the fact that (export ...) does not
            # actually accept types -  just export all the types that we
            # come across
            self.exported_types[name] = decl_type
        elif isinstance(decl_type, types.FunctionDecl):
            self.all_values[name] = decl_type
        elif isinstance(decl_type, types.AliasDef):
            # See above comment
            self.exported_types[name] = decl_type

    def handle_exports(self, names):
        """
        Moves the exported names into the export list, so that they are
        visible to the main backend.
        """
        for name in names:
            try:
                type_obj = self.all_values[name]
            except KeyError:
                raise CompilerError(self.filename, self.line, self.col,
                    'Cannot export undefined value "{}"')

            self.exported_values[name] = type_obj
            
            if name in self.all_arrays:
                self.exported_arrays[name] = True
