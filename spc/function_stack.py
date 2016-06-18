"""
Utilities for storing and locating values on the call stack.
"""
from . import types

class FunctionStack:
    """
    Tracks where variables are on the function's stack.

    Note that this makes a number of assumptions about how things are stored:

    - All arguments are stored on the stack, in reverse order. This goes
      against the calling conventions for register rich architectures, like
      MIPS, but there are enough corner cases (like copying structs by value)
      that ignoring the calling convention is worthwhile for a non-optimizing
      compiler like this.

    - Locals and temporaries are stored on the stack, in order of creation.
    """
    def __init__(self, backend):
        self.backend = backend
        self.local_offset = self._starting_locals_offset()
        self.param_offset = 0
        self.vars = {}

    def _starting_locals_offset(self):
        """
        Returns the starting offset of the local variables on the stack.
        """
        raise NotImplementedError

    def _starting_param_offset(self):
        """
        Returns the starting offset of the parameter on the stack.
        """
        raise NotImplementedError

    def _expand_stack(self, size):
        """
        Emits code to expand the stack frame by the given size.
        """
        raise NotImplementedError

    def _shrink_stack(self, size):
        """
        Emits code to reduce the stack frame by the given size.
        """
        raise NotImplementedError

    def add_param(self, name, size, alignment):
        """
        Adds a new parameter to the stack.
        """
        self.param_offset = types.align_address(self.param_offset, alignment)

        self.vars[name] = self.param_offset
        self.param_offset += size

    def add_local(self, name, size, alignment):
        """
        Adds a local variable to the stack.
        """
        self.local_offset = (
            types.align_address(self.local_offset - size, alignment, 
                types.Alignment.Down))

        self.vars[name] = self.local_offset

    def get_temp_context(self, backend):
        """
        Returns a context which can be used for putting temporary values on 
        the stack. When the context exits, the space used by the temporary
        variables is cleaned up.
        """
        root = self

        class TemporaryContext:
            def __init__(self, start_offset):
                self.tmp_offset = start_offset
                self.total_tmp_size = 0

            def __enter__(self):
                pass

            def __exit__(self, *exc_info):
                root._shrink_stack(self.total_tmp_size)

            def add_temp(self, size, alignment):
                """
                Makes space for a new temporary, returning the $fp offset at 
                which to write it.
                """
                old_tmp_offset = self.tmp_offset
                self.tmp_offset = (
                    types.align_address(self.tmp_offset - size, alignment,
                        types.Alignment.Down))

                size_used = old_tmp_offset - self.tmp_offset
                self.total_tmp_size += size_used
                root._expand_stack(size_used)

                return self.tmp_offset

            def get_temp_context(self):
                """
                Creates a temporary context, which starts at this temporary context.
                """
                return TemporaryContext(self.tmp_offset)

        return TemporaryContext(self.local_offset)

    def locals_size(self):
        """
        Gets the size used by all the locals.
        """
        return abs(self.local_offset) - abs(self._starting_locals_offset())

    def cleanup_locals(self):
        """
        Cleans up the space used by local variables.
        """

    def __getitem__(self, name):
        """
        Gets the offset to the variable on the stack, or a Register (if the
        name was bound to one of the first four parameters)
        """
        return self.vars[name]

