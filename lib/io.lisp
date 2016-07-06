(*if (platform? "linux_x86")
 (require "arch/linux_x86.lisp"))
(*if (platform? "mars")
 (require "arch/mars.lisp"))

(require "lib/str.lisp")

(declare
 (io.print (function byte string))
 (io.println (function byte string))
 (io.read-int (function integer)))

(export io.print io.println io.read-int)

(define io.print (str)
 (declare)
 (block
  (*if (platform? "linux_x86")
   (linux.write 1 str (str.len str)))

  (*if (platform? "mars")
   (mars.print-string str))))

(define io.println (str)
 (declare
  (newline (ascii "\n")))
 (block
  (io.print str)
  (io.print newline)))

(*if (platform? "linux_x86")
 (define io.read-int ()
  (declare
   (buffsz integer)
   (buffer (ascii "            ")))
  (block
   (set buffsz (str.len buffer))
   (linux.read 0 buffer buffsz)
   (return (str.str->int buffer)))))

(*if (platform? "mars")
 (define io.read-int ()
  (declare)
  (block
   (return (mars.read-int)))))
