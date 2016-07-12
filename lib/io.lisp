(*if (platform? "linux" "x86")
 (require "arch/linux_x86.lisp"))
(*if (platform? "mars" "mips")
 (require "arch/mars_mips.lisp"))

(require "lib/str.lisp")

(declare
 (io.print (function byte string))
 (io.println (function byte string))
 (io.read-int (function integer)))

(export 'io.print 'io.println 'io.read-int)

(define io.print (str)
 (declare)
 (block
  (*if (platform? "linux" "*")
   (linux.write 1 str (str.len str)))

  (*if (platform? "mars" "mips")
   (mars.print-string str))))

(define io.println (str)
 (declare
  (newline (ascii "\n")))
 (block
  (io.print str)
  (io.print newline)))

(*if (platform? "linux" "*")
 (define io.read-int ()
  (declare
   (buffsz integer)
   (buffer (ascii "            ")))
  (block
   (set buffsz (str.len buffer))
   (linux.read 0 buffer buffsz)
   (return (str.str->int buffer)))))

(*if (platform? "mars" "mips")
 (define io.read-int ()
  (declare)
  (block
   (return (mars.read-int)))))
