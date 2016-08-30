(namespace io)
(*if (platform? "linux" "x86")
 (require "arch/linux_x86.lisp"))
(*if (platform? "mars" "mips")
 (require "arch/mars_mips.lisp"))

(require "lib/str.lisp")

(declare
 (printc (function byte byte))
 (print (function byte string))
 (println (function byte string))
 (read-int (function integer)))

(export 'printc 'print 'println 'read-int)

(define printc (char)
 (declare
  (buffer (array-of byte 2)))
 (block
  (set (array buffer 0) char)
  (set (array buffer 1) #\0)
  (print buffer)))

(define print (str)
 (declare)
 (block
  (*if (platform? "linux" "*")
   (linux:write 1 str (str:len str)))

  (*if (platform? "mars" "mips")
   (mars:print-string str))))

(define println (str)
 (declare)
 (block
  (print str)
  (printc #\n)))

(*if (platform? "linux" "*")
 (define read-int ()
  (declare
   (buffsz integer)
   (buffer (ascii "            ")))
  (block
   (set buffsz (str:len buffer))
   (linux:read 0 buffer buffsz)
   (return (str:str->int buffer)))))

(*if (platform? "mars" "mips")
 (define read-int ()
  (declare)
  (block
   (return (mars:read-int)))))
