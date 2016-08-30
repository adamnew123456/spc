(namespace linux)
(declare
 (exit (function byte integer))
 (read (function integer integer string integer))
 (write (function integer integer string integer)))

(export 'exit 'read 'write)

(assemble exit
 "movl $1, %eax
  movl 4(%esp), %ebx
  int $0x80
  ret")

(assemble read
 "movl $3, %eax
  movl 4(%esp), %ebx
  movl 8(%esp), %ecx
  movl 12(%esp), %edx
  int $0x80
  ret")

(assemble write
 "movl $4, %eax
  movl 4(%esp), %ebx
  movl 8(%esp), %ecx
  movl 12(%esp), %edx
  int $0x80
  ret")
