(declare
 (linux.exit (function byte integer))
 (linux.read (function integer integer string integer))
 (linux.write (function integer integer string integer)))

(export 'linux.exit 'linux.read 'linux.write)

(assemble linux.exit
 "movl $1, %eax
  movl 4(%esp), %ebx
  int $0x80
  ret")

(assemble linux.read
 "movl $3, %eax
  movl 4(%esp), %ebx
  movl 8(%esp), %ecx
  movl 12(%esp), %edx
  int $0x80
  ret")

(assemble linux.write
 "movl $4, %eax
  movl 4(%esp), %ebx
  movl 8(%esp), %ecx
  movl 12(%esp), %edx
  int $0x80
  ret")
