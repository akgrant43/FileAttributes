primitives
primReaddir: directoryPointerBytes
	"Read the next directory entry from the directory stream associated with
	directoryPointerBytes. Answer the name of the entry, ornil for end of directory stream."

	"self primReaddir: (self primOpendir: '/etc')"
	"self primReaddir: (self primOpendir: '/no/such/directory')"

	<primitive: 'primitiveReaddir' module: 'FileAttributesPlugin' error: error>
	^self primitiveFailed