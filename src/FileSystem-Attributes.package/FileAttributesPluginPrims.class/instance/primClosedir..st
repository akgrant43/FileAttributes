akg dev
primClosedir: directoryPointerBytes
	"Close the directory stream associated with directoryPointerBytes. Answer
	anExternalAddress on success, or nil on failure. Caution: do not call this
	twice on the same externalAddress."

	"self primClosedir: (self primOpendir: '/etc')"
	"self primClosedir: (self primOpendir: '/no/such/directory')"

	<primitive: 'primitiveClosedir' module: 'FileAttributesPlugin'>
	^self primitiveFailed