akg dev
primOpendir: pathString
	"Answer an ExternalAddress for a directory stream on pathString, or nil if
	the directory cannot be opened"

	"self primOpendir: '/etc'"
	"self primOpendir: '.'"
	"self primOpendir: '/no/such/directory'"

	<primitive: 'primitiveOpendir' module: 'FileAttributesPlugin'>
	^self primitiveFailed