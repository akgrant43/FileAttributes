akg dev
primRewinddir: directoryPointerBytes
	"Rewind the directory stream associated with directoryPointerBytes. Answer
	anExternalAddress on success, or nil on failure."

	"self primRewinddir: (self primOpendir: '/etc')"
	"self primRewinddir: (self primOpendir: '/no/such/directory')"

	<primitive: 'primitiveRewinddir' module: 'FileAttributesPlugin'>
	^self primitiveFailed
