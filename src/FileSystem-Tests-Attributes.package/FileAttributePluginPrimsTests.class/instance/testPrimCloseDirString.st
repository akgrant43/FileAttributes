tests
testPrimCloseDirString
	"FileAttributesPluginPrims>>primClosedir: only accepts a ByteArray, anything else should raise an error"

	self
		should: [ primitives primClosedir: 'a string' ]
		raise: PrimitiveFailed
		withExceptionDo: [ :exception | 
			self assert: exception selector equals: #primClosedir: ].
