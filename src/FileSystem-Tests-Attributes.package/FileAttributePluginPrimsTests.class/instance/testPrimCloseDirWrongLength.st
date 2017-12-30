tests
testPrimCloseDirWrongLength
	"FileAttributesPluginPrims>>primClosedir: only accepts a ByteArray of size 'void *'"

	self
		should: [ primitives primClosedir: (ByteArray new: 1) ]
		raise: PrimitiveFailed
		withExceptionDo: [ :exception | 
			self assert: exception selector equals: #primClosedir:.
			self assert: exception additionalInformation isNil ].
