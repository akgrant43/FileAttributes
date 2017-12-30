tests
testPrimCloseDirNil
	"FileAttributesPluginPrims>>primClosedir: nil should raise a bad argument error"

	self
		should: [ primitives primClosedir: nil ]
		raise: PrimitiveFailed
		withExceptionDo: [ :exception | 
			self assert: exception selector equals: #primClosedir:.
			self assert: exception additionalInformation isNil ].
