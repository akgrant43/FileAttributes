tests
testPrimExistsNil
	"FileAttributesPluginPrims>>primClosedir: nil should raise a bad argument error"

	self
		should: [ primitives primExists: nil ]
		raise: PrimitiveFailed
		withExceptionDo: [ :exception | 
			self assert: exception selector equals: #primExists: ].
