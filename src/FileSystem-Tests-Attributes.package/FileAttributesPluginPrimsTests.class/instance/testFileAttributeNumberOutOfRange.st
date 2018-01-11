tests
testFileAttributeNumberOutOfRange
	"Check that the appropriate Bad Argument error is raised if an invalid attribute number is supplied"

	self 
		should: [ primitives fileAttribute: 'ignored' number: 0 ]
		raise: PrimitiveFailed.
	self 
		should: [ primitives fileAttribute: 'ignore' number: primitives fileAttributeNumberMap size + 1 ]
		raise: PrimitiveFailed.