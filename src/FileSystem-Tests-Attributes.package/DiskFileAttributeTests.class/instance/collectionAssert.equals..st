helper methods
collectionAssert: aBlock equals: expected
	"Evaluate the assertion block for each of the FileAttribute cache modes"

	self attributesCollectionDo:
		[ :each | self assert: (aBlock value: each) equals: expected ].