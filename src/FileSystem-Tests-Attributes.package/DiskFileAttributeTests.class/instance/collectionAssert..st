helper methods
collectionAssert: aBlock
	"Evaluate the assertion block for each of the FileAttribute cache modes"

	self attributesCollectionDo:
		[ :each | self assert: (aBlock value: each) ].