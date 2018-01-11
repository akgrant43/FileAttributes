tests
testIsFIFO

	self collectionAssert: [ :each | each isFIFO ] equals: false.