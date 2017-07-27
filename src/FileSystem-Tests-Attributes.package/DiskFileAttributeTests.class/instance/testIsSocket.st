tests
testIsSocket

	self collectionAssert: [ :each | each isSocket ] equals: false.