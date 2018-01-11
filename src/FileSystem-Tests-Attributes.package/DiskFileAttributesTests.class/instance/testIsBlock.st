tests
testIsBlock

	self collectionAssert: [ :each | each isBlock ] equals: false.