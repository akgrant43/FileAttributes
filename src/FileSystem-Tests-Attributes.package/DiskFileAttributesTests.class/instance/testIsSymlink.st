tests
testIsSymlink

	self collectionAssert: [ :each | each isSymlink ] equals: false.