tests
testIsCharacter

	self collectionAssert: [ :each | each isCharacter ] equals: false.