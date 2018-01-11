tests
testNLink
	"Assume that there is only one hard link to the just created temporary file"

	self collectionAssert: [ :each | each numberOfHardLinks ] equals: 1.