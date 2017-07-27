tests
testNLink
	"Assume that there is only one hard link to the just created temporary file"

	self assert: self tempFileResource file numberOfHardLinks equals: 1.
	self assert: self tempFileResource file attributes numberOfHardLinks equals: self tempFileResource file numberOfHardLinks.