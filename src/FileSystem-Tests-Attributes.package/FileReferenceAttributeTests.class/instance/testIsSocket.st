tests
testIsSocket

	self assert: self tempFileResource file isSocket equals: false.
	self assert: self tempFileResource file attributes isSocket equals: self tempFileResource file isSocket.