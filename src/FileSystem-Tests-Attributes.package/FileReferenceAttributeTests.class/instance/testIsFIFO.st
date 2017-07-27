tests
testIsFIFO

	self assert: self tempFileResource file isFIFO equals: false.
	self assert: self tempFileResource file attributes isFIFO equals: self tempFileResource file isFIFO.