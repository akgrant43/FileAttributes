tests
testIsCharacter

	self assert: self tempFileResource file isCharacter equals: false.
	self assert: self tempFileResource file attributes isCharacter equals: self tempFileResource file isCharacter.
