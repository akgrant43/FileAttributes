tests
testCreationTime

	| resource creationTime |
	resource := self tempFileResource.
	creationTime := resource file creationTime.
	self assert: resource beforeCreationTime <= creationTime.
	self assert: resource afterCreationTime >= creationTime.
	self assert: resource file attributes creationTime equals: creationTime.
