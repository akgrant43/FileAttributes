tests
testCreationTime

	| resource creationTime |
	resource := self tempFileResource.
	self attributesCollectionDo: [ :each |
		creationTime := each creationTime.
		self assert: resource beforeCreationTime <= creationTime.
		self assert: resource afterCreationTime >= creationTime.
		].