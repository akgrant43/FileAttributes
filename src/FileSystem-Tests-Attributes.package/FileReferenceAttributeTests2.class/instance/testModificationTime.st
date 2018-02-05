tests
testModificationTime

	| resource modificationTime |
	resource := self tempFileResource.
	modificationTime := resource file modificationTime.
	self assert: resource beforeCreationTime <= modificationTime.
	self assert: resource afterCreationTime >= modificationTime.
	self assert: resource file attributes modificationTime equals: modificationTime.
