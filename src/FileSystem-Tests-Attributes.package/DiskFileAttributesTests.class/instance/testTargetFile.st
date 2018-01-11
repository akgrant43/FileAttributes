tests
testTargetFile

	"The temporary file isn't a symbolic link, so the targetFile is not supported"
	self attributesCollectionDo: [ :each |
		self should: [ each targetFileReference ] raise: FileAttributeNotSupported ].
