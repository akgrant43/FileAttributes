printing
printLongFormOn: aStream

	path printOn: aStream.
	aStream cr.
	self
		printAttributeNamed: #accessTime on: aStream;
		printAttributeNamed: #modificationTime on: aStream;
		printAttributeNamed: #changeTime on: aStream;
		printAttributeNamed: #creationTime on: aStream;
		printAttributeNamed: #deviceId on: aStream;
		printAttributeNamed: #inode on: aStream;
		printAttributeNamed: #fileSize on: aStream;
		printAttributeNamed: #uid on: aStream;
		printAttributeNamed: #gid on: aStream;
		printAttributeNamed: #numberOfHardLinks on: aStream;
		printAttributeNamed: #permissions on: aStream;
		printAttributeNamed: #targetFile on: aStream;
		printAttributeNamed: #isBlock on: aStream;
		printAttributeNamed: #isCharacter on: aStream;
		printAttributeNamed: #isDirectory on: aStream;
		printAttributeNamed: #isExecutable on: aStream;
		printAttributeNamed: #isFIFO on: aStream;
		printAttributeNamed: #isFile on: aStream;
		printAttributeNamed: #isReadable on: aStream;
		printAttributeNamed: #isSocket on: aStream;
		printAttributeNamed: #isSymlink on: aStream;
		printAttributeNamed: #isWritable on: aStream.