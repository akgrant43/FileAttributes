'From Pharo7.0SNAPSHOT of 22 July 2017 [Latest update: #0] on 27 July 2017 at 7:40:34.182703 pm'!
	^ self resolve gid! !
	^ self resolve numberOfHardLinks! !
	^ self resolve accessTime ! !
	^ self resolve targetFile! !
	^ self resolve isBlock ! !
	^ self resolve uid! !
	^ self resolve inode! !
	^ self resolve changeTime ! !
	^ self resolve isCharacter! !
	^ self resolve isSocket! !
	^ self resolve isFIFO! !
	^ self resolve deviceId! !
	^ self resolve isSymlink! !
	^ self resolve isExecutable! !
	"Return a boolean indicating whether the File described by aPath is a block device"
	^ filesystem isBlock: path! !
	"Return a boolean indicating whether the File described by aPath is a socket"
	^ filesystem isSocket: path! !
	"Return a boolean indicating whether the File described by aPath is FIFO"
	^ filesystem isFIFO: path! !
	"Return the target file of the File described by aPath.  For a regular file, this is itself, for a symbolic link, it is the file pointed to by the symbolic link"
	^ self class fileSystem: filesystem path: (filesystem targetFile: path)! !
	"Return the number of hard links to the File described by aPath"
	^ filesystem numberOfHardLinks: path! !
	"Return a boolean indicating whether the File described by aPath is a regular file"
	^ filesystem isRegular: path! !
	"Answer a boolean indicating whether the receiver is a symlink"
	^ filesystem isSymlink: path! !
	^ filesystem isExecutable: path! !
	"Return a boolean indicating whether the File described by aPath is character based"
	^ filesystem isCharacter: path! !
	^ filesystem accessTimeOf: self path! !
	^ filesystem changeTimeOf: self path! !
	^ filesystem gidOf: self path! !
	"Answer the receivers creation time.
	Note that the interpretation varies by platform."
	^ filesystem creationTimeOf: self path! !
	^ filesystem inodeOf: self path! !
	^ filesystem uid: self path! !
	^ filesystem deviceIdOf: self path! !
	"Return the target file of the File described by aPath.  For a regular file, this is itself, for a symbolic link, it is the file pointed to by the symbolic link"

	^ store targetFile: (self resolve: aResolvable)! !
	"Resolve the argument, and answer true if the there is
	a file that can be written to or directory that can be changed."
	
	^ store isWritable: (self resolve: aResolvable)! !
	"Return a boolean indicating whether the File described by aPath is a regular file"

	^ store isRegular: (self resolve: aResolvable)! !
	"Return a boolean indicating whether the File described by aPath is character based"

	^ store isCharacter: (self resolve: aResolvable)! !
	"Return a boolean indicating whether the File described by aPath is a block device"

	^ store isBlock: (self resolve: aResolvable)! !
	"Return a boolean indicating whether the File described by aPath is FIFO"

	^ store isFIFO: (self resolve: aResolvable)! !
	"Resolve the argument, and answer true if the there is
	a file or directory that can be read from."
	
	^ store isExecutable: (self resolve: aResolvable)! !
	"Resolve the argument, and answer true if the there is
	a file or directory that can be read from."
	
	^ store isReadable: (self resolve: aResolvable)! !
	"Return a boolean indicating whether the File described by aPath is a file (not a directory)"

	^ store isFile: (self resolve: aResolvable)! !
	"Resolve the argument, and answer true if the result refers
	to a directory, false if it refers to a file or doesn't exist."

	^ store numberOfHardLinks: (self resolve: aResolvable)! !
	"Return a boolean indicating whether the File described by aPath is a regular file"

	^ store isSocket: (self resolve: aResolvable)! !
	"Returns the group id of aResolvable"

	^ store gidOf: (self resolve: aResolvable)! !
	"Answer the FileSystemDirectoryEntry for aResolvable"

	^ store entryAt: (self resolve: aResolvable) fileSystem: self.
! !
	"Returns the access date of aResolvable"

	^ store accessTimeOf: (self resolve: aResolvable)! !
	"Returns the inode number of aResolvable"

	^ store inodeOf: (self resolve: aResolvable)! !
	"Returns the device id of aResolvable"

	^ store deviceIdOf: (self resolve: aResolvable)! !
	"Returns the user id of aResolvable"

	^ store uid: (self resolve: aResolvable)! !
	"Returns the change time of aResolvable"

	^ store changeTimeOf: (self resolve: aResolvable)! !
	| path |
	path := self resolve: aResolvable.
	store
		directoryAt: path
		fileNodesDo: [ :entry | 
			aBlock value: path / (store basenameFromEntry: entry) ]! !
	| path |
	path := self resolve: aResolvable.
	store
		directoryAt: path
		directoryNodesDo: [ :entry | 
			aBlock value: path / (store basenameFromEntry: entry) ]! !
	| path |
	path := self resolve: aResolvable.
	store
		nodesAt: path
		do: [ :node | 
			aBlock value: path / (store basenameFromEntry: node) ]! !

	^[self entriesAt: aResolvable do: aBlock]
		on: DirectoryDoesNotExist, FileDoesNotExist 
		do: [ :error | absentBlock value ].
! !
	| path |
	path := self resolve: aResolvable.
	store
		directoryAt: path
		directoryNodesDo: [ :node | 
			aBlock value: (store basenameFromEntry: node) ]! !
	| path |
	path := self resolve: aResolvable.
	store
		nodesAt: path
		do: [ :node | 
			aBlock value: (store basenameFromNode: node) ]! !
	| path |
	path := self resolve: aResolvable.
	store
		directoryAt: path
		fileNodesDo: [ :entry | 
			aBlock value: (store basenameFromEntry: entry) ]! !

	| path |
	path := self resolve: aResolvable.
	^store nodesAt: path do:
		[ :node | aBlock value: (store entryFromNode: node path: path for: self) ].
! !
	"Return whether the receiver is a directory"
	
	^ self attributes isDirectory! !

	^self attributes accessTime! !
	"Return the modification date and time of the entry receiver in seconds."
	^ self modificationTime asSeconds! !
	^self attributes posixPermissions! !
	^self attributes permissions! !

	^self attributes inode! !
	"Return the modification date and time of the entry receiver."

	^  self attributes modificationTime! !

	^attributes ifNil: [ attributes := reference attributes ].! !

	^self attributes isCharacter! !

	^self attributes gid! !
	"Answer the receivers creation time.
	Note that the interpretation varies by platform."
	
	^ self attributes creationTime! !
	^self attributes uid! !

	reference := fileReference! !

	^self attributes deviceId! !

	^self attributes isBlock! !
	"Returns the receiver size"
	^ self attributes fileSize! !
	"Return the creation date and time of the entry receiver in seconds."
		
	^ self creationTime asSeconds! !
	^self attributes targetFile! !

	attributes := fileAttributes! !
	^self attributes isWritable! !
	^self attributes numberOfHardLinks! !

	^self attributes isFIFO! !
	^self attributes isSymlink! !

	^self attributes isRegular! !

	^self attributes isReadable! !

	^self attributes isExecutable! !
	"Return whether the receiver is a file"
	
	^ self attributes isFile! !

	^self attributes isSocket! !

	^self new reference: (FileReference fileSystem: aFilesystem path: aPath)! !

	^self new
		reference: (FileReference fileSystem: aFilesystem path: aPath);
		attributes: attributes.! !
	^ self 
		directoryAt: aPath 
		ifAbsent: absentBlock 
		nodesDo: [ :node |
			(self basicIsDirectory: node) 
				ifFalse: [ aBlock value: node ]].! !
	^ self 
		nodesAt: aPath 
		do: [ :node |
			(self basicIsDirectory: node) 
				ifTrue: [ aBlock value: node ]].! !
	^ self 
		directoryAt: aPath 
		ifAbsent: absentBlock 
		nodesDo: [ :node |
			(self basicIsDirectory: node) 
				ifTrue: [ aBlock value: node ]].! !
	self 
		deprecated: 'Use FileAttributes instead'
		on: 	'30 Apr 2017' 
		in: 'Pharo-7.0-hopefully-'.
	^ self 
		nodeAt: aPath 
		ifPresent: [ :node| node ]
		ifAbsent: [ NotFound signalFor: aPath in: self ]! !
	^ self 
		nodesAt: aPath 
		do: [ :node |
			(self basicIsDirectory: node) 
				ifFalse: [ aBlock value: node ]].! !
	"Used to extract the basename from the low-level representation (node / entry) from the store."
	self subclassResponsibility! !
	"Used to extract the basename from the low-level representation (node / entry) from the store."
	self 
		deprecated: 'Use basicCreationTimeOf: instead'
		on: 	'3 May 2016' 
		in: 'Pharo-5.0-50746-'.
	self subclassResponsibility! !
	^self subclassResponsibility
	! !

	^[ self nodesAt: aPath do: aBlock ]
		on: DirectoryDoesNotExist, FileDoesNotExist
		do: [ absentBlock value ].
! !
	| entryPath entry |
	entryPath := path / (self basenameFromNode: node).
	entry := FileSystemDirectoryEntry
		fileSystem: aFileSystem
		path: entryPath.
	entry attributes: (self attributesClass store: self path: entryPath node: node).
	^entry! !
	"Used to decide whether the low-level representation (node / entry) from the store is 
	a writable file or can be changed."
	self 
		deprecated: 'Use FileAttributes instead'
		on: 	'30 Apr 2017' 
		in: 'Pharo-7.0-hopefully-'.
	self subclassResponsibility ! !
	"Used to decide whether the low-level representation (node / entry) from the store is a readable
	file or a directory whose contents can be listed."
	self 
		deprecated: 'Use FileAttributes instead'
		on: 	'30 Apr 2017' 
		in: 'Pharo-7.0-hopefully-'.
	self subclassResponsibility ! !
	self 
		deprecated: 'Use FileAttributes instead'
		on: 	'30 Apr 2017' 
		in: 'Pharo-7.0-hopefully-'.
	^self subclassResponsibility 
	! !
	"Used to decide whether the low-level representation (node / entry) from the store is a file.
	This private message should only be called form within the store."
	self 
		deprecated: 'Use FileAttributes instead'
		on: 	'30 Apr 2017' 
		in: 'Pharo-7.0-hopefully-'.
	self subclassResponsibility ! !
	"Used to get the size of the low-level representation (node / entry) "
	self 
		deprecated: 'Use FileAttributes instead'
		on: 	'30 Apr 2017' 
		in: 'Pharo-7.0-hopefully-'.
	self subclassResponsibility ! !
	"Used to decide whether the low-level representation (node / entry) from the store is a readable
	file or a directory whose contents can be listed."
	self 
		deprecated: 'Use FileAttributes instead'
		on: 	'30 Apr 2017' 
		in: 'Pharo-7.0-hopefully-'.
	self subclassResponsibility ! !
	"Used to get the posix permissions from a low-level filesystem entry / node"
	self 
		deprecated: 'Use FileAttributes instead'
		on: 	'30 Apr 2017' 
		in: 'Pharo-7.0-hopefully-'.
	self subclassResponsibility ! !
	"Return the size of the File described by aPath"
	^ (self attributesOf: aPath) fileSize! !
	"Answer the FileAttributes of the supplied path in the receiver"

	^[ presentBlock value: (self attributesClass store: self path: aPath) ]
		on: FileDoesNotExist 
		do: [ :error | absentBlock value ]! !
	"Return the uid of the File described by aPath"
	^ (self attributesOf: aPath) uid.
	! !
	^(self attributesOf: aPath) permissions
	! !
	"Return the gid of the File described by aPath"
	^ (self attributesOf: aPath) gid.
! !
	"Return the target file of the File described by aPath.  For a regular file, this is itself, for a symbolic link, it is the file pointed to by the symbolic link"
	^ (self attributesOf: aPath) targetFile.! !
	"Return the number of hard links to the File described by aPath"
	^ (self attributesOf: aPath) numberOfHardLinks.! !
	"Returns the last date of modification of the File described by aPath"
	^(self attributesOf: aPath) modificationTime.! !
	"Return the date of last access of the File described by aPath"
	^ (self attributesOf: aPath) accessTime.
	! !
	"Return the date of creation of the File described by aPath.
	Unlike most attribute methods in the file system store, retrieve the attributes object for aPath to minimise the number of primitive calls as creationTime often needs to get change time as well."
	^ (self attributesOf: aPath) creationTime.
	! !
	"Return the device id of the File described by aPath"
	^ (self attributesOf: aPath) deviceId.! !
	"open the file at the given path and return an identifier"
	self subclassResponsibility! !
	"Return the inode number of the File described by aPath"
	^ (self attributesOf: aPath) inode.
! !
	self 
		deprecated: 'Use FileAttributes instead'
		on: 	'30 Apr 2017' 
		in: 'Pharo-7.0-hopefully-'.
	^self subclassResponsibility ! !
	self 
		deprecated: 'Use FileAttributes instead'
		on: 	'30 Apr 2017' 
		in: 'Pharo-7.0-hopefully-'.
	self subclassResponsibility ! !

	| attributes |
	
	self 
		deprecated: 'FileSystemDirectoryEntry class>>fileSystem:path:'
		on: 	'30 Apr 2017' 
		in: 'Pharo-7.0-hopefully-'.
	^ FileSystemDirectoryEntry
		fileSystem: aFilesystem
		path: aPath! !
	"Used to decide whether the low-level representation (node) from the store is a directory.
	This private message should only be called form within the store."
	self subclassResponsibility ! !

	self checkExists: aPath.
	^ FileSystemDirectoryEntry
		fileSystem: aFilesystem
		path: aPath! !

	^self subclassResponsibility! !
	"Return a boolean indicating whether the File described by aPath is character based"
	^ (self attributesOf: aPath) isCharacter.
	! !
	"Return a boolean indicating whether the File described by aPath is FIFO"
	^ (self attributesOf: aPath) isFIFO.
	! !
	^(self attributesOf: aPath) isDirectory.
	! !

	(self exists: aPath) ifFalse:
		[ FileDoesNotExist signalWith: aPath ].! !
	^(self attributesOf: aPath) isSymlink! !
	"Return a boolean indicating whether the File described by aPath is a socket"
	^ (self attributesOf: aPath) isSocket.
	! !
	^(self attributesOf: aPath) isExecutable! !
	^(self attributesOf: aPath) isWritable! !
	^(self attributesOf: aPath) isReadable! !
	"Return a boolean indicating whether the File described by aPath is a block device"
	^ (self attributesOf: aPath) isBlock.
	! !
	"Return a boolean indicating whether the File described by aPath is a file (not a directory)"
	^ (self attributesOf: aPath) isFile.
	! !
	"Return a boolean indicating whether the File described by aPath is a regular file"
	^ (self attributesOf: aPath) isRegular.
	! !
	"Answer the primitives for file attribute and directory enumerating operations."

	^FileAttributePrimitives! !
	"Return the gid of the File described by aPath"
	^FileAttributePrimitives fileAttribute: (self stringFromPath: aPath) number: 7! !
	"Return the date of last access of the File described by aPath"
	^DateAndTime fromUnixTime:
		(FileAttributePrimitives fileAttribute: (self stringFromPath: aPath) number: 10).! !
	"Answer a boolean indicating whether the supplied path is a Directory file"
	^FileAttributePrimitives isDirectory: (self stringFromPath: aPath)! !
	"Return the date of last access of the File described by aPath"
	^DateAndTime fromUnixTime:
		(FileAttributePrimitives fileAttribute: (self stringFromPath: aPath) number: 9).! !
	"Return the size of the File described by aPath"
	^FileAttributePrimitives fileAttribute: (self stringFromPath: aPath) number: 8.! !
	"Return the device id of the File described by aPath"
	^ FileAttributePrimitives fileAttribute: (self stringFromPath: aPath) number: 4! !

	^DateAndTime fromUnixTime:
		(FileAttributePrimitives fileAttribute: (self stringFromPath: aPath) number: 11).! !
	"Return the inode number of the File described by aPath"
	^FileAttributePrimitives fileAttribute: (self stringFromPath: aPath) number: 3! !
	^FileAttributePrimitives permissions: (self stringFromPath: aPath)! !
	"Answer a boolean indicating whether the supplied path is a FIFO file"
	^FileAttributePrimitives isFIFO: (self stringFromPath: aPath)! !
	"Answer a boolean indicating whether the supplied path is a Socket file"
	^FileAttributePrimitives isSocket: (self stringFromPath: aPath)! !
	"Answer a boolean indicating whether the supplied path is a Regular file"
	^FileAttributePrimitives isRegular: (self stringFromPath: aPath)! !
	"Answer a boolean indicating whether the supplied path is a Character file"
	^FileAttributePrimitives isCharacter: (self stringFromPath: aPath)! !
	"Answer a boolean indicating whether the supplied path is a Symlink file"
	^FileAttributePrimitives isSymlink: (self stringFromPath: aPath)! !

	aPath isRoot ifTrue: [ ^true ].
	^self fileAttributePrimitives exists: (self stringFromPath: aPath).! !
	"Answer a boolean indicating whether the supplied path is a Block file"
	^FileAttributePrimitives isBlock: (self stringFromPath: aPath)! !
	self 
		deprecated: 'Use FileAttributes instead'
		on: 	'30 Apr 2017' 
		in: 'Pharo-7.0-hopefully-'.
	^ (anEntry at: 4) not! !
	
	| entry |

	self 
		deprecated: 'Use FileAttributes instead'
		on: 	'30 Apr 2017' 
		in: 'Pharo-7.0-hopefully-'.

	aPath isRoot ifTrue: [ ^ presentBlock value: self rootNode ].
	
	entry := self basicEntryAt: aPath.
	
	^ entry == #badDirectoryPath 
		ifTrue: absentBlock
		ifFalse: [
			entry at: 1 put: aPath basename.
			presentBlock value: entry ].! !
	" the entry contains the seconds since the squeak epoch in local time"
	self 
		deprecated: 'Use FileAttributes instead'
		on: 	'30 Apr 2017' 
		in: 'Pharo-7.0-hopefully-'.
	^  (DateAndTime fromSeconds: (anEntry at: 2) offset: 0) translateTo: DateAndTime localOffset! !

	| pathString dirPointer entryData fileName attributes targetName |
	
	pathString := Primitives encode: (self stringFromPath: aPath).
	(self attributesOf: aPath) isDirectory ifFalse:
		[ ^self signalDirectoryDoesNotExist: aPath ].

	dirPointer := FileAttributePrimitives primOpendir: pathString.
	dirPointer ifNil: [ ^nil ].
	[
		entryData := FileAttributePrimitives primReaddir: dirPointer.
		[ entryData notNil ] whileTrue:
			[
				fileName := entryData first.
				entryData at: 1 put: (FileAttributePrimitives decode: fileName).
				attributes := entryData at: 2.
				attributes ifNotNil: 
					[ targetName := attributes at: 1.
					targetName ifNotNil: [ attributes at: 1 put: (Primitives decode: targetName) ] ].
				aBlock value: entryData.
				entryData := FileAttributePrimitives primReaddir: dirPointer.
			]
	] ensure: [ FileAttributePrimitives primClosedir: dirPointer ].! !
	"Answer the stat attributes array from the supplied node"
	
	^aNode at: 2! !
	" the entry contains the seconds since the squeak epoch in local time"
	self 
		deprecated: 'Use FileAttributes instead'
		on: 	'30 Apr 2017' 
		in: 'Pharo-7.0-hopefully-'.
	^ (DateAndTime fromSeconds: (anEntry at: 3) offset: 0) translateTo: DateAndTime localOffset! !
	self 
		deprecated: 'Use FileAttributes instead'
		on: 	'30 Apr 2017' 
		in: 'Pharo-7.0-hopefully-'.
	^(anEntry size >= 7)
		ifTrue: [ anEntry at: 7 ]
		ifFalse: [ false ]! !
	^ aNode at: 1! !
	"Answer the statBuf.st_mode for the supplied path"
	
	^FileAttributePrimitives fileAttribute: (self stringFromPath: aPath) number: 2! !
	self 
		deprecated: 'Use FileAttributes instead'
		on: 	'30 Apr 2017' 
		in: 'Pharo-7.0-hopefully-'.
	^ (anEntry size >= 6)
		ifTrue: [ anEntry at: 6 ]
		ifFalse: [ nil ].! !
	| mask statAttributes |

	statAttributes := aNode at: 2.
	statAttributes ifNil: [ ^false ].
	mask := statAttributes at: 2.
	^(mask bitAnd: self fileAttributePrimitives s_IFMT) = self fileAttributePrimitives s_IFDIR! !
	self 
		deprecated: 'Use FileAttributes instead'
		on: 	'30 Apr 2017' 
		in: 'Pharo-7.0-hopefully-'.
	^ (anEntry at: 5)! !
	| encodedPathString index entry pathString |

	self 
		deprecated: 'Use #nodesAt:do: instead'
		on: 	'30 Apr 2017' 
		in: 'Pharo-7.0-hopefully-'.
	index := 1.
	pathString := self stringFromPath: aPath.
	encodedPathString := Primitives encode: pathString.
	entry := Primitives lookupEntryIn: encodedPathString index: index.
	entry = #badDirectoryPath ifTrue: [ ^ self signalDirectoryDoesNotExist: aPath ].
	
	[ entry isNil ]
		whileFalse: [ 
			entry at: 1 put: (Primitives decode: entry first).
			aBlock value: entry.
			index := index + 1.
			entry := Primitives lookupEntryIn: encodedPathString index: index ].! !
	| pathString encodedPathString |
	
	((self exists: path) or: [ self isSymlink: path ])
		ifFalse: [ ^ FileDoesNotExist signalWith: path ].
		
	pathString := self stringFromPath: path.
	encodedPathString := Primitives encode: pathString.
	
	(self isDirectory: path)
		ifTrue: [ Primitives deleteDirectory: encodedPathString ]
		ifFalse: [ 
			StandardFileStream
				retryWithGC: [ Primitives deleteFile: encodedPathString ]
				until: [ :result | result notNil ]
				forFileNamed: pathString ]! !

	^DiskFileAttributes ! !
	Primitives := FilePluginPrims new.
	FileAttributePrimitives class reset.
	DiskFileAttributes initializeWithPrimitives: FileAttributePrimitives.! !
	"The concrete class used to access file attributes in the receiver"

	^MemoryFileAttributes! !
	self 
		deprecated: 'Use FileAttributes instead'
		on: 	'30 Apr 2017' 
		in: 'Pharo-7.0-hopefully-'.
	entry fileEntriesDo: aBlock! !
	self 
		deprecated: 'Use FileAttributes instead'
		on: 	'30 Apr 2017' 
		in: 'Pharo-7.0-hopefully-'.
	^ aMemoryFileSystemEntry isFile! !

	^(self nodeAt: aPath) fileEntriesDo:
		[ :each | aBlock value: each ]. ! !
	| current |
	current := self root.
	aPath do: [ :segment | 
		current isDirectory
			ifTrue: [ current := current fileEntryAt: segment ifAbsent: [ ^ absentBlock value ]]
			ifFalse: [ ^ absentBlock value ]].
	^ presentBlock value: current! !
	"Returns the creation date of aMemoryFileSystemEntry"
	self 
		deprecated: 'Use FileAttributes instead'
		on: 	'30 Apr 2017' 
		in: 'Pharo-7.0-hopefully-'.
	^ aMemoryFileSystemEntry creationTime! !
	"Return the basic modification time of aMemoryFileSystemEntry"
	self 
		deprecated: 'Use FileAttributes instead'
		on: 	'30 Apr 2017' 
		in: 'Pharo-7.0-hopefully-'.
	^ aMemoryFileSystemEntry modificationTime! !
	"Return the basic size of aMemoryFileSystemEntry"
	self 
		deprecated: 'Use FileAttributes instead'
		on: 	'30 Apr 2017' 
		in: 'Pharo-7.0-hopefully-'.
	^ aMemoryFileSystemEntry fileSize! !
	| current |
	current := self root.
	aPath do: [ :segment | 
		current isDirectory
			ifTrue: [ current := current fileEntryAt: segment ifAbsent: [ ^ FileDoesNotExist signalWith: aPath ]]
			ifFalse: [ ^ FileDoesNotExist signalWith: aPath ]].
	^ current! !
	self 
		deprecated: 'Use FileAttributes instead'
		on: 	'30 Apr 2017' 
		in: 'Pharo-7.0-hopefully-'.
	^false! !
	self 
		deprecated: 'Use FileAttributes instead'
		on: 	'30 Apr 2017' 
		in: 'Pharo-7.0-hopefully-'.
	^ 8r777! !

	^self nodeAt: aPath
		ifPresent: [ :node | true ]
		ifAbsent: [ false ]! !
	^ aMemoryFileSystemEntry basename! !

	FileAttributePrimitives := UnixFileAttributesPluginPrims new.
	super useFilePlugin.
! !

	| mask drive node |

	aPath isRoot ifFalse: [ ^super nodesAt: aPath do: aBlock ].

	mask := FileAttributePrimitives primLogicalDrives.
	0 to: 25 do: [ :i |
		(mask bitAnd: (1 bitShift: i)) ~= 0 ifTrue: [ 
			drive := String with: (Character value: ($A asciiValue + i)) with: $:.
			node := Array
				with: drive
				with: drive asFileReference attributes statAttributes.
			aBlock value: node.
			]
		].! !

	FileAttributePrimitives := WindowsFileAttributesPluginPrims new.
	super useFilePlugin.
! !