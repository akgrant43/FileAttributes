'From Pharo7.0SNAPSHOT of 22 July 2017 [Latest update: #0] on 27 July 2017 at 7:40:34.182703 pm'!Object subclass: #FileSystemDirectoryEntry	instanceVariableNames: 'reference creation modification isDirectory isSymlink size posixPermissions attributes '	classVariableNames: ''	poolDictionaries: ''	category: 'FileSystem-Core-Public'!FileSystemStore subclass: #DiskStore	instanceVariableNames: 'maxFileNameLength '	classVariableNames: 'CurrentFS DefaultWorkingDirectory Primitives FileAttributePrimitives '	poolDictionaries: ''	category: 'FileSystem-Disk'!!AbstractFileReference methodsFor: 'accessing' stamp: 'AlistairGrant 5/18/2017 09:45'!gid
	^ self resolve gid! !!AbstractFileReference methodsFor: 'accessing' stamp: 'AlistairGrant 5/18/2017 09:46'!numberOfHardLinks
	^ self resolve numberOfHardLinks! !!AbstractFileReference methodsFor: 'accessing' stamp: 'AlistairGrant 5/11/2017 21:58'!accessTime
	^ self resolve accessTime ! !!AbstractFileReference methodsFor: 'accessing' stamp: 'AlistairGrant 5/18/2017 09:47'!targetFile
	^ self resolve targetFile! !!AbstractFileReference methodsFor: 'accessing' stamp: 'AlistairGrant 5/18/2017 09:41'!isBlock
	^ self resolve isBlock ! !!AbstractFileReference methodsFor: 'accessing' stamp: 'AlistairGrant 5/18/2017 09:47'!uid
	^ self resolve uid! !!AbstractFileReference methodsFor: 'accessing' stamp: 'AlistairGrant 5/18/2017 09:45'!inode
	^ self resolve inode! !!AbstractFileReference methodsFor: 'accessing' stamp: 'AlistairGrant 5/18/2017 09:27'!changeTime
	^ self resolve changeTime ! !!AbstractFileReference methodsFor: 'accessing' stamp: 'AlistairGrant 5/18/2017 09:41'!isCharacter
	^ self resolve isCharacter! !!AbstractFileReference methodsFor: 'testing' stamp: 'AlistairGrant 5/18/2017 09:46'!isSocket
	^ self resolve isSocket! !!AbstractFileReference methodsFor: 'testing' stamp: 'AlistairGrant 5/18/2017 09:42'!isFIFO
	^ self resolve isFIFO! !!AbstractFileReference methodsFor: 'testing' stamp: 'AlistairGrant 5/18/2017 09:44'!deviceId
	^ self resolve deviceId! !!AbstractFileReference methodsFor: 'testing' stamp: 'AlistairGrant 5/18/2017 09:46'!isSymlink
	^ self resolve isSymlink! !!AbstractFileReference methodsFor: 'testing' stamp: 'AlistairGrant 5/18/2017 09:42'!isExecutable
	^ self resolve isExecutable! !!FileReference methodsFor: 'testing' stamp: 'AlistairGrant 5/1/2017 09:03'!isBlock
	"Return a boolean indicating whether the File described by aPath is a block device"
	^ filesystem isBlock: path! !!FileReference methodsFor: 'testing' stamp: 'AlistairGrant 5/1/2017 09:06'!isSocket
	"Return a boolean indicating whether the File described by aPath is a socket"
	^ filesystem isSocket: path! !!FileReference methodsFor: 'testing' stamp: 'AlistairGrant 5/1/2017 09:05'!isFIFO
	"Return a boolean indicating whether the File described by aPath is FIFO"
	^ filesystem isFIFO: path! !!FileReference methodsFor: 'testing' stamp: 'AlistairGrant 5/1/2017 09:20'!targetFile
	"Return the target file of the File described by aPath.  For a regular file, this is itself, for a symbolic link, it is the file pointed to by the symbolic link"
	^ self class fileSystem: filesystem path: (filesystem targetFile: path)! !!FileReference methodsFor: 'testing' stamp: 'AlistairGrant 5/1/2017 09:17'!numberOfHardLinks
	"Return the number of hard links to the File described by aPath"
	^ filesystem numberOfHardLinks: path! !!FileReference methodsFor: 'testing' stamp: 'AlistairGrant 5/1/2017 09:05'!isRegular
	"Return a boolean indicating whether the File described by aPath is a regular file"
	^ filesystem isRegular: path! !!FileReference methodsFor: 'testing' stamp: 'AlistairGrant 6/4/2017 16:41'!isSymlink
	"Answer a boolean indicating whether the receiver is a symlink"
	^ filesystem isSymlink: path! !!FileReference methodsFor: 'testing' stamp: 'AlistairGrant 4/30/2017 23:09'!isExecutable
	^ filesystem isExecutable: path! !!FileReference methodsFor: 'testing' stamp: 'AlistairGrant 5/1/2017 09:04'!isCharacter
	"Return a boolean indicating whether the File described by aPath is character based"
	^ filesystem isCharacter: path! !!FileReference methodsFor: 'accessing' stamp: 'AlistairGrant 4/30/2017 23:10'!accessTime 
	^ filesystem accessTimeOf: self path! !!FileReference methodsFor: 'accessing' stamp: 'AlistairGrant 5/18/2017 09:28'!changeTime 
	^ filesystem changeTimeOf: self path! !!FileReference methodsFor: 'accessing' stamp: 'AlistairGrant 5/18/2017 12:35'!gid
	^ filesystem gidOf: self path! !!FileReference methodsFor: 'accessing' stamp: 'AlistairGrant 5/11/2017 21:59'!creationTime
	"Answer the receivers creation time.
	Note that the interpretation varies by platform."
	^ filesystem creationTimeOf: self path! !!FileReference methodsFor: 'accessing' stamp: 'AlistairGrant 5/18/2017 12:36'!inode
	^ filesystem inodeOf: self path! !!FileReference methodsFor: 'accessing' stamp: 'AlistairGrant 5/1/2017 08:41'!uid
	^ filesystem uid: self path! !!FileReference methodsFor: 'accessing' stamp: 'AlistairGrant 4/30/2017 23:14'!deviceId
	^ filesystem deviceIdOf: self path! !!FileSystem methodsFor: 'public-testing' stamp: 'AlistairGrant 5/1/2017 09:01'!targetFile: aResolvable
	"Return the target file of the File described by aPath.  For a regular file, this is itself, for a symbolic link, it is the file pointed to by the symbolic link"

	^ store targetFile: (self resolve: aResolvable)! !!FileSystem methodsFor: 'public-testing' stamp: 'AlistairGrant 4/30/2017 20:21'!isWritable: aResolvable
	"Resolve the argument, and answer true if the there is
	a file that can be written to or directory that can be changed."
	
	^ store isWritable: (self resolve: aResolvable)! !!FileSystem methodsFor: 'public-testing' stamp: 'AlistairGrant 5/1/2017 08:58'!isRegular: aResolvable
	"Return a boolean indicating whether the File described by aPath is a regular file"

	^ store isRegular: (self resolve: aResolvable)! !!FileSystem methodsFor: 'public-testing' stamp: 'AlistairGrant 5/1/2017 08:56'!isCharacter: aResolvable
	"Return a boolean indicating whether the File described by aPath is character based"

	^ store isCharacter: (self resolve: aResolvable)! !!FileSystem methodsFor: 'public-testing' stamp: 'AlistairGrant 5/1/2017 08:55'!isBlock: aResolvable
	"Return a boolean indicating whether the File described by aPath is a block device"

	^ store isBlock: (self resolve: aResolvable)! !!FileSystem methodsFor: 'public-testing' stamp: 'AlistairGrant 5/1/2017 08:57'!isFIFO: aResolvable
	"Return a boolean indicating whether the File described by aPath is FIFO"

	^ store isFIFO: (self resolve: aResolvable)! !!FileSystem methodsFor: 'public-testing' stamp: 'AlistairGrant 4/30/2017 23:09'!isExecutable: aResolvable
	"Resolve the argument, and answer true if the there is
	a file or directory that can be read from."
	
	^ store isExecutable: (self resolve: aResolvable)! !!FileSystem methodsFor: 'public-testing' stamp: 'AlistairGrant 4/30/2017 20:14'!isReadable: aResolvable
	"Resolve the argument, and answer true if the there is
	a file or directory that can be read from."
	
	^ store isReadable: (self resolve: aResolvable)! !!FileSystem methodsFor: 'public-testing' stamp: 'AlistairGrant 5/1/2017 08:57'!isFile: aResolvable
	"Return a boolean indicating whether the File described by aPath is a file (not a directory)"

	^ store isFile: (self resolve: aResolvable)! !!FileSystem methodsFor: 'public-testing' stamp: 'AlistairGrant 5/1/2017 08:59'!numberOfHardLinks: aResolvable
	"Resolve the argument, and answer true if the result refers
	to a directory, false if it refers to a file or doesn't exist."

	^ store numberOfHardLinks: (self resolve: aResolvable)! !!FileSystem methodsFor: 'public-testing' stamp: 'AlistairGrant 5/1/2017 08:58'!isSocket: aResolvable
	"Return a boolean indicating whether the File described by aPath is a regular file"

	^ store isSocket: (self resolve: aResolvable)! !!FileSystem methodsFor: 'public' stamp: 'AlistairGrant 5/18/2017 12:35'!gidOf: aResolvable
	"Returns the group id of aResolvable"

	^ store gidOf: (self resolve: aResolvable)! !!FileSystem methodsFor: 'public' stamp: 'AlistairGrant 5/1/2017 19:58'!entryAt: aResolvable
	"Answer the FileSystemDirectoryEntry for aResolvable"

	^ store entryAt: (self resolve: aResolvable) fileSystem: self.
! !!FileSystem methodsFor: 'public' stamp: 'AlistairGrant 5/18/2017 09:29'!accessTimeOf: aResolvable
	"Returns the access date of aResolvable"

	^ store accessTimeOf: (self resolve: aResolvable)! !!FileSystem methodsFor: 'public' stamp: 'AlistairGrant 5/18/2017 12:36'!inodeOf: aResolvable
	"Returns the inode number of aResolvable"

	^ store inodeOf: (self resolve: aResolvable)! !!FileSystem methodsFor: 'public' stamp: 'AlistairGrant 4/30/2017 23:14'!deviceIdOf: aResolvable
	"Returns the device id of aResolvable"

	^ store deviceIdOf: (self resolve: aResolvable)! !!FileSystem methodsFor: 'public' stamp: 'AlistairGrant 5/1/2017 08:41'!uid: aResolvable
	"Returns the user id of aResolvable"

	^ store uid: (self resolve: aResolvable)! !!FileSystem methodsFor: 'public' stamp: 'AlistairGrant 5/18/2017 09:30'!changeTimeOf: aResolvable
	"Returns the change time of aResolvable"

	^ store changeTimeOf: (self resolve: aResolvable)! !!FileSystem methodsFor: 'public-enumerating' stamp: 'AlistairGrant 5/7/2017 13:10'!filesAt: aResolvable do: aBlock
	| path |
	path := self resolve: aResolvable.
	store
		directoryAt: path
		fileNodesDo: [ :entry | 
			aBlock value: path / (store basenameFromEntry: entry) ]! !!FileSystem methodsFor: 'public-enumerating' stamp: 'AlistairGrant 5/7/2017 13:09'!directoriesAt: aResolvable do: aBlock
	| path |
	path := self resolve: aResolvable.
	store
		directoryAt: path
		directoryNodesDo: [ :entry | 
			aBlock value: path / (store basenameFromEntry: entry) ]! !!FileSystem methodsFor: 'public-enumerating' stamp: 'AlistairGrant 5/7/2017 13:13'!childrenAt: aResolvable do: aBlock
	| path |
	path := self resolve: aResolvable.
	store
		nodesAt: path
		do: [ :node | 
			aBlock value: path / (store basenameFromEntry: node) ]! !!FileSystem methodsFor: 'public-enumerating' stamp: 'AlistairGrant 5/7/2017 18:44'!entriesAt: aResolvable ifAbsent: absentBlock do: aBlock

	^[self entriesAt: aResolvable do: aBlock]
		on: DirectoryDoesNotExist, FileDoesNotExist 
		do: [ :error | absentBlock value ].
! !!FileSystem methodsFor: 'public-enumerating' stamp: 'AlistairGrant 5/7/2017 13:10'!directoryNamesAt: aResolvable do: aBlock
	| path |
	path := self resolve: aResolvable.
	store
		directoryAt: path
		directoryNodesDo: [ :node | 
			aBlock value: (store basenameFromEntry: node) ]! !!FileSystem methodsFor: 'public-enumerating' stamp: 'AlistairGrant 5/7/2017 17:49'!childNamesAt: aResolvable do: aBlock
	| path |
	path := self resolve: aResolvable.
	store
		nodesAt: path
		do: [ :node | 
			aBlock value: (store basenameFromNode: node) ]! !!FileSystem methodsFor: 'public-enumerating' stamp: 'AlistairGrant 5/7/2017 13:10'!fileNamesAt: aResolvable do: aBlock
	| path |
	path := self resolve: aResolvable.
	store
		directoryAt: path
		fileNodesDo: [ :entry | 
			aBlock value: (store basenameFromEntry: entry) ]! !!FileSystem methodsFor: 'public-enumerating' stamp: 'AlistairGrant 5/7/2017 15:04'!entriesAt: aResolvable do: aBlock

	| path |
	path := self resolve: aResolvable.
	^store nodesAt: path do:
		[ :node | aBlock value: (store entryFromNode: node path: path for: self) ].
! !!FileSystemDirectoryEntry methodsFor: 'accessing' stamp: 'AlistairGrant 5/6/2017 18:51'!isDirectory
	"Return whether the receiver is a directory"
	
	^ self attributes isDirectory! !!FileSystemDirectoryEntry methodsFor: 'accessing' stamp: 'AlistairGrant 5/6/2017 18:49'!accessTime

	^self attributes accessTime! !!FileSystemDirectoryEntry methodsFor: 'accessing' stamp: 'AlistairGrant 5/6/2017 18:50'!modificationSeconds
	"Return the modification date and time of the entry receiver in seconds."
	^ self modificationTime asSeconds! !!FileSystemDirectoryEntry methodsFor: 'accessing' stamp: 'AlistairGrant 5/6/2017 18:54'!posixPermissions 
	^self attributes posixPermissions! !!FileSystemDirectoryEntry methodsFor: 'accessing' stamp: 'AlistairGrant 5/6/2017 18:54'!permissions 
	^self attributes permissions! !!FileSystemDirectoryEntry methodsFor: 'accessing' stamp: 'AlistairGrant 5/6/2017 18:51'!inode

	^self attributes inode! !!FileSystemDirectoryEntry methodsFor: 'accessing' stamp: 'AlistairGrant 5/6/2017 18:50'!modificationTime
	"Return the modification date and time of the entry receiver."

	^  self attributes modificationTime! !!FileSystemDirectoryEntry methodsFor: 'accessing' stamp: 'AlistairGrant 5/6/2017 18:48'!attributes

	^attributes ifNil: [ attributes := reference attributes ].! !!FileSystemDirectoryEntry methodsFor: 'accessing' stamp: 'AlistairGrant 5/6/2017 18:51'!isCharacter

	^self attributes isCharacter! !!FileSystemDirectoryEntry methodsFor: 'accessing' stamp: 'AlistairGrant 5/6/2017 18:50'!gid

	^self attributes gid! !!FileSystemDirectoryEntry methodsFor: 'accessing' stamp: 'AlistairGrant 5/11/2017 21:58'!creationTime
	"Answer the receivers creation time.
	Note that the interpretation varies by platform."
	
	^ self attributes creationTime! !!FileSystemDirectoryEntry methodsFor: 'accessing' stamp: 'AlistairGrant 5/6/2017 18:54'!uid 
	^self attributes uid! !!FileSystemDirectoryEntry methodsFor: 'accessing' stamp: 'AlistairGrant 5/6/2017 18:58'!reference: fileReference

	reference := fileReference! !!FileSystemDirectoryEntry methodsFor: 'accessing' stamp: 'AlistairGrant 5/6/2017 18:50'!deviceId

	^self attributes deviceId! !!FileSystemDirectoryEntry methodsFor: 'accessing' stamp: 'AlistairGrant 5/6/2017 18:51'!isBlock

	^self attributes isBlock! !!FileSystemDirectoryEntry methodsFor: 'accessing' stamp: 'AlistairGrant 5/6/2017 18:50'!size
	"Returns the receiver size"
	^ self attributes fileSize! !!FileSystemDirectoryEntry methodsFor: 'accessing' stamp: 'AlistairGrant 5/6/2017 18:49'!creationSeconds
	"Return the creation date and time of the entry receiver in seconds."
		
	^ self creationTime asSeconds! !!FileSystemDirectoryEntry methodsFor: 'accessing' stamp: 'AlistairGrant 5/6/2017 18:54'!targetFile 
	^self attributes targetFile! !!FileSystemDirectoryEntry methodsFor: 'accessing' stamp: 'AlistairGrant 5/6/2017 18:56'!attributes: fileAttributes

	attributes := fileAttributes! !!FileSystemDirectoryEntry methodsFor: 'testing' stamp: 'AlistairGrant 5/6/2017 18:53'!isWritable
	^self attributes isWritable! !!FileSystemDirectoryEntry methodsFor: 'testing' stamp: 'AlistairGrant 5/6/2017 18:54'!numberOfHardLinks
	^self attributes numberOfHardLinks! !!FileSystemDirectoryEntry methodsFor: 'testing' stamp: 'AlistairGrant 5/6/2017 18:52'!isFIFO

	^self attributes isFIFO! !!FileSystemDirectoryEntry methodsFor: 'testing' stamp: 'AlistairGrant 5/6/2017 18:53'!isSymlink 
	^self attributes isSymlink! !!FileSystemDirectoryEntry methodsFor: 'testing' stamp: 'AlistairGrant 5/6/2017 18:53'!isRegular

	^self attributes isRegular! !!FileSystemDirectoryEntry methodsFor: 'testing' stamp: 'AlistairGrant 5/6/2017 18:52'!isReadable

	^self attributes isReadable! !!FileSystemDirectoryEntry methodsFor: 'testing' stamp: 'AlistairGrant 5/6/2017 18:52'!isExecutable

	^self attributes isExecutable! !!FileSystemDirectoryEntry methodsFor: 'testing' stamp: 'AlistairGrant 5/6/2017 18:52'!isFile
	"Return whether the receiver is a file"
	
	^ self attributes isFile! !!FileSystemDirectoryEntry methodsFor: 'testing' stamp: 'AlistairGrant 5/6/2017 18:53'!isSocket

	^self attributes isSocket! !!FileSystemDirectoryEntry class methodsFor: 'instance creation' stamp: 'AlistairGrant 5/6/2017 19:00'!fileSystem: aFilesystem path: aPath

	^self new reference: (FileReference fileSystem: aFilesystem path: aPath)! !!FileSystemDirectoryEntry class methodsFor: 'instance creation' stamp: 'AlistairGrant 5/7/2017 08:33'!fileSystem: aFilesystem path: aPath attributes: attributes

	^self new
		reference: (FileReference fileSystem: aFilesystem path: aPath);
		attributes: attributes.! !!FileSystemStore methodsFor: 'public' stamp: 'AlistairGrant 5/7/2017 13:02'!directoryAt: aPath ifAbsent: absentBlock fileNodesDo: aBlock
	^ self 
		directoryAt: aPath 
		ifAbsent: absentBlock 
		nodesDo: [ :node |
			(self basicIsDirectory: node) 
				ifFalse: [ aBlock value: node ]].! !!FileSystemStore methodsFor: 'public' stamp: 'AlistairGrant 5/7/2017 13:09'!directoryAt: aPath  directoryNodesDo: aBlock
	^ self 
		nodesAt: aPath 
		do: [ :node |
			(self basicIsDirectory: node) 
				ifTrue: [ aBlock value: node ]].! !!FileSystemStore methodsFor: 'public' stamp: 'AlistairGrant 5/7/2017 13:02'!directoryAt: aPath ifAbsent: absentBlock directoryNodesDo: aBlock
	^ self 
		directoryAt: aPath 
		ifAbsent: absentBlock 
		nodesDo: [ :node |
			(self basicIsDirectory: node) 
				ifTrue: [ aBlock value: node ]].! !!FileSystemStore methodsFor: 'public' stamp: 'AlistairGrant 5/7/2017 12:46'!nodeAt: aPath
	self 
		deprecated: 'Use FileAttributes instead'
		on: 	'30 Apr 2017' 
		in: 'Pharo-7.0-hopefully-'.
	^ self 
		nodeAt: aPath 
		ifPresent: [ :node| node ]
		ifAbsent: [ NotFound signalFor: aPath in: self ]! !!FileSystemStore methodsFor: 'public' stamp: 'AlistairGrant 5/7/2017 13:08'!directoryAt: aPath fileNodesDo: aBlock
	^ self 
		nodesAt: aPath 
		do: [ :node |
			(self basicIsDirectory: node) 
				ifFalse: [ aBlock value: node ]].! !!FileSystemStore methodsFor: 'public' stamp: 'AlistairGrant 5/7/2017 10:53'!basenameFromNode: aNode
	"Used to extract the basename from the low-level representation (node / entry) from the store."
	self subclassResponsibility! !!FileSystemStore methodsFor: 'public' stamp: 'AlistairGrant 5/7/2017 13:03'!basenameFromEntry: aNode
	"Used to extract the basename from the low-level representation (node / entry) from the store."
	self 
		deprecated: 'Use basicCreationTimeOf: instead'
		on: 	'3 May 2016' 
		in: 'Pharo-5.0-50746-'.
	self subclassResponsibility! !!FileSystemStore methodsFor: 'public' stamp: 'AlistairGrant 4/30/2017 12:26'!exists: aPath
	^self subclassResponsibility
	! !!FileSystemStore methodsFor: 'public' stamp: 'AlistairGrant 5/7/2017 18:44'!directoryAt: aPath ifAbsent: absentBlock nodesDo: aBlock

	^[ self nodesAt: aPath do: aBlock ]
		on: DirectoryDoesNotExist, FileDoesNotExist
		do: [ absentBlock value ].
! !!FileSystemStore methodsFor: 'public' stamp: 'AlistairGrant 5/8/2017 23:16'!entryFromNode: node path: path for: aFileSystem
	| entryPath entry |
	entryPath := path / (self basenameFromNode: node).
	entry := FileSystemDirectoryEntry
		fileSystem: aFileSystem
		path: entryPath.
	entry attributes: (self attributesClass store: self path: entryPath node: node).
	^entry! !!FileSystemStore methodsFor: 'deprecated' stamp: 'AlistairGrant 4/30/2017 12:41'!basicIsWritable: aNode
	"Used to decide whether the low-level representation (node / entry) from the store is 
	a writable file or can be changed."
	self 
		deprecated: 'Use FileAttributes instead'
		on: 	'30 Apr 2017' 
		in: 'Pharo-7.0-hopefully-'.
	self subclassResponsibility ! !!FileSystemStore methodsFor: 'deprecated' stamp: 'AlistairGrant 4/30/2017 12:41'!basicModificationTime: aNode
	"Used to decide whether the low-level representation (node / entry) from the store is a readable
	file or a directory whose contents can be listed."
	self 
		deprecated: 'Use FileAttributes instead'
		on: 	'30 Apr 2017' 
		in: 'Pharo-7.0-hopefully-'.
	self subclassResponsibility ! !!FileSystemStore methodsFor: 'deprecated' stamp: 'AlistairGrant 4/30/2017 12:41'!basicIsSymlink: aNode
	self 
		deprecated: 'Use FileAttributes instead'
		on: 	'30 Apr 2017' 
		in: 'Pharo-7.0-hopefully-'.
	^self subclassResponsibility 
	! !!FileSystemStore methodsFor: 'deprecated' stamp: 'AlistairGrant 4/30/2017 12:41'!basicIsFile: aNode
	"Used to decide whether the low-level representation (node / entry) from the store is a file.
	This private message should only be called form within the store."
	self 
		deprecated: 'Use FileAttributes instead'
		on: 	'30 Apr 2017' 
		in: 'Pharo-7.0-hopefully-'.
	self subclassResponsibility ! !!FileSystemStore methodsFor: 'deprecated' stamp: 'AlistairGrant 4/30/2017 12:41'!basicSizeOf: aNode
	"Used to get the size of the low-level representation (node / entry) "
	self 
		deprecated: 'Use FileAttributes instead'
		on: 	'30 Apr 2017' 
		in: 'Pharo-7.0-hopefully-'.
	self subclassResponsibility ! !!FileSystemStore methodsFor: 'deprecated' stamp: 'AlistairGrant 4/30/2017 12:40'!basicCreationTimeOf: aNode
	"Used to decide whether the low-level representation (node / entry) from the store is a readable
	file or a directory whose contents can be listed."
	self 
		deprecated: 'Use FileAttributes instead'
		on: 	'30 Apr 2017' 
		in: 'Pharo-7.0-hopefully-'.
	self subclassResponsibility ! !!FileSystemStore methodsFor: 'deprecated' stamp: 'AlistairGrant 4/30/2017 12:41'!basicPosixPermissions: aNode
	"Used to get the posix permissions from a low-level filesystem entry / node"
	self 
		deprecated: 'Use FileAttributes instead'
		on: 	'30 Apr 2017' 
		in: 'Pharo-7.0-hopefully-'.
	self subclassResponsibility ! !!FileSystemStore methodsFor: 'accessing' stamp: 'AlistairGrant 4/30/2017 12:38'!sizeOf: aPath
	"Return the size of the File described by aPath"
	^ (self attributesOf: aPath) fileSize! !!FileSystemStore methodsFor: 'accessing' stamp: 'AlistairGrant 4/30/2017 10:20'!attributesOf: aPath ifPresent: presentBlock ifAbsent: absentBlock
	"Answer the FileAttributes of the supplied path in the receiver"

	^[ presentBlock value: (self attributesClass store: self path: aPath) ]
		on: FileDoesNotExist 
		do: [ :error | absentBlock value ]! !!FileSystemStore methodsFor: 'accessing' stamp: 'AlistairGrant 5/1/2017 08:41'!uid: aPath
	"Return the uid of the File described by aPath"
	^ (self attributesOf: aPath) uid.
	! !!FileSystemStore methodsFor: 'accessing' stamp: 'AlistairGrant 4/30/2017 12:38'!permissions: aPath
	^(self attributesOf: aPath) permissions
	! !!FileSystemStore methodsFor: 'accessing' stamp: 'AlistairGrant 5/18/2017 12:35'!gidOf: aPath
	"Return the gid of the File described by aPath"
	^ (self attributesOf: aPath) gid.
! !!FileSystemStore methodsFor: 'accessing' stamp: 'AlistairGrant 5/1/2017 09:18'!targetFile: aPath
	"Return the target file of the File described by aPath.  For a regular file, this is itself, for a symbolic link, it is the file pointed to by the symbolic link"
	^ (self attributesOf: aPath) targetFile.! !!FileSystemStore methodsFor: 'accessing' stamp: 'AlistairGrant 5/1/2017 08:53'!numberOfHardLinks: aPath
	"Return the number of hard links to the File described by aPath"
	^ (self attributesOf: aPath) numberOfHardLinks.! !!FileSystemStore methodsFor: 'accessing' stamp: 'AlistairGrant 4/30/2017 12:37'!modificationTimeOf: aPath
	"Returns the last date of modification of the File described by aPath"
	^(self attributesOf: aPath) modificationTime.! !!FileSystemStore methodsFor: 'accessing' stamp: 'AlistairGrant 4/30/2017 23:12'!accessTimeOf: aPath
	"Return the date of last access of the File described by aPath"
	^ (self attributesOf: aPath) accessTime.
	! !!FileSystemStore methodsFor: 'accessing' stamp: 'AlistairGrant 5/18/2017 09:36'!creationTimeOf: aPath
	"Return the date of creation of the File described by aPath.
	Unlike most attribute methods in the file system store, retrieve the attributes object for aPath to minimise the number of primitive calls as creationTime often needs to get change time as well."
	^ (self attributesOf: aPath) creationTime.
	! !!FileSystemStore methodsFor: 'accessing' stamp: 'AlistairGrant 4/30/2017 23:15'!deviceIdOf: aPath
	"Return the device id of the File described by aPath"
	^ (self attributesOf: aPath) deviceId.! !!FileSystemStore methodsFor: 'error signalling' stamp: 'AlistairGrant 4/30/2017 12:43'!basicOpen: aPath writable: aBoolean
	"open the file at the given path and return an identifier"
	self subclassResponsibility! !!FileSystemStore methodsFor: 'abstract' stamp: 'AlistairGrant 5/18/2017 12:36'!inodeOf: aPath
	"Return the inode number of the File described by aPath"
	^ (self attributesOf: aPath) inode.
! !!FileSystemStore methodsFor: 'abstract' stamp: 'AlistairGrant 5/7/2017 12:46'!nodeAt: aPath ifPresent: presentBlock ifAbsent: absentBlock
	self 
		deprecated: 'Use FileAttributes instead'
		on: 	'30 Apr 2017' 
		in: 'Pharo-7.0-hopefully-'.
	^self subclassResponsibility ! !!FileSystemStore methodsFor: 'abstract' stamp: 'AlistairGrant 5/7/2017 12:56'!basicEntry: directoryEntry path: aPath nodesDo: aBlock
	self 
		deprecated: 'Use FileAttributes instead'
		on: 	'30 Apr 2017' 
		in: 'Pharo-7.0-hopefully-'.
	self subclassResponsibility ! !!FileSystemStore methodsFor: 'private' stamp: 'AlistairGrant 5/6/2017 19:00'!entryFromNode: aNode fileSystem: aFilesystem path: aPath

	| attributes |
	
	self 
		deprecated: 'FileSystemDirectoryEntry class>>fileSystem:path:'
		on: 	'30 Apr 2017' 
		in: 'Pharo-7.0-hopefully-'.
	^ FileSystemDirectoryEntry
		fileSystem: aFilesystem
		path: aPath! !!FileSystemStore methodsFor: 'private' stamp: 'AlistairGrant 5/7/2017 12:57'!basicIsDirectory: aNode
	"Used to decide whether the low-level representation (node) from the store is a directory.
	This private message should only be called form within the store."
	self subclassResponsibility ! !!FileSystemStore methodsFor: 'private' stamp: 'AlistairGrant 5/7/2017 17:57'!entryAt: aPath fileSystem: aFilesystem

	self checkExists: aPath.
	^ FileSystemDirectoryEntry
		fileSystem: aFilesystem
		path: aPath! !!FileSystemStore methodsFor: 'private' stamp: 'AlistairGrant 5/7/2017 10:55'!nodesAt: aPath do: aBlock

	^self subclassResponsibility! !!FileSystemStore methodsFor: 'testing' stamp: 'AlistairGrant 5/1/2017 08:51'!isCharacter: aPath
	"Return a boolean indicating whether the File described by aPath is character based"
	^ (self attributesOf: aPath) isCharacter.
	! !!FileSystemStore methodsFor: 'testing' stamp: 'AlistairGrant 5/1/2017 08:51'!isFIFO: aPath
	"Return a boolean indicating whether the File described by aPath is FIFO"
	^ (self attributesOf: aPath) isFIFO.
	! !!FileSystemStore methodsFor: 'testing' stamp: 'AlistairGrant 4/30/2017 12:31'!isDirectory: aPath
	^(self attributesOf: aPath) isDirectory.
	! !!FileSystemStore methodsFor: 'testing' stamp: 'AlistairGrant 5/7/2017 17:58'!checkExists: aPath

	(self exists: aPath) ifFalse:
		[ FileDoesNotExist signalWith: aPath ].! !!FileSystemStore methodsFor: 'testing' stamp: 'AlistairGrant 4/30/2017 12:34'!isSymlink: aPath
	^(self attributesOf: aPath) isSymlink! !!FileSystemStore methodsFor: 'testing' stamp: 'AlistairGrant 5/1/2017 09:06'!isSocket: aPath
	"Return a boolean indicating whether the File described by aPath is a socket"
	^ (self attributesOf: aPath) isSocket.
	! !!FileSystemStore methodsFor: 'testing' stamp: 'AlistairGrant 4/30/2017 12:37'!isExecutable: aPath
	^(self attributesOf: aPath) isExecutable! !!FileSystemStore methodsFor: 'testing' stamp: 'AlistairGrant 4/30/2017 12:36'!isWritable: aPath
	^(self attributesOf: aPath) isWritable! !!FileSystemStore methodsFor: 'testing' stamp: 'AlistairGrant 4/30/2017 20:16'!isReadable: aPath
	^(self attributesOf: aPath) isReadable! !!FileSystemStore methodsFor: 'testing' stamp: 'AlistairGrant 5/1/2017 08:51'!isBlock: aPath
	"Return a boolean indicating whether the File described by aPath is a block device"
	^ (self attributesOf: aPath) isBlock.
	! !!FileSystemStore methodsFor: 'testing' stamp: 'AlistairGrant 5/1/2017 08:52'!isFile: aPath
	"Return a boolean indicating whether the File described by aPath is a file (not a directory)"
	^ (self attributesOf: aPath) isFile.
	! !!FileSystemStore methodsFor: 'testing' stamp: 'AlistairGrant 5/1/2017 08:52'!isRegular: aPath
	"Return a boolean indicating whether the File described by aPath is a regular file"
	^ (self attributesOf: aPath) isRegular.
	! !!DiskStore methodsFor: 'accessing' stamp: 'AlistairGrant 5/18/2017 08:34'!fileAttributePrimitives
	"Answer the primitives for file attribute and directory enumerating operations."

	^FileAttributePrimitives! !!DiskStore methodsFor: 'accessing' stamp: 'AlistairGrant 5/18/2017 12:35'!gidOf: aPath
	"Return the gid of the File described by aPath"
	^FileAttributePrimitives fileAttribute: (self stringFromPath: aPath) number: 7! !!DiskStore methodsFor: 'accessing' stamp: 'AlistairGrant 5/17/2017 22:11'!modificationTimeOf: aPath
	"Return the date of last access of the File described by aPath"
	^DateAndTime fromUnixTime:
		(FileAttributePrimitives fileAttribute: (self stringFromPath: aPath) number: 10).! !!DiskStore methodsFor: 'accessing' stamp: 'AlistairGrant 5/18/2017 15:40'!isDirectory: aPath
	"Answer a boolean indicating whether the supplied path is a Directory file"
	^FileAttributePrimitives isDirectory: (self stringFromPath: aPath)! !!DiskStore methodsFor: 'accessing' stamp: 'AlistairGrant 5/17/2017 22:05'!accessTimeOf: aPath
	"Return the date of last access of the File described by aPath"
	^DateAndTime fromUnixTime:
		(FileAttributePrimitives fileAttribute: (self stringFromPath: aPath) number: 9).! !!DiskStore methodsFor: 'accessing' stamp: 'AlistairGrant 5/18/2017 12:33'!sizeOf: aPath
	"Return the size of the File described by aPath"
	^FileAttributePrimitives fileAttribute: (self stringFromPath: aPath) number: 8.! !!DiskStore methodsFor: 'accessing' stamp: 'AlistairGrant 5/17/2017 22:30'!deviceIdOf: aPath
	"Return the device id of the File described by aPath"
	^ FileAttributePrimitives fileAttribute: (self stringFromPath: aPath) number: 4! !!DiskStore methodsFor: 'accessing' stamp: 'AlistairGrant 5/17/2017 22:28'!changeTimeOf: aPath

	^DateAndTime fromUnixTime:
		(FileAttributePrimitives fileAttribute: (self stringFromPath: aPath) number: 11).! !!DiskStore methodsFor: 'accessing' stamp: 'AlistairGrant 5/18/2017 12:36'!inodeOf: aPath
	"Return the inode number of the File described by aPath"
	^FileAttributePrimitives fileAttribute: (self stringFromPath: aPath) number: 3! !!DiskStore methodsFor: 'accessing' stamp: 'AlistairGrant 6/6/2017 22:37'!permissions: aPath
	^FileAttributePrimitives permissions: (self stringFromPath: aPath)! !!DiskStore methodsFor: 'testing' stamp: 'AlistairGrant 5/18/2017 15:45'!isFIFO: aPath
	"Answer a boolean indicating whether the supplied path is a FIFO file"
	^FileAttributePrimitives isFIFO: (self stringFromPath: aPath)! !!DiskStore methodsFor: 'testing' stamp: 'AlistairGrant 5/18/2017 15:45'!isSocket: aPath
	"Answer a boolean indicating whether the supplied path is a Socket file"
	^FileAttributePrimitives isSocket: (self stringFromPath: aPath)! !!DiskStore methodsFor: 'testing' stamp: 'AlistairGrant 5/18/2017 15:45'!isRegular: aPath
	"Answer a boolean indicating whether the supplied path is a Regular file"
	^FileAttributePrimitives isRegular: (self stringFromPath: aPath)! !!DiskStore methodsFor: 'testing' stamp: 'AlistairGrant 5/18/2017 15:45'!isCharacter: aPath
	"Answer a boolean indicating whether the supplied path is a Character file"
	^FileAttributePrimitives isCharacter: (self stringFromPath: aPath)! !!DiskStore methodsFor: 'testing' stamp: 'AlistairGrant 5/18/2017 15:45'!isSymlink: aPath
	"Answer a boolean indicating whether the supplied path is a Symlink file"
	^FileAttributePrimitives isSymlink: (self stringFromPath: aPath)! !!DiskStore methodsFor: 'testing' stamp: 'AlistairGrant 5/15/2017 23:22'!exists: aPath

	aPath isRoot ifTrue: [ ^true ].
	^self fileAttributePrimitives exists: (self stringFromPath: aPath).! !!DiskStore methodsFor: 'testing' stamp: 'AlistairGrant 5/18/2017 15:45'!isBlock: aPath
	"Answer a boolean indicating whether the supplied path is a Block file"
	^FileAttributePrimitives isBlock: (self stringFromPath: aPath)! !!DiskStore methodsFor: 'private' stamp: 'AlistairGrant 5/1/2017 19:24'!basicIsFile: anEntry
	self 
		deprecated: 'Use FileAttributes instead'
		on: 	'30 Apr 2017' 
		in: 'Pharo-7.0-hopefully-'.
	^ (anEntry at: 4) not! !!DiskStore methodsFor: 'private' stamp: 'AlistairGrant 5/7/2017 12:47'!nodeAt: aPath ifPresent: presentBlock ifAbsent: absentBlock
	
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
			presentBlock value: entry ].! !!DiskStore methodsFor: 'private' stamp: 'AlistairGrant 5/1/2017 19:23'!basicCreationTimeOf: anEntry
	" the entry contains the seconds since the squeak epoch in local time"
	self 
		deprecated: 'Use FileAttributes instead'
		on: 	'30 Apr 2017' 
		in: 'Pharo-7.0-hopefully-'.
	^  (DateAndTime fromSeconds: (anEntry at: 2) offset: 0) translateTo: DateAndTime localOffset! !!DiskStore methodsFor: 'private' stamp: 'AlistairGrant 5/15/2017 23:22'!nodesAt: aPath do: aBlock

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
	] ensure: [ FileAttributePrimitives primClosedir: dirPointer ].! !!DiskStore methodsFor: 'private' stamp: 'AlistairGrant 5/7/2017 10:54'!statAttributesFromNode: aNode
	"Answer the stat attributes array from the supplied node"
	
	^aNode at: 2! !!DiskStore methodsFor: 'private' stamp: 'AlistairGrant 5/1/2017 19:24'!basicModificationTimeOf: anEntry
	" the entry contains the seconds since the squeak epoch in local time"
	self 
		deprecated: 'Use FileAttributes instead'
		on: 	'30 Apr 2017' 
		in: 'Pharo-7.0-hopefully-'.
	^ (DateAndTime fromSeconds: (anEntry at: 3) offset: 0) translateTo: DateAndTime localOffset! !!DiskStore methodsFor: 'private' stamp: 'AlistairGrant 5/1/2017 19:24'!basicIsSymlink: anEntry
	self 
		deprecated: 'Use FileAttributes instead'
		on: 	'30 Apr 2017' 
		in: 'Pharo-7.0-hopefully-'.
	^(anEntry size >= 7)
		ifTrue: [ anEntry at: 7 ]
		ifFalse: [ false ]! !!DiskStore methodsFor: 'private' stamp: 'AlistairGrant 5/7/2017 10:53'!basenameFromNode: aNode
	^ aNode at: 1! !!DiskStore methodsFor: 'private' stamp: 'AlistairGrant 5/18/2017 15:01'!modeOf: aPath
	"Answer the statBuf.st_mode for the supplied path"
	
	^FileAttributePrimitives fileAttribute: (self stringFromPath: aPath) number: 2! !!DiskStore methodsFor: 'private' stamp: 'AlistairGrant 5/1/2017 19:24'!basicPosixPermissions: anEntry
	self 
		deprecated: 'Use FileAttributes instead'
		on: 	'30 Apr 2017' 
		in: 'Pharo-7.0-hopefully-'.
	^ (anEntry size >= 6)
		ifTrue: [ anEntry at: 6 ]
		ifFalse: [ nil ].! !!DiskStore methodsFor: 'private' stamp: 'AlistairGrant 6/6/2017 21:07'!basicIsDirectory: aNode
	| mask statAttributes |

	statAttributes := aNode at: 2.
	statAttributes ifNil: [ ^false ].
	mask := statAttributes at: 2.
	^(mask bitAnd: self fileAttributePrimitives s_IFMT) = self fileAttributePrimitives s_IFDIR! !!DiskStore methodsFor: 'private' stamp: 'AlistairGrant 5/1/2017 19:25'!basicSizeOf: anEntry
	self 
		deprecated: 'Use FileAttributes instead'
		on: 	'30 Apr 2017' 
		in: 'Pharo-7.0-hopefully-'.
	^ (anEntry at: 5)! !!DiskStore methodsFor: 'private' stamp: 'AlistairGrant 5/7/2017 10:17'!basicEntry: directoryEntry path: aPath nodesDo: aBlock
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
			entry := Primitives lookupEntryIn: encodedPathString index: index ].! !!DiskStore methodsFor: 'public' stamp: 'AlistairGrant 6/6/2017 23:22'!delete: path
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
				forFileNamed: pathString ]! !!DiskStore class methodsFor: 'accessing' stamp: 'AlistairGrant 5/3/2017 23:10'!attributesClass

	^DiskFileAttributes ! !!DiskStore class methodsFor: 'class initialization' stamp: 'AlistairGrant 5/19/2017 08:29'!useFilePlugin
	Primitives := FilePluginPrims new.
	FileAttributePrimitives class reset.
	DiskFileAttributes initializeWithPrimitives: FileAttributePrimitives.! !!MemoryStore methodsFor: 'accessing' stamp: 'AlistairGrant 5/1/2017 17:10'!attributesClass
	"The concrete class used to access file attributes in the receiver"

	^MemoryFileAttributes! !!MemoryStore methodsFor: 'private' stamp: 'AlistairGrant 5/1/2017 19:26'!basicEntry: entry nodesDo: aBlock
	self 
		deprecated: 'Use FileAttributes instead'
		on: 	'30 Apr 2017' 
		in: 'Pharo-7.0-hopefully-'.
	entry fileEntriesDo: aBlock! !!MemoryStore methodsFor: 'private' stamp: 'AlistairGrant 5/1/2017 19:26'!basicIsFile: aMemoryFileSystemEntry
	self 
		deprecated: 'Use FileAttributes instead'
		on: 	'30 Apr 2017' 
		in: 'Pharo-7.0-hopefully-'.
	^ aMemoryFileSystemEntry isFile! !!MemoryStore methodsFor: 'private' stamp: 'AlistairGrant 5/7/2017 20:51'!nodesAt: aPath do: aBlock

	^(self nodeAt: aPath) fileEntriesDo:
		[ :each | aBlock value: each ]. ! !!MemoryStore methodsFor: 'private' stamp: 'AlistairGrant 5/7/2017 18:31'!nodeAt: aPath ifPresent: presentBlock ifAbsent: absentBlock
	| current |
	current := self root.
	aPath do: [ :segment | 
		current isDirectory
			ifTrue: [ current := current fileEntryAt: segment ifAbsent: [ ^ absentBlock value ]]
			ifFalse: [ ^ absentBlock value ]].
	^ presentBlock value: current! !!MemoryStore methodsFor: 'private' stamp: 'AlistairGrant 5/1/2017 19:25'!basicCreationTimeOf: aMemoryFileSystemEntry
	"Returns the creation date of aMemoryFileSystemEntry"
	self 
		deprecated: 'Use FileAttributes instead'
		on: 	'30 Apr 2017' 
		in: 'Pharo-7.0-hopefully-'.
	^ aMemoryFileSystemEntry creationTime! !!MemoryStore methodsFor: 'private' stamp: 'AlistairGrant 5/1/2017 19:26'!basicModificationTimeOf: aMemoryFileSystemEntry
	"Return the basic modification time of aMemoryFileSystemEntry"
	self 
		deprecated: 'Use FileAttributes instead'
		on: 	'30 Apr 2017' 
		in: 'Pharo-7.0-hopefully-'.
	^ aMemoryFileSystemEntry modificationTime! !!MemoryStore methodsFor: 'private' stamp: 'AlistairGrant 5/1/2017 19:27'!basicSizeOf: aMemoryFileSystemEntry
	"Return the basic size of aMemoryFileSystemEntry"
	self 
		deprecated: 'Use FileAttributes instead'
		on: 	'30 Apr 2017' 
		in: 'Pharo-7.0-hopefully-'.
	^ aMemoryFileSystemEntry fileSize! !!MemoryStore methodsFor: 'private' stamp: 'AlistairGrant 5/7/2017 18:05'!nodeAt: aPath
	| current |
	current := self root.
	aPath do: [ :segment | 
		current isDirectory
			ifTrue: [ current := current fileEntryAt: segment ifAbsent: [ ^ FileDoesNotExist signalWith: aPath ]]
			ifFalse: [ ^ FileDoesNotExist signalWith: aPath ]].
	^ current! !!MemoryStore methodsFor: 'private' stamp: 'AlistairGrant 5/1/2017 19:26'!basicIsSymlink: aNode
	self 
		deprecated: 'Use FileAttributes instead'
		on: 	'30 Apr 2017' 
		in: 'Pharo-7.0-hopefully-'.
	^false! !!MemoryStore methodsFor: 'private' stamp: 'AlistairGrant 5/1/2017 19:27'!basicPosixPermissions: anEntry
	self 
		deprecated: 'Use FileAttributes instead'
		on: 	'30 Apr 2017' 
		in: 'Pharo-7.0-hopefully-'.
	^ 8r777! !!MemoryStore methodsFor: 'testing' stamp: 'AlistairGrant 5/1/2017 17:25'!exists: aPath

	^self nodeAt: aPath
		ifPresent: [ :node | true ]
		ifAbsent: [ false ]! !!MemoryStore methodsFor: 'public' stamp: 'AlistairGrant 5/7/2017 10:53'!basenameFromNode: aMemoryFileSystemEntry
	^ aMemoryFileSystemEntry basename! !!UnixStore class methodsFor: 'class initialization' stamp: 'AlistairGrant 5/18/2017 16:36'!useFilePlugin

	FileAttributePrimitives := UnixFileAttributesPluginPrims new.
	super useFilePlugin.
! !!WindowsStore methodsFor: 'private' stamp: 'AlistairGrant 5/17/2017 08:59'!nodesAt: aPath do: aBlock

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
		].! !!WindowsStore class methodsFor: 'class initialization' stamp: 'AlistairGrant 5/18/2017 16:36'!useFilePlugin

	FileAttributePrimitives := WindowsFileAttributesPluginPrims new.
	super useFilePlugin.
! !DiskStore removeSelector: #isFile:!DiskStore removeSelector: #isReadable:!DiskStore removeSelector: #isWritable:!!DiskStore reorganize!(#comparing = hash)(#accessing defaultWorkingDirectory handleClass fileAttributePrimitives gidOf: modificationTimeOf: isDirectory: accessTimeOf: sizeOf: deviceIdOf: changeTimeOf: inodeOf: permissions:)(#printing forReferencePrintOn:)(#'*Deprecated60' basicModificationTime: basicCreationTime: basicSize:)(#testing isFIFO: isSocket: isRegular: isCharacter: isSymlink: exists: isDiskFileSystem isBlock:)(#private rootNode basicIsFile: nodeAt:ifPresent:ifAbsent: basicCreationTimeOf: nodesAt:do: statAttributesFromNode: basicModificationTimeOf: basicIsSymlink: basenameFromEntry: basenameFromNode: modeOf: basicPosixPermissions: basicIsDirectory: basicSizeOf: basicEntry:path:nodesDo:)(#'*FileSystem-Attributes' attributesClass symlinkAttributesClass)(#initialization initialize)(#public maxFileNameLength rename:to: createDirectory: basicOpen:writable: checkName:fixErrors: delete: basicEntryAt: openFileStream:writable:)!!FileSystemStore reorganize!(#'*FileSystem-Disk' isDiskFileSystem)(#public directoryAt:ifAbsent:fileNodesDo: directoryAt:directoryNodesDo: openFileStream:writable: directoryAt:ifAbsent:directoryNodesDo: mimeTypesAt: nodeAt: modificationTime: directoryAt:fileNodesDo: ensureCreateDirectory: basenameFromNode: basenameFromEntry: exists: checkName:fixErrors: size: directoryAt:ifAbsent:nodesDo: entryFromNode:path:for: creationTime:)(#deprecated basicIsWritable: basicModificationTime: basicIsSymlink: basicIsFile: basicSizeOf: basicCreationTimeOf: basicPosixPermissions:)(#accessing sizeOf: defaultWorkingDirectory attributesOf:ifPresent:ifAbsent: separator isCaseSensitive symlinkAttributesClass uid: permissions: gidOf: targetFile: numberOfHardLinks: modificationTimeOf: accessTimeOf: creationTimeOf: deviceIdOf: delimiter)(#'error signalling' signalFileDoesNotExist: basicOpen:writable: signalDirectoryDoesNotExist: signalDirectoryExists: signalFileExists:)(#'*FileSystem-Attributes' symlinkAttributesOf:ifPresent:ifAbsent: symlinkAttributesOf: attributesClass attributesOf:)(#abstract basicCreationTime: rename:to: inodeOf: delete: close basicSize: createDirectory: nodeAt:ifPresent:ifAbsent: open basicEntry:path:nodesDo:)(#'*FileSystem-Memory' isMemoryFileSystem)(#private entryFromNode:fileSystem:path: copy:ifAbsent:to:ifPresent:fileSystem: filename:matches: rename:ifAbsent:to:ifPresent:fileSystem: basicIsDirectory: entryAt:fileSystem: nodesAt:do:)(#testing isCharacter: isFIFO: isDirectory: checkExists: isSymlink: isSocket: isExecutable: isWritable: isReadable: isBlock: isFile: isRegular:)(#converting stringFromPath: pathFromString: printPath:on:)!FileSystemDirectoryEntry class removeSelector: #fileSystem:path:creation:modification:isDir:size:posixPermissions:isSymlink:!FileSystemDirectoryEntry class removeSelector: #reference:creation:modification:isDir:size:posixPermissions:isSymlink:!FileSystemDirectoryEntry removeSelector: #initializeWithRef:creation:modification:isDir:size:posixPermissions:isSymlink:!Object subclass: #FileSystemDirectoryEntry	instanceVariableNames: 'reference attributes'	classVariableNames: ''	poolDictionaries: ''	category: 'FileSystem-Core-Public'!!FileSystemDirectoryEntry reorganize!(#'*Iceberg-Core' packageName)(#printing printOn:)(#delegate fullName extension writeStream readStream pathSegments)(#accessing isDirectory accessTime modificationSeconds posixPermissions permissions inode modificationTime modification attributes isCharacter gid creationTime uid reference: basename deviceId isBlock size creation creationSeconds reference targetFile attributes:)(#testing isWritable numberOfHardLinks isFIFO isSymlink isRegular isReadable isExecutable isFile isSocket)(#converting asFileReference)(#'*monticellofiletree-filesystem-utilities' readStreamDo: name)!