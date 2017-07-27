'From Pharo7.0SNAPSHOT of 22 July 2017 [Latest update: #0] on 27 July 2017 at 7:40:37.4449 pm'!Object subclass: #DiskFileAttributesPerformanceTests	instanceVariableNames: 'files'	classVariableNames: ''	poolDictionaries: ''	category: 'AKG-FileSystem-Performance'!!DiskFileAttributesPerformanceTests methodsFor: 'tests - enumerating' stamp: 'AlistairGrant 5/18/2017 10:17'!timeFileSystemWalking: aStream
	"Measure the time required to collect all files in a directory"

	| times |
	
	times := OrderedCollection new.
	10 timesRepeat: 
	[
	SmalltalkImage current garbageCollect.
	times add: [ 5 timesRepeat: [ '/usr/bin' asFileReference entries ] ] timeToRun.
	].

	"Report the results"
	self addLabel: 'Directory Walking' times: times to: aStream.
! !!DiskFileAttributesPerformanceTests methodsFor: 'initialize' stamp: 'AlistairGrant 4/29/2017 21:13'!initialize

	super initialize.
	files := '/usr/bin' asFileReference entries collect: [ :each | each reference ].! !!DiskFileAttributesPerformanceTests methodsFor: 'private' stamp: 'AlistairGrant 5/18/2017 10:20'!addLabel: label times: times to: aStream

	aStream
		<< (label padRightTo: 40);
		<< ':  '.
	times average totalSeconds asFloat printOn: aStream.
	aStream << '  '.
	times min totalSeconds asFloat printOn: aStream.
	aStream << '  '.
	times max totalSeconds asFloat printOn: aStream.
	aStream cr.! !!DiskFileAttributesPerformanceTests methodsFor: 'private' stamp: 'AlistairGrant 5/20/2017 07:34'!timeAttibutes: attributeNames reportOn: aStream 
	"Measure the time taken to retrieve the size and modification date of the file set"

	| times |

	times := OrderedCollection new: 10.
	Smalltalk image garbageCollect.
	10 timesRepeat: [
		times add: ([20 timesRepeat: [
			files do: [ :each |
				attributeNames do: [ :attrName |
					attrName value: each ] ] ] ] timeToRun) ].

	self addLabel: attributeNames size printString, ' attribute(s)' times: times to: aStream.

	"If FileAttributes is present, test FileAttributes object"
	self haveFileAttributes ifTrue: [
	times := OrderedCollection new: 10.
	Smalltalk image garbageCollect.
	10 timesRepeat: [
		times add: ([20 timesRepeat: [
			files do: [ :each | | fileAttributes |
				fileAttributes := each attributes.
				attributeNames do: [ :attrName |
					attrName value: fileAttributes ] ] ] ] timeToRun) ].
	self addLabel: attributeNames size printString, ' attribute(s) (FileAttributes)' 
			times: times to: aStream.
	].! !!DiskFileAttributesPerformanceTests methodsFor: 'private' stamp: 'AlistairGrant 5/18/2017 10:04'!haveFileAttributes

	^Smalltalk includesKey: #FileAttributes! !!DiskFileAttributesPerformanceTests methodsFor: 'running' stamp: 'AlistairGrant 5/20/2017 07:34'!run

	| start end |
	^String streamContents: [ :stream |
		start := DateAndTime now.
		self timeEmpty: stream.
		self timeExists: stream.
		self timeFileSystemWalking: stream.
		stream
			cr;
			<< 'Stat Attributes Times'; cr;
			<< '====================='; cr.
		self timeStatAttributes: stream.
		stream
			cr;
			<< 'Access Attributes Times'; cr;
			<< '======================='; cr.
		self timeAccessAttributes: stream.
		end := DateAndTime now.
		stream
			cr;
			<< 'Total Run Time: '.
		(end - start) printOn: stream.
		].! !!DiskFileAttributesPerformanceTests methodsFor: 'private - accessing' stamp: 'AlistairGrant 5/20/2017 07:07'!statAttributes

	^#(#creationTime #isDirectory #isFile #isSymlink #modificationTime #permissions).
! !!DiskFileAttributesPerformanceTests methodsFor: 'private - accessing' stamp: 'AlistairGrant 5/20/2017 07:20'!accessAttributes

	^#(#isReadable #isWritable).
! !!DiskFileAttributesPerformanceTests methodsFor: 'file attributes' stamp: 'AlistairGrant 5/20/2017 07:10'!timeEmpty: aStream
	"Measure the time taken to run an empty test"

	| times attributeNames |

	attributeNames := self statAttributes.
	times := OrderedCollection new: 10.
	Smalltalk image garbageCollect.
	10 timesRepeat: [
		times add: ([20 timesRepeat: [
			files do: [ :each |
				attributeNames do: [ :attrName |
					attrName yourself. ] ] ] ] timeToRun) ].

	"Report the results"
	self addLabel: 'Empty Test' times: times to: aStream.

! !!DiskFileAttributesPerformanceTests methodsFor: 'file attributes' stamp: 'AlistairGrant 5/20/2017 07:07'!timeStatAttributes: aStream
	"Measure the time taken to retrieve the stat attributes"

	| attributes |

	attributes := self statAttributes.
	1 to: attributes size do: 
		[ :i | self timeAttibutes: (attributes copyFrom: 1 to: i) reportOn: aStream ]. 
! !!DiskFileAttributesPerformanceTests methodsFor: 'file attributes' stamp: 'AlistairGrant 5/18/2017 09:57'!timeExists: aStream
	"Measure the time taken to check whether each file in the file set exists"

	| times |

	times := OrderedCollection new: 10.
	Smalltalk image garbageCollect.
	10 timesRepeat: [
		times add: ([50 timesRepeat: [ files do: [ :each |
			each exists. ] ] ] timeToRun) ].

	"Report the results"
	self addLabel: 'File existance' times: times to: aStream.

! !!DiskFileAttributesPerformanceTests methodsFor: 'file attributes' stamp: 'AlistairGrant 5/20/2017 07:19'!timeAccessAttributes: aStream
	"Measure the time taken to retrieve the stat attributes"

	| attributes |

	attributes := self accessAttributes.
	1 to: attributes size do: 
		[ :i | self timeAttibutes: (attributes copyFrom: 1 to: i) reportOn: aStream ]. 
! !