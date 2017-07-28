'From Pharo7.0SNAPSHOT of 22 July 2017 [Latest update: #0] on 27 July 2017 at 7:40:37.4449 pm'!
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
! !

	super initialize.
	files := '/usr/bin' asFileReference entries collect: [ :each | each reference ].! !

	aStream
		<< (label padRightTo: 40);
		<< ':  '.
	times average totalSeconds asFloat printOn: aStream.
	aStream << '  '.
	times min totalSeconds asFloat printOn: aStream.
	aStream << '  '.
	times max totalSeconds asFloat printOn: aStream.
	aStream cr.! !
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
	].! !

	^Smalltalk includesKey: #FileAttributes! !

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
		].! !

	^#(#creationTime #isDirectory #isFile #isSymlink #modificationTime #permissions).
! !

	^#(#isReadable #isWritable).
! !
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

! !
	"Measure the time taken to retrieve the stat attributes"

	| attributes |

	attributes := self statAttributes.
	1 to: attributes size do: 
		[ :i | self timeAttibutes: (attributes copyFrom: 1 to: i) reportOn: aStream ]. 
! !
	"Measure the time taken to check whether each file in the file set exists"

	| times |

	times := OrderedCollection new: 10.
	Smalltalk image garbageCollect.
	10 timesRepeat: [
		times add: ([50 timesRepeat: [ files do: [ :each |
			each exists. ] ] ] timeToRun) ].

	"Report the results"
	self addLabel: 'File existance' times: times to: aStream.

! !
	"Measure the time taken to retrieve the stat attributes"

	| attributes |

	attributes := self accessAttributes.
	1 to: attributes size do: 
		[ :i | self timeAttibutes: (attributes copyFrom: 1 to: i) reportOn: aStream ]. 
! !