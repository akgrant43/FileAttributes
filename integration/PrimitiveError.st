'From Pharo7.0alpha of 7 January 2018 [Build information: Pharo-7.0+alpha.build.412.sha.2e40948016aa14ef9979c3403147a1a9f7302764 (32 Bit)] on 11 January 2018 at 10:06:29.992386 am'!
Object subclass: #PrimitiveError
	instanceVariableNames: 'errorName errorCode'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'System-Support'!
!PrimitiveError commentStamp: 'akg 1/8/2018 15:09' prior: 0!
A PrimitiveError is used to answer a primitive failure code that has an associated operating system/library error.

Instance Variables
	errorName:		<Symbol>
	errorCode:		<Integer>

errorName
	- typically #'operating system error'

errorCode
	- the value of the error, a signed 64-bit value, a representation imposed by the VM; specific clients must map this error value into an unsigned value as appropriate if required

Typical usage is shown in the ficticious method below:

primitiveOperation
	<primitive: 'primitiveOperation' module: 'APlugin' error: error>
	^(error isPrimitiveError)
		ifTrue: [ self processErrorCode: error errorCode ]
		ifFalse: [ self primitiveFailed ].!


!PrimitiveError methodsFor: 'testing' stamp: 'akg 1/8/2018 14:58'!
isPrimitiveError
	"Answer a boolean indicating if the receiver is an error object returned by a primitive"
	^true! !


!PrimitiveError methodsFor: 'accessing' stamp: 'AlistairGrant 1/6/2018 22:53'!
errorName
	"Answer the error name returned by the primitive"

	^errorName! !

!PrimitiveError methodsFor: 'accessing' stamp: 'AlistairGrant 1/6/2018 22:52'!
errorCode
	"Answer the integer error number returned by the primitive"

	^errorCode! !

!PrimitiveError methodsFor: 'accessing' stamp: 'akg 1/8/2018 15:11'!
errorName: aSymbol
	
	errorName := aSymbol! !

!PrimitiveError methodsFor: 'accessing' stamp: 'akg 1/8/2018 15:11'!
errorCode: anInteger
	
	errorCode := anInteger! !

!Object methodsFor: '*System-Support-error handling' stamp: 'eem 12/12/2017 09:16'!
isPrimitiveError
	^false! !
