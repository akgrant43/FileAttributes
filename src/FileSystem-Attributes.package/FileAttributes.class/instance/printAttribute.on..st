printing
printAttribute: anAttribute on: aStream

	[ (anAttribute value: self) printOn: aStream ]
	on: FileAttributeNotSupported 
	do: [ aStream << '(not supported)' ]