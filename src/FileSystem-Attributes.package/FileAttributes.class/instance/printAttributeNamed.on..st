printing
printAttributeNamed: attributeName on: aStream

	aStream
		tab; 
		<< attributeName;
		<< ':';
		tab.
	self printAttribute: attributeName on: aStream.
	aStream cr.
