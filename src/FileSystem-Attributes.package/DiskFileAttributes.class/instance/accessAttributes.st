private - accessing
accessAttributes
	^ accessAttributes ifNil: [ self getAccessAttributes ]