tests
testIsExecutable

	self assert: FileLocator vmBinary isExecutable.
	self assert: FileLocator vmBinary attributes isExecutable equals: FileLocator vmBinary isExecutable