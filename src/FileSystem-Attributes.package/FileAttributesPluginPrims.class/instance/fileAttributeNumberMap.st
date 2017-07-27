private
fileAttributeNumberMap
	"Answer a mapping of attribute number to name for #primFileAttribute:number:"
	
	^#(
		#targetName
		#mode
		#inode
		#deviceId
		#numberOfHardLinks
		#uid
		#gid
		#fileSize
		#accessUnixTime
		#modificationUnixTime
		#changeUnixTime
		#creationUnixTime
		#isReadable
		#isWritable
		#isExecutable
		)