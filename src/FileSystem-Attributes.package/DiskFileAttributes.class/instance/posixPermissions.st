accessing
posixPermissions
	"Answer the receivers posix permissions"
	
	^self mode bitAnd: 8r777