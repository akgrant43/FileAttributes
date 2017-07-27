FileAttributesPluginPrims provides primitives to:

- Check the existence of files (#primExists:)
- Retrieve information returned by the libc stat() and access() functions (#primFileAttributes:mask:, #primFileAttribute:number:)
- Retrieve masks used by stat.st_mode (#primFileMasks)

Public API and Key Messages

- primExists: - check whether the given file exists
- primFileAttributes:mask: - retrieve stat() and/or access() information
- primFileAttribute:number: - retrieve a single stat() or access() value.
- primFileMasks - retrieve stat.st_mode masks

See the method comments for more information.

E.g. to check file existance:

FileAttributesPluginPrims new primExists: '/bin/bash'
 
See FileAttributes comments for more information.