 # debhelp

A project to help people learn Debian. A help file contains entries with tags and associated help text. A search can be made with tags to find associated help text.

 # Syntax of help file

A tag line begins with a hash sign (#), followed by a series of tags. The tags must not contain upper-case letters, and the tags and the dollar sign must all be separated by exactly one space. After the tag line follows any number of lines containing text aimed to help the Debian newbie, or experienced user.

 # Todo

Fill the help file
Get the helpfile to be in a global directory. "/usr/share" didn't work due to permissions. Currently using home folder to avoid that

 # Contribute

New entries are added at the top of the help file. Just commit. Ah, I need to learn git

 # Installation

1. Make sure you have wget installed
2. Clone this repository
3. cd into the newly created directory
4. run "make -s"
5. run "mv -i debhelp /usr/local/bin/debhelp" as root

You can now enter the assistance tool by running "debhelp"

