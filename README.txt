## Post Office, a plugin for Movable Type
## Author: Six Apart
## Version: 1.1
## Released under MIT

## OVERVIEW ##

This plugin enables users to post to their blog via email.

## PREREQUISITES ##

* Mail::IMAPClient (for IMAP)
* Mail::POP3Client (for POP)
* Email::Address
* Email::MIME
* IO::Socket::SSL

## FEATURES ##

Post Office is a plugin for Movable Type that allows users to post to 
their blog via email. It works by connecting Movable Type to an 
existing email account, like GMail or any POP or IMAP compliant mailbox, 
and periodically scanning for messages to post. Each user can be given 
a unique email address to which to post to uniquely identify them and 
the blog they want to post to when sending an email. 

## INSTALLATION ##

  1. Copy the contents of PostOffice/plugins into /path/to/mt/plugins/
  2. Navigate to the settings area for Post Office and enter in the
     connection info for your email provider.
  3. Ensure that you have an API Password selected for yourself. Edit
     your profile if you need to select one.
  4. Click the Write Entry button and scroll to the bottom of the screen.
     Look for the text "Email to <blog name>".
  5. Save the email address linked to in your address book. Send a test
     email.

## SOURCE CODE ##

Source repository:
    http://github.com/movabletype/mt-plugin-post-office

## LICENSE ##

MIT

## AUTHOR ##

Copyright 2008-2013, Six Apart Ltd., All rights reserved.
