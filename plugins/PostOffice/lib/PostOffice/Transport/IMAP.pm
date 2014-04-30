############################################################################
# Copyright © 2008-2010 Six Apart Ltd.
# This program is free software: you can redistribute it and/or modify it
# under the terms of version 2 of the GNU General Public License as published
# by the Free Software Foundation, or (at your option) any later version.
# This program is distributed in the hope that it will be useful, but WITHOUT
# ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
# FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License
# version 2 for more details. You should have received a copy of the GNU
# General Public License version 2 along with this program. If not, see
# <http://www.gnu.org/licenses/>.

package PostOffice::Transport::IMAP;

use strict;
use base qw(PostOffice::Transport);

sub init {
    my $obj = shift;
    my (%param) = @_;

    my $socket;
    if ($param{ssl}) {
        require IO::Socket::SSL;
        $socket = IO::Socket::SSL->new(
            Proto    => 'tcp',
            PeerAddr => $param{host},
            PeerPort => 993, # IMAP over SSL standard port
        );
    }

    require Mail::IMAPClient;
    $obj->{client} = new Mail::IMAPClient(
        User     => $param{username},
        Password => $param{password},
        ($socket ? ( Socket => $socket ) : ( Server => $param{host} )),
        IgnoreSizeErrors => 1,
    ) or die "Failed to connect: " . $@;

    $obj->{imap_folder} = $param{imap_folder};
    $obj->{client}->Connected()
        or die "Failed to connect";
    $obj->{client}->Authenticated()
        or die "Failed to authenticate";

    if ( $obj->{client}->IsAuthenticated() ) {
        print STDERR "[PostOffice] IMAP folders: "
            . join( ', ', $obj->{client}->folders() ) . "\n"
            if $PostOffice::DEBUG;
    };

    return $obj;
}

sub remove {
    my $obj = shift;
    my ($msg) = @_;

    my $client = $obj->{client};
    return undef unless $client;

    print STDERR "[PostOffice] Deleting message id $msg->{sequence}\n"
      if $PostOffice::DEBUG;

    $client->delete_message($msg->{sequence});

    return 1;
}

sub message_iter {
    my $obj = shift;

    my $client = $obj->{client};
    return undef unless $client;

    my $box = $obj->{imap_folder} || 'INBOX';

    print STDERR "[PostOffice] Selecting mailbox $box\n"
      if $PostOffice::DEBUG;

    $client->select($box);

    # my $count = $client->message_count();
    # return undef unless $count;

    my $msgs = $client->messages;
    unless ($msgs && @$msgs) {
        print STDERR "[PostOffice] No messages found. Returning...\n"
          if $PostOffice::DEBUG;
        return undef;
    }

    return sub {
        if (!@$msgs) {
            print STDERR "[PostOffice] Closing server connection.\n"
              if $PostOffice::DEBUG;
            $client->close();
            return undef;
        }

        my $msg = {};
        my $msg_seq = shift @$msgs;

        my $headers = $client->parse_headers($msg_seq, 'ALL');

        my $last_key;
        foreach ( keys %$headers ) {
            $last_key = lc $_;
            my $value = $headers->{$_};
            if ((ref($value) eq 'ARRAY') && (scalar @$value == 1)) {
                $value = $value->[0];
            }
            $msg->{lc $_} = $value;
        }
        $msg->{body} = $client->body_string($msg_seq);
        $msg->{message} = $client->message_string($msg_seq);
        if (!defined $msg->{message}) {
            print STDERR "[PostOffice] Bad message_string from IMAPClient: " . $client->LastError . "\n";
        }
        $msg->{sequence} = $msg_seq;

        $msg;
    };
}

1;
