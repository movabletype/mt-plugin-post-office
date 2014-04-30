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

package PostOffice::Transport::POP3;

use strict;
use base qw(PostOffice::Transport);

sub init {
    my $obj = shift;
    my (%param) = @_;

    require Mail::POP3Client;
    $obj->{client} = new Mail::POP3Client(
        USER     => $param{username},
        PASSWORD => $param{password},
        HOST     => $param{host},
        ($param{ssl} ? ( USESSL => 'true' ) : () ),
        #DEBUG    => 1,
    ) or die "Failed to connect: " . $@;

    # Reports "welcome" if successful, or an explanation of the connection
    # failure if not connected. That means yes, the "or die..." above doesn't
    # seem to work.
    print STDERR '[PostOffice] POP3 client connection: ' . $obj->{client}->{MESG} . "\n"
        if $PostOffice::DEBUG;

    return;
}

sub remove {
    my $obj = shift;
    my ($msg) = @_;
    my $client = $obj->{client};
    return undef unless $client;
    return $client->Delete($msg->{sequence});
}

sub message_iter {
    my $obj = shift;

    my $client = $obj->{client};
    return undef unless $client;

    my $count = $client->Count();
    return undef unless $count;

    my $counter = 1;

    return sub {
        if ($counter > $count) {
            $client->Close();
            return undef;
        }

        my $msg = {};

        my $last_key;
        foreach ( $client->Head($counter) ) {
            if ( m/^([^\s][^:]+?):\s*(.+)$/ ) {
                $last_key = lc $1;
                $msg->{$last_key} = $2;
            } elsif (defined $last_key) {
                $msg->{$last_key} .= "\n" . $_;
            }
        }

        # get Body
        $msg->{body} = $client->Body($counter);
        $msg->{message} = $client->HeadAndBody($counter);
        $msg->{sequence} = $counter;
        $counter++;

        $msg;
    };
}

1;
