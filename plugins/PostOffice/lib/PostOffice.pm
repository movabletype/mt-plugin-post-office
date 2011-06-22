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

package PostOffice;

use strict;
use MT::Util qw(html_text_transform perl_sha1_digest_hex);

our $DEBUG = 1;

sub plugin {
    return MT->component('postoffice');
}

sub deliver {
    my $pkg = shift;

    my $plugin = $pkg->plugin;
    require MT::PluginData;
    my $pd_iter = MT::PluginData->load_iter({ plugin => $plugin->key });
    return unless $pd_iter;

    my $count = 0;
    while (my $pd = $pd_iter->()) {
        next unless $pd->key =~ m/^configuration:blog:(\d+)/;
        my $blog_id = $1;
        print STDERR "[PostOffice] Checking inbox for blog $blog_id...\n"
          if $DEBUG;
        my $cfg = $pd->data() || {};
        next unless $cfg->{email_username};
        my $blog_count = $pkg->process_messages($blog_id, $cfg);
        if (defined $blog_count) {
            print STDERR "[PostOffice] Delivered $blog_count messages for blog $blog_id...\n"
              if $DEBUG;
            $count += $blog_count;
        }
    }

    my $sys_cfg = $plugin->get_config_hash() || {};
    if ($sys_cfg->{email_username}) {
        print STDERR "[PostOffice] Checking inbox for system-configured inbox...\n";
        eval {
            my $sys_count = $pkg->process_messages(undef, $sys_cfg);
            if (defined $sys_count) {
                print STDERR "[PostOffice] Delivered $sys_count messages...\n"
                  if $DEBUG;
                $count += $sys_count;
            }
        };
        if ($@) {
            print STDERR "[PostOffice] Error during delivery: $@\n";
        }
    }

    return $count;
}

sub save_attachment {
    my $pkg = shift;
    my ($blog, $file) = @_;

    require MT::Asset;
    my $asset_pkg = MT::Asset->handler_for_file($file->{path});
    my $asset;
    $asset = $asset_pkg->new();
    $asset->file_path($file->{path});
    $asset->file_name($file->{name});
    $file->{name} =~ /(.*)(?:\.)(.*$)/;
    $asset->file_ext($2);
    $asset->url($file->{url});
    $asset->mime_type($file->{media_type});
    $asset->blog_id($blog->id);
    $asset->save || return undef;

    MT->run_callbacks(
        'api_upload_file.' . $asset->class,
        File  => $file->{path},
        file  => $file->{path},
        Url   => $file->{url},
        url   => $file->{url},
        Size  => $file->{size},
        size  => $file->{size},
        Asset => $asset,
        asset => $asset,
        Type  => $asset->class,
        type  => $asset->class,
        $blog ? (Blog => $blog) : (),
        $blog ? (blog => $blog) : ()
    );

    return $asset;
}

sub _parse_content_type {
    my ($content_type) = @_;

    require Email::MIME::ContentType;
    my $data = Email::MIME::ContentType::parse_content_type($content_type);
    my $media_type = $data->{discrete} . "/" . $data->{composite};
    my $charset    = $data->{attributes}->{charset};
    return ($media_type, $charset);
}

sub _unique_filename {
    my ($fmgr, $site_path, $filename) = @_;

    require File::Basename;
    my ($basename, undef, $suffix) =
      File::Basename::fileparse($filename, qr/\.[^.]*/);
    require File::Spec;
    my $u = '';
    my $i = 1;
    while (
        $fmgr->exists(
            File::Spec->catfile($site_path, $basename . $u . $suffix)
        )
      )
    {
        $u = '_' . $i++;
    }
    return $basename . $u . $suffix;
}

sub process_message_parts {
    my $pkg = shift;
    my ($blog, $msg, $cfg, $author) = @_;

    require Email::MIME;
    my $parsed = Email::MIME->new($msg->{message});

    require MT::I18N;
    my ($text, $charset);
    my $format = $author->text_format || $blog->convert_paras || '__default__';

    my @files;
    my @parts = $parsed->parts;

    if (@parts == 2) {
        if (($parts[0]->content_type =~ m!^text/plain!) && ($parts[1]->content_type =~ m!^multipart/(related|alternative|mixed)!)) {
            # these are the parts we're looking for
            @parts = $parts[1]->subparts;
        }
    }

    print STDERR sprintf("[PostOffice] Found %d parts in message\n", scalar @parts)
      if $DEBUG;

    my $cidmap;
    # prescan for embedded images
    foreach my $part (@parts) {
        if ($part->filename) {
            my ($media_type, $part_charset) =
              _parse_content_type($part->header('Content-Type'));
            my $filename =
              MT::I18N::encode_text($part->filename, $part_charset);
            my $fmgr = $blog->file_mgr;
            $filename =
              _unique_filename($fmgr, $blog->site_path, $filename);
            require File::Spec;
            my $filepath =
              File::Spec->catfile($blog->site_path, $filename);
            my $bytes = $fmgr->put_data($part->body, $filepath);
            my $cid = $part->header('Content-Id');
            if ($cid) {
                $cid =~ s/^<|>$//g;
            }
            my $file = {
                name       => $filename,
                path       => $filepath,
                url        => $blog->site_url . $filename,
                media_type => $media_type,
                size       => $bytes,
                content_id => $cid,
            };
            $cidmap->{$cid} = $file if $cid;
            my $asset = $pkg->save_attachment($blog, $file);

            if ($asset) {
                $file->{asset} = $asset;
                push @files, $file;
            }
        }
    }

    # now process, and build document
    $text = '';
    my $file_num = 0;
    

    foreach my $part (@parts) {
        my ($media_type, $part_charset) =
          _parse_content_type($part->header('Content-Type'));
        if (!defined $charset) {
            $charset = $part_charset;
        }
        if ($media_type =~ m!^text/(html|plain)!) {

            print STDERR '[PostOffice body text] From $part: ' . $part->body . "\n";

            my $body = MT::I18N::encode_text($part->body, $part_charset);
            
            print STDERR "[PostOffice body text] Processed through encode_text: $body\n";

            if (($media_type eq 'text/plain') && ($format eq 'richtext')) {
                # we're embedding html, so format must be richtext.
                $body = html_text_transform($body);
            }
            elsif (($media_type eq 'text/html')) {
                # scan html for img tags with a cid: src; swap with
                # markup for asset
                $body =~ s/
                    (
                        (<[iI][mM][gG]\s+?[^>]*?\b
                            [sS][rR][cC]=)
                        (['"]?)
                        cid:([^>\s'"]+)
                        (['"]?)
                    )
                /$cidmap->{$4} ? $2 . $3 . $cidmap->{$4}{asset}->url . $5 : $1/gsex;
            }

            $text .= $body;
        }
        else {
            if ($part->filename) {
                my $file = $files[$file_num];
                $file_num++;
                # this is a file embedded for reference in the html; don't
                # output it in the body of the post.
                next if $file->{content_id};

                # Has the plugin has been configured to embed a link to the 
                # asset in the Entry Body? The Objectasset relationship is
                # created independent of this.
                if ($cfg->{embed_attachments}) {
                    $text .= $pkg->format_embedded_asset($file);
                }
            }
        }
    }

    print STDERR '[PostOffice body text] from $text: ' . $text . "\n";

    $msg->{subject} = $parsed->header('Subject');

    # Process for [Category] prefixes
    if ($msg->{subject} =~ m/^[ ]*\[([^\]]+?)\][ ]*(.+)$/) {
        $msg->{category} = MT::I18N::encode_text($1, $charset);
        $msg->{subject}  = MT::I18N::encode_text($2, $charset);
    }
    else {
        $msg->{subject}  = MT::I18N::encode_text($msg->{subject}, $charset);
    }

    # Process for #hashtags
    if ($msg->{subject} =~ m/#/) {
        my @tags = $msg->{subject} =~ m/#(\D[^ ]*)\s*/g;
        if (@tags) {
            $msg->{tags} = [];
            foreach my $tag (@tags) {
                push @{$msg->{tags}}, MT::I18N::encode_text($tag, $charset);
            }
            $msg->{subject} =~ s/#(\D[^ ]*)\s*//g;
            $msg->{subject} =~ s/\s+$//;
        }
    }

    # TBD: Allow user to specify the sanitize spec for this
    require MT::Sanitize;
    $text = MT::Sanitize->sanitize($text,
        "a href rel,b,i,strong,em,p,br/,ul,li,ol,blockquote,pre,div,span,table,tr,th rowspan colspan,td rowspan colspan,dd,dl,dt,img height width src alt");
    
    print STDERR "[PostOffice body text] Sanitized: $text\n";
    
    $msg->{text}  = $text;
    $msg->{format} = $format;
    $msg->{files} = \@files;

    return;
}

sub format_embedded_asset {
    my $pkg = shift;
    my ($file) = @_;

    my $embed;
    if ($file->{media_type} =~ /^image/) {
        $embed = $file->{asset}->as_html({ align => 'none', include => 1 });
    }
    else {
        $embed = $file->{asset}->as_html();
    }
    $embed .= "\n";

    return $embed;
}

sub _get_valid_addresses {
    my $pkg = shift;
    my ($blog_id, $cfg) = @_;

    # Get Addresses out of plugindata
    my @addresses = split(/\s*,\s*/, lc($cfg->{allowed_emails} || ''));
    my %addresses;
    $addresses{$_} = 1 for @addresses;

    require MT::Permission;
    require MT::Author;

    # FIXME: This doesn't include any sysadmins who have no direct
    # relationship with the blog...
    if ($cfg->{allow_mt_authors}) {

        # get addresses for this blog
        my $iter = MT::Permission->load_iter({ blog_id => $blog_id, });
        while (my $perm = $iter->()) {
            my $au = MT::Author->load({ id => $perm->author_id });
            if ($au && $au->email) {
                $addresses{ lc $au->email } = $au;
            }
        }
    }
    return \%addresses;
}

sub process_message {
    my $pkg = shift;
    my ($blog_id, $cfg, $au, $perm, $msg) = @_;

    require MT::Blog;
    my $blog = MT::Blog->load($blog_id);

    $pkg->process_message_parts($blog, $msg, $cfg, $au);

    print STDERR '[PostOffice body text] final $msg ' . $msg->{text} . "\n";

    require MT::Entry;
    my $entry = MT::Entry->new();
    $entry->title($msg->{subject});
    $entry->text($msg->{text});
    $entry->author_id($au->id);
    $entry->blog_id($blog_id);
    $entry->status($cfg->{post_status} || 1);
    $entry->tags(@{$msg->{tags}}) if $msg->{tags};
    $entry->convert_breaks($msg->{format});

    MT->run_callbacks(
        'postoffice_pre_save',
        blog_id     => $blog_id,
        config      => $cfg,
        author      => $au,
        permissions => $perm,
        message     => $msg,
        entry       => $entry,
    );

    print STDERR "[PostOffice] Saving entry [" . $entry->title . "]\n"
      if $DEBUG;

    if (! $entry->save) {
        print STDERR "[PostOffice] Error saving entry [" . $entry->title . "]: "
            . $entry->errstr . "\n";
        return 0;
    }

    # create ObjectAsset associations for attachments if they don't already exist
    if ($msg->{files}) {
        require MT::ObjectAsset;
        foreach my $file (@{$msg->{files}}) {
            next unless $file->{asset};
            my $asset = $file->{asset};
            my $obj_asset = MT::ObjectAsset->load({ asset_id => $asset->id,
                object_ds => 'entry', object_id => $entry->id });
            unless ($obj_asset) {
                $obj_asset = new MT::ObjectAsset;
                $obj_asset->blog_id($blog_id);
                $obj_asset->asset_id($asset->id);
                $obj_asset->object_ds('entry');
                $obj_asset->object_id($entry->id);
                $obj_asset->save;
            }
        }
    }

    my $cat;
    my $place;
    if ($msg->{category}) {
        require MT::Category;
        # TBD: is $msg->{cateogry} encoded properly here?
        $cat = MT::Category->load({ label => $msg->{category} });
        unless ($cat) {
            if ($perm->can_edit_categories) {
                $cat = MT::Category->new();
                $cat->blog_id($blog_id);
                $cat->label($msg->{category});
                $cat->parent(0);
                $cat->save
                  or die $cat->errstr;
            }
        }

        if ($cat) {
            require MT::Placement;
            $place = MT::Placement->new;
            $place->entry_id($entry->id);
            $place->blog_id($blog_id);
            $place->category_id($cat->id);
            $place->is_primary(1);
            $place->save
              or die $place->errstr;
        }
    }

    MT->run_callbacks(
        'postoffice_post_save',
        blog_id     => $blog_id,
        config      => $cfg,
        author      => $au,
        permissions => $perm,
        message     => $msg,
        entry       => $entry,
        ($cat   ? (category  => $cat)   : ()),
        ($place ? (placement => $place) : ()),
    );

    if ($entry->status == 2) {    # publish
        MT->rebuild_entry(
            Entry => $entry,
            BuildDependecies => 1,
        )
            or MT->log({
                blog_id => $blog_id,
                level   => MT::Log::ERROR(),
                message => 'Post Office encountered an error while trying to '
                    . 'publish: ' . MT->errstr,
            });
    }

    MT->run_callbacks('api_post_save.entry', MT->instance, $entry, undef);

    return 1;
}

sub process_messages {
    my $pkg = shift;
    my ($blog_id, $cfg) = @_;

    my $app = MT->app;
    if ($app->isa('MT::App')) {
        $blog_id ||= $app->param('blog_id');
    }

    $cfg ||= $blog_id ? $pkg->plugin->get_config_hash('blog:' . $blog_id) : $pkg->plugin->get_config_hash();

    my $default_author_id = $cfg->{default_author};
    require MT::Author;
    my $default_author = MT::Author->load($default_author_id)
      if $default_author_id;

    my $xp = $pkg->transport($cfg)
      or die "[PostOffice] No mail transport configured";
    my $iter = $xp->message_iter or return 0;

    require Email::Address;
    my $count = 0;

    my $addresses_by_blog = {};

    while (my $msg = $iter->()) {

        # determine blog_id for active message
        my $extension = $msg->{to};
        my $api_key;
        my $local_blog_id = $blog_id;
        if ($extension =~ m/^[^@]+\+(.+?)@/) {
            $extension = $1;
            if ($extension =~ m!^(?:(.+)\.)?(\d+)$!) {
                $local_blog_id = $2;
                $api_key = $1 if $1;
            }
            else {
                $api_key = $extension;
            }
        }
        else {
            $extension = undef;
        }
        if (!$local_blog_id) {
            print STDERR "[PostOffice] No blog_id parameter present for message "
              . $msg->{'message-id'}
              . " from "
              . $msg->{from} . "\n";
        }

        my $addresses = $addresses_by_blog->{$local_blog_id} ||=
          $pkg->_get_valid_addresses($local_blog_id, $cfg);

        my ($addr) = Email::Address->parse($msg->{from});
        if (!$addr) {
            print STDERR "[PostOffice] error parsing 'from' address for message "
              . $msg->{'message-id'}
              . " from "
              . $msg->{from} . "\n";
            next;
        }
        my $from = lc $addr->address;
        
        # If any email address is allowed to post then just set the current
        # $from to be a key in the $addresses hash.
        if ($cfg->{allow_any_email}) {
            $addresses->{$from} = 1;
        }
        
        unless ($addresses->{$from}) {
            print STDERR "[PostOffice] Unknown author address for message "
              . $msg->{'message-id'}
              . " from "
              . $msg->{from} . "\n";
            next;
        }

        my $au =
          ref $addresses->{$from}
          ? $addresses->{$from}
          : MT::Author->load({ email => $from });
        $au ||= $default_author;
        if (!$au) {
            print STDERR "[PostOffice] No MT author found for message "
              . $msg->{'message-id'}
              . " from "
              . $msg->{from} . "\n";
            next;
        }

        # Test for API key requirement
        if ($cfg->{require_api_key}) {
            if (!$api_key) {

                # TBD: Log API key missing message??
                print STDERR "[PostOffice] Missing API key for message "
                  . $msg->{'message-id'}
                  . " from "
                  . $msg->{from} . " to "
                  . $msg->{to} . "\n";
                next;
            }

            # TBD: Log incorrect API key?
            if (
                $api_key
                && (perl_sha1_digest_hex($au->api_password) ne
                    $api_key)
              )
            {
                print STDERR "[PostOffice] Invalid API key for message "
                  . $msg->{'message-id'}
                  . " from "
                  . $msg->{from} . " to "
                  . $msg->{to} . "\n";
                next;
            }
        }

        require MT::Permission;
        my $perm =
          MT::Permission->load(
            { author_id => $au->id, blog_id => $local_blog_id });
        if (!$perm) {
            print STDERR sprintf("[PostOffice] User '%s' has no permissions on this blog.\n", $au->name);
            next;
        }

        if (!($au->is_superuser || $perm->can_administer_blog || $perm->can_post)) {
            print STDERR sprintf("[PostOffice] User '%s' has no permissions to post to this blog.\n", $au->name);
            next;
        }

        if ($pkg->process_message($local_blog_id, $cfg, $au, $perm, $msg)) {
            $xp->remove($msg);
            $count++;
        }
    }

    return $count;
}

sub transport {
    my $pkg            = shift;
    my ($cfg)          = @_;
    my $transport      = lc($cfg->{email_transport}) || 'pop3';
    my $all_transports = MT->registry("postoffice_transports");
    my $tp             = $all_transports->{$transport};
    my $label          = $tp->{label};
    $label = $label->() if ref($label);
    my $class = $tp->{class} if $tp;
    $class ||= 'PostOffice::Transport::POP3';
    eval qq{require $class; 1;}
      or die "[PostOffice] failed to load transport class $class";

    print STDERR "[PostOffice] Connecting to " . $label . " server " . $cfg->{email_host} . "...\n"
      if $DEBUG;

    my %param = (
        %$cfg,
        username => $cfg->{email_username},
        password => $cfg->{email_password},
        host     => $cfg->{email_host},
        ssl      => $cfg->{use_ssl},
    );
    return $class->new(%param);
}

1;
