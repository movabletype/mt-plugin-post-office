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

package PostOffice::CMS;

use strict;

sub edit_entry_src {
    my $cb = shift;
    my ($app, $tmpl) = @_;
    my $plugin = $cb->plugin;

    my $user = $app->user;
    return unless $user && $user->api_password;

    my $blog_id = $app->blog->id;
    my $cfg = $plugin->get_config_hash('blog:' . $blog_id);
    if (!$cfg->{email_username}) {
        $cfg = $plugin->get_config_hash();
    }
    return unless $cfg->{email_username};

    my $ext = $blog_id;
    if ($cfg->{require_api_key}) {
        require MT::Util;
        $ext = MT::Util::perl_sha1_digest_hex($user->api_password) . '.' . $blog_id;
    }
    my $mailto = $cfg->{email_address};
    $mailto =~ s/@/+$ext@/;

    my $out = <<EOT;
<div id="postoffice">
    <__trans phrase="[_1]Email to [_2]</a> - Save this address to post to this blog by email." params="<a href='mailto:$mailto'>%%<mt:var name='blog_name' escape='html'>">
</div>
EOT
    $$tmpl =~ s!(<div id="quickpost">)!$out$1!;
}

sub config_template {
    my ($plugin, $param, $scope) = @_;
    my @allowed_authors;
    my $config = $plugin->get_config_hash($scope);
    my $default_author = $config->{default_author} || 0;
    my @authors = MT::Author->load( { type => 1 } ); # only authors
    if ($scope =~ m/blog:/) {
        $scope =~ s/blog://;
        foreach my $iter (@authors) {
            my $perms = MT::Permission->load({ blog_id => $scope, author_id => $iter->id });
            if ($perms) {
                push @allowed_authors, $iter;
            }
        }
    }
    else {
        @allowed_authors = @authors;
    }

    my $author_options = '';
    foreach my $a (@allowed_authors) {
        $author_options .= "<option value=\"" . $a->id . "\"";
        if ($a->id == $default_author) {
            $author_options .= " selected=\"selected\"";
        }
        $author_options .= ">" . MT::Util::encode_html($a->name) . "</option>\n";
    }

    my $transport = lc($config->{email_transport}) || 'pop3';
    my $transports = '';
    my $app = MT->instance;
    my $all_transports = $app->registry("postoffice_transports") || {};
    $all_transports = $app->filter_conditional_list($all_transports);
    foreach my $t (keys %$all_transports) {
        my $tp = $all_transports->{$t};
        $transports .= qq{<option value="$t" };
        if ($t eq $transport) {
            $transports .= 'selected="selected"';
        }
        my $label = $tp->{label};
        $label = $label->() if ref($label);
        $transports .= qq{>$label</option>\n};
    }

    my $imap_selected = $transport eq 'imap';
    if (!$imap_selected) {
        $config->{imap_folder} = undef;
    }

    my $form =  <<HTML;
<div class="field field-top-label pkg" style="margin-bottom: 0;">
    <div class="field-header">
        <h3>Email Destination Configuration</h3>
    </div>
</div>

<mtapp:setting
    id="email_address"
    label="<__trans phrase="Destination Inbox">"
    show_hint="1"
    hint="<__trans phrase="This is the email address authors send posts to when they want Movable Type to post those messages to this blog.">">
    <input name="email_address" id="email_address" value="<mt:var name="email_address" escape="html">" class="full-width" />
</mtapp:setting>

<mtapp:setting
    id="email_transport"
    label="<__trans phrase="Mail Server Type">">
    <select id="email_transport" name="email_transport">
        $transports
    </select>
</mtapp:setting>

<mtapp:setting
    id="email_host"
    label="<__trans phrase="Email Account Host">"
    show_hint="1"
    hint="<__trans phrase="This is the host for the email account which Movable Type uses to post to this blog.">">
    <input name="email_host" id="email_host" value="<mt:var name="email_host" escape="html">" class="full-width" />
</mtapp:setting>

<mtapp:setting
    id="use_ssl"
    label="<__trans phrase="Use SSL">"
    show_hint="1"
    hint="<__trans phrase="The host requires an encrypted connection.">">
    <input type="checkbox" name="use_ssl" <mt:if name="use_ssl">checked="checked"</mt:if> id="use_ssl"  class="cb" />
</mtapp:setting>

<mtapp:setting
    id="imap_folder"
    label="<__trans phrase="IMAP Folder">"
    show_hint="1"
    shown="$imap_selected"
    hint="<__trans phrase="The IMAP folder to check for new posts.">">
    <input id="imap_folder" type="text" name="imap_folder" value="<mt:var name="imap_folder" escape="html">" class="full-width" />
</mtapp:setting>

<mtapp:setting
    id="email_username"
    label="<__trans phrase="Email Account Username">"
    show_hint="1"
    hint="<__trans phrase="This is the username for the email account which Movable Type uses to post to this blog.">">
    <input name="email_username" id="email_username" value="<mt:var name="email_username" escape="html">" class="full-width" />
</mtapp:setting>

<mtapp:setting
    id="email_password"
    label="<__trans phrase="Email Account Password">"
    show_hint="1"
    hint="<__trans phrase="This is the password for the email account which Movable Type uses to post to this blog.">">
    <input type="password" name="email_password" id="email_password" value="<mt:var name="email_password" escape="html">" class="full-width" />
</mtapp:setting>


<div class="field field-top-label pkg" style="margin-bottom: 0;">
    <div class="field-header">
        <h3>Entry and Author Configuration</h3>
    </div>
</div>

<mtapp:setting
    id="post_status"
    label="<__trans phrase="Default Entry Status">"
    show_hint="1"
    hint="<__trans phrase="This determines if entries are automatically published as they are received or if they must be manually published.">">
    <select name="post_status" id="post_status">
        <option<mt:if name="post_status" eq="2"> selected="selected"</mt:if> value="2"><__trans phrase="Published"></option>
        <option<mt:if name="post_status" eq="1"> selected="selected"</mt:if> value="1"><__trans phrase="Draft"></option>
    </select>
</mtapp:setting>

<mtapp:setting
    id="embed_attachments"
    label="<__trans phrase="Embed Attachments">"
    show_hint="1"
    hint="<__trans phrase="Embed attachments in the Entry Body? (An entry-asset relationship will be created for any attachments regardless of this setting.)">">
    <input type="checkbox" name="embed_attachments" <mt:if name="embed_attachments">checked="checked"</mt:if> id="embed_attachments" class="cb" value="1" />
</mtapp:setting>

<mtapp:setting
    id="allow_mt_authors"
    label="<__trans phrase="Allow all MT Authors from this Blog to Post">"
    show_hint="1"
    hint="<__trans phrase="If you check this box, Movable Type will allow all of the authors of this weblog to post via email using the email address in their author profile.">">
    <input type="checkbox" name="allow_mt_authors" <mt:if name="allow_mt_authors">checked="checked"</mt:if> id="allow_mt_authors"  class="cb" />
</mtapp:setting>

<mtapp:setting
    id="require_api_key"
    label="<__trans phrase="Require Web Services Password in Address">"
    show_hint="1"
    hint="<__trans phrase="If you check this box, Post Office will require users to include their Web Service Password as an extension on their e-mail address. So the 'From' address should contain a '+' followed by their Web Services Password value. This provides additional authentication for incoming messages. (i.e., user+nnnnn@...)">">
    <input type="checkbox" name="require_api_key" <mt:if name="require_api_key">checked="checked"</mt:if> id="require_api_key"  class="cb" />
</mtapp:setting>

<mtapp:setting
    id="allowed_emails"
    label="<__trans phrase="Email Addresses Allowed to Post">"
    show_hint="1"
    hint="<__trans phrase="Movable Type will post messages received from these email addresses. Separate multiple addresses with a comma.">">
    <textarea name="allowed_emails" id="allowed_emails" cols="" rows="2" class="full-width"><mt:var name="allowed_emails" escape="html"></textarea>
</mtapp:setting>

<mtapp:setting
    id="allow_any_email"
    label="<__trans phrase="Allow Any Email Address to Post">"
    show_hint="1"
    hint="<__trans phrase="Checking this box will allow anybody to post to this blog. Note that this basically allows for unauthenticated, anonymous posting.">">
    <input type="checkbox" name="allow_any_email" <mt:if name="allow_any_email">checked="checked"</mt:if> id="allow_any_email" class="cb" value="1" />
</mtapp:setting>

<mtapp:setting
    id="default_author"
    label="<__trans phrase="Default Author">"
    show_hint="1"
    hint="<__trans phrase="This is the &ldquo;default&rdquo; author, the person to whom entries are assigned if no other valid author exists. Email addresses specified in the above field will be attributed to this author if they are not valid Authors.">">
    <select name="default_author" id="default_author">
        $author_options
    </select>
</mtapp:setting>
HTML

    return $form;
}

1;
