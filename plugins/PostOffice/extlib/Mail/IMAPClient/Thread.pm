package Mail::IMAPClient::Thread;
use Parse::RecDescent;
{ my $ERRORS;


package Parse::RecDescent::Mail::IMAPClient::Thread;
use strict;
use vars qw($skip $AUTOLOAD  );
@Parse::RecDescent::Mail::IMAPClient::Thread::ISA = ();
$skip = '\s*';


{
local $SIG{__WARN__} = sub {0};
# PRETEND TO BE IN Parse::RecDescent NAMESPACE
*Parse::RecDescent::Mail::IMAPClient::Thread::AUTOLOAD   = sub
{
    no strict 'refs';

    ${"AUTOLOAD"} =~ s/^Parse::RecDescent::Mail::IMAPClient::Thread/Parse::RecDescent/;
    goto &{${"AUTOLOAD"}};
}
}

push @Parse::RecDescent::Mail::IMAPClient::Thread::ISA, 'Parse::RecDescent';
# ARGS ARE: ($parser, $text; $repeating, $_noactions, \@args, $_itempos)
sub Parse::RecDescent::Mail::IMAPClient::Thread::start
{
	my $thisparser = $_[0];
	use vars q{$tracelevel};
	local $tracelevel = ($tracelevel||0)+1;
	$ERRORS = 0;
    my $thisrule = $thisparser->{"rules"}{"start"};

    Parse::RecDescent::_trace(q{Trying rule: [start]},
                  Parse::RecDescent::_tracefirst($_[1]),
                  q{start},
                  $tracelevel)
                    if defined $::RD_TRACE;

    
    my $err_at = @{$thisparser->{errors}};

    my $score;
    my $score_return;
    my $_tok;
    my $return = undef;
    my $_matched=0;
    my $commit=0;
    my @item = ();
    my %item = ();
    my $repeating =  $_[2];
    my $_noactions = $_[3];
    my @arg =    defined $_[4] ? @{ &{$_[4]} } : ();
    my $_itempos = $_[5];
    my %arg =    ($#arg & 01) ? @arg : (@arg, undef);
    my $text;
    my $lastsep;
    my $current_match;
    my $expectation = new Parse::RecDescent::Expectation(q{/^\\* THREAD /i});
    $expectation->at($_[1]);
    
    my $thisline;
    tie $thisline, q{Parse::RecDescent::LineCounter}, \$text, $thisparser;

    

    while (!$_matched && !$commit)
    {
        
        Parse::RecDescent::_trace(q{Trying production: [/^\\* THREAD /i thread]},
                      Parse::RecDescent::_tracefirst($_[1]),
                      q{start},
                      $tracelevel)
                        if defined $::RD_TRACE;
        my $thisprod = $thisrule->{"prods"}[0];
        $text = $_[1];
        my $_savetext;
        @item = (q{start});
        %item = (__RULE__ => q{start});
        my $repcount = 0;


        Parse::RecDescent::_trace(q{Trying terminal: [/^\\* THREAD /i]}, Parse::RecDescent::_tracefirst($text),
                      q{start},
                      $tracelevel)
                        if defined $::RD_TRACE;
        undef $lastsep;
        $expectation->is(q{})->at($text);
        

        unless ($text =~ s/\A($skip)/$lastsep=$1 and ""/e and   $text =~ m/\A(?:^\* THREAD )/i)
        {
            $text = $lastsep . $text if defined $lastsep;
            $expectation->failed();
            Parse::RecDescent::_trace(q{<<Didn't match terminal>>},
                          Parse::RecDescent::_tracefirst($text))
                    if defined $::RD_TRACE;

            last;
        }
        $current_match = substr($text, $-[0], $+[0] - $-[0]);
        substr($text,0,length($current_match),q{});
        Parse::RecDescent::_trace(q{>>Matched terminal<< (return value: [}
                        . $current_match . q{])},
                          Parse::RecDescent::_tracefirst($text))
                    if defined $::RD_TRACE;
        push @item, $item{__PATTERN1__}=$current_match;
        

        Parse::RecDescent::_trace(q{Trying repeated subrule: [thread]},
                  Parse::RecDescent::_tracefirst($text),
                  q{start},
                  $tracelevel)
                    if defined $::RD_TRACE;
        $expectation->is(q{thread})->at($text);
        
        unless (defined ($_tok = $thisparser->_parserepeat($text, \&Parse::RecDescent::Mail::IMAPClient::Thread::thread, 0, 100000000, $_noactions,$expectation,sub { \@arg },undef)))
        {
            Parse::RecDescent::_trace(q{<<Didn't match repeated subrule: [thread]>>},
                          Parse::RecDescent::_tracefirst($text),
                          q{start},
                          $tracelevel)
                            if defined $::RD_TRACE;
            last;
        }
        Parse::RecDescent::_trace(q{>>Matched repeated subrule: [thread]<< (}
                    . @$_tok . q{ times)},

                      Parse::RecDescent::_tracefirst($text),
                      q{start},
                      $tracelevel)
                        if defined $::RD_TRACE;
        $item{q{thread(s?)}} = $_tok;
        push @item, $_tok;
        


        Parse::RecDescent::_trace(q{Trying action},
                      Parse::RecDescent::_tracefirst($text),
                      q{start},
                      $tracelevel)
                        if defined $::RD_TRACE;
        

        $_tok = ($_noactions) ? 0 : do {
	$return=$item{'thread(s?)'}||undef;
};
        unless (defined $_tok)
        {
            Parse::RecDescent::_trace(q{<<Didn't match action>> (return value: [undef])})
                    if defined $::RD_TRACE;
            last;
        }
        Parse::RecDescent::_trace(q{>>Matched action<< (return value: [}
                      . $_tok . q{])},
                      Parse::RecDescent::_tracefirst($text))
                        if defined $::RD_TRACE;
        push @item, $_tok;
        $item{__ACTION1__}=$_tok;
        

        Parse::RecDescent::_trace(q{>>Matched production: [/^\\* THREAD /i thread]<<},
                      Parse::RecDescent::_tracefirst($text),
                      q{start},
                      $tracelevel)
                        if defined $::RD_TRACE;



        $_matched = 1;
        last;
    }


    unless ( $_matched || defined($score) )
    {
        

        $_[1] = $text;  # NOT SURE THIS IS NEEDED
        Parse::RecDescent::_trace(q{<<Didn't match rule>>},
                     Parse::RecDescent::_tracefirst($_[1]),
                     q{start},
                     $tracelevel)
                    if defined $::RD_TRACE;
        return undef;
    }
    if (!defined($return) && defined($score))
    {
        Parse::RecDescent::_trace(q{>>Accepted scored production<<}, "",
                      q{start},
                      $tracelevel)
                        if defined $::RD_TRACE;
        $return = $score_return;
    }
    splice @{$thisparser->{errors}}, $err_at;
    $return = $item[$#item] unless defined $return;
    if (defined $::RD_TRACE)
    {
        Parse::RecDescent::_trace(q{>>Matched rule<< (return value: [} .
                      $return . q{])}, "",
                      q{start},
                      $tracelevel);
        Parse::RecDescent::_trace(q{(consumed: [} .
                      Parse::RecDescent::_tracemax(substr($_[1],0,-length($text))) . q{])},
                      Parse::RecDescent::_tracefirst($text),
                      , q{start},
                      $tracelevel)
    }
    $_[1] = $text;
    return $return;
}

# ARGS ARE: ($parser, $text; $repeating, $_noactions, \@args, $_itempos)
sub Parse::RecDescent::Mail::IMAPClient::Thread::NUMBER
{
	my $thisparser = $_[0];
	use vars q{$tracelevel};
	local $tracelevel = ($tracelevel||0)+1;
	$ERRORS = 0;
    my $thisrule = $thisparser->{"rules"}{"NUMBER"};

    Parse::RecDescent::_trace(q{Trying rule: [NUMBER]},
                  Parse::RecDescent::_tracefirst($_[1]),
                  q{NUMBER},
                  $tracelevel)
                    if defined $::RD_TRACE;

    
    my $err_at = @{$thisparser->{errors}};

    my $score;
    my $score_return;
    my $_tok;
    my $return = undef;
    my $_matched=0;
    my $commit=0;
    my @item = ();
    my %item = ();
    my $repeating =  $_[2];
    my $_noactions = $_[3];
    my @arg =    defined $_[4] ? @{ &{$_[4]} } : ();
    my $_itempos = $_[5];
    my %arg =    ($#arg & 01) ? @arg : (@arg, undef);
    my $text;
    my $lastsep;
    my $current_match;
    my $expectation = new Parse::RecDescent::Expectation(q{/\\d+/});
    $expectation->at($_[1]);
    
    my $thisline;
    tie $thisline, q{Parse::RecDescent::LineCounter}, \$text, $thisparser;

    

    while (!$_matched && !$commit)
    {
        
        Parse::RecDescent::_trace(q{Trying production: [/\\d+/]},
                      Parse::RecDescent::_tracefirst($_[1]),
                      q{NUMBER},
                      $tracelevel)
                        if defined $::RD_TRACE;
        my $thisprod = $thisrule->{"prods"}[0];
        $text = $_[1];
        my $_savetext;
        @item = (q{NUMBER});
        %item = (__RULE__ => q{NUMBER});
        my $repcount = 0;


        Parse::RecDescent::_trace(q{Trying terminal: [/\\d+/]}, Parse::RecDescent::_tracefirst($text),
                      q{NUMBER},
                      $tracelevel)
                        if defined $::RD_TRACE;
        undef $lastsep;
        $expectation->is(q{})->at($text);
        

        unless ($text =~ s/\A($skip)/$lastsep=$1 and ""/e and   $text =~ m/\A(?:\d+)/)
        {
            $text = $lastsep . $text if defined $lastsep;
            $expectation->failed();
            Parse::RecDescent::_trace(q{<<Didn't match terminal>>},
                          Parse::RecDescent::_tracefirst($text))
                    if defined $::RD_TRACE;

            last;
        }
        $current_match = substr($text, $-[0], $+[0] - $-[0]);
        substr($text,0,length($current_match),q{});
        Parse::RecDescent::_trace(q{>>Matched terminal<< (return value: [}
                        . $current_match . q{])},
                          Parse::RecDescent::_tracefirst($text))
                    if defined $::RD_TRACE;
        push @item, $item{__PATTERN1__}=$current_match;
        

        Parse::RecDescent::_trace(q{>>Matched production: [/\\d+/]<<},
                      Parse::RecDescent::_tracefirst($text),
                      q{NUMBER},
                      $tracelevel)
                        if defined $::RD_TRACE;



        $_matched = 1;
        last;
    }


    unless ( $_matched || defined($score) )
    {
        

        $_[1] = $text;  # NOT SURE THIS IS NEEDED
        Parse::RecDescent::_trace(q{<<Didn't match rule>>},
                     Parse::RecDescent::_tracefirst($_[1]),
                     q{NUMBER},
                     $tracelevel)
                    if defined $::RD_TRACE;
        return undef;
    }
    if (!defined($return) && defined($score))
    {
        Parse::RecDescent::_trace(q{>>Accepted scored production<<}, "",
                      q{NUMBER},
                      $tracelevel)
                        if defined $::RD_TRACE;
        $return = $score_return;
    }
    splice @{$thisparser->{errors}}, $err_at;
    $return = $item[$#item] unless defined $return;
    if (defined $::RD_TRACE)
    {
        Parse::RecDescent::_trace(q{>>Matched rule<< (return value: [} .
                      $return . q{])}, "",
                      q{NUMBER},
                      $tracelevel);
        Parse::RecDescent::_trace(q{(consumed: [} .
                      Parse::RecDescent::_tracemax(substr($_[1],0,-length($text))) . q{])},
                      Parse::RecDescent::_tracefirst($text),
                      , q{NUMBER},
                      $tracelevel)
    }
    $_[1] = $text;
    return $return;
}

# ARGS ARE: ($parser, $text; $repeating, $_noactions, \@args, $_itempos)
sub Parse::RecDescent::Mail::IMAPClient::Thread::threadmember
{
	my $thisparser = $_[0];
	use vars q{$tracelevel};
	local $tracelevel = ($tracelevel||0)+1;
	$ERRORS = 0;
    my $thisrule = $thisparser->{"rules"}{"threadmember"};

    Parse::RecDescent::_trace(q{Trying rule: [threadmember]},
                  Parse::RecDescent::_tracefirst($_[1]),
                  q{threadmember},
                  $tracelevel)
                    if defined $::RD_TRACE;

    
    my $err_at = @{$thisparser->{errors}};

    my $score;
    my $score_return;
    my $_tok;
    my $return = undef;
    my $_matched=0;
    my $commit=0;
    my @item = ();
    my %item = ();
    my $repeating =  $_[2];
    my $_noactions = $_[3];
    my @arg =    defined $_[4] ? @{ &{$_[4]} } : ();
    my $_itempos = $_[5];
    my %arg =    ($#arg & 01) ? @arg : (@arg, undef);
    my $text;
    my $lastsep;
    my $current_match;
    my $expectation = new Parse::RecDescent::Expectation(q{NUMBER, or thread});
    $expectation->at($_[1]);
    
    my $thisline;
    tie $thisline, q{Parse::RecDescent::LineCounter}, \$text, $thisparser;

    

    while (!$_matched && !$commit)
    {
        
        Parse::RecDescent::_trace(q{Trying production: [NUMBER]},
                      Parse::RecDescent::_tracefirst($_[1]),
                      q{threadmember},
                      $tracelevel)
                        if defined $::RD_TRACE;
        my $thisprod = $thisrule->{"prods"}[0];
        $text = $_[1];
        my $_savetext;
        @item = (q{threadmember});
        %item = (__RULE__ => q{threadmember});
        my $repcount = 0;


        Parse::RecDescent::_trace(q{Trying subrule: [NUMBER]},
                  Parse::RecDescent::_tracefirst($text),
                  q{threadmember},
                  $tracelevel)
                    if defined $::RD_TRACE;
        if (1) { no strict qw{refs};
        $expectation->is(q{})->at($text);
        unless (defined ($_tok = Parse::RecDescent::Mail::IMAPClient::Thread::NUMBER($thisparser,$text,$repeating,$_noactions,sub { \@arg },undef)))
        {
            
            Parse::RecDescent::_trace(q{<<Didn't match subrule: [NUMBER]>>},
                          Parse::RecDescent::_tracefirst($text),
                          q{threadmember},
                          $tracelevel)
                            if defined $::RD_TRACE;
            $expectation->failed();
            last;
        }
        Parse::RecDescent::_trace(q{>>Matched subrule: [NUMBER]<< (return value: [}
                    . $_tok . q{]},

                      Parse::RecDescent::_tracefirst($text),
                      q{threadmember},
                      $tracelevel)
                        if defined $::RD_TRACE;
        $item{q{NUMBER}} = $_tok;
        push @item, $_tok;
        
        }

        Parse::RecDescent::_trace(q{Trying action},
                      Parse::RecDescent::_tracefirst($text),
                      q{threadmember},
                      $tracelevel)
                        if defined $::RD_TRACE;
        

        $_tok = ($_noactions) ? 0 : do { $return = $item{NUMBER} ; };
        unless (defined $_tok)
        {
            Parse::RecDescent::_trace(q{<<Didn't match action>> (return value: [undef])})
                    if defined $::RD_TRACE;
            last;
        }
        Parse::RecDescent::_trace(q{>>Matched action<< (return value: [}
                      . $_tok . q{])},
                      Parse::RecDescent::_tracefirst($text))
                        if defined $::RD_TRACE;
        push @item, $_tok;
        $item{__ACTION1__}=$_tok;
        

        Parse::RecDescent::_trace(q{>>Matched production: [NUMBER]<<},
                      Parse::RecDescent::_tracefirst($text),
                      q{threadmember},
                      $tracelevel)
                        if defined $::RD_TRACE;



        $_matched = 1;
        last;
    }


    while (!$_matched && !$commit)
    {
        
        Parse::RecDescent::_trace(q{Trying production: [thread]},
                      Parse::RecDescent::_tracefirst($_[1]),
                      q{threadmember},
                      $tracelevel)
                        if defined $::RD_TRACE;
        my $thisprod = $thisrule->{"prods"}[1];
        $text = $_[1];
        my $_savetext;
        @item = (q{threadmember});
        %item = (__RULE__ => q{threadmember});
        my $repcount = 0;


        Parse::RecDescent::_trace(q{Trying subrule: [thread]},
                  Parse::RecDescent::_tracefirst($text),
                  q{threadmember},
                  $tracelevel)
                    if defined $::RD_TRACE;
        if (1) { no strict qw{refs};
        $expectation->is(q{})->at($text);
        unless (defined ($_tok = Parse::RecDescent::Mail::IMAPClient::Thread::thread($thisparser,$text,$repeating,$_noactions,sub { \@arg },undef)))
        {
            
            Parse::RecDescent::_trace(q{<<Didn't match subrule: [thread]>>},
                          Parse::RecDescent::_tracefirst($text),
                          q{threadmember},
                          $tracelevel)
                            if defined $::RD_TRACE;
            $expectation->failed();
            last;
        }
        Parse::RecDescent::_trace(q{>>Matched subrule: [thread]<< (return value: [}
                    . $_tok . q{]},

                      Parse::RecDescent::_tracefirst($text),
                      q{threadmember},
                      $tracelevel)
                        if defined $::RD_TRACE;
        $item{q{thread}} = $_tok;
        push @item, $_tok;
        
        }

        Parse::RecDescent::_trace(q{Trying action},
                      Parse::RecDescent::_tracefirst($text),
                      q{threadmember},
                      $tracelevel)
                        if defined $::RD_TRACE;
        

        $_tok = ($_noactions) ? 0 : do { $return = $item{thread} ; };
        unless (defined $_tok)
        {
            Parse::RecDescent::_trace(q{<<Didn't match action>> (return value: [undef])})
                    if defined $::RD_TRACE;
            last;
        }
        Parse::RecDescent::_trace(q{>>Matched action<< (return value: [}
                      . $_tok . q{])},
                      Parse::RecDescent::_tracefirst($text))
                        if defined $::RD_TRACE;
        push @item, $_tok;
        $item{__ACTION1__}=$_tok;
        

        Parse::RecDescent::_trace(q{>>Matched production: [thread]<<},
                      Parse::RecDescent::_tracefirst($text),
                      q{threadmember},
                      $tracelevel)
                        if defined $::RD_TRACE;



        $_matched = 1;
        last;
    }


    unless ( $_matched || defined($score) )
    {
        

        $_[1] = $text;  # NOT SURE THIS IS NEEDED
        Parse::RecDescent::_trace(q{<<Didn't match rule>>},
                     Parse::RecDescent::_tracefirst($_[1]),
                     q{threadmember},
                     $tracelevel)
                    if defined $::RD_TRACE;
        return undef;
    }
    if (!defined($return) && defined($score))
    {
        Parse::RecDescent::_trace(q{>>Accepted scored production<<}, "",
                      q{threadmember},
                      $tracelevel)
                        if defined $::RD_TRACE;
        $return = $score_return;
    }
    splice @{$thisparser->{errors}}, $err_at;
    $return = $item[$#item] unless defined $return;
    if (defined $::RD_TRACE)
    {
        Parse::RecDescent::_trace(q{>>Matched rule<< (return value: [} .
                      $return . q{])}, "",
                      q{threadmember},
                      $tracelevel);
        Parse::RecDescent::_trace(q{(consumed: [} .
                      Parse::RecDescent::_tracemax(substr($_[1],0,-length($text))) . q{])},
                      Parse::RecDescent::_tracefirst($text),
                      , q{threadmember},
                      $tracelevel)
    }
    $_[1] = $text;
    return $return;
}

# ARGS ARE: ($parser, $text; $repeating, $_noactions, \@args, $_itempos)
sub Parse::RecDescent::Mail::IMAPClient::Thread::thread
{
	my $thisparser = $_[0];
	use vars q{$tracelevel};
	local $tracelevel = ($tracelevel||0)+1;
	$ERRORS = 0;
    my $thisrule = $thisparser->{"rules"}{"thread"};

    Parse::RecDescent::_trace(q{Trying rule: [thread]},
                  Parse::RecDescent::_tracefirst($_[1]),
                  q{thread},
                  $tracelevel)
                    if defined $::RD_TRACE;

    
    my $err_at = @{$thisparser->{errors}};

    my $score;
    my $score_return;
    my $_tok;
    my $return = undef;
    my $_matched=0;
    my $commit=0;
    my @item = ();
    my %item = ();
    my $repeating =  $_[2];
    my $_noactions = $_[3];
    my @arg =    defined $_[4] ? @{ &{$_[4]} } : ();
    my $_itempos = $_[5];
    my %arg =    ($#arg & 01) ? @arg : (@arg, undef);
    my $text;
    my $lastsep;
    my $current_match;
    my $expectation = new Parse::RecDescent::Expectation(q{'('});
    $expectation->at($_[1]);
    
    my $thisline;
    tie $thisline, q{Parse::RecDescent::LineCounter}, \$text, $thisparser;

    

    while (!$_matched && !$commit)
    {
        
        Parse::RecDescent::_trace(q{Trying production: ['(' threadmember ')']},
                      Parse::RecDescent::_tracefirst($_[1]),
                      q{thread},
                      $tracelevel)
                        if defined $::RD_TRACE;
        my $thisprod = $thisrule->{"prods"}[0];
        $text = $_[1];
        my $_savetext;
        @item = (q{thread});
        %item = (__RULE__ => q{thread});
        my $repcount = 0;


        Parse::RecDescent::_trace(q{Trying terminal: ['(']},
                      Parse::RecDescent::_tracefirst($text),
                      q{thread},
                      $tracelevel)
                        if defined $::RD_TRACE;
        undef $lastsep;
        $expectation->is(q{})->at($text);
        

        unless ($text =~ s/\A($skip)/$lastsep=$1 and ""/e and   do { $_tok = "("; 1 } and
             substr($text,0,length($_tok)) eq $_tok and
             do { substr($text,0,length($_tok)) = ""; 1; }
        )
        {
            $text = $lastsep . $text if defined $lastsep;
            
            $expectation->failed();
            Parse::RecDescent::_trace(q{<<Didn't match terminal>>},
                          Parse::RecDescent::_tracefirst($text))
                            if defined $::RD_TRACE;
            last;
        }
        Parse::RecDescent::_trace(q{>>Matched terminal<< (return value: [}
                        . $_tok . q{])},
                          Parse::RecDescent::_tracefirst($text))
                            if defined $::RD_TRACE;
        push @item, $item{__STRING1__}=$_tok;
        

        Parse::RecDescent::_trace(q{Trying repeated subrule: [threadmember]},
                  Parse::RecDescent::_tracefirst($text),
                  q{thread},
                  $tracelevel)
                    if defined $::RD_TRACE;
        $expectation->is(q{threadmember})->at($text);
        
        unless (defined ($_tok = $thisparser->_parserepeat($text, \&Parse::RecDescent::Mail::IMAPClient::Thread::threadmember, 1, 100000000, $_noactions,$expectation,sub { \@arg },undef)))
        {
            Parse::RecDescent::_trace(q{<<Didn't match repeated subrule: [threadmember]>>},
                          Parse::RecDescent::_tracefirst($text),
                          q{thread},
                          $tracelevel)
                            if defined $::RD_TRACE;
            last;
        }
        Parse::RecDescent::_trace(q{>>Matched repeated subrule: [threadmember]<< (}
                    . @$_tok . q{ times)},

                      Parse::RecDescent::_tracefirst($text),
                      q{thread},
                      $tracelevel)
                        if defined $::RD_TRACE;
        $item{q{threadmember(s)}} = $_tok;
        push @item, $_tok;
        


        Parse::RecDescent::_trace(q{Trying terminal: [')']},
                      Parse::RecDescent::_tracefirst($text),
                      q{thread},
                      $tracelevel)
                        if defined $::RD_TRACE;
        undef $lastsep;
        $expectation->is(q{')'})->at($text);
        

        unless ($text =~ s/\A($skip)/$lastsep=$1 and ""/e and   do { $_tok = ")"; 1 } and
             substr($text,0,length($_tok)) eq $_tok and
             do { substr($text,0,length($_tok)) = ""; 1; }
        )
        {
            $text = $lastsep . $text if defined $lastsep;
            
            $expectation->failed();
            Parse::RecDescent::_trace(q{<<Didn't match terminal>>},
                          Parse::RecDescent::_tracefirst($text))
                            if defined $::RD_TRACE;
            last;
        }
        Parse::RecDescent::_trace(q{>>Matched terminal<< (return value: [}
                        . $_tok . q{])},
                          Parse::RecDescent::_tracefirst($text))
                            if defined $::RD_TRACE;
        push @item, $item{__STRING2__}=$_tok;
        

        Parse::RecDescent::_trace(q{Trying action},
                      Parse::RecDescent::_tracefirst($text),
                      q{thread},
                      $tracelevel)
                        if defined $::RD_TRACE;
        

        $_tok = ($_noactions) ? 0 : do {
		$return = $item{'threadmember(s)'}||undef;	
	};
        unless (defined $_tok)
        {
            Parse::RecDescent::_trace(q{<<Didn't match action>> (return value: [undef])})
                    if defined $::RD_TRACE;
            last;
        }
        Parse::RecDescent::_trace(q{>>Matched action<< (return value: [}
                      . $_tok . q{])},
                      Parse::RecDescent::_tracefirst($text))
                        if defined $::RD_TRACE;
        push @item, $_tok;
        $item{__ACTION1__}=$_tok;
        

        Parse::RecDescent::_trace(q{>>Matched production: ['(' threadmember ')']<<},
                      Parse::RecDescent::_tracefirst($text),
                      q{thread},
                      $tracelevel)
                        if defined $::RD_TRACE;



        $_matched = 1;
        last;
    }


    unless ( $_matched || defined($score) )
    {
        

        $_[1] = $text;  # NOT SURE THIS IS NEEDED
        Parse::RecDescent::_trace(q{<<Didn't match rule>>},
                     Parse::RecDescent::_tracefirst($_[1]),
                     q{thread},
                     $tracelevel)
                    if defined $::RD_TRACE;
        return undef;
    }
    if (!defined($return) && defined($score))
    {
        Parse::RecDescent::_trace(q{>>Accepted scored production<<}, "",
                      q{thread},
                      $tracelevel)
                        if defined $::RD_TRACE;
        $return = $score_return;
    }
    splice @{$thisparser->{errors}}, $err_at;
    $return = $item[$#item] unless defined $return;
    if (defined $::RD_TRACE)
    {
        Parse::RecDescent::_trace(q{>>Matched rule<< (return value: [} .
                      $return . q{])}, "",
                      q{thread},
                      $tracelevel);
        Parse::RecDescent::_trace(q{(consumed: [} .
                      Parse::RecDescent::_tracemax(substr($_[1],0,-length($text))) . q{])},
                      Parse::RecDescent::_tracefirst($text),
                      , q{thread},
                      $tracelevel)
    }
    $_[1] = $text;
    return $return;
}
}
package Mail::IMAPClient::Thread; sub new { my $self = bless( {
                 '_AUTOTREE' => undef,
                 'namespace' => 'Parse::RecDescent::Mail::IMAPClient::Thread',
                 '_AUTOACTION' => undef,
                 'localvars' => '',
                 'rules' => {
                              'start' => bless( {
                                                  'impcount' => 0,
                                                  'name' => 'start',
                                                  'prods' => [
                                                               bless( {
                                                                        'uncommit' => undef,
                                                                        'patcount' => 1,
                                                                        'actcount' => 1,
                                                                        'number' => 0,
                                                                        'line' => undef,
                                                                        'dircount' => 0,
                                                                        'error' => undef,
                                                                        'strcount' => 0,
                                                                        'items' => [
                                                                                     bless( {
                                                                                              'pattern' => '^\\* THREAD ',
                                                                                              'ldelim' => '/',
                                                                                              'lookahead' => 0,
                                                                                              'hashname' => '__PATTERN1__',
                                                                                              'description' => '/^\\\\* THREAD /i',
                                                                                              'mod' => 'i',
                                                                                              'rdelim' => '/',
                                                                                              'line' => 16
                                                                                            }, 'Parse::RecDescent::Token' ),
                                                                                     bless( {
                                                                                              'lookahead' => 0,
                                                                                              'argcode' => undef,
                                                                                              'min' => 0,
                                                                                              'subrule' => 'thread',
                                                                                              'matchrule' => 0,
                                                                                              'repspec' => 's?',
                                                                                              'line' => 16,
                                                                                              'expected' => undef,
                                                                                              'max' => 100000000
                                                                                            }, 'Parse::RecDescent::Repetition' ),
                                                                                     bless( {
                                                                                              'code' => '{
	$return=$item{\'thread(s?)\'}||undef;
}',
                                                                                              'hashname' => '__ACTION1__',
                                                                                              'line' => 16,
                                                                                              'lookahead' => 0
                                                                                            }, 'Parse::RecDescent::Action' )
                                                                                   ]
                                                                      }, 'Parse::RecDescent::Production' )
                                                             ],
                                                  'calls' => [
                                                               'thread'
                                                             ],
                                                  'changed' => 0,
                                                  'opcount' => 0,
                                                  'vars' => '',
                                                  'line' => 15
                                                }, 'Parse::RecDescent::Rule' ),
                              'NUMBER' => bless( {
                                                   'prods' => [
                                                                bless( {
                                                                         'number' => 0,
                                                                         'line' => undef,
                                                                         'actcount' => 0,
                                                                         'patcount' => 1,
                                                                         'uncommit' => undef,
                                                                         'dircount' => 0,
                                                                         'strcount' => 0,
                                                                         'items' => [
                                                                                      bless( {
                                                                                               'hashname' => '__PATTERN1__',
                                                                                               'description' => '/\\\\d+/',
                                                                                               'mod' => '',
                                                                                               'rdelim' => '/',
                                                                                               'line' => 3,
                                                                                               'ldelim' => '/',
                                                                                               'pattern' => '\\d+',
                                                                                               'lookahead' => 0
                                                                                             }, 'Parse::RecDescent::Token' )
                                                                                    ],
                                                                         'error' => undef
                                                                       }, 'Parse::RecDescent::Production' )
                                                              ],
                                                   'calls' => [],
                                                   'line' => 1,
                                                   'vars' => '',
                                                   'opcount' => 0,
                                                   'changed' => 0,
                                                   'impcount' => 0,
                                                   'name' => 'NUMBER'
                                                 }, 'Parse::RecDescent::Rule' ),
                              'threadmember' => bless( {
                                                         'impcount' => 0,
                                                         'name' => 'threadmember',
                                                         'calls' => [
                                                                      'NUMBER',
                                                                      'thread'
                                                                    ],
                                                         'prods' => [
                                                                      bless( {
                                                                               'line' => undef,
                                                                               'number' => 0,
                                                                               'actcount' => 1,
                                                                               'patcount' => 0,
                                                                               'uncommit' => undef,
                                                                               'strcount' => 0,
                                                                               'items' => [
                                                                                            bless( {
                                                                                                     'line' => 7,
                                                                                                     'lookahead' => 0,
                                                                                                     'argcode' => undef,
                                                                                                     'implicit' => undef,
                                                                                                     'subrule' => 'NUMBER',
                                                                                                     'matchrule' => 0
                                                                                                   }, 'Parse::RecDescent::Subrule' ),
                                                                                            bless( {
                                                                                                     'hashname' => '__ACTION1__',
                                                                                                     'code' => '{ $return = $item{NUMBER} ; }',
                                                                                                     'lookahead' => 0,
                                                                                                     'line' => 7
                                                                                                   }, 'Parse::RecDescent::Action' )
                                                                                          ],
                                                                               'error' => undef,
                                                                               'dircount' => 0
                                                                             }, 'Parse::RecDescent::Production' ),
                                                                      bless( {
                                                                               'uncommit' => undef,
                                                                               'patcount' => 0,
                                                                               'number' => 1,
                                                                               'actcount' => 1,
                                                                               'line' => 7,
                                                                               'error' => undef,
                                                                               'strcount' => 0,
                                                                               'items' => [
                                                                                            bless( {
                                                                                                     'subrule' => 'thread',
                                                                                                     'matchrule' => 0,
                                                                                                     'implicit' => undef,
                                                                                                     'argcode' => undef,
                                                                                                     'lookahead' => 0,
                                                                                                     'line' => 8
                                                                                                   }, 'Parse::RecDescent::Subrule' ),
                                                                                            bless( {
                                                                                                     'line' => 8,
                                                                                                     'lookahead' => 0,
                                                                                                     'code' => '{ $return = $item{thread} ; }',
                                                                                                     'hashname' => '__ACTION1__'
                                                                                                   }, 'Parse::RecDescent::Action' )
                                                                                          ],
                                                                               'dircount' => 0
                                                                             }, 'Parse::RecDescent::Production' )
                                                                    ],
                                                         'line' => 5,
                                                         'vars' => '',
                                                         'opcount' => 0,
                                                         'changed' => 0
                                                       }, 'Parse::RecDescent::Rule' ),
                              'thread' => bless( {
                                                   'changed' => 0,
                                                   'opcount' => 0,
                                                   'vars' => '',
                                                   'line' => 10,
                                                   'calls' => [
                                                                'threadmember'
                                                              ],
                                                   'prods' => [
                                                                bless( {
                                                                         'items' => [
                                                                                      bless( {
                                                                                               'hashname' => '__STRING1__',
                                                                                               'pattern' => '(',
                                                                                               'description' => '\'(\'',
                                                                                               'lookahead' => 0,
                                                                                               'line' => 10
                                                                                             }, 'Parse::RecDescent::InterpLit' ),
                                                                                      bless( {
                                                                                               'argcode' => undef,
                                                                                               'lookahead' => 0,
                                                                                               'min' => 1,
                                                                                               'matchrule' => 0,
                                                                                               'subrule' => 'threadmember',
                                                                                               'repspec' => 's',
                                                                                               'line' => 10,
                                                                                               'expected' => undef,
                                                                                               'max' => 100000000
                                                                                             }, 'Parse::RecDescent::Repetition' ),
                                                                                      bless( {
                                                                                               'hashname' => '__STRING2__',
                                                                                               'pattern' => ')',
                                                                                               'description' => '\')\'',
                                                                                               'lookahead' => 0,
                                                                                               'line' => 10
                                                                                             }, 'Parse::RecDescent::InterpLit' ),
                                                                                      bless( {
                                                                                               'line' => 11,
                                                                                               'lookahead' => 0,
                                                                                               'code' => '{
		$return = $item{\'threadmember(s)\'}||undef;	
	}',
                                                                                               'hashname' => '__ACTION1__'
                                                                                             }, 'Parse::RecDescent::Action' )
                                                                                    ],
                                                                         'strcount' => 2,
                                                                         'error' => undef,
                                                                         'dircount' => 0,
                                                                         'actcount' => 1,
                                                                         'number' => 0,
                                                                         'line' => undef,
                                                                         'patcount' => 0,
                                                                         'uncommit' => undef
                                                                       }, 'Parse::RecDescent::Production' )
                                                              ],
                                                   'name' => 'thread',
                                                   'impcount' => 0
                                                 }, 'Parse::RecDescent::Rule' )
                            },
                 '_check' => {
                               'itempos' => '',
                               'thisoffset' => '',
                               'thiscolumn' => '',
                               'prevcolumn' => '',
                               'prevline' => '',
                               'prevoffset' => ''
                             },
                 'startcode' => ''
               }, 'Parse::RecDescent' );
}