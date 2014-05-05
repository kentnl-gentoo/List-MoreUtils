package lmu_config_bundle;

use IPC::Cmd qw(run QUOTE);

sub check_module
{
    my ($mod, $ver) = @_;
    my $test_code = QUOTE . "$mod->VERSION($ver)" . QUOTE;
    ($ok, $err, $full_buf, $stdout_buff, $stderr_buff) = run( command => "$^X -M$mod -e $test_code");
    return $ok;
}

sub remaining_deps
{
    return {
  "Carp" => 0,
  "Config" => 0,
  "Cwd" => 0,
  "Exporter" => 0,
  "ExtUtils::CBuilder" => "0.23",
  "File::Basename" => 0,
  "File::Spec" => "3.00",
  "File::Temp" => 0,
  "IO::Handle" => 0,
  "Scalar::Util" => 0,
  "Text::ParseWords" => 0,
  "base" => 0,
  "if" => 0,
  "strict" => 0,
  "warnings" => 0
}

}

sub provided_bundle
{
    return {
  "Capture::Tiny" => "0.24",
  "Config::AutoConf" => "0.28",
  "Data::Tumbler" => "0.005",
  "Devel::InnerPackage" => "0.4",
  "Module::Pluggable" => "5.1",
  "Module::Pluggable::Object" => "5.1",
  "Test::WriteVariants" => "0.007",
  "Test::WriteVariants::Context" => 0
}

}

sub required_order
{
    return [
  "Devel::InnerPackage",
  "Module::Pluggable::Object",
  "Module::Pluggable",
  "Data::Tumbler",
  "Capture::Tiny",
  "Test::WriteVariants::Context",
  "Test::WriteVariants",
  "Config::AutoConf"
]

}

check_module("Devel::InnerPackage", "0") or do { eval <<'END_OF_EXTUTILS_BUNDLE_MAKER_MARKER';
package Devel::InnerPackage;

use strict;
use base qw(Exporter);
use vars qw($VERSION @EXPORT_OK);

use if $] > 5.017, 'deprecate';

$VERSION = '0.4';
@EXPORT_OK = qw(list_packages);

=pod

=head1 NAME

Devel::InnerPackage - find all the inner packages of a package

=head1 SYNOPSIS

    use Foo::Bar;
    use Devel::InnerPackage qw(list_packages);

    my @inner_packages = list_packages('Foo::Bar');


=head1 DESCRIPTION


Given a file like this


    package Foo::Bar;

    sub foo {}


    package Foo::Bar::Quux;

    sub quux {}

    package Foo::Bar::Quirka;

    sub quirka {}

    1;

then

    list_packages('Foo::Bar');

will return

    Foo::Bar::Quux
    Foo::Bar::Quirka

=head1 METHODS

=head2 list_packages <package name>

Return a list of all inner packages of that package.

=cut

sub list_packages {
            my $pack = shift; $pack .= "::" unless $pack =~ m!::$!;

            no strict 'refs';
            my @packs;
            my @stuff = grep !/^(main|)::$/, keys %{$pack};
            for my $cand (grep /::$/, @stuff)
            {
                $cand =~ s!::$!!;
                my @children = list_packages($pack.$cand);
    
                push @packs, "$pack$cand" unless $cand =~ /^::/ ||
                    !__PACKAGE__->_loaded($pack.$cand); # or @children;
                push @packs, @children;
            }
            return grep {$_ !~ /::(::ISA::CACHE|SUPER)/} @packs;
}

### XXX this is an inlining of the Class-Inspector->loaded()
### method, but inlined to remove the dependency.
sub _loaded {
       my ($class, $name) = @_;

        no strict 'refs';

       # Handle by far the two most common cases
       # This is very fast and handles 99% of cases.
       return 1 if defined ${"${name}::VERSION"};
       return 1 if @{"${name}::ISA"};

       # Are there any symbol table entries other than other namespaces
       foreach ( keys %{"${name}::"} ) {
               next if substr($_, -2, 2) eq '::';
               return 1 if defined &{"${name}::$_"};
       }

       # No functions, and it doesn't have a version, and isn't anything.
       # As an absolute last resort, check for an entry in %INC
       my $filename = join( '/', split /(?:'|::)/, $name ) . '.pm';
       return 1 if defined $INC{$filename};

       '';
}


=head1 AUTHOR

Simon Wistow <simon@thegestalt.org>

=head1 COPYING

Copyright, 2005 Simon Wistow

Distributed under the same terms as Perl itself.

=head1 BUGS

None known.

=cut 





1;

END_OF_EXTUTILS_BUNDLE_MAKER_MARKER

    $@ and die $@;
    defined $INC{'Devel/InnerPackage.pm'} or $INC{'Devel/InnerPackage.pm'} = 'Bundled';
};

check_module("Module::Pluggable::Object", "4.9") or do { eval <<'END_OF_EXTUTILS_BUNDLE_MAKER_MARKER';
package Module::Pluggable::Object;

use strict;
use File::Find ();
use File::Basename;
use File::Spec::Functions qw(splitdir catdir curdir catfile abs2rel);
use Carp qw(croak carp confess);
use Devel::InnerPackage;
use vars qw($VERSION);

use if $] > 5.017, 'deprecate';

$VERSION = '5.1';


sub new {
    my $class = shift;
    my %opts  = @_;

    return bless \%opts, $class;

}

### Eugggh, this code smells 
### This is what happens when you keep adding patches
### *sigh*


sub plugins {
    my $self = shift;
    my @args = @_;

    # override 'require'
    $self->{'require'} = 1 if $self->{'inner'};

    my $filename   = $self->{'filename'};
    my $pkg        = $self->{'package'};

    # Get the exception params instantiated
    $self->_setup_exceptions;

    # automatically turn a scalar search path or namespace into a arrayref
    for (qw(search_path search_dirs)) {
        $self->{$_} = [ $self->{$_} ] if exists $self->{$_} && !ref($self->{$_});
    }

    # default search path is '<Module>::<Name>::Plugin'
    $self->{'search_path'} ||= ["${pkg}::Plugin"]; 

    # default error handler
    $self->{'on_require_error'} ||= sub { my ($plugin, $err) = @_; carp "Couldn't require $plugin : $err"; return 0 };
    $self->{'on_instantiate_error'} ||= sub { my ($plugin, $err) = @_; carp "Couldn't instantiate $plugin: $err"; return 0 };

    # default whether to follow symlinks
    $self->{'follow_symlinks'} = 1 unless exists $self->{'follow_symlinks'};

    # check to see if we're running under test
    my @SEARCHDIR = exists $INC{"blib.pm"} && defined $filename && $filename =~ m!(^|/)blib/! && !$self->{'force_search_all_paths'} ? grep {/blib/} @INC : @INC;

    # add any search_dir params
    unshift @SEARCHDIR, @{$self->{'search_dirs'}} if defined $self->{'search_dirs'};

    # set our @INC up to include and prefer our search_dirs if necessary
    my @tmp = @INC;
    unshift @tmp, @{$self->{'search_dirs'} || []};
    local @INC = @tmp if defined $self->{'search_dirs'};

    my @plugins = $self->search_directories(@SEARCHDIR);
    push(@plugins, $self->handle_inc_hooks($_, @SEARCHDIR)) for @{$self->{'search_path'}};
    push(@plugins, $self->handle_innerpackages($_)) for @{$self->{'search_path'}};
    
    # return blank unless we've found anything
    return () unless @plugins;

    # remove duplicates
    # probably not necessary but hey ho
    my %plugins;
    for(@plugins) {
        next unless $self->_is_legit($_);
        $plugins{$_} = 1;
    }

    # are we instantiating or requiring?
    if (defined $self->{'instantiate'}) {
        my $method = $self->{'instantiate'};
        my @objs   = ();
        foreach my $package (sort keys %plugins) {
            next unless $package->can($method);
            my $obj = eval { $package->$method(@_) };
            $self->{'on_instantiate_error'}->($package, $@) if $@;
            push @objs, $obj if $obj;           
        }
        return @objs;
    } else { 
        # no? just return the names
        my @objs= sort keys %plugins;
        return @objs;
    }
}

sub _setup_exceptions {
    my $self = shift;

    my %only;   
    my %except; 
    my $only;
    my $except;

    if (defined $self->{'only'}) {
        if (ref($self->{'only'}) eq 'ARRAY') {
            %only   = map { $_ => 1 } @{$self->{'only'}};
        } elsif (ref($self->{'only'}) eq 'Regexp') {
            $only = $self->{'only'}
        } elsif (ref($self->{'only'}) eq '') {
            $only{$self->{'only'}} = 1;
        }
    }
        

    if (defined $self->{'except'}) {
        if (ref($self->{'except'}) eq 'ARRAY') {
            %except   = map { $_ => 1 } @{$self->{'except'}};
        } elsif (ref($self->{'except'}) eq 'Regexp') {
            $except = $self->{'except'}
        } elsif (ref($self->{'except'}) eq '') {
            $except{$self->{'except'}} = 1;
        }
    }
    $self->{_exceptions}->{only_hash}   = \%only;
    $self->{_exceptions}->{only}        = $only;
    $self->{_exceptions}->{except_hash} = \%except;
    $self->{_exceptions}->{except}      = $except;
        
}

sub _is_legit {
    my $self   = shift;
    my $plugin = shift;
    my %only   = %{$self->{_exceptions}->{only_hash}||{}};
    my %except = %{$self->{_exceptions}->{except_hash}||{}};
    my $only   = $self->{_exceptions}->{only};
    my $except = $self->{_exceptions}->{except};
    my $depth  = () = split '::', $plugin, -1;

    return 0 if     (keys %only   && !$only{$plugin}     );
    return 0 unless (!defined $only || $plugin =~ m!$only!     );

    return 0 if     (keys %except &&  $except{$plugin}   );
    return 0 if     (defined $except &&  $plugin =~ m!$except! );
    
    return 0 if     defined $self->{max_depth} && $depth>$self->{max_depth};
    return 0 if     defined $self->{min_depth} && $depth<$self->{min_depth};

    return 1;
}

sub search_directories {
    my $self      = shift;
    my @SEARCHDIR = @_;

    my @plugins;
    # go through our @INC
    foreach my $dir (@SEARCHDIR) {
        push @plugins, $self->search_paths($dir);
    }
    return @plugins;
}


sub search_paths {
    my $self = shift;
    my $dir  = shift;
    my @plugins;

    my $file_regex = $self->{'file_regex'} || qr/\.pm$/;


    # and each directory in our search path
    foreach my $searchpath (@{$self->{'search_path'}}) {
        # create the search directory in a cross platform goodness way
        my $sp = catdir($dir, (split /::/, $searchpath));

        # if it doesn't exist or it's not a dir then skip it
        next unless ( -e $sp && -d _ ); # Use the cached stat the second time

        my @files = $self->find_files($sp);

        # foreach one we've found 
        foreach my $file (@files) {
            # untaint the file; accept .pm only
            next unless ($file) = ($file =~ /(.*$file_regex)$/); 
            # parse the file to get the name
            my ($name, $directory, $suffix) = fileparse($file, $file_regex);

            next if (!$self->{include_editor_junk} && $self->_is_editor_junk($name));

            $directory = abs2rel($directory, $sp);

            # If we have a mixed-case package name, assume case has been preserved
            # correctly.  Otherwise, root through the file to locate the case-preserved
            # version of the package name.
            my @pkg_dirs = ();
            if ( $name eq lc($name) || $name eq uc($name) ) {
                my $pkg_file = catfile($sp, $directory, "$name$suffix");
                open PKGFILE, "<$pkg_file" or die "search_paths: Can't open $pkg_file: $!";
                my $in_pod = 0;
                while ( my $line = <PKGFILE> ) {
                    $in_pod = 1 if $line =~ m/^=\w/;
                    $in_pod = 0 if $line =~ /^=cut/;
                    next if ($in_pod || $line =~ /^=cut/);  # skip pod text
                    next if $line =~ /^\s*#/;               # and comments
                    if ( $line =~ m/^\s*package\s+(.*::)?($name)\s*;/i ) {
                        @pkg_dirs = split /::/, $1 if defined $1;;
                        $name = $2;
                        last;
                    }
                }
                close PKGFILE;
            }

            # then create the class name in a cross platform way
            $directory =~ s/^[a-z]://i if($^O =~ /MSWin32|dos/);       # remove volume
            my @dirs = ();
            if ($directory) {
                ($directory) = ($directory =~ /(.*)/);
                @dirs = grep(length($_), splitdir($directory)) 
                    unless $directory eq curdir();
                for my $d (reverse @dirs) {
                    my $pkg_dir = pop @pkg_dirs; 
                    last unless defined $pkg_dir;
                    $d =~ s/\Q$pkg_dir\E/$pkg_dir/i;  # Correct case
                }
            } else {
                $directory = "";
            }
            my $plugin = join '::', $searchpath, @dirs, $name;

            next unless $plugin =~ m!(?:[a-z\d]+)[a-z\d]*!i;

            $self->handle_finding_plugin($plugin, \@plugins)
        }

        # now add stuff that may have been in package
        # NOTE we should probably use all the stuff we've been given already
        # but then we can't unload it :(
        push @plugins, $self->handle_innerpackages($searchpath);
    } # foreach $searchpath

    return @plugins;
}

sub _is_editor_junk {
    my $self = shift;
    my $name = shift;

    # Emacs (and other Unix-y editors) leave temp files ending in a
    # tilde as a backup.
    return 1 if $name =~ /~$/;
    # Emacs makes these files while a buffer is edited but not yet
    # saved.
    return 1 if $name =~ /^\.#/;
    # Vim can leave these files behind if it crashes.
    return 1 if $name =~ /\.sw[po]$/;

    return 0;
}

sub handle_finding_plugin {
    my $self    = shift;
    my $plugin  = shift;
    my $plugins = shift;
    my $no_req  = shift || 0;
    
    return unless $self->_is_legit($plugin);
    unless (defined $self->{'instantiate'} || $self->{'require'}) {
        push @$plugins, $plugin;
        return;
    } 

    $self->{before_require}->($plugin) || return if defined $self->{before_require};
    unless ($no_req) {
        my $tmp = $@;
        my $res = eval { $self->_require($plugin) };
        my $err = $@;
        $@      = $tmp;
        if ($err) {
            if (defined $self->{on_require_error}) {
                $self->{on_require_error}->($plugin, $err) || return; 
            } else {
                return;
            }
        }
    }
    $self->{after_require}->($plugin) || return if defined $self->{after_require};
    push @$plugins, $plugin;
}

sub find_files {
    my $self         = shift;
    my $search_path  = shift;
    my $file_regex   = $self->{'file_regex'} || qr/\.pm$/;


    # find all the .pm files in it
    # this isn't perfect and won't find multiple plugins per file
    #my $cwd = Cwd::getcwd;
    my @files = ();
    { # for the benefit of perl 5.6.1's Find, localize topic
        local $_;
        File::Find::find( { no_chdir => 1, 
                            follow   => $self->{'follow_symlinks'}, 
                            wanted   => sub { 
                             # Inlined from File::Find::Rule C< name => '*.pm' >
                             return unless $File::Find::name =~ /$file_regex/;
                             (my $path = $File::Find::name) =~ s#^\\./##;
                             push @files, $path;
                           }
                      }, $search_path );
    }
    #chdir $cwd;
    return @files;

}

sub handle_inc_hooks {
    my $self      = shift;
    my $path      = shift;
    my @SEARCHDIR = @_;

    my @plugins;
    for my $dir ( @SEARCHDIR ) {
        next unless ref $dir && eval { $dir->can( 'files' ) };

        foreach my $plugin ( $dir->files ) {
            $plugin =~ s/\.pm$//;
            $plugin =~ s{/}{::}g;
            next unless $plugin =~ m!^${path}::!;
            $self->handle_finding_plugin( $plugin, \@plugins );
        }
    }
    return @plugins;
}

sub handle_innerpackages {
    my $self = shift;
    return () if (exists $self->{inner} && !$self->{inner});

    my $path = shift;
    my @plugins;

    foreach my $plugin (Devel::InnerPackage::list_packages($path)) {
        $self->handle_finding_plugin($plugin, \@plugins, 1);
    }
    return @plugins;

}


sub _require {
    my $self   = shift;
    my $pack   = shift;
    eval "CORE::require $pack";
    die ($@) if $@;
    return 1;
}


1;

=pod

=head1 NAME

Module::Pluggable::Object - automatically give your module the ability to have plugins

=head1 SYNOPSIS


Simple use Module::Pluggable -

    package MyClass;
    use Module::Pluggable::Object;
    
    my $finder = Module::Pluggable::Object->new(%opts);
    print "My plugins are: ".join(", ", $finder->plugins)."\n";

=head1 DESCRIPTION

Provides a simple but, hopefully, extensible way of having 'plugins' for 
your module. Obviously this isn't going to be the be all and end all of
solutions but it works for me.

Essentially all it does is export a method into your namespace that 
looks through a search path for .pm files and turn those into class names. 

Optionally it instantiates those classes for you.

This object is wrapped by C<Module::Pluggable>. If you want to do something
odd or add non-general special features you're probably best to wrap this
and produce your own subclass.

=head1 OPTIONS

See the C<Module::Pluggable> docs.

=head1 AUTHOR

Simon Wistow <simon@thegestalt.org>

=head1 COPYING

Copyright, 2006 Simon Wistow

Distributed under the same terms as Perl itself.

=head1 BUGS

None known.

=head1 SEE ALSO

L<Module::Pluggable>

=cut 


END_OF_EXTUTILS_BUNDLE_MAKER_MARKER

    $@ and die $@;
    defined $INC{'Module/Pluggable/Object.pm'} or $INC{'Module/Pluggable/Object.pm'} = 'Bundled';
};

check_module("Module::Pluggable", "0") or do { eval <<'END_OF_EXTUTILS_BUNDLE_MAKER_MARKER';
package Module::Pluggable;

use strict;
use vars qw($VERSION $FORCE_SEARCH_ALL_PATHS);
use Module::Pluggable::Object;

use if $] > 5.017, 'deprecate';

# ObQuote:
# Bob Porter: Looks like you've been missing a lot of work lately. 
# Peter Gibbons: I wouldn't say I've been missing it, Bob! 


$VERSION = '5.1';
$FORCE_SEARCH_ALL_PATHS = 0;

sub import {
    my $class        = shift;
    my %opts         = @_;

    my ($pkg, $file) = caller; 
    # the default name for the method is 'plugins'
    my $sub          = $opts{'sub_name'}  || 'plugins';
    # get our package 
    my ($package)    = $opts{'package'} || $pkg;
    $opts{filename}  = $file;
    $opts{package}   = $package;
    $opts{force_search_all_paths} = $FORCE_SEARCH_ALL_PATHS unless exists $opts{force_search_all_paths};


    my $finder       = Module::Pluggable::Object->new(%opts);
    my $subroutine   = sub { my $self = shift; return $finder->plugins(@_) };

    my $searchsub = sub {
              my $self = shift;
              my ($action,@paths) = @_;

              $finder->{'search_path'} = ["${package}::Plugin"] if ($action eq 'add'  and not   $finder->{'search_path'} );
              push @{$finder->{'search_path'}}, @paths      if ($action eq 'add');
              $finder->{'search_path'}       = \@paths      if ($action eq 'new');
              return $finder->{'search_path'};
    };


    my $onlysub = sub {
        my ($self, $only) = @_;

        if (defined $only) {
            $finder->{'only'} = $only;
        };
        
        return $finder->{'only'};
    };

    my $exceptsub = sub {
        my ($self, $except) = @_;

        if (defined $except) {
            $finder->{'except'} = $except;
        };
        
        return $finder->{'except'};
    };


    no strict 'refs';
    no warnings qw(redefine prototype);
    
    *{"$package\::$sub"}        = $subroutine;
    *{"$package\::search_path"} = $searchsub;
    *{"$package\::only"}        = $onlysub;
    *{"$package\::except"}      = $exceptsub;

}

1;

=pod

=head1 NAME

Module::Pluggable - automatically give your module the ability to have plugins

=head1 SYNOPSIS


Simple use Module::Pluggable -

    package MyClass;
    use Module::Pluggable;
    

and then later ...

    use MyClass;
    my $mc = MyClass->new();
    # returns the names of all plugins installed under MyClass::Plugin::*
    my @plugins = $mc->plugins(); 

=head1 EXAMPLE

Why would you want to do this? Say you have something that wants to pass an
object to a number of different plugins in turn. For example you may 
want to extract meta-data from every email you get sent and do something
with it. Plugins make sense here because then you can keep adding new 
meta data parsers and all the logic and docs for each one will be 
self contained and new handlers are easy to add without changing the 
core code. For that, you might do something like ...

    package Email::Examiner;

    use strict;
    use Email::Simple;
    use Module::Pluggable require => 1;

    sub handle_email {
        my $self  = shift;
        my $email = shift;

        foreach my $plugin ($self->plugins) {
            $plugin->examine($email);
        }

        return 1;
    }



.. and all the plugins will get a chance in turn to look at it.

This can be trivially extended so that plugins could save the email
somewhere and then no other plugin should try and do that. 
Simply have it so that the C<examine> method returns C<1> if 
it has saved the email somewhere. You might also want to be paranoid
and check to see if the plugin has an C<examine> method.

        foreach my $plugin ($self->plugins) {
            next unless $plugin->can('examine');
            last if     $plugin->examine($email);
        }


And so on. The sky's the limit.


=head1 DESCRIPTION

Provides a simple but, hopefully, extensible way of having 'plugins' for 
your module. Obviously this isn't going to be the be all and end all of
solutions but it works for me.

Essentially all it does is export a method into your namespace that 
looks through a search path for .pm files and turn those into class names. 

Optionally it instantiates those classes for you.

=head1 ADVANCED USAGE

Alternatively, if you don't want to use 'plugins' as the method ...

    package MyClass;
    use Module::Pluggable sub_name => 'foo';


and then later ...

    my @plugins = $mc->foo();


Or if you want to look in another namespace

    package MyClass;
    use Module::Pluggable search_path => ['Acme::MyClass::Plugin', 'MyClass::Extend'];

or directory 

    use Module::Pluggable search_dirs => ['mylibs/Foo'];


Or if you want to instantiate each plugin rather than just return the name

    package MyClass;
    use Module::Pluggable instantiate => 'new';

and then

    # whatever is passed to 'plugins' will be passed 
    # to 'new' for each plugin 
    my @plugins = $mc->plugins(@options); 


alternatively you can just require the module without instantiating it

    package MyClass;
    use Module::Pluggable require => 1;

since requiring automatically searches inner packages, which may not be desirable, you can turn this off


    package MyClass;
    use Module::Pluggable require => 1, inner => 0;


You can limit the plugins loaded using the except option, either as a string,
array ref or regex

    package MyClass;
    use Module::Pluggable except => 'MyClass::Plugin::Foo';

or

    package MyClass;
    use Module::Pluggable except => ['MyClass::Plugin::Foo', 'MyClass::Plugin::Bar'];

or

    package MyClass;
    use Module::Pluggable except => qr/^MyClass::Plugin::(Foo|Bar)$/;


and similarly for only which will only load plugins which match.

Remember you can use the module more than once

    package MyClass;
    use Module::Pluggable search_path => 'MyClass::Filters' sub_name => 'filters';
    use Module::Pluggable search_path => 'MyClass::Plugins' sub_name => 'plugins';

and then later ...

    my @filters = $self->filters;
    my @plugins = $self->plugins;
    
=head1 PLUGIN SEARCHING

Every time you call 'plugins' the whole search path is walked again. This allows 
for dynamically loading plugins even at run time. However this can get expensive 
and so if you don't expect to want to add new plugins at run time you could do


  package Foo;
  use strict;
  use Module::Pluggable sub_name => '_plugins';

  our @PLUGINS;
  sub plugins { @PLUGINS ||= shift->_plugins }
  1;

=head1 INNER PACKAGES

If you have, for example, a file B<lib/Something/Plugin/Foo.pm> that
contains package definitions for both C<Something::Plugin::Foo> and 
C<Something::Plugin::Bar> then as long as you either have either 
the B<require> or B<instantiate> option set then we'll also find 
C<Something::Plugin::Bar>. Nifty!

=head1 OPTIONS

You can pass a hash of options when importing this module.

The options can be ...

=head2 sub_name

The name of the subroutine to create in your namespace. 

By default this is 'plugins'

=head2 search_path

An array ref of namespaces to look in. 

=head2 search_dirs 

An array ref of directories to look in before @INC.

=head2 instantiate

Call this method on the class. In general this will probably be 'new'
but it can be whatever you want. Whatever arguments are passed to 'plugins' 
will be passed to the method.

The default is 'undef' i.e just return the class name.

=head2 require

Just require the class, don't instantiate (overrides 'instantiate');

=head2 inner

If set to 0 will B<not> search inner packages. 
If set to 1 will override C<require>.

=head2 only

Takes a string, array ref or regex describing the names of the only plugins to 
return. Whilst this may seem perverse ... well, it is. But it also 
makes sense. Trust me.

=head2 except

Similar to C<only> it takes a description of plugins to exclude 
from returning. This is slightly less perverse.

=head2 package

This is for use by extension modules which build on C<Module::Pluggable>:
passing a C<package> option allows you to place the plugin method in a
different package other than your own.

=head2 file_regex

By default C<Module::Pluggable> only looks for I<.pm> files.

By supplying a new C<file_regex> then you can change this behaviour e.g

    file_regex => qr/\.plugin$/

=head2 include_editor_junk

By default C<Module::Pluggable> ignores files that look like they were
left behind by editors. Currently this means files ending in F<~> (~),
the extensions F<.swp> or F<.swo>, or files beginning with F<.#>.

Setting C<include_editor_junk> changes C<Module::Pluggable> so it does
not ignore any files it finds.

=head2 follow_symlinks

Whether, when searching directories, to follow symlinks.

Defaults to 1 i.e do follow symlinks.

=head2 min_depth, max_depth

This will allow you to set what 'depth' of plugin will be allowed.

So, for example, C<MyClass::Plugin::Foo> will have a depth of 3 and 
C<MyClass::Plugin::Foo::Bar> will have a depth of 4 so to only get the former 
(i.e C<MyClass::Plugin::Foo>) do

        package MyClass;
        use Module::Pluggable max_depth => 3;
        
and to only get the latter (i.e C<MyClass::Plugin::Foo::Bar>)

        package MyClass;
        use Module::Pluggable min_depth => 4;


=head1 TRIGGERS

Various triggers can also be passed in to the options.

If any of these triggers return 0 then the plugin will not be returned.

=head2 before_require <plugin>

Gets passed the plugin name. 

If 0 is returned then this plugin will not be required either.

=head2 on_require_error <plugin> <err>

Gets called when there's an error on requiring the plugin.

Gets passed the plugin name and the error. 

The default on_require_error handler is to C<carp> the error and return 0.

=head2 on_instantiate_error <plugin> <err>

Gets called when there's an error on instantiating the plugin.

Gets passed the plugin name and the error. 

The default on_instantiate_error handler is to C<carp> the error and return 0.

=head2 after_require <plugin>

Gets passed the plugin name. 

If 0 is returned then this plugin will be required but not returned as a plugin.

=head1 METHODs

=head2 search_path

The method C<search_path> is exported into you namespace as well. 
You can call that at any time to change or replace the 
search_path.

    $self->search_path( add => "New::Path" ); # add
    $self->search_path( new => "New::Path" ); # replace

=head1 BEHAVIOUR UNDER TEST ENVIRONMENT

In order to make testing reliable we exclude anything not from blib if blib.pm is 
in %INC. 

However if the module being tested used another module that itself used C<Module::Pluggable> 
then the second module would fail. This was fixed by checking to see if the caller 
had (^|/)blib/ in their filename.

There's an argument that this is the wrong behaviour and that modules should explicitly
trigger this behaviour but that particular code has been around for 7 years now and I'm 
reluctant to change the default behaviour.

You can now (as of version 4.1) force Module::Pluggable to look outside blib in a test environment by doing either

        require Module::Pluggable;
        $Module::Pluggable::FORCE_SEARCH_ALL_PATHS = 1;
        import Module::Pluggable;

or

        use Module::Pluggable force_search_all_paths => 1;

=head1 @INC hooks and App::FatPacker

If a module's @INC has a hook and that hook is an object which has a C<files()> method then we will
try and require those files too. See C<t/26inc_hook.t> for an example.

This has allowed L<App::FatPacker> (as of version 0.10.0) to provide support for Module::Pluggable.

This should also, theoretically, allow someone to modify PAR to do the same thing.

=head1 FUTURE PLANS

This does everything I need and I can't really think of any other 
features I want to add. Famous last words of course (not least 
because we're up to version 5.0 at the time of writing).

However suggestions (and patches) are always welcome.

=head1 DEVELOPMENT

The master repo for this module is at

https://github.com/simonwistow/Module-Pluggable

=head1 AUTHOR

Simon Wistow <simon@thegestalt.org>

=head1 COPYING

Copyright, 2006 Simon Wistow

Distributed under the same terms as Perl itself.

=head1 BUGS

None known.

=head1 SEE ALSO

L<File::Spec>, L<File::Find>, L<File::Basename>, L<Class::Factory::Util>, L<Module::Pluggable::Ordered>

=cut 



END_OF_EXTUTILS_BUNDLE_MAKER_MARKER

    $@ and die $@;
    defined $INC{'Module/Pluggable.pm'} or $INC{'Module/Pluggable.pm'} = 'Bundled';
};

check_module("Data::Tumbler", "0.002") or do { eval <<'END_OF_EXTUTILS_BUNDLE_MAKER_MARKER';
package Data::Tumbler;

use strict;
use warnings;

=head1 NAME

Data::Tumbler - Dynamic generation of nested combinations of variants

=head1 SYNOPSIS

    $tumbler = Data::Tumbler->new(

        add_path => sub {
            my ($path, $name) = @_;
            return [ @$path, $name ];
        },

        add_context => sub {
            my ($context, $value) = @_;
            return [ @$context, $value ]
        },

        consumer  => sub {
            my ($path, $context, $payload) = @_;
            print "@$path: @$context\n";
        },
    );

    $tumbler->tumble(
        [   # provider code refs
            sub { (red => 42, green => 24, mauve => 19) },
            sub { (circle => 1, square => 2) },
            # ...
        ],
        [], # initial path
        [], # initial context
        [], # initial payload
    );

The consumer code outputs:

    green circle: 24 1
    green square: 24 2
    mauve circle: 19 1
    mauve square: 19 2
    red circle: 42 1
    red square: 42 2

Here's a longer example showing more features:

    use List::Util qw(sum);

    $tumbler = Data::Tumbler->new(

        # The default add_path is as shown above
        # The default add_context is as shown above

        consumer  => sub {
            my ($path, $context, $payload) = @_;
            printf "path: %-20s  context: %-12s  payload: %s\n",
                join("/",  @$path),
                join(", ", @$context),
                join(", ", map { "$_=>$payload->{$_}" } sort keys %$payload);
        },
    );

    $tumbler->tumble(
        [   # providers
            sub {
                my ($path, $context, $payload) = @_;

                my %variants = (red => 42, green => 24, mauve => 19);

                return %variants;
            },
            sub {
                my ($path, $context, $payload) = @_;

                # change paint to matt based on context
                $payload->{paint} = 'matt' if sum(@$context) > 20;

                my %variants = (circle => 10, square => 20);

                # add an extra triangular variant for mauve
                $variants{triangle} = 13 if grep { $_ eq 'mauve' } @$path;

                return %variants;
            },
            sub {
                my ($path, $context, $payload) = @_;

                # skip all variants if path contains anything red or circular
                return if grep { $_ eq 'red' or $_ eq 'circle' } @$path;

                $payload->{spotty} = 1 if sum(@$context) > 35;

                my %variants = (small => 17, large => 92);

                return %variants;
            },
            # ...
        ],
        [], # initial path
        [], # initial context
        { paint => 'gloss' }, # initial payload
    );

The consumer code outputs:

    path: green/square/large    context: 24, 20, 92    payload: paint=>matt, spotty=>1
    path: green/square/small    context: 24, 20, 17    payload: paint=>matt, spotty=>1
    path: mauve/square/large    context: 19, 20, 92    payload: paint=>gloss, spotty=>1
    path: mauve/square/small    context: 19, 20, 17    payload: paint=>gloss, spotty=>1
    path: mauve/triangle/large  context: 19, 13, 92    payload: paint=>gloss
    path: mauve/triangle/small  context: 19, 13, 17    payload: paint=>gloss

=head1 DESCRIPTION

NOTE: This is alpha code and liable to change while it and L<Test::WriteVariants>
mature.

The tumble() method calls a sequence of 'provider' code references each of
which returns a hash.  The first provider is called and then, for each hash
item it returns, the tumble() method recurses to call the next provider.

The recursion continues until there are no more providers to call, at which
point the consumer code reference is called.  Effectively the providers create
a tree of combinations and the consumer is called at the leafs of the tree.

If a provider returns no items then that part of the tree is pruned. Further
providers, if any, are not called and the consumer is not called.

During a call to tumble() three values are passed down through the tree and
into the consumer: path, context, and payload.

The path and context are derived from the names and values of the hashes
returned by the providers. Typically the path define the current "path"
through the tree of combinations.

The providers are passed the current path, context, and payload.
The payload is cloned at each level of recursion so that any changes made to it
by providers are only visible within the scope of the generated sub-tree.

Note that although the example above shows the path, context and payload as
array references, the tumbler code makes no assumptions about them. They can be
any kinds of values.

See L<Test::WriteVariants> for a practical example use.

=head1 ATTRIBUTES

=head2 consumer

    $tumbler->consumer( sub { my ($path, $context, $payload) = @_; ... } );

Defines the code reference to call at the leafs of the generated tree of combinations.
The default is to throw an exception.

=head2 add_path

    $tumbler->add_path( sub { my ($path, $name) = @_; return [ @$path, $name ] } )

Defines the code reference to call to create a new path value that combines
the existing path and the new name. The default is shown in the example above.


=head2 add_context

    $tumbler->add_context( sub { my ($context, $value) = @_; return [ @$context, $value ] } )

Defines the code reference to call to create a new context value that combines
the existing context and the new value. The default is shown in the example above.

=cut

use Storable qw(dclone);
use Carp qw(confess);

our $VERSION = '0.005';


sub new {
    my ($class, %args) = @_;

    my %defaults = (
        consumer    => sub { confess "No Data::Tumbler consumer defined" },
        add_path    => sub { my ($path,    $name ) = @_; return [ @$path,    $name  ] },
        add_context => sub { my ($context, $value) = @_; return [ @$context, $value ] },
    );
    my $self = bless \%defaults => $class;

    for my $attribute (qw(consumer add_path add_context)) {
        next unless exists $args{$attribute};
        $self->$attribute(delete $args{$attribute});
    }
    confess "Unknown $class arguments: @{[ keys %args ]}"
        if %args;

    return $self;
}


sub consumer {
    my $self = shift;
    $self->{consumer} = shift if @_;
    return $self->{consumer};
}

sub add_path {
    my $self = shift;
    $self->{add_path} = shift if @_;
    return $self->{add_path};
}

sub add_context {
    my $self = shift;
    $self->{add_context} = shift if @_;
    return $self->{add_context};
}


sub tumble {
    my ($self, $providers, $path, $context, $payload) = @_;

    if (not @$providers) { # no more providers in this context
        $self->consumer->($path, $context, $payload);
        return;
    }

    # clone the $payload so the provider can alter it for the consumer
    # at and below this point in the tree of variants
    $payload = dclone($payload) if ref $payload;

    my ($current_provider, @remaining_providers) = @$providers;

    # call the current provider to supply the variants for this context
    # returns empty if the consumer shouldn't be called in the current context
    # returns a single (possibly nil/empty/dummy) variant if there are
    # no actual variations needed.
    my %variants = $current_provider->($path, $context, $payload);

    # for each variant in turn, call the next level of provider
    # with the name and value of the variant appended to the
    # path and context.

    for my $name (sort keys %variants) {

        $self->tumble(
            \@remaining_providers,
            $self->add_path->($path,  $name),
            $self->add_context->($context, $variants{$name}),
            $payload,
        );
    }

    return;
}

1;

__END__

=head1 LICENSE

This program is free software; you can redistribute it and/or modify
it under the terms of either:

        a) the GNU General Public License as published by the Free
        Software Foundation; either version 1, or (at your option) any
        later version, or

        b) the "Artistic License" which comes with this Kit.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See either
the GNU General Public License or the Artistic License for more details.

=cut

END_OF_EXTUTILS_BUNDLE_MAKER_MARKER

    $@ and die $@;
    defined $INC{'Data/Tumbler.pm'} or $INC{'Data/Tumbler.pm'} = 'Bundled';
};

check_module("Capture::Tiny", "0") or do { eval <<'END_OF_EXTUTILS_BUNDLE_MAKER_MARKER';
use 5.006;
use strict;
use warnings;
package Capture::Tiny;
# ABSTRACT: Capture STDOUT and STDERR from Perl, XS or external programs
our $VERSION = '0.24'; # VERSION
use Carp ();
use Exporter ();
use IO::Handle ();
use File::Spec ();
use File::Temp qw/tempfile tmpnam/;
use Scalar::Util qw/reftype blessed/;
# Get PerlIO or fake it
BEGIN {
  local $@;
  eval { require PerlIO; PerlIO->can('get_layers') }
    or *PerlIO::get_layers = sub { return () };
}

#--------------------------------------------------------------------------#
# create API subroutines and export them
# [do STDOUT flag, do STDERR flag, do merge flag, do tee flag]
#--------------------------------------------------------------------------#

my %api = (
  capture         => [1,1,0,0],
  capture_stdout  => [1,0,0,0],
  capture_stderr  => [0,1,0,0],
  capture_merged  => [1,1,1,0],
  tee             => [1,1,0,1],
  tee_stdout      => [1,0,0,1],
  tee_stderr      => [0,1,0,1],
  tee_merged      => [1,1,1,1],
);

for my $sub ( keys %api ) {
  my $args = join q{, }, @{$api{$sub}};
  eval "sub $sub(&;@) {unshift \@_, $args; goto \\&_capture_tee;}"; ## no critic
}

our @ISA = qw/Exporter/;
our @EXPORT_OK = keys %api;
our %EXPORT_TAGS = ( 'all' => \@EXPORT_OK );

#--------------------------------------------------------------------------#
# constants and fixtures
#--------------------------------------------------------------------------#

my $IS_WIN32 = $^O eq 'MSWin32';

##our $DEBUG = $ENV{PERL_CAPTURE_TINY_DEBUG};
##
##my $DEBUGFH;
##open $DEBUGFH, "> DEBUG" if $DEBUG;
##
##*_debug = $DEBUG ? sub(@) { print {$DEBUGFH} @_ } : sub(){0};

our $TIMEOUT = 30;

#--------------------------------------------------------------------------#
# command to tee output -- the argument is a filename that must
# be opened to signal that the process is ready to receive input.
# This is annoying, but seems to be the best that can be done
# as a simple, portable IPC technique
#--------------------------------------------------------------------------#
my @cmd = ($^X, '-C0', '-e', <<'HERE');
use Fcntl;
$SIG{HUP}=sub{exit};
if ( my $fn=shift ) {
    sysopen(my $fh, qq{$fn}, O_WRONLY|O_CREAT|O_EXCL) or die $!;
    print {$fh} $$;
    close $fh;
}
my $buf; while (sysread(STDIN, $buf, 2048)) {
    syswrite(STDOUT, $buf); syswrite(STDERR, $buf);
}
HERE

#--------------------------------------------------------------------------#
# filehandle manipulation
#--------------------------------------------------------------------------#

sub _relayer {
  my ($fh, $layers) = @_;
  # _debug("# requested layers (@{$layers}) for @{[fileno $fh]}\n");
  my %seen = ( unix => 1, perlio => 1 ); # filter these out
  my @unique = grep { !$seen{$_}++ } @$layers;
  # _debug("# applying unique layers (@unique) to @{[fileno $fh]}\n");
  binmode($fh, join(":", ":raw", @unique));
}

sub _name {
  my $glob = shift;
  no strict 'refs'; ## no critic
  return *{$glob}{NAME};
}

sub _open {
  open $_[0], $_[1] or Carp::confess "Error from open(" . join(q{, }, @_) . "): $!";
  # _debug( "# open " . join( ", " , map { defined $_ ? _name($_) : 'undef' } @_ ) . " as " . fileno( $_[0] ) . "\n" );
}

sub _close {
  # _debug( "# closing " . ( defined $_[0] ? _name($_[0]) : 'undef' )  . " on " . fileno( $_[0] ) . "\n" );
  close $_[0] or Carp::confess "Error from close(" . join(q{, }, @_) . "): $!";
}

my %dup; # cache this so STDIN stays fd0
my %proxy_count;
sub _proxy_std {
  my %proxies;
  if ( ! defined fileno STDIN ) {
    $proxy_count{stdin}++;
    if (defined $dup{stdin}) {
      _open \*STDIN, "<&=" . fileno($dup{stdin});
      # _debug( "# restored proxy STDIN as " . (defined fileno STDIN ? fileno STDIN : 'undef' ) . "\n" );
    }
    else {
      _open \*STDIN, "<" . File::Spec->devnull;
      # _debug( "# proxied STDIN as " . (defined fileno STDIN ? fileno STDIN : 'undef' ) . "\n" );
      _open $dup{stdin} = IO::Handle->new, "<&=STDIN";
    }
    $proxies{stdin} = \*STDIN;
    binmode(STDIN, ':utf8') if $] >= 5.008; ## no critic
  }
  if ( ! defined fileno STDOUT ) {
    $proxy_count{stdout}++;
    if (defined $dup{stdout}) {
      _open \*STDOUT, ">&=" . fileno($dup{stdout});
      # _debug( "# restored proxy STDOUT as " . (defined fileno STDOUT ? fileno STDOUT : 'undef' ) . "\n" );
    }
    else {
      _open \*STDOUT, ">" . File::Spec->devnull;
       # _debug( "# proxied STDOUT as " . (defined fileno STDOUT ? fileno STDOUT : 'undef' ) . "\n" );
      _open $dup{stdout} = IO::Handle->new, ">&=STDOUT";
    }
    $proxies{stdout} = \*STDOUT;
    binmode(STDOUT, ':utf8') if $] >= 5.008; ## no critic
  }
  if ( ! defined fileno STDERR ) {
    $proxy_count{stderr}++;
    if (defined $dup{stderr}) {
      _open \*STDERR, ">&=" . fileno($dup{stderr});
       # _debug( "# restored proxy STDERR as " . (defined fileno STDERR ? fileno STDERR : 'undef' ) . "\n" );
    }
    else {
      _open \*STDERR, ">" . File::Spec->devnull;
       # _debug( "# proxied STDERR as " . (defined fileno STDERR ? fileno STDERR : 'undef' ) . "\n" );
      _open $dup{stderr} = IO::Handle->new, ">&=STDERR";
    }
    $proxies{stderr} = \*STDERR;
    binmode(STDERR, ':utf8') if $] >= 5.008; ## no critic
  }
  return %proxies;
}

sub _unproxy {
  my (%proxies) = @_;
  # _debug( "# unproxying: " . join(" ", keys %proxies) . "\n" );
  for my $p ( keys %proxies ) {
    $proxy_count{$p}--;
    # _debug( "# unproxied " . uc($p) . " ($proxy_count{$p} left)\n" );
    if ( ! $proxy_count{$p} ) {
      _close $proxies{$p};
      _close $dup{$p} unless $] < 5.008; # 5.6 will have already closed this as dup
      delete $dup{$p};
    }
  }
}

sub _copy_std {
  my %handles;
  for my $h ( qw/stdout stderr stdin/ ) {
    next if $h eq 'stdin' && ! $IS_WIN32; # WIN32 hangs on tee without STDIN copied
    my $redir = $h eq 'stdin' ? "<&" : ">&";
    _open $handles{$h} = IO::Handle->new(), $redir . uc($h); # ">&STDOUT" or "<&STDIN"
  }
  return \%handles;
}

# In some cases we open all (prior to forking) and in others we only open
# the output handles (setting up redirection)
sub _open_std {
  my ($handles) = @_;
  _open \*STDIN, "<&" . fileno $handles->{stdin} if defined $handles->{stdin};
  _open \*STDOUT, ">&" . fileno $handles->{stdout} if defined $handles->{stdout};
  _open \*STDERR, ">&" . fileno $handles->{stderr} if defined $handles->{stderr};
}

#--------------------------------------------------------------------------#
# private subs
#--------------------------------------------------------------------------#

sub _start_tee {
  my ($which, $stash) = @_; # $which is "stdout" or "stderr"
  # setup pipes
  $stash->{$_}{$which} = IO::Handle->new for qw/tee reader/;
  pipe $stash->{reader}{$which}, $stash->{tee}{$which};
  # _debug( "# pipe for $which\: " .  _name($stash->{tee}{$which}) . " " . fileno( $stash->{tee}{$which} ) . " => " . _name($stash->{reader}{$which}) . " " . fileno( $stash->{reader}{$which}) . "\n" );
  select((select($stash->{tee}{$which}), $|=1)[0]); # autoflush
  # setup desired redirection for parent and child
  $stash->{new}{$which} = $stash->{tee}{$which};
  $stash->{child}{$which} = {
    stdin   => $stash->{reader}{$which},
    stdout  => $stash->{old}{$which},
    stderr  => $stash->{capture}{$which},
  };
  # flag file is used to signal the child is ready
  $stash->{flag_files}{$which} = scalar tmpnam();
  # execute @cmd as a separate process
  if ( $IS_WIN32 ) {
    local $@;
    eval "use Win32API::File qw/CloseHandle GetOsFHandle SetHandleInformation fileLastError HANDLE_FLAG_INHERIT INVALID_HANDLE_VALUE/ ";
    # _debug( "# Win32API::File loaded\n") unless $@;
    my $os_fhandle = GetOsFHandle( $stash->{tee}{$which} );
    # _debug( "# Couldn't get OS handle: " . fileLastError() . "\n") if ! defined $os_fhandle || $os_fhandle == INVALID_HANDLE_VALUE();
    my $result = SetHandleInformation( $os_fhandle, HANDLE_FLAG_INHERIT(), 0);
    # _debug( $result ? "# set no-inherit flag on $which tee\n" : ("# can't disable tee handle flag inherit: " . fileLastError() . "\n"));
    _open_std( $stash->{child}{$which} );
    $stash->{pid}{$which} = system(1, @cmd, $stash->{flag_files}{$which});
    # not restoring std here as it all gets redirected again shortly anyway
  }
  else { # use fork
    _fork_exec( $which, $stash );
  }
}

sub _fork_exec {
  my ($which, $stash) = @_; # $which is "stdout" or "stderr"
  my $pid = fork;
  if ( not defined $pid ) {
    Carp::confess "Couldn't fork(): $!";
  }
  elsif ($pid == 0) { # child
    # _debug( "# in child process ...\n" );
    untie *STDIN; untie *STDOUT; untie *STDERR;
    _close $stash->{tee}{$which};
    # _debug( "# redirecting handles in child ...\n" );
    _open_std( $stash->{child}{$which} );
    # _debug( "# calling exec on command ...\n" );
    exec @cmd, $stash->{flag_files}{$which};
  }
  $stash->{pid}{$which} = $pid
}

my $have_usleep = eval "use Time::HiRes 'usleep'; 1";
sub _files_exist {
  return 1 if @_ == grep { -f } @_;
  Time::HiRes::usleep(1000) if $have_usleep;
  return 0;
}

sub _wait_for_tees {
  my ($stash) = @_;
  my $start = time;
  my @files = values %{$stash->{flag_files}};
  my $timeout = defined $ENV{PERL_CAPTURE_TINY_TIMEOUT}
              ? $ENV{PERL_CAPTURE_TINY_TIMEOUT} : $TIMEOUT;
  1 until _files_exist(@files) || ($timeout && (time - $start > $timeout));
  Carp::confess "Timed out waiting for subprocesses to start" if ! _files_exist(@files);
  unlink $_ for @files;
}

sub _kill_tees {
  my ($stash) = @_;
  if ( $IS_WIN32 ) {
    # _debug( "# closing handles with CloseHandle\n");
    CloseHandle( GetOsFHandle($_) ) for values %{ $stash->{tee} };
    # _debug( "# waiting for subprocesses to finish\n");
    my $start = time;
    1 until wait == -1 || (time - $start > 30);
  }
  else {
    _close $_ for values %{ $stash->{tee} };
    waitpid $_, 0 for values %{ $stash->{pid} };
  }
}

sub _slurp {
  my ($name, $stash) = @_;
  my ($fh, $pos) = map { $stash->{$_}{$name} } qw/capture pos/;
  # _debug( "# slurping captured $name from " . fileno($fh) . " at pos $pos with layers: @{[PerlIO::get_layers($fh)]}\n");
  seek( $fh, $pos, 0 ) or die "Couldn't seek on capture handle for $name\n";
  my $text = do { local $/; scalar readline $fh };
  return defined($text) ? $text : "";
}

#--------------------------------------------------------------------------#
# _capture_tee() -- generic main sub for capturing or teeing
#--------------------------------------------------------------------------#

sub _capture_tee {
  # _debug( "# starting _capture_tee with (@_)...\n" );
  my ($do_stdout, $do_stderr, $do_merge, $do_tee, $code, @opts) = @_;
  my %do = ($do_stdout ? (stdout => 1) : (),  $do_stderr ? (stderr => 1) : ());
  Carp::confess("Custom capture options must be given as key/value pairs\n")
    unless @opts % 2 == 0;
  my $stash = { capture => { @opts } };
  for ( keys %{$stash->{capture}} ) {
    my $fh = $stash->{capture}{$_};
    Carp::confess "Custom handle for $_ must be seekable\n"
      unless ref($fh) eq 'GLOB' || (blessed($fh) && $fh->isa("IO::Seekable"));
  }
  # save existing filehandles and setup captures
  local *CT_ORIG_STDIN  = *STDIN ;
  local *CT_ORIG_STDOUT = *STDOUT;
  local *CT_ORIG_STDERR = *STDERR;
  # find initial layers
  my %layers = (
    stdin   => [PerlIO::get_layers(\*STDIN) ],
    stdout  => [PerlIO::get_layers(\*STDOUT, output => 1)],
    stderr  => [PerlIO::get_layers(\*STDERR, output => 1)],
  );
  # _debug( "# existing layers for $_\: @{$layers{$_}}\n" ) for qw/stdin stdout stderr/;
  # get layers from underlying glob of tied filehandles if we can
  # (this only works for things that work like Tie::StdHandle)
  $layers{stdout} = [PerlIO::get_layers(tied *STDOUT)]
    if tied(*STDOUT) && (reftype tied *STDOUT eq 'GLOB');
  $layers{stderr} = [PerlIO::get_layers(tied *STDERR)]
    if tied(*STDERR) && (reftype tied *STDERR eq 'GLOB');
  # _debug( "# tied object corrected layers for $_\: @{$layers{$_}}\n" ) for qw/stdin stdout stderr/;
  # bypass scalar filehandles and tied handles
  # localize scalar STDIN to get a proxy to pick up FD0, then restore later to CT_ORIG_STDIN
  my %localize;
  $localize{stdin}++,  local(*STDIN)
    if grep { $_ eq 'scalar' } @{$layers{stdin}};
  $localize{stdout}++, local(*STDOUT)
    if $do_stdout && grep { $_ eq 'scalar' } @{$layers{stdout}};
  $localize{stderr}++, local(*STDERR)
    if ($do_stderr || $do_merge) && grep { $_ eq 'scalar' } @{$layers{stderr}};
  $localize{stdin}++, local(*STDIN), _open( \*STDIN, "<&=0")
    if tied *STDIN && $] >= 5.008;
  $localize{stdout}++, local(*STDOUT), _open( \*STDOUT, ">&=1")
    if $do_stdout && tied *STDOUT && $] >= 5.008;
  $localize{stderr}++, local(*STDERR), _open( \*STDERR, ">&=2")
    if ($do_stderr || $do_merge) && tied *STDERR && $] >= 5.008;
  # _debug( "# localized $_\n" ) for keys %localize;
  # proxy any closed/localized handles so we don't use fds 0, 1 or 2
  my %proxy_std = _proxy_std();
  # _debug( "# proxy std: @{ [%proxy_std] }\n" );
  # update layers after any proxying
  $layers{stdout} = [PerlIO::get_layers(\*STDOUT, output => 1)] if $proxy_std{stdout};
  $layers{stderr} = [PerlIO::get_layers(\*STDERR, output => 1)] if $proxy_std{stderr};
  # _debug( "# post-proxy layers for $_\: @{$layers{$_}}\n" ) for qw/stdin stdout stderr/;
  # store old handles and setup handles for capture
  $stash->{old} = _copy_std();
  $stash->{new} = { %{$stash->{old}} }; # default to originals
  for ( keys %do ) {
    $stash->{new}{$_} = ($stash->{capture}{$_} ||= File::Temp->new);
    seek( $stash->{capture}{$_}, 0, 2 ) or die "Could not seek on capture handle for $_\n";
    $stash->{pos}{$_} = tell $stash->{capture}{$_};
    # _debug("# will capture $_ on " . fileno($stash->{capture}{$_})."\n" );
    _start_tee( $_ => $stash ) if $do_tee; # tees may change $stash->{new}
  }
  _wait_for_tees( $stash ) if $do_tee;
  # finalize redirection
  $stash->{new}{stderr} = $stash->{new}{stdout} if $do_merge;
  # _debug( "# redirecting in parent ...\n" );
  _open_std( $stash->{new} );
  # execute user provided code
  my ($exit_code, $inner_error, $outer_error, @result);
  {
    local *STDIN = *CT_ORIG_STDIN if $localize{stdin}; # get original, not proxy STDIN
    # _debug( "# finalizing layers ...\n" );
    _relayer(\*STDOUT, $layers{stdout}) if $do_stdout;
    _relayer(\*STDERR, $layers{stderr}) if $do_stderr;
    # _debug( "# running code $code ...\n" );
    local $@;
    eval { @result = $code->(); $inner_error = $@ };
    $exit_code = $?; # save this for later
    $outer_error = $@; # save this for later
  }
  # restore prior filehandles and shut down tees
  # _debug( "# restoring filehandles ...\n" );
  _open_std( $stash->{old} );
  _close( $_ ) for values %{$stash->{old}}; # don't leak fds
  # shouldn't need relayering originals, but see rt.perl.org #114404
  _relayer(\*STDOUT, $layers{stdout}) if $do_stdout;
  _relayer(\*STDERR, $layers{stderr}) if $do_stderr;
  _unproxy( %proxy_std );
  # _debug( "# killing tee subprocesses ...\n" ) if $do_tee;
  _kill_tees( $stash ) if $do_tee;
  # return captured output, but shortcut in void context
  # unless we have to echo output to tied/scalar handles;
  my %got;
  if ( defined wantarray or ($do_tee && keys %localize) ) {
    for ( keys %do ) {
      _relayer($stash->{capture}{$_}, $layers{$_});
      $got{$_} = _slurp($_, $stash);
      # _debug("# slurped " . length($got{$_}) . " bytes from $_\n");
    }
    print CT_ORIG_STDOUT $got{stdout}
      if $do_stdout && $do_tee && $localize{stdout};
    print CT_ORIG_STDERR $got{stderr}
      if $do_stderr && $do_tee && $localize{stderr};
  }
  $? = $exit_code;
  $@ = $inner_error if $inner_error;
  die $outer_error if $outer_error;
  # _debug( "# ending _capture_tee with (@_)...\n" );
  return unless defined wantarray;
  my @return;
  push @return, $got{stdout} if $do_stdout;
  push @return, $got{stderr} if $do_stderr && ! $do_merge;
  push @return, @result;
  return wantarray ? @return : $return[0];
}

1;

__END__

=pod

=encoding UTF-8

=head1 NAME

Capture::Tiny - Capture STDOUT and STDERR from Perl, XS or external programs

=head1 VERSION

version 0.24

=head1 SYNOPSIS

   use Capture::Tiny ':all';
 
   # capture from external command
 
   ($stdout, $stderr, $exit) = capture {
     system( $cmd, @args );
   };
 
   # capture from arbitrary code (Perl or external)
 
   ($stdout, $stderr, @result) = capture {
     # your code here
   };
 
   # capture partial or merged output
 
   $stdout = capture_stdout { ... };
   $stderr = capture_stderr { ... };
   $merged = capture_merged { ... };
 
   # tee output
 
   ($stdout, $stderr) = tee {
     # your code here
   };
 
   $stdout = tee_stdout { ... };
   $stderr = tee_stderr { ... };
   $merged = tee_merged { ... };

=head1 DESCRIPTION

Capture::Tiny provides a simple, portable way to capture almost anything sent
to STDOUT or STDERR, regardless of whether it comes from Perl, from XS code or
from an external program.  Optionally, output can be teed so that it is
captured while being passed through to the original filehandles.  Yes, it even
works on Windows (usually).  Stop guessing which of a dozen capturing modules
to use in any particular situation and just use this one.

=head1 USAGE

The following functions are available.  None are exported by default.

=head2 capture

   ($stdout, $stderr, @result) = capture \&code;
   $stdout = capture \&code;

The C<<< capture >>> function takes a code reference and returns what is sent to
STDOUT and STDERR as well as any return values from the code reference.  In
scalar context, it returns only STDOUT.  If no output was received for a
filehandle, it returns an empty string for that filehandle.  Regardless of calling
context, all output is captured -- nothing is passed to the existing filehandles.

It is prototyped to take a subroutine reference as an argument. Thus, it
can be called in block form:

   ($stdout, $stderr) = capture {
     # your code here ...
   };

Note that the coderef is evaluated in list context.  If you wish to force
scalar context on the return value, you must use the C<<< scalar >>> keyword.

   ($stdout, $stderr, $count) = capture {
     my @list = qw/one two three/;
     return scalar @list; # $count will be 3
   };

Also note that within the coderef, the C<<< @_ >>> variable will be empty.  So don't
use arguments from a surrounding subroutine without copying them to an array
first:

   sub wont_work {
     my ($stdout, $stderr) = capture { do_stuff( @_ ) };    # WRONG
     ...
   }
 
   sub will_work {
     my @args = @_;
     my ($stdout, $stderr) = capture { do_stuff( @args ) }; # RIGHT
     ...
   }

Captures are normally done to an anonymous temporary filehandle.  To
capture via a named file (e.g. to externally monitor a long-running capture),
provide custom filehandles as a trailing list of option pairs:

   my $out_fh = IO::File->new("out.txt", "w+");
   my $err_fh = IO::File->new("out.txt", "w+");
   capture { ... } stdout => $out_fh, stderr => $err_fh;

The filehandles must be readE<sol>write and seekable.  Modifying the files or
filehandles during a capture operation will give unpredictable results.
Existing IO layers on them may be changed by the capture.

When called in void context, C<<< capture >>> saves memory and time by
not reading back from the capture handles.

=head2 capture_stdout

   ($stdout, @result) = capture_stdout \&code;
   $stdout = capture_stdout \&code;

The C<<< capture_stdout >>> function works just like C<<< capture >>> except only
STDOUT is captured.  STDERR is not captured.

=head2 capture_stderr

   ($stderr, @result) = capture_stderr \&code;
   $stderr = capture_stderr \&code;

The C<<< capture_stderr >>> function works just like C<<< capture >>> except only
STDERR is captured.  STDOUT is not captured.

=head2 capture_merged

   ($merged, @result) = capture_merged \&code;
   $merged = capture_merged \&code;

The C<<< capture_merged >>> function works just like C<<< capture >>> except STDOUT and
STDERR are merged. (Technically, STDERR is redirected to the same capturing
handle as STDOUT before executing the function.)

Caution: STDOUT and STDERR output in the merged result are not guaranteed to be
properly ordered due to buffering.

=head2 tee

   ($stdout, $stderr, @result) = tee \&code;
   $stdout = tee \&code;

The C<<< tee >>> function works just like C<<< capture >>>, except that output is captured
as well as passed on to the original STDOUT and STDERR.

When called in void context, C<<< tee >>> saves memory and time by
not reading back from the capture handles, except when the
original STDOUT OR STDERR were tied or opened to a scalar
handle.

=head2 tee_stdout

   ($stdout, @result) = tee_stdout \&code;
   $stdout = tee_stdout \&code;

The C<<< tee_stdout >>> function works just like C<<< tee >>> except only
STDOUT is teed.  STDERR is not teed (output goes to STDERR as usual).

=head2 tee_stderr

   ($stderr, @result) = tee_stderr \&code;
   $stderr = tee_stderr \&code;

The C<<< tee_stderr >>> function works just like C<<< tee >>> except only
STDERR is teed.  STDOUT is not teed (output goes to STDOUT as usual).

=head2 tee_merged

   ($merged, @result) = tee_merged \&code;
   $merged = tee_merged \&code;

The C<<< tee_merged >>> function works just like C<<< capture_merged >>> except that output
is captured as well as passed on to STDOUT.

Caution: STDOUT and STDERR output in the merged result are not guaranteed to be
properly ordered due to buffering.

=head1 LIMITATIONS

=head2 Portability

Portability is a goal, not a guarantee.  C<<< tee >>> requires fork, except on
Windows where C<<< system(1, @cmd) >>> is used instead.  Not tested on any
particularly esoteric platforms yet.  See the
L<CPAN Testers Matrix|http://matrix.cpantesters.org/?dist=Capture-Tiny>
for test result by platform.

=head2 PerlIO layers

Capture::Tiny does it's best to preserve PerlIO layers such as ':utf8' or
':crlf' when capturing (only for Perl 5.8.1+) .  Layers should be applied to
STDOUT or STDERR I<before> the call to C<<< capture >>> or C<<< tee >>>.  This may not work
for tied filehandles (see below).

=head2 Modifying filehandles before capturing

Generally speaking, you should do little or no manipulation of the standard IO
filehandles prior to using Capture::Tiny.  In particular, closing, reopening,
localizing or tying standard filehandles prior to capture may cause a variety of
unexpected, undesirable andE<sol>or unreliable behaviors, as described below.
Capture::Tiny does its best to compensate for these situations, but the
results may not be what you desire.

B<Closed filehandles>

Capture::Tiny will work even if STDIN, STDOUT or STDERR have been previously
closed.  However, since they will be reopened to capture or tee output, any
code within the captured block that depends on finding them closed will, of
course, not find them to be closed.  If they started closed, Capture::Tiny will
close them again when the capture block finishes.

Note that this reopening will happen even for STDIN or a filehandle not being
captured to ensure that the filehandle used for capture is not opened to file
descriptor 0, as this causes problems on various platforms.

Prior to Perl 5.12, closed STDIN combined with PERL_UNICODE=D leaks filehandles
and also breaks tee() for undiagnosed reasons.  So don't do that.

B<Localized filehandles>

If code localizes any of Perl's standard filehandles before capturing, the capture
will affect the localized filehandles and not the original ones.  External system
calls are not affected by localizing a filehandle in Perl and will continue
to send output to the original filehandles (which will thus not be captured).

B<Scalar filehandles>

If STDOUT or STDERR are reopened to scalar filehandles prior to the call to
C<<< capture >>> or C<<< tee >>>, then Capture::Tiny will override the output filehandle for
the duration of the C<<< capture >>> or C<<< tee >>> call and then, for C<<< tee >>>, send captured
output to the output filehandle after the capture is complete.  (Requires Perl
5.8)

Capture::Tiny attempts to preserve the semantics of STDIN opened to a scalar
reference, but note that external processes will not be able to read from such
a handle.  Capture::Tiny tries to ensure that external processes will read from
the null device instead, but this is not guaranteed.

B<Tied output filehandles>

If STDOUT or STDERR are tied prior to the call to C<<< capture >>> or C<<< tee >>>, then
Capture::Tiny will attempt to override the tie for the duration of the
C<<< capture >>> or C<<< tee >>> call and then send captured output to the tied filehandle after
the capture is complete.  (Requires Perl 5.8)

Capture::Tiny may not succeed resending UTF-8 encoded data to a tied
STDOUT or STDERR filehandle.  Characters may appear as bytes.  If the tied filehandle
is based on L<Tie::StdHandle>, then Capture::Tiny will attempt to determine
appropriate layers like C<<< :utf8 >>> from the underlying filehandle and do the right
thing.

B<Tied input filehandle>

Capture::Tiny attempts to preserve the semantics of tied STDIN, but this
requires Perl 5.8 and is not entirely predictable.  External processes
will not be able to read from such a handle.

Unless having STDIN tied is crucial, it may be safest to localize STDIN when
capturing:

   my ($out, $err) = do { local *STDIN; capture { ... } };

=head2 Modifying filehandles during a capture

Attempting to modify STDIN, STDOUT or STDERR I<during> C<<< capture >>> or C<<< tee >>> is
almost certainly going to cause problems.  Don't do that.

=head2 No support for Perl 5.8.0

It's just too buggy when it comes to layers and UTF-8.  Perl 5.8.1 or later
is recommended.

=head2 Limited support for Perl 5.6

Perl 5.6 predates PerlIO.  UTF-8 data may not be captured correctly.

=head1 ENVIRONMENT

=head2 PERL_CAPTURE_TINY_TIMEOUT

Capture::Tiny uses subprocesses for C<<< tee >>>.  By default, Capture::Tiny will
timeout with an error if the subprocesses are not ready to receive data within
30 seconds (or whatever is the value of C<<< $Capture::Tiny::TIMEOUT >>>).  An
alternate timeout may be specified by setting the C<<< PERL_CAPTURE_TINY_TIMEOUT >>>
environment variable.  Setting it to zero will disable timeouts.

=head1 SEE ALSO

This module was, inspired by L<IO::CaptureOutput>, which provides
similar functionality without the ability to tee output and with more
complicated code and API.  L<IO::CaptureOutput> does not handle layers
or most of the unusual cases described in the L</Limitations> section and
I no longer recommend it.

There are many other CPAN modules that provide some sort of output capture,
albeit with various limitations that make them appropriate only in particular
circumstances.  I'm probably missing some.  The long list is provided to show
why I felt Capture::Tiny was necessary.

=over

=item *

L<IO::Capture>

=item *

L<IO::Capture::Extended>

=item *

L<IO::CaptureOutput>

=item *

L<IPC::Capture>

=item *

L<IPC::Cmd>

=item *

L<IPC::Open2>

=item *

L<IPC::Open3>

=item *

L<IPC::Open3::Simple>

=item *

L<IPC::Open3::Utils>

=item *

L<IPC::Run>

=item *

L<IPC::Run::SafeHandles>

=item *

L<IPC::Run::Simple>

=item *

L<IPC::Run3>

=item *

L<IPC::System::Simple>

=item *

L<Tee>

=item *

L<IO::Tee>

=item *

L<File::Tee>

=item *

L<Filter::Handle>

=item *

L<Tie::STDERR>

=item *

L<Tie::STDOUT>

=item *

L<Test::Output>

=back

=for :stopwords cpan testmatrix url annocpan anno bugtracker rt cpants kwalitee diff irc mailto metadata placeholders metacpan

=head1 SUPPORT

=head2 Bugs / Feature Requests

Please report any bugs or feature requests through the issue tracker
at L<https://github.com/dagolden/Capture-Tiny/issues>.
You will be notified automatically of any progress on your issue.

=head2 Source Code

This is open source software.  The code repository is available for
public review and contribution under the terms of the license.

L<https://github.com/dagolden/Capture-Tiny>

  git clone https://github.com/dagolden/Capture-Tiny.git

=head1 AUTHOR

David Golden <dagolden@cpan.org>

=head1 CONTRIBUTOR

Dagfinn Ilmari Mannsåker <ilmari@ilmari.org>

=head1 COPYRIGHT AND LICENSE

This software is Copyright (c) 2009 by David Golden.

This is free software, licensed under:

  The Apache License, Version 2.0, January 2004

=cut

END_OF_EXTUTILS_BUNDLE_MAKER_MARKER

    $@ and die $@;
    defined $INC{'Capture/Tiny.pm'} or $INC{'Capture/Tiny.pm'} = 'Bundled';
};

check_module("Test::WriteVariants::Context", "0") or do { eval <<'END_OF_EXTUTILS_BUNDLE_MAKER_MARKER';
package Test::WriteVariants::Context;

use strict;

my $ContextClass = __PACKAGE__;

# a Context is an ordered list of various kinds of named values (such as env vars, our vars)
# possibly including other Context objects.
#
# Values can be looked up by name. The first match will be returned.

sub new {
    my $class = shift;
    $class = ref $class if ref $class;
    return bless [ @_ ], $class;
}


sub new_composite { shift->new(@_) } # see Test::WriteVariants::Context::BaseItem


sub push_var { # add a var to an existing config
    my ($self, $var) = @_;
    push @$self, $var;
    return;
}


sub _new_var    {
    my ($self, $t, $n, $v, %e) = @_;
    my $var = $t->new($n, $v, %e);
    return $self->new( $var ); # wrap var item in a context list
}
sub new_env_var    { shift->_new_var($ContextClass.'::EnvVar', @_) }
sub new_our_var    { shift->_new_var($ContextClass.'::OurVar', @_) }
sub new_module_use { shift->_new_var($ContextClass.'::ModuleUse', @_) }
sub new_meta_info  { shift->_new_var($ContextClass.'::MetaInfo', @_) }


# XXX should ensure that a given type+name is only output once (the latest one)
sub get_code  {
    my $self = shift;
    my @code;
    for my $setting (reverse @$self) {
        push @code, (ref $setting) ? $setting->get_code : $setting;
    }
    return join "", @code;
}


sub get_var { # search backwards through list of settings, stop at first match
    my ($self, $name, $type) = @_;
    for my $setting (reverse @$self) {
        next unless $setting;
        my @value = $setting->get_var($name, $type);
        return $value[0] if @value;
    }
    return;
}

sub get_env_var    { my ($self, $name) = @_; return $self->get_var($name, $ContextClass.'::EnvVar') }
sub get_our_var    { my ($self, $name) = @_; return $self->get_var($name, $ContextClass.'::OurVar') }
sub get_module_use { my ($self, $name) = @_; return $self->get_var($name, $ContextClass.'::ModuleUse') }
sub get_meta_info  { my ($self, $name) = @_; return $self->get_var($name, $ContextClass.'::MetaInfo') }



{
    package Test::WriteVariants::Context::BaseItem;
    use strict;
    require Data::Dumper;
    require Carp;

    # base class for an item (a name-value-type triple)

    sub new {
        my ($class, $name, $value) = @_;

        my $self = bless {} => $class;
        $self->name($name);
        $self->value($value);

        return $self;
    }

    sub name {
        my $self = shift;
        $self->{name} = shift if @_;
        return $self->{name};
    }

    sub value {
        my $self = shift;
        $self->{value} = shift if @_;
        return $self->{value};
    }

    sub get_code  {
        return '';
    }

    sub get_var {
        my ($self, $name, $type) = @_;
        return if $type && !$self->isa($type);  # empty list
        return if $name ne $self->name;         # empty list
        return $self->value;                    # scalar
    }

    sub quote_values_as_perl {
        my $self = shift;
        my @perl_values = map {
            my $val = Data::Dumper->new([$_])->Terse(1)->Purity(1)->Useqq(1)->Sortkeys(1)->Dump;
            chomp $val;
            $val;
        } @_;
        Carp::confess("quote_values_as_perl called with multiple items in scalar context (@perl_values)")
            if @perl_values > 1 && !wantarray;
        return $perl_values[0] unless wantarray;
        return @perl_values;
    }

    # utility method to get a new composite when you only have a value object
    sub new_composite { $ContextClass->new(@_) }

} # ::BaseItem


{
    package Test::WriteVariants::Context::EnvVar;
    use strict;
    use parent -norequire, 'Test::WriteVariants::Context::BaseItem';

    # subclass representing a named environment variable

    sub get_code {
        my $self = shift;
        my $name = $self->{name};
        my @lines;
        if (defined $self->{value}) {
            my $perl_value = $self->quote_values_as_perl($self->{value});
            push @lines, sprintf('$ENV{%s} = %s;', $name, $perl_value);
            push @lines, sprintf('END { delete $ENV{%s} } # for VMS', $name);
        }
        else {
            # we treat undef to mean the ENV var should not exist in %ENV
            push @lines, sprintf('local  $ENV{%s};', $name); # preserve old value for VMS
            push @lines, sprintf('delete $ENV{%s};', $name); # delete from %ENV
        }
        return join "\n", @lines, '';
    }
}


{
    package Test::WriteVariants::Context::OurVar;
    use strict;
    use parent -norequire, 'Test::WriteVariants::Context::BaseItem';

    # subclass representing a named 'our' variable

    sub get_code {
        my $self = shift;
        my $perl_value = $self->quote_values_as_perl($self->{value});
        return sprintf 'our $%s = %s;%s', $self->{name}, $perl_value, "\n";
    }
}


{
    package Test::WriteVariants::Context::ModuleUse;
    use strict;
    use parent -norequire, 'Test::WriteVariants::Context::BaseItem';

    # subclass representing 'use $name (@$value)'

    sub get_code {
        my $self = shift;
        my @imports = $self->quote_values_as_perl(@{$self->{value}});
        return sprintf 'use %s (%s);%s', $self->{name}, join(", ", @imports), "\n";
    }
}

{
    package Test::WriteVariants::Context::MetaInfo;
    use strict;
    use parent -norequire, 'Test::WriteVariants::Context::BaseItem';

    # subclass that doesn't generate any code
    # It's just used to convey information between plugins
}

1;

END_OF_EXTUTILS_BUNDLE_MAKER_MARKER

    $@ and die $@;
    defined $INC{'Test/WriteVariants/Context.pm'} or $INC{'Test/WriteVariants/Context.pm'} = 'Bundled';
};

check_module("Test::WriteVariants", "0.005") or do { eval <<'END_OF_EXTUTILS_BUNDLE_MAKER_MARKER';
package Test::WriteVariants;

=head1 NAME

Test::WriteVariants - Dynamic generation of tests in nested combinations of contexts

=head1 SYNOPSIS

    use Test::WriteVariants;

    my $test_writer = Test::WriteVariants->new();

    $test_writer->write_test_variants(

        # tests we want to run in various contexts
        input_tests => {
            'core/10-foo' => { require => 't/core/10-foo.t' },
            'core/20-bar' => { require => 't/core/20-bar.t' },
        },

        # one or more providers of variant contexts
        variant_providers => [
            sub {
                my ($path, $context, $tests) = @_;
                my %variants = (
                    plain    => $context->new_env_var(MY_MODULE_PUREPERL => 0),
                    pureperl => $context->new_env_var(MY_MODULE_PUREPERL => 1),
                );
                return %variants;
            },
            sub {
                my ($path, $context, $tests) = @_;
                my %variants = map {
                    $_ => $context->new_env_var(MY_MODULE_WIBBLE => $_),
                } 1..3;
                delete $variants{3} if $context->get_env_var("MY_MODULE_PUREPERL");
                return %variants;
            },
        ],

        # where to generate the .t files that wrap the input_tests
        output_dir => 't/variants',
    );

When run that generates the desired test variants:

    Writing t/variants/plain/1/core/10-foo.t
    Writing t/variants/plain/1/core/20-bar.t
    Writing t/variants/plain/2/core/10-foo.t
    Writing t/variants/plain/2/core/20-bar.t
    Writing t/variants/plain/3/core/10-foo.t
    Writing t/variants/plain/3/core/20-bar.t
    Writing t/variants/pureperl/1/core/10-foo.t
    Writing t/variants/pureperl/1/core/20-bar.t
    Writing t/variants/pureperl/2/core/10-foo.t
    Writing t/variants/pureperl/2/core/20-bar.t

Here's what t/variants/pureperl/2/core/20-bar.t looks like:

    #!perl
    $ENV{MY_MODULE_WIBBLE} = 2;
    END { delete $ENV{MY_MODULE_WIBBLE} } # for VMS
    $ENV{MY_MODULE_PUREPERL} = 1;
    END { delete $ENV{MY_MODULE_PUREPERL} } # for VMS
    require 't/core/20-bar.t';


Here's an example that uses plugins to provide the tests and the variants:

    my $test_writer = Test::WriteVariants->new();

    # gather set of input tests that we want to run in various contexts
    # these can come from various sources, including modules and test files
    my $input_tests = $test_writer->find_input_test_modules(
        search_path => [ 'DBI::TestCase' ]
    );

    $test_writer->write_test_variants(

        # tests we want to run in various contexts
        input_tests => $input_tests,

        # one or more providers of variant contexts
        # (these can be code refs or plugin namespaces)
        variant_providers => [
            "DBI::Test::VariantDBI",
            "DBI::Test::VariantDriver",
            "DBI::Test::VariantDBD",
        ],

        # where to generate the .t files that wrap the input_tests
        output_dir => $output_dir,
    );

=head1 DESCRIPTION

NOTE: This is alpha code that's still evolving - nothing is stable.

See L<List::MoreUtils> (on github) for an example use.

=cut

use strict;
use warnings;

use File::Find;
use File::Path;
use File::Basename;
use Carp qw(croak confess);

use Module::Pluggable::Object;

use Test::WriteVariants::Context;
use Data::Tumbler;

our $VERSION = '0.007';

=head1 METHODS

=head2 new

    $test_writer = Test::WriteVariants->new(%attributes);

Instanciates a Test::WriteVariants instance and sets the specified attributes, if any.

=cut

sub new {
    my ($class, %args) = @_;

    my $self = bless {} => $class;

    for my $attribute (qw(allow_dir_overwrite allow_file_overwrite)) {
        next unless exists $args{$attribute};
        $self->$attribute(delete $args{$attribute});
    }
    confess "Unknown $class arguments: @{[ keys %args ]}"
        if %args;

    return $self;
}


=head2 allow_dir_overwrite

    $test_writer->allow_dir_overwrite($bool);
    $bool = $test_writer->allow_dir_overwrite;

If the output directory already exists when tumble() is called it'll
throw an exception (and warn if it wasn't created during the run).
Setting allow_dir_overwrite true disables this safety check.

=cut

sub allow_dir_overwrite {
    my $self = shift;
    $self->{allow_dir_overwrite} = shift if @_;
    return $self->{allow_dir_overwrite};
}


=head2 allow_file_overwrite

    $test_writer->allow_file_overwrite($bool);
    $bool = $test_writer->allow_file_overwrite;

If the test file that's about to be written already exists
then write_output_files() will throw an exception.
Setting allow_file_overwrite true disables this safety check.

=cut

sub allow_file_overwrite {
    my $self = shift;
    $self->{allow_file_overwrite} = shift if @_;
    return $self->{allow_file_overwrite};
}


=head2 write_test_variants

    $test_writer->write_test_variants(
        input_tests => \%input_tests,
        variant_providers => \@variant_providers,
        output_dir => $output_dir,
    );

Instanciates a L<Data::Tumbler>. Sets its C<consumer> to call:

    $self->write_output_files($path, $context, $payload, $output_dir)

and sets its C<add_context> to call:

    $context->new($context, $item);

and then calls its C<tumble> method:

    $tumbler->tumble(
        $self->normalize_providers($variant_providers),
        [],
        Test::WriteVariants::Context->new(),
        $input_tests,
    );

=cut

sub write_test_variants {
    my ($self, %args) = @_;

    my $input_tests = delete $args{input_tests}
        or croak "input_tests not specified";
    my $variant_providers = delete $args{variant_providers}
        or croak "variant_providers not specified";
    my $output_dir = delete $args{output_dir}
        or croak "output_dir not specified";
    croak "write_test_variants: unknown arguments: @{[ keys %args ]}"
        if keys %args;

    croak "write_test_variants: $output_dir already exists"
        if -d $output_dir and not $self->allow_dir_overwrite;

    my $tumbler = Data::Tumbler->new(
        consumer => sub {
            my ($path, $context, $payload) = @_;
            # payload is a clone of input_tests possibly modified by providers
            $self->write_output_files($path, $context, $payload, $output_dir);
        },
        add_context => sub {
            my ($context, $item) = @_;
            return $context->new($context, $item);
        },
    );

    $tumbler->tumble(
        $self->normalize_providers($variant_providers),
        [],
        Test::WriteVariants::Context->new(),
        $input_tests, # payload
    );

    warn "No tests written to $output_dir!\n"
        if not -d $output_dir and not $self->allow_dir_overwrite;

    return;
}



# ------

# XXX also implement a find_input_test_files - that finds .t files

=head2 find_input_test_modules

    $input_tests = $test_writer->find_input_test_modules(
    );

=cut

sub find_input_test_modules {
    my ($self, %args) = @_;

    my $namespaces = delete $args{search_path}
        or croak "search_path not specified";
    my $search_dirs = delete $args{search_dirs};
    my $test_prefix = delete $args{test_prefix};
    my $input_tests = delete $args{input_tests} || {};
    croak "find_input_test_modules: unknown arguments: @{[ keys %args ]}"
        if keys %args;

    my $edit_test_name;
    if (defined $test_prefix) {
        my $namespaces_regex = join "|", map { quotemeta($_) } @$namespaces;
        my $namespaces_qr    = qr/^($namespaces_regex)::/;
        $edit_test_name = sub { s/$namespaces_qr/$test_prefix/ };
    }

    my @test_case_modules = Module::Pluggable::Object->new(
        require => 0,
        search_path => $namespaces,
        search_dirs => $search_dirs,
    )->plugins;

    #warn "find_input_test_modules @$namespaces: @test_case_modules";

    for my $module_name (@test_case_modules) {
        $self->add_test_module($input_tests, $module_name, $edit_test_name);
    }

    return $input_tests;
}


=head2 find_input_test_files

Not yet implemented - will file .t files.

=cut


=head2 add_test

    $test_writer->add_test(
        $input_tests,   # the \%input_tests to add the test module to
        $test_name,     # the key to use in \%input_tests
        $test_spec      # the details of the test file
    );

Adds the $test_spec to %$input_tests keys by $test_name. In other words:

    $input_tests->{ $test_name } = $test_spec;

An exception will be thrown if a test with $test_name already exists
in %$input_tests.

This is a low-level interface that's not usually called directly.
See L</add_test_module>.

=cut

sub add_test {
    my ($self, $input_tests, $test_name, $test_spec) = @_;

    confess "Can't add test $test_name because a test with that name exists"
        if $input_tests->{ $test_name };

    $input_tests->{ $test_name } = $test_spec;
    return;
}


=head2 add_test_module

    $test_writer->add_test_module(
        $input_tests,     # the \%input_tests to add the test module to
        $module_name,     # the package name of the test module
        $edit_test_name   # a code ref to edit the test module name in $_
    );

=cut

sub add_test_module {
    my ($self, $input_tests, $module_name, $edit_test_name) = @_;

    # map module name, without the namespace prefix, to a dir path
    local $_ = $module_name;
    $edit_test_name->() if $edit_test_name;
    s{[^\w:]+}{_}g;
    s{::}{/}g;

    $self->add_test($input_tests, $_, {
        class => $module_name,
        method => 'run_tests',
    });

    return;
}


=head2 normalize_providers

    $providers = $test_writer->normalize_providers($providers);

Given a reference to an array of providers, returns a reference to a new array.
Any code references in the original array are passed through unchanged.

Any other value is treated as a package name and passed to
L<Module::Pluggable::Object> as a namespace C<search_path> to find plugins.
An exception is thrown if no plugins are found.

The corresponding element of the original $providers array is replaced with a
new provider code reference which calls the C<provider_initial>, C<provider>,
and C<provider_final> methods, if present, for each plugin namespace in turn.

Normal L<Data::Tumbler> provider subroutines are called with these arguments:

    ($path, $context, $tests)

and the return value is expected to be a hash.  Whereas the plugin provider
methods are called with these arguments:

    ($test_writer, $path, $context, $tests, $variants)

and the return value is ignored. The $variants argument is a reference to a
hash that will be returned to Data::Tumbler and which should be edited by the
plugin provider method. This allows a plugin to see, and change, the variants
requested by any other plugins that have already been run for this provider.

=cut

sub normalize_providers {
    my ($self, $input_providers) = @_;
    my @providers = @$input_providers;

    # if a provider is a namespace name instead of a code ref
    # then replace it with a code ref that uses Module::Pluggable
    # to load and run the provider classes in that namespace

    for my $provider (@providers) {
        next if ref $provider eq 'CODE';

        my @test_variant_modules = Module::Pluggable::Object->new(
            search_path => [ $provider ],
            # for sanity:
            require => 1,
            on_require_error     => sub { croak "@_" },
            on_instantiate_error => sub { croak "@_" },
        )->plugins;
        @test_variant_modules = sort @test_variant_modules;

        croak "No variant providers found in $provider\:: namespace"
            unless @test_variant_modules;

        warn sprintf "Variant providers in %s: %s\n", $provider, join(", ", map {
            (my $n=$_) =~ s/^${provider}:://; $n
        } @test_variant_modules);

        $provider = sub {
            my ($path, $context, $tests) = @_;

            my %variants;
            # loop over several methods as a basic way of letting plugins
            # hook in either early or late if they need to
            for my $method (qw(provider_initial provider provider_final)) {
                for my $test_variant_module (@test_variant_modules) {
                    next unless $test_variant_module->can($method);
                    #warn "$test_variant_module $method...\n";
                    my $fqsn = "$test_variant_module\::$method";
                    $self->$fqsn($path, $context, $tests, \%variants);
                    #warn "$test_variant_module $method: @{[ keys %variants ]}\n";
                }
            }

            return %variants;
        };
    }

    return \@providers;
}


=head2 write_output_files

    $test_writer->write_output_files($path, $context, $input_tests, $output_dir);

Writes test files for each test in %$input_tests, for the given $path and $context,
into the $output_dir.

The $output_dir, @$path, and key of %$input_tests are concatenated to form a
file name. A ".t" is added if not already present.

Calls L</get_test_file_body> to get the content of the test file, and then
calls L</write_file> to write it.

=cut

sub write_output_files {
    my ($self, $path, $context, $input_tests, $output_dir) = @_;

    my $base_dir_path = join "/", $output_dir, @$path;

    for my $testname (sort keys %$input_tests) {
        my $test_spec = $input_tests->{$testname};

        # note that $testname can include a subdirectory path
        $testname .= ".t" unless $testname =~ m/\.t$/;
        my $full_path = "$base_dir_path/$testname";

        warn "Writing $full_path\n";
        #warn "test_spec: @{[ %$test_spec ]}";

        my $test_script = $self->get_test_file_body($context, $test_spec);

        $self->write_file($full_path, $test_script);
    }

    return;
}


=head2 write_file

    $test_writer->write_file($filepath, $content);

Throws an exception if $filepath already exists and L</allow_file_overwrite> is
not true.

Creates $filepath and writes $content to it.
Creates any directories that are needed.
Throws an exception on error.

=cut

sub write_file {
    my ($self, $filepath, $content) = @_;

    croak "$filepath already exists!\n"
        if -e $filepath and not $self->allow_file_overwrite;

    my $full_dir_path = dirname($filepath);
    mkpath($full_dir_path, 0)
        unless -d $full_dir_path;

    open my $fh, ">", $filepath
        or croak "Can't write to $filepath: $!";
    print $fh $content;
    close $fh
        or croak "Error writing to $filepath: $!";

    return;
}


=head2 get_test_file_body

    $test_body = $test_writer->get_test_file_body($context, $test_spec);

XXX This should probably be a method call on an object
instanciated by the find_input_test_* methods.

=cut

sub get_test_file_body {
    my ($self, $context, $test_spec) = @_;

    my @body;

    push @body, $test_spec->{prologue} || qq{#!perl\n\n};

    push @body, $context->get_code;
    push @body, "\n";

    push @body, "use lib '$test_spec->{lib}';\n\n"
        if $test_spec->{lib};

    push @body, "require '$test_spec->{require}';\n\n"
        if $test_spec->{require};

    if (my $class = $test_spec->{class}) {
        push @body, "require $class;\n\n";
        my $method = $test_spec->{method};
        push @body, "$class->$method;\n\n" if $method;
    }

    push @body, "$test_spec->{code}\n\n"
        if $test_spec->{code};

    return join "", @body;
}



1;

END_OF_EXTUTILS_BUNDLE_MAKER_MARKER

    $@ and die $@;
    defined $INC{'Test/WriteVariants.pm'} or $INC{'Test/WriteVariants.pm'} = 'Bundled';
};

check_module("Config::AutoConf", "0.26") or do { eval <<'END_OF_EXTUTILS_BUNDLE_MAKER_MARKER';
package Config::AutoConf;
use ExtUtils::CBuilder;

use Config;
use Carp qw/croak/;

use File::Temp qw/tempfile/;
use File::Basename;
use File::Spec;
use Text::ParseWords qw//;

use Capture::Tiny qw/capture/;

# in core since 5.7.3
eval "use Scalar::Util qw/looks_like_number/;";
__PACKAGE__->can("looks_like_number") or eval <<'EOP';
# from PP part of Params::Util
sub looks_like_number {
    local $_ = shift;

    # checks from perlfaq4
    return 0 if !defined($_);
    if (ref($_)) {
        return overload::Overloaded($_) ? defined(0 + $_) : 0;
    }
    return 1 if (/^[+-]?[0-9]+$/); # is a +/- integer
    return 1 if (/^([+-]?)(?=[0-9]|\.[0-9])[0-9]*(\.[0-9]*)?([Ee]([+-]?[0-9]+))?$/); # a C float
    return 1 if ($] >= 5.008 and /^(Inf(inity)?|NaN)$/i) or ($] >= 5.006001 and /^Inf$/i);

    0;
}
EOP

use base 'Exporter';

our @EXPORT = ('$LIBEXT', '$EXEEXT');

use warnings;
use strict;

# PA-RISC1.1-thread-multi
my %special_dlext = (
  darwin => ".dylib",
  MSWin32 => ".dll",
  ($Config{archname} =~ m/PA-RISC/i ? ("hpux" => ".sl") : ()),
);

our ($LIBEXT, $EXEEXT);

defined $LIBEXT
  or $LIBEXT = defined $Config{so} ? "." . $Config{so} :
               defined $special_dlext{$^O} ? $special_dlext{$^O} : ".so";
defined $EXEEXT
  or $EXEEXT = ($^O =~ /mswin32/i) ? ".exe" : "";

=encoding UTF-8

=head1 NAME

Config::AutoConf - A module to implement some of AutoConf macros in pure perl.

=cut

our $VERSION = '0.28';

=head1 ABSTRACT

With this module I pretend to simulate some of the tasks AutoConf
macros do. To detect a command, to detect a library, etc.

=head1 SYNOPSIS

    use Config::AutoConf;

    Config::AutoConf->check_prog("agrep");
    my $grep = Config::AutoConf->check_progs("agrep", "egrep", "grep");

    Config::AutoConf->check_header("ncurses.h");
    my $curses = Config::AutoConf->check_headers("ncurses.h","curses.h");

    Config::AutoConf->check_prog_awk;
    Config::AutoConf->check_prog_egrep;

    Config::AutoConf->check_cc();

    Config::AutoConf->check_lib("ncurses", "tgoto");

    Config::AutoConf->check_file("/etc/passwd"); # -f && -r

=head1 FUNCTIONS

=cut

my $glob_instance;

=head2 new

This function instantiates a new instance of Config::AutoConf, eg. to
configure child components.

=cut

sub new {
  my $class = shift;
  ref $class and $class = ref $class;
  my %args = @_;

  my %instance = (
    msg_prefix => 'configure: ',
    lang => "C",
    lang_stack => [],
    lang_supported => {
      "C" => "ExtUtils::CBuilder",
    },
    cache => {},
    defines => {},
    extra_libs => [],
    extra_lib_dirs => [],
    extra_include_dirs => [],
    extra_preprocess_flags => [],
    extra_compile_flags => {
      "C" => [],
    },
    extra_link_flags => [],
    logfile => "config.log",
    %args
  );
  my $self = bless( \%instance, $class );

  return $self;
}

=head2 check_file

This function checks if a file exists in the system and is readable by
the user. Returns a boolean. You can use '-f $file && -r $file' so you
don't need to use a function call.

=cut

sub check_file {
  my $self = shift;
  my $file = shift;

  return (-f $file && -r $file);
}


=head2 check_files

This function checks if a set of files exist in the system and are
readable by the user. Returns a boolean.

=cut

sub check_files {
  my $self = shift;

  for (@_) {
    return 0 unless $self->check_file($_)
  }

  return 1;
}


=head2 check_prog

This function checks for a program with the supplied name. In success
returns the full path for the executable;

=cut

sub check_prog {
  my $self = shift;
  # sanitize ac_prog
  my $ac_prog = _sanitize(shift @_);
  my $PATH = $ENV{PATH};
  my $p;

  my $ext = "";
  $ext = ".exe" if $^O =~ /mswin/i;
	
  for $p (split /$Config{path_sep}/,$PATH) {
    my $cmd = File::Spec->catfile($p,$ac_prog.$ext);
    return $cmd if -x $cmd;
  }
  return undef;
}

=head2 check_progs

This function takes a list of program names. Returns the full path for
the first found on the system. Returns undef if none was found.

=cut

sub check_progs {
  my $self = shift;
  my @progs = @_;
  for (@progs) {
    my $ans = $self->check_prog($_);
    return $ans if $ans;
  }
  return undef;
}

=head2 check_prog_yacc

From the autoconf documentation,

  If `bison' is found, set [...] `bison -y'.
  Otherwise, if `byacc' is found, set [...] `byacc'. 
  Otherwise set [...] `yacc'.

Returns the full path, if found.

=cut

sub check_prog_yacc {
  my $self = shift;
  my $binary = $self->check_progs(qw/bison byacc yacc/);
  defined $binary and $binary =~ /bison$/ and $binary .= " -y";
  return $binary;
}

=head2 check_prog_awk

From the autoconf documentation,

  Check for `gawk', `mawk', `nawk', and `awk', in that order, and
  set output [...] to the first one that is found.  It tries
  `gawk' first because that is reported to be the best
  implementation.

Note that it returns the full path, if found.

=cut

sub check_prog_awk {
  my $self = shift;
  return $self->check_progs(qw/gawk mawk nawk awk/);
}


=head2 check_prog_egrep

From the autoconf documentation,

  Check for `grep -E' and `egrep', in that order, and [...] output
  [...] the first one that is found.

Note that it returns the full path, if found.

=cut

sub check_prog_egrep {
  my $self = shift;

  my $grep;

  if ($grep = $self->check_prog("grep")) {
    my $ans = `echo a | ($grep -E '(a|b)') 2>/dev/null`;
    return "$grep -E" if $ans eq "a\n";
  }

  if ($grep = $self->check_prog("egrep")) {
    return $grep;
  }
  return undef;
}

=head2 check_prog_pkg_config

Checks for C<pkg-config> program. No additional tests are made for it ...

=cut
 
sub check_prog_pkg_config {
  my $self = shift->_get_instance();
  my $cache_name = $self->_cache_name("prog", "pkg-config");
  return $self->check_cached( $cache_name, "for pkg-config",
    sub {$self->check_prog("pkg-config")} );
}

=head2 check_cc

This function checks if you have a running C compiler.

=cut

sub check_cc {
  ExtUtils::CBuilder->new(quiet => 1)->have_compiler;
}

=head2 msg_checking

Prints "Checking @_ ..."

=cut

sub msg_checking {
  my $self = shift->_get_instance();
  $self->{quiet} or
    print "Checking " . join(" ", @_) . "... ";
  $self->_add2log( "Checking " . join( " ", @_, "..." ) );
  return;
}

=head2 msg_result

Prints result \n

=cut

my @_num_to_msg = qw/no yes/;

sub _neat
{
    defined $_[0] or return "";
    looks_like_number( $_[0] ) and defined $_num_to_msg[$_[0]] and return $_num_to_msg[$_[0]];
    return $_[0];
}

sub msg_result {
  my $self = shift->_get_instance();
  $self->{quiet} or
    print join( " ", map { _neat $_ } @_ ), "\n";
  return;
}

=head2 msg_notice

Prints "configure: " @_ to stdout

=cut

sub msg_notice {
  my $self = shift->_get_instance();
  $self->{quiet} or
    print $self->{msg_prefix} . join( " ", @_ ) . "\n";
  return;
}

=head2 msg_warn

Prints "configure: " @_ to stderr

=cut

sub msg_warn {
  my $self = shift->_get_instance();
  $self->{quiet} or
    print STDERR $self->{msg_prefix} . join( " ", @_ ) . "\n";
  return;
}

=head2 msg_error

Prints "configure: " @_ to stderr and exits with exit code 0 (tells
toolchain to stop here and report unsupported environment)

=cut

sub msg_error {
  my $self = shift->_get_instance();
  $self->{quiet} or
    print STDERR $self->{msg_prefix} . join( " ", @_ ) . "\n";
  exit(0); # #toolchain agreement: prevents configure stage to finish
}

=head2 msg_failure

Prints "configure: " @_ to stderr and exits with exit code 0 (tells
toolchain to stop here and report unsupported environment). Additional
details are provides in config.log (probably more information in a
later stage).

=cut

sub msg_failure {
  my $self = shift->_get_instance();
  $self->{quiet} or
    print STDERR $self->{msg_prefix} . join( " ", @_ ) . "\n";
  exit(0); # #toolchain agreement: prevents configure stage to finish
}

=head2 define_var( $name, $value [, $comment ] )

Defines a check variable for later use in further checks or code to compile.

=cut

sub define_var {
  my $self = shift->_get_instance();
  my ($name, $value, $comment) = @_;

  defined( $name ) or croak( "Need a name to add a define" );

  $self->{defines}->{$name} = [ $value, $comment ];

  return;
}

=head2 write_config_h( [$target] )

Writes the defined constants into given target:

  Config::AutoConf->write_config_h( "config.h" );

=cut

sub write_config_h {
  my $self = shift->_get_instance();
  my $tgt;
  
  defined( $_[0] )
    ? ( ref( $_[0] )
      ? $tgt = $_[0]
      : open( $tgt, ">", $_[0] ) )
    : open( $tgt, ">", "config.h" );

  my $conf_h = <<'EOC';
/**
 * Generated from Config::AutoConf
 *
 * Do not edit this file, all modifications will be lost,
 * modify Makefile.PL or Build.PL instead.
 *
 * Inspired by GNU AutoConf.
 *
 * (c) 2011 Alberto Simoes & Jens Rehsack
 */
#ifndef __CONFIG_H__

EOC

  while( my ($defname, $defcnt) = each( %{ $self->{defines} } ) ) {
    if( $defcnt->[0] ) {
      defined $defcnt->[1] and $conf_h .= "/* " . $defcnt->[1] . " */\n";
      $conf_h .= join( " ", "#define", $defname, $defcnt->[0] ) . "\n";
    }
    else {
      defined $defcnt->[1] and $conf_h .= "/* " . $defcnt->[1] . " */\n";
      $conf_h .= "/* " . join( " ", "#undef", $defname ) . " */\n\n";
    }
  }
  $conf_h .= "#endif /* ?__CONFIG_H__ */\n";

  print {$tgt} $conf_h;

  return;
}

=head2 push_lang(lang [, implementor ])

Puts the current used language on the stack and uses specified language
for subsequent operations until ending pop_lang call.

=cut

sub push_lang {
  my $self = shift->_get_instance();

  push @{$self->{lang_stack}}, [ $self->{lang} ];

  return $self->_set_language( @_ );
}

=head2 pop_lang([ lang ])

Pops the currently used language from the stack and restores previously used
language. If I<lang> specified, it's asserted that the current used language
equals to specified language (helps finding control flow bugs).

=cut

sub pop_lang {
  my $self = shift->_get_instance();

  scalar( @{$self->{lang_stack}} ) > 0 or croak( "Language stack empty" );
  defined( $_[0] ) and $self->{lang} ne $_[0] and
    croak( "pop_lang( $_[0] ) doesn't match language in use (" . $self->{lang} . ")" );

  return $self->_set_language( @{ pop @{ $self->{lang} } } );
}

=head2 lang_call( [prologue], function )

Builds program which simply calls given function.
When given, prologue is prepended otherwise, the default
includes are used.

=cut

sub lang_call {
  my $self = shift->_get_instance();
  my ($prologue, $function) = @_;

  defined( $prologue ) or $prologue = $self->_default_includes();
  $prologue .= <<"_ACEOF";
/* Override any GCC internal prototype to avoid an error.
   Use char because int might match the return type of a GCC
   builtin and then its argument prototype would still apply.  */
#ifdef __cplusplus
extern "C" {
#endif
char $function ();
#ifdef __cplusplus
}
#endif
_ACEOF
  my $body = "return $function ();";
  $body = $self->_build_main( $body );

  my $conftest  = $self->_fill_defines();
     $conftest .= "\n$prologue\n";
     $conftest .= "\n$body\n";

  return $conftest;
}

=head2 lang_build_program( prologue, body )

Builds program for current chosen language. If no prologue is given
(I<undef>), the default headers are used. If body is missing, default
body is used.

Typical call of

  Config::AutoConf->lang_build_program( "const char hw[] = \"Hello, World\\n\";",
                                        "fputs (hw, stdout);" )

will create

  const char hw[] = "Hello, World\n";

  /* Override any gcc2 internal prototype to avoid an error.  */
  #ifdef __cplusplus
  extern "C" {
  #endif

  int
  main (int argc, char **argv)
  {
    (void)argc;
    (void)argv;
    fputs (hw, stdout);;
    return 0;
  }

  #ifdef __cplusplus
  }
  #endif

=cut

sub lang_build_program {
  my $self = shift->_get_instance();
  my ($prologue, $body) = @_;

  defined( $prologue ) or $prologue = $self->_default_includes();
  defined( $body ) or $body = "";
  $body = $self->_build_main( $body );

  my $conftest  = $self->_fill_defines();
     $conftest .= "\n$prologue\n";
     $conftest .= "\n$body\n";

  return $conftest;
}

=head2 lang_build_bool_test (prologue, test, [@decls])

Builds a static test which will fail to compile when test
evaluates to false. If C<@decls> is given, it's prepended
before the test code at the variable definition place.

=cut

sub lang_build_bool_test {
  my $self = shift->_get_instance();
  my ($prologue, $test, @decls) = @_;

  defined( $test ) or $test = "1";
  my $test_code = <<ACEOF;
  static int test_array [($test) ? 1 : -1 ];
  test_array [0] = 0
ACEOF
  if( @decls ) {
    $test_code = join( "\n", @decls, $test_code );
  }
  return $self->lang_build_program( $prologue, $test_code );
}

=head2 push_includes

Adds given list of directories to preprocessor/compiler
invocation. This is not proved to allow adding directories
which might be created during the build.

=cut

sub push_includes {
  my $self = shift->_get_instance();
  my @includes = @_;

  push( @{$self->{extra_include_dirs}}, @includes );

  return;
}

=head2 push_preprocess_flags

Adds given flags to the parameter list for preprocessor invocation.

=cut

sub push_preprocess_flags {
  my $self = shift->_get_instance();
  my @cpp_flags = @_;

  push( @{$self->{extra_preprocess_flags}}, @cpp_flags );

  return;
}

=head2 push_compiler_flags

Adds given flags to the parameter list for compiler invocation.

=cut

sub push_compiler_flags {
  my $self = shift->_get_instance();
  my @compiler_flags = @_;
  my $lang = $self->{lang};

  if( scalar( @compiler_flags ) && ( ref($compiler_flags[-1]) eq "HASH" ) ) {
    my $lang_opt = pop( @compiler_flags );
    defined( $lang_opt->{lang} ) or croak( "Missing lang attribute in language options" );
    $lang = $lang_opt->{lang};
    defined( $self->{lang_supported}->{$lang} ) or croak( "Unsupported language '$lang'" );
  }

  push( @{$self->{extra_compile_flags}->{$lang}}, @compiler_flags );

  return;
}

=head2 push_libraries

Adds given list of libraries to the parameter list for linker invocation.

=cut

sub push_libraries {
  my $self = shift->_get_instance();
  my @libs = @_;

  push( @{$self->{extra_libs}}, @libs );

  return;
}

=head2 push_library_paths

Adds given list of library paths to the parameter list for linker invocation.

=cut

sub push_library_paths {
  my $self = shift->_get_instance();
  my @libdirs = @_;

  push( @{$self->{extra_lib_dirs}}, @libdirs );

  return;
}

=head2 push_link_flags

Adds given flags to the parameter list for linker invocation.

=cut

sub push_link_flags {
  my $self = shift->_get_instance();
  my @link_flags = @_;

  push( @{$self->{extra_link_flags}}, @link_flags );

  return;
}

=head2 compile_if_else( $src [, action-if-true [, action-if-false ] ] )

This function trys to compile specified code and runs action-if-true on success
or action-if-false otherwise.

Returns a boolean value containing check success state.

=cut

sub compile_if_else {
  my ($self, $src, $action_if_true, $action_if_false) = @_;
  ref $self or $self = $self->_get_instance();
  my $builder = $self->_get_builder();

  my ($fh, $filename) = tempfile( "testXXXXXX", SUFFIX => '.c');

  print {$fh} $src;
  close $fh;

  my ($obj_file, $errbuf, $exception);
  (undef, $errbuf) = capture {
    eval {
      $obj_file = $builder->compile(
        source => $filename,
        include_dirs => $self->{extra_include_dirs},
        extra_compiler_flags => $self->_get_extra_compiler_flags() );
    };

    $exception = $@;
  };

  unlink $filename;
  unlink $obj_file if $obj_file;

  if ($exception || !$obj_file) {
    $self->_add2log( "compile stage failed" . ( $exception ? " - " . $exception : "" ) );
    $errbuf and
      $self->_add2log( $errbuf );
    $self->_add2log( "failing program is:\n" . $src );

    defined( $action_if_false ) and "CODE" eq ref( $action_if_false ) and &{$action_if_false}();
    return 0;
  }

  defined( $action_if_true ) and "CODE" eq ref( $action_if_true ) and &{$action_if_true}();
  return 1;
}

=head2 link_if_else( $src [, action-if-true [, action-if-false ] ] )

This function trys to compile and link specified code and runs action-if-true on success
or action-if-false otherwise.

Returns a boolean value containing check success state.

=cut

sub link_if_else {
  my ($self, $src, $action_if_true, $action_if_false) = @_;
  ref $self or $self = $self->_get_instance();
  my $builder = $self->_get_builder();

  my ($fh, $filename) = tempfile( "testXXXXXX", SUFFIX => '.c');

  print {$fh} $src;
  close $fh;

  my ($obj_file, $errbuf, $exception);
  (undef, $errbuf) = capture {
    eval {
      $obj_file = $builder->compile(
        source => $filename,
        include_dirs => $self->{extra_include_dirs},
        extra_compiler_flags => $self->_get_extra_compiler_flags() );
    };

    $exception = $@;
  };

  if ($exception || !$obj_file) {
    $self->_add2log( "compile stage failed" . ( $exception ? " - " . $exception : "" ) );
    $errbuf and
      $self->_add2log( $errbuf );
    $self->_add2log( "failing program is:\n" . $src );

    unlink $filename;
    unlink $obj_file if $obj_file;
    defined( $action_if_false ) and "CODE" eq ref( $action_if_false ) and &{$action_if_false}();
    return 0;
  }

  my $exe_file;
  (undef, $errbuf) = capture {
    eval {
      $exe_file = $builder->link_executable(
        objects => $obj_file,
        extra_linker_flags => $self->_get_extra_linker_flags() );
    };

    $exception = $@;
  };
  unlink $filename;
  unlink $obj_file if $obj_file;
  unlink $exe_file if $exe_file;

  if ($exception || !$exe_file) {
    $self->_add2log( "link stage failed" . ( $exception ? " - " . $exception : "" ) );
    $errbuf and
      $self->_add2log( $errbuf );
    $self->_add2log( "failing program is:\n" . $src );

    defined( $action_if_false ) and "CODE" eq ref( $action_if_false ) and &{$action_if_false}();
    return 0;
  }

  defined( $action_if_true ) and "CODE" eq ref( $action_if_true ) and &{$action_if_true}();
  return 1;
}

=head2 check_cached( cache-var, message, sub-to-check )

This function checks whether a specified cache variable is set or not, and if not
it's going to set it using specified sub-to-check.

=cut

sub check_cached {
  my ($self, $cache_name, $message, $check_sub) = @_;
  ref $self or $self = $self->_get_instance();

  $self->msg_checking( $message );

  if( defined($self->{cache}->{$cache_name}) ) {
    $self->msg_result( "(cached)", $self->{cache}->{$cache_name} );
  }
  else {
    $self->{cache}->{$cache_name} = &{$check_sub}();
    $self->msg_result( $self->{cache}->{$cache_name} );
  }

  return $self->{cache}->{$cache_name};
}

=head2 cache_val

This functions returns the value of a previously check_cached call.

=cut

sub cache_val {
  my $self = shift->_get_instance();
  my $cache_name = shift;
  defined $self->{cache}->{$cache_name} or return;
  return $self->{cache}->{$cache_name};
}

=head2 check_decl( symbol, [action-if-found], [action-if-not-found], [prologue = default includes] )

If symbol (a function, variable or constant) is not declared in includes and
a declaration is needed, run the code ref given in I<action-if-not-found>,
otherwise I<action-if-found>. includes is a series of include directives,
defaulting to I<default includes>, which are used prior to the declaration
under test.

This method actually tests whether symbol is defined as a macro or can be
used as an r-value, not whether it is really declared, because it is much
safer to avoid introducing extra declarations when they are not needed.
In order to facilitate use of C++ and overloaded function declarations, it
is possible to specify function argument types in parentheses for types
which can be zero-initialized:

          Config::AutoConf->check_decl("basename(char *)")

This method caches its result in the C<ac_cv_decl_E<lt>set langE<gt>>_symbol variable.

=cut

sub check_decl {
  my ($self, $symbol, $action_if_found, $action_if_not_found, $prologue) = @_;
  $self = $self->_get_instance();
  defined( $symbol ) or return; # XXX prefer croak
  ref( $symbol ) eq "" or return;
  ( my $sym_plain = $symbol ) =~ s/ *\(.*//;
  my $sym_call = $symbol;
  $sym_call =~ s/\(/((/;
  $sym_call =~ s/\)/) 0)/;
  $sym_call =~ s/,/) 0, (/g;

  my $cache_name = $self->_cache_name( "decl", $self->{lang}, $symbol );
  my $check_sub = sub {
  
    my $body = <<ACEOF;
#ifndef $sym_plain
  (void) $sym_call;
#endif
ACEOF
    my $conftest = $self->lang_build_program( $prologue, $body );

    my $have_decl = $self->compile_if_else( $conftest );
    if( $have_decl ) {
      if( defined( $action_if_found ) and "CODE" eq ref( $action_if_found ) ) {
	&{$action_if_found}();
      }
    }
    else {
      if( defined( $action_if_not_found ) and "CODE" eq ref( $action_if_not_found ) ) {
	&{$action_if_not_found}();
      }
    }

    return $have_decl;
  };

  return $self->check_cached( $cache_name, "whether $symbol is declared", $check_sub );
}

=head2 check_decls( symbols, [action-if-found], [action-if-not-found], [prologue = default includes] )

For each of the symbols (with optional function argument types for C++
overloads), run L<check_decl>. If I<action-if-not-found> is given, it
is additional code to execute when one of the symbol declarations is
needed, otherwise I<action-if-found> is executed.

Contrary to GNU autoconf, this method does not declare HAVE_DECL_symbol
macros for the resulting C<confdefs.h>, because it differs as C<check_decl>
between compiling languages.

=cut

sub check_decls {
  my ($self, $symbols, $action_if_found, $action_if_not_found, $prologue) = @_;
  $self = $self->_get_instance();

  my $have_syms = 1;
  foreach my $symbol (@$symbols) {
    $have_syms &= $self->check_decl( $symbol, undef, undef, $prologue );
  }

  if( $have_syms ) {
    if( defined( $action_if_found ) and "CODE" eq ref( $action_if_found ) ) {
      &{$action_if_found}();
    }
  }
  else {
    if( defined( $action_if_not_found ) and "CODE" eq ref( $action_if_not_found ) ) {
      &{$action_if_not_found}();
    }
  }

  return $have_syms;
}

sub _have_type_define_name {
  my $type = $_[0];
  my $have_name = "HAVE_" . uc($type);
  $have_name =~ tr/*/P/;
  $have_name =~ tr/_A-Za-z0-9/_/c;
  return $have_name;
}

=head2 check_type (type, [action-if-found], [action-if-not-found], [prologue = default includes])

Check whether type is defined. It may be a compiler builtin type or defined
by the includes. I<prologue> should be a series of include directives,
defaulting to I<default includes>, which are used prior to the type under
test.

In C, type must be a type-name, so that the expression C<sizeof (type)> is
valid (but C<sizeof ((type))> is not)

If I<type> type is defined, preprocessor macro HAVE_I<type> (in all
capitals, with "*" replaced by "P" and spaces and dots replaced by
underscores) is defined.

This method caches its result in the C<ac_cv_type_>type variable.

=cut

sub check_type {
  my ($self, $type, $action_if_found, $action_if_not_found, $prologue) = @_;
  $self = $self->_get_instance();
  defined( $type ) or return; # XXX prefer croak
  ref( $type ) eq "" or return;

  my $cache_name = $self->_cache_type_name( "type", $type );
  my $check_sub = sub {
  
    my $body = <<ACEOF;
  if( sizeof ($type) )
    return 0;
ACEOF
    my $conftest = $self->lang_build_program( $prologue, $body );

    my $have_type = $self->compile_if_else( $conftest );
    $self->define_var( _have_type_define_name( $type ), $have_type ? $have_type : undef, "defined when $type is available" );
    if( $have_type ) {
      if( defined( $action_if_found ) and "CODE" eq ref( $action_if_found ) ) {
	&{$action_if_found}();
      }
    }
    else {
      if( defined( $action_if_not_found ) and "CODE" eq ref( $action_if_not_found ) ) {
	&{$action_if_not_found}();
      }
    }

    return $have_type;
  };

  return $self->check_cached( $cache_name, "for $type", $check_sub );
}

=head2 check_types (types, [action-if-found], [action-if-not-found], [prologue = default includes])

For each type L<check_type> is called to check for type.

If I<action-if-found> is given, it is additionally executed when all of the
types are found. If I<action-if-not-found> is given, it is executed when one
of the types is not found.

=cut

sub check_types {
  my ($self, $types, $action_if_found, $action_if_not_found, $prologue) = @_;
  $self = $self->_get_instance();

  my $have_types = 1;
  foreach my $type (@$types) {
    $have_types &= $self->check_type( $type, undef, undef, $prologue );
  }

  if( $have_types ) {
    if( defined( $action_if_found ) and "CODE" eq ref( $action_if_found ) ) {
      &{$action_if_found}();
    }
  }
  else {
    if( defined( $action_if_not_found ) and "CODE" eq ref( $action_if_not_found ) ) {
      &{$action_if_not_found}();
    }
  }

  return $have_types;
}

sub _compute_int_compile {
  my ($self, $expr, $prologue, @decls) = @_;
  $self = $self->_get_instance();

  my( $body, $conftest, $compile_result );

  my ($low, $mid, $high) = (0, 0, 0);
  if( $self->compile_if_else( $self->lang_build_bool_test( $prologue, "((long int)($expr)) >= 0", @decls ) ) ) {
    $low = $mid = 0;
    while( 1 ) {
      if( $self->compile_if_else( $self->lang_build_bool_test( $prologue, "((long int)($expr)) <= $mid", @decls ) ) ) {
	$high = $mid;
	last;
      }
      $low = $mid + 1;
      # avoid overflow
      if( $low <= $mid ) {
	$low = 0;
	last;
      }
      $mid = $low * 2;
    }
  }
  elsif( $self->compile_if_else( $self->lang_build_bool_test( $prologue, "((long int)($expr)) < 0", @decls ) ) ) {
    $high = $mid = -1;
    while( 1 ) {
      if( $self->compile_if_else( $self->lang_build_bool_test( $prologue, "((long int)($expr)) >= $mid", @decls ) ) ) {
	$low = $mid;
	last;
      }
      $high = $mid - 1;
      # avoid overflow
      if( $mid < $high ) {
	$high = 0;
	last;
      }
      $mid = $high * 2;
    }
  }

  # perform binary search between $low and $high
  while( $low <= $high ) {
    $mid = int( ( $high - $low ) / 2 + $low );
    if( $self->compile_if_else( $self->lang_build_bool_test( $prologue, "((long int)($expr)) < $mid", @decls ) ) ) {
      $high = $mid - 1;
    }
    elsif( $self->compile_if_else( $self->lang_build_bool_test( $prologue, "((long int)($expr)) > $mid", @decls ) ) ) {
      $low = $mid + 1;
    }
    else {
      return $mid;
    }
  }

  return;
}

=head2 compute_int (expression, [action-if-fails], [prologue = default includes], [@decls])

Returns the value of the integer I<expression>. The value should fit in an
initializer in a C variable of type signed long.  It should be possible
to evaluate the expression at compile-time. If no includes are specified,
the default includes are used.

Execute I<action-if-fails> if the value cannot be determined correctly.

=cut

sub compute_int {
  my ($self, $expr, $action_if_fails, $prologue, @decls) = @_;
  $self = $self->_get_instance();

  my $cache_name = $self->_cache_type_name( "compute_int", $self->{lang}, $expr );
  my $check_sub = sub {

    my $val = $self->_compute_int_compile( $expr, $prologue, @decls);
    unless( defined( $val ) ) {
      if( defined( $action_if_fails ) and "CODE" eq ref( $action_if_fails ) ) {
	&{$action_if_fails}();
      }
    }

    return $val;
  };

  return $self->check_cached( $cache_name, "for compute result of ($expr)", $check_sub );
}

sub _sizeof_type_define_name {
  my $type = $_[0];
  my $have_name = "SIZEOF_" . uc($type);
  $have_name =~ tr/*/P/;
  $have_name =~ tr/_A-Za-z0-9/_/c;
  return $have_name;
}

=head2 check_sizeof_type (type, [action-if-found], [action-if-not-found], [prologue = default includes])

Checks for the size of the specified type by compiling. If no size can
determined, I<action-if-not-found> is invoked when given. Otherwise
I<action-if-found> is invoked and C<SIZEOF_type> is defined using the
determined size.

In opposition to GNU AutoConf, this method can determine size of structure
members, eg.

  $ac->check_sizeof_type( "SV.sv_refcnt", undef, undef, $include_perl );
  # or
  $ac->check_sizeof_type( "struct utmpx.ut_id", undef, undef, "#include <utmpx.h>" );

This method caches its result in the C<ac_cv_sizeof_E<lt>set langE<gt>>_type variable.

=cut

sub check_sizeof_type {
  my ($self, $type, $action_if_found, $action_if_not_found, $prologue) = @_;
  $self = $self->_get_instance();
  defined( $type ) or return; # XXX prefer croak
  ref( $type ) eq "" or return;

  my $cache_name = $self->_cache_type_name( "sizeof", $self->{lang}, $type );
  my $check_sub = sub {

    my @decls;
    if( $type =~ m/^([^.]+)\.([^.]+)$/ ) {
      my $struct = $1;
      $type = "_ac_test_aggr.$2";
      my $decl = "static $struct _ac_test_aggr;";
      push( @decls, $decl );
    }
  
    my $typesize = $self->_compute_int_compile( "sizeof($type)", $prologue, @decls );
    $self->define_var( _sizeof_type_define_name( $type ), $typesize ? $typesize : undef, "defined when sizeof($type) is available" );
    if( $typesize ) {
      if( defined( $action_if_found ) and "CODE" eq ref( $action_if_found ) ) {
	&{$action_if_found}();
      }
    }
    else {
      if( defined( $action_if_not_found ) and "CODE" eq ref( $action_if_not_found ) ) {
	&{$action_if_not_found}();
      }
    }

    return $typesize;
  };

  return $self->check_cached( $cache_name, "for size of $type", $check_sub );
}

=head2 check_sizeof_types (type, [action-if-found], [action-if-not-found], [prologue = default includes])

For each type L<check_sizeof_type> is called to check for size of type.

If I<action-if-found> is given, it is additionally executed when all of the
sizes of the types could determined. If I<action-if-not-found> is given, it
is executed when one size of the types could not determined.

=cut

sub check_sizeof_types {
  my ($self, $types, $action_if_found, $action_if_not_found, $prologue) = @_;
  $self = $self->_get_instance();

  my $have_sizes = 1;
  foreach my $type (@$types) {
    $have_sizes &= ! ! ($self->check_sizeof_type ( $type, undef, undef, $prologue ));
  }

  if( $have_sizes ) {
    if( defined( $action_if_found ) and "CODE" eq ref( $action_if_found ) ) {
      &{$action_if_found}();
    }
  }
  else {
    if( defined( $action_if_not_found ) and "CODE" eq ref( $action_if_not_found ) ) {
      &{$action_if_not_found}();
    }
  }

  return $have_sizes;
}

sub _alignof_type_define_name {
  my $type = $_[0];
  my $have_name = "ALIGNOF_" . uc($type);
  $have_name =~ tr/*/P/;
  $have_name =~ tr/_A-Za-z0-9/_/c;
  return $have_name;
}

=head2 check_alignof_type (type, [action-if-found], [action-if-not-found], [prologue = default includes])

Define ALIGNOF_type to be the alignment in bytes of type. I<type y;> must
be valid as a structure member declaration or I<type> must be a structure
member itself.

This method caches its result in the C<ac_cv_alignof_E<lt>set langE<gt>>_type
variable, with I<*> mapped to C<p> and other characters not suitable for a
variable name mapped to underscores.

=cut

sub check_alignof_type {
  my ($self, $type, $action_if_found, $action_if_not_found, $prologue) = @_;
  $self = $self->_get_instance();
  defined( $type ) or return; # XXX prefer croak
  ref( $type ) eq "" or return;

  my $cache_name = $self->_cache_type_name( "alignof", $self->{lang}, $type );
  my $check_sub = sub {

    my @decls = (
      "#ifndef offsetof",
      "# ifdef __ICC",
      "#  define offsetof(type,memb) ((size_t)(((char *)(&((type*)0)->memb)) - ((char *)0)))",
      "# else",
      "#  define offsetof(type,memb) ((size_t)&((type*)0)->memb)",
      "# endif",
      "#endif"
    );

    my ($struct, $memb);
    if( $type =~ m/^([^.]+)\.([^.]+)$/ ) {
      $struct = $1;
      $memb = $2;
    }
    else {
      push( @decls, "typedef struct { char x; $type y; } ac__type_alignof_;" );
      $struct = "ac__type_alignof_";
      $memb = "y";
    }
  
    my $typealign = $self->_compute_int_compile( "offsetof($struct, $memb)", $prologue, @decls );
    $self->define_var( _alignof_type_define_name( $type ), $typealign ? $typealign : undef, "defined when alignof($type) is available" );
    if( $typealign ) {
      if( defined( $action_if_found ) and "CODE" eq ref( $action_if_found ) ) {
	&{$action_if_found}();
      }
    }
    else {
      if( defined( $action_if_not_found ) and "CODE" eq ref( $action_if_not_found ) ) {
	&{$action_if_not_found}();
      }
    }

    return $typealign;
  };

  return $self->check_cached( $cache_name, "for align of $type", $check_sub );
}

=head2 check_alignof_types (type, [action-if-found], [action-if-not-found], [prologue = default includes])

For each type L<check_alignof_type> is called to check for align of type.

If I<action-if-found> is given, it is additionally executed when all of the
aligns of the types could determined. If I<action-if-not-found> is given, it
is executed when one align of the types could not determined.

=cut

sub check_alignof_types {
  my ($self, $types, $action_if_found, $action_if_not_found, $prologue) = @_;
  $self = $self->_get_instance();

  my $have_aligns = 1;
  foreach my $type (@$types) {
    $have_aligns &= ! ! ($self->check_alignof_type ( $type, undef, undef, $prologue ));
  }

  if( $have_aligns ) {
    if( defined( $action_if_found ) and "CODE" eq ref( $action_if_found ) ) {
      &{$action_if_found}();
    }
  }
  else {
    if( defined( $action_if_not_found ) and "CODE" eq ref( $action_if_not_found ) ) {
      &{$action_if_not_found}();
    }
  }

  return $have_aligns;
}

sub _have_member_define_name {
  my $member = $_[0];
  my $have_name = "HAVE_" . uc($member);
  $have_name =~ tr/_A-Za-z0-9/_/c;
  return $have_name;
}

=head2 check_member (member, [action-if-found], [action-if-not-found], [prologue = default includes])

Check whether I<member> is in form of I<aggregate>.I<member> and
I<member> is a member of the I<aggregate> aggregate. I<prologue>
should be a series of include directives, defaulting to
I<default includes>, which are used prior to the aggregate under test.

  Config::AutoConf->check_member(
    "struct STRUCT_SV.sv_refcnt",
    undef,
    sub { Config::AutoConf->msg_failure( "sv_refcnt member required for struct STRUCT_SV" ); }
    "#include <EXTERN.h>\n#include <perl.h>"
  );

If I<aggregate> aggregate has I<member> member, preprocessor
macro HAVE_I<aggregate>_I<MEMBER> (in all capitals, with spaces
and dots replaced by underscores) is defined.

This macro caches its result in the C<ac_cv_>aggr_member variable.

=cut

sub check_member {
  my ($self, $member, $action_if_found, $action_if_not_found, $prologue) = @_;
  $self = $self->_get_instance();
  defined( $member ) or return; # XXX prefer croak
  ref( $member ) eq "" or return;

  $member =~ m/^([^.]+)\.([^.]+)$/ or return;
  my $type = $1;
  $member = $2;

  my $cache_name = $self->_cache_type_name( "member", $type );
  my $check_sub = sub {
  
    my $body = <<ACEOF;
  static $type check_aggr;
  if( check_aggr.$member )
    return 0;
ACEOF
    my $conftest = $self->lang_build_program( $prologue, $body );

    my $have_member = $self->compile_if_else( $conftest );
    $self->define_var( _have_member_define_name( $member ), $have_member ? $have_member : undef, "defined when $member is available" );
    if( $have_member ) {
      if( defined( $action_if_found ) and "CODE" eq ref( $action_if_found ) ) {
	&{$action_if_found}();
      }
    }
    else {
      if( defined( $action_if_not_found ) and "CODE" eq ref( $action_if_not_found ) ) {
	&{$action_if_not_found}();
      }
    }

    return $have_member;
  };

  return $self->check_cached( $cache_name, "for $type.$member", $check_sub );
}

=head2 check_members (members, [action-if-found], [action-if-not-found], [prologue = default includes])

For each member L<check_member> is called to check for member of aggregate.

If I<action-if-found> is given, it is additionally executed when all of the
aggregate members are found. If I<action-if-not-found> is given, it is
executed when one of the aggregate members is not found.

=cut

sub check_members {
  my ($self, $members, $action_if_found, $action_if_not_found, $prologue) = @_;
  $self = $self->_get_instance();

  my $have_members = 1;
  foreach my $member (@$members) {
    $have_members &= $self->check_member( $member, undef, undef, $prologue );
  }

  if( $have_members ) {
    if( defined( $action_if_found ) and "CODE" eq ref( $action_if_found ) ) {
      &{$action_if_found}();
    }
  }
  else {
    if( defined( $action_if_not_found ) and "CODE" eq ref( $action_if_not_found ) ) {
      &{$action_if_not_found}();
    }
  }

  return $have_members;
}

=head2 check_headers

This function uses check_header to check if a set of include files exist in the system and can
be included and compiled by the available compiler. Returns the name of the first header file found.

=cut

sub check_headers {
  my $self = shift;

  for (@_) {
    return $_ if $self->check_header($_)
  }

  return undef;
}

sub _have_header_define_name {
  my $header = $_[0];
  my $have_name = "HAVE_" . uc($header);
  $have_name =~ tr/_A-Za-z0-9/_/c;
  return $have_name;
}

sub _check_header {
  my ($self, $header, $prologue, $body) = @_;

  $prologue .= <<"_ACEOF";
    #include <$header>
_ACEOF
  my $conftest = $self->lang_build_program( $prologue, $body );

  my $have_header = $self->compile_if_else( $conftest );
  return $have_header;
}


=head2 check_header

This function is used to check if a specific header file is present in
the system: if we detect it and if we can compile anything with that
header included. Note that normally you want to check for a header
first, and then check for the corresponding library (not all at once).

The standard usage for this module is:

  Config::AutoConf->check_header("ncurses.h");
  
This function will return a true value (1) on success, and a false value
if the header is not present or not available for common usage.

=cut

sub check_header {
  my $self = shift;
  my $header = shift;
  my $pre_inc = shift;
  

  return 0 unless $header;
  my $prologue  = "";
  defined $pre_inc
    and $prologue .= "$pre_inc\n";

  my $cache_name = $self->_cache_name( $header );
  my $check_sub = sub {
  
    my $have_header = $self->_check_header( $header, $prologue, "" );
    $self->define_var( _have_header_define_name( $header ), $have_header ? $have_header : undef, "defined when $header is available" );

    return $have_header;
  };

  return $self->check_cached( $cache_name, "for $header", $check_sub );
}

=head2 check_all_headers

This function checks each given header for usability.

=cut

sub check_all_headers {
  my $self = shift->_get_instance();
  @_ or return;
  my $rc = 1;
  foreach my $header (@_) {
    $rc &= $self->check_header( $header );
  }
  return $rc;
}

=head2 check_stdc_headers

Checks for standard C89 headers, namely stdlib.h, stdarg.h, string.h and float.h.
If those are found, additional all remaining C89 headers are checked: assert.h,
ctype.h, errno.h, limits.h, locale.h, math.h, setjmp.h, signal.h, stddef.h,
stdio.h and time.h.

Returns a false value if it fails.

=cut

sub check_stdc_headers {
  my $self = shift->_get_instance();
  my $rc = 0;
  if( $rc = $self->check_all_headers( qw(stdlib.h stdarg.h string.h float.h) ) ) {
    $rc &= $self->check_all_headers( qw/assert.h ctype.h errno.h limits.h/ );
    $rc &= $self->check_all_headers( qw/locale.h math.h setjmp.h signal.h/ );
    $rc &= $self->check_all_headers( qw/stddef.h stdio.h time.h/ );
  }
  if( $rc ) {
    $self->define_var( "STDC_HEADERS", 1, "Define to 1 if you have the ANSI C header files." );
  }
  return $rc;
}

=head2 check_default_headers

This function checks for some default headers, the std c89 haeders and
sys/types.h, sys/stat.h, memory.h, strings.h, inttypes.h, stdint.h and unistd.h

=cut

sub check_default_headers {
  my $self = shift->_get_instance();
  my $rc = $self->check_stdc_headers() and $self->check_all_headers( qw(sys/types.h sys/stat.h memory.h strings.h inttypes.h stdint.h unistd.h) );
  return $rc;
}

=head2 check_dirent_header

Check for the following header files. For the first one that is found and
defines 'DIR', define the listed C preprocessor macro:

  dirent.h 	HAVE_DIRENT_H
  sys/ndir.h 	HAVE_SYS_NDIR_H
  sys/dir.h 	HAVE_SYS_DIR_H
  ndir.h 	HAVE_NDIR_H

The directory-library declarations in your source code should look
something like the following:

  #include <sys/types.h>
  #ifdef HAVE_DIRENT_H
  # include <dirent.h>
  # define NAMLEN(dirent) strlen ((dirent)->d_name)
  #else
  # define dirent direct
  # define NAMLEN(dirent) ((dirent)->d_namlen)
  # ifdef HAVE_SYS_NDIR_H
  #  include <sys/ndir.h>
  # endif
  # ifdef HAVE_SYS_DIR_H
  #  include <sys/dir.h>
  # endif
  # ifdef HAVE_NDIR_H
  #  include <ndir.h>
  # endif
  #endif

Using the above declarations, the program would declare variables to be of
type C<struct dirent>, not C<struct direct>, and would access the length
of a directory entry name by passing a pointer to a C<struct dirent> to
the C<NAMLEN> macro.

This macro might be obsolescent, as all current systems with directory
libraries have C<<E<lt>dirent.hE<gt>>>. Programs supporting only newer OS
might not need touse this macro.

=cut

sub check_dirent_header {
  my $self = shift->_get_instance();

  my $cache_name = $self->_cache_name( "header_dirent" );
  my $check_sub = sub {

    my $have_dirent;
    foreach my $header (qw(dirent.h sys/ndir.h sys/dir.h ndir.h)) {
      $have_dirent = $self->_check_header( $header, "#include <sys/types.h>\n", "if ((DIR *) 0) { return 0; }" );
      $self->define_var( _have_header_define_name( $header ), $have_dirent ? $have_dirent : undef, "defined when $header is available" );
      $have_dirent and $have_dirent = $header and last;
    }

    return $have_dirent;
  };


  return $self->check_cached( $cache_name, "for header defining DIR *", $check_sub );
}

sub _have_lib_define_name {
  my $lib = $_[0];
  my $have_name = "HAVE_LIB" . uc($lib);
  $have_name =~ tr/_A-Za-z0-9/_/c;
  return $have_name;
}

=head2 check_lm( [ action-if-found ], [ action-if-not-found ] )

This method is used to check if some common C<math.h> functions are
available, and if C<-lm> is needed. Returns the empty string if no
library is needed, or the "-lm" string if libm is needed.

Actions are only called at the end of the list of tests. If one fails,
I<action-if-not-found> is run. Otherwise, I<action-if-found> is run.

=cut

sub check_lm {
    my ($self, $aif, $ainf) = @_;
    ref($self) or $self = $self->_get_instance();

    my $fail = 0;
    my $required = "";
    for my $func (qw(log2 pow log10 log exp sqrt)) {

        my $ans = $self->search_libs( $func, ['m'] );

        $ans or $fail = 1;
        ($ans ne "none required") and $required = $ans;
    }

    if ($fail) { $ainf && $ainf->() }
    else       { $aif  && $aif->() }

    return $required;
}

=head2 check_lib( lib, func, [ action-if-found ], [ action-if-not-found ], [ @other-libs ] )

This function is used to check if a specific library includes some
function. Call it with the library name (without the lib portion), and
the name of the function you want to test:

  Config::AutoConf->check_lib("z", "gzopen");

It returns 1 if the function exist, 0 otherwise.

I<action-if-found> and I<action-if-not-found> can be CODE references
whereby the default action in case of function found is to define
the HAVE_LIBlibrary (all in capitals) preprocessor macro with 1 and
add $lib to the list of libraries to link.

If linking with library results in unresolved symbols that would be
resolved by linking with additional libraries, give those libraries
as the I<other-libs> argument: e.g., C<[qw(Xt X11)]>.
Otherwise, this routine may fail to detect that library is present,
because linking the test program can fail with unresolved symbols.
The other-libraries argument should be limited to cases where it is
desirable to test for one library in the presence of another that
is not already in LIBS. 

It's recommended to use L<search_libs> instead of check_lib these days.

=cut

sub check_lib {
    my ( $self, $lib, $func, $action_if_found, $action_if_not_found, @other_libs ) = @_;
    ref($self) or $self = $self->_get_instance();

    return 0 unless $lib;
    return 0 unless $func;

    scalar( @other_libs ) == 1 and ref( $other_libs[0] ) eq "ARRAY"
      and @other_libs = @{ $other_libs[0] };

    my $cache_name = $self->_cache_name( "lib", $lib, $func );
    my $check_sub = sub {
        my $conftest = $self->lang_call( "", $func );

        my @save_libs = @{$self->{extra_libs}};
        push( @{$self->{extra_libs}}, $lib, @other_libs );
        my $have_lib = $self->link_if_else( $conftest );
        $self->{extra_libs} = [ @save_libs ];

        if( $have_lib ) {
            if( defined( $action_if_found ) and "CODE" eq ref( $action_if_found ) ) {
                &{$action_if_found}();
            }
            else {
                $self->define_var( _have_lib_define_name( $lib ), $have_lib,
                                   "defined when library $lib is available" );
                push( @{$self->{extra_libs}}, $lib );
            }
        }
        else {
            if( defined( $action_if_not_found ) and "CODE" eq ref( $action_if_not_found ) ) {
                &{$action_if_not_found}();
            }
            else {
                $self->define_var( _have_lib_define_name( $lib ), undef,
                                   "defined when library $lib is available" );
            }
        }
        return $have_lib;
    };

    return $self->check_cached( $cache_name, "for $func in -l$lib", $check_sub );
}

=head2 search_libs( function, search-libs, [action-if-found], [action-if-not-found], [other-libs] )

Search for a library defining function if it's not already available.
This equates to calling

    Config::AutoConf->link_if_else(
        Config::AutoConf->lang_call( "", "$function" ) );

first with no libraries, then for each library listed in search-libs.
I<search-libs> must be specified as an array reference to avoid
confusion in argument order.

Prepend -llibrary to LIBS for the first library found to contain function,
and run I<action-if-found>. If the function is not found, run
I<action-if-not-found>.

If linking with library results in unresolved symbols that would be
resolved by linking with additional libraries, give those libraries as
the I<other-libraries> argument: e.g., C<[qw(Xt X11)]>. Otherwise, this
method fails to detect that function is present, because linking the
test program always fails with unresolved symbols.

The result of this test is cached in the ac_cv_search_function variable
as "none required" if function is already available, as C<0> if no
library containing function was found, otherwise as the -llibrary option
that needs to be prepended to LIBS.

=cut

sub search_libs {
  my ( $self, $func, $libs, $action_if_found, $action_if_not_found, @other_libs ) = @_;
  ref($self) or $self = $self->_get_instance();

  ( defined( $libs ) and "ARRAY" eq ref( $libs ) and scalar( @{$libs} ) > 0 )
    or return 0; # XXX would prefer croak
  return 0 unless $func;

  scalar( @other_libs ) == 1 and ref( $other_libs[0] ) eq "ARRAY"
    and @other_libs = @{ $other_libs[0] };

  my $cache_name = $self->_cache_name( "search", $func );
  my $check_sub = sub {

    my $conftest = $self->lang_call( "", $func );

    my @save_libs = @{$self->{extra_libs}};
    my $have_lib = 0;
    foreach my $libstest ( undef, @$libs ) {
      # XXX would local work on array refs? can we omit @save_libs?
      $self->{extra_libs} = [ @save_libs ];
      defined( $libstest ) and unshift( @{$self->{extra_libs}}, $libstest, @other_libs );
      $self->link_if_else( $conftest ) and ( $have_lib = defined( $libstest ) ? $libstest : "none required" ) and last;
    }
    $self->{extra_libs} = [ @save_libs ];
    if( $have_lib ) {
      $have_lib eq "none required" or unshift( @{$self->{extra_libs}}, $have_lib );

      if( defined( $action_if_found ) and "CODE" eq ref( $action_if_found ) ) {
	&{$action_if_found}();
      }
    }
    else {
      if( defined( $action_if_not_found ) and "CODE" eq ref( $action_if_not_found ) ) {
	&{$action_if_not_found}();
      }
    }

    return $have_lib;
  };

  return $self->check_cached( $cache_name, "for library containing $func", $check_sub );
}

=head2 pkg_config_package_flags($package, [action-if-found], [action-if-not-found])

Search for pkg-config flags for package as specified. The flags which are
extracted are C<--cflags> and C<--libs>. The extracted flags are appended
to the global C<extra_compile_flags> and C<extra_link_flags>, respectively.

Call it with the package you're looking for and optional callback whether
found or not.

=cut

my $_pkg_config_prog;

sub _pkg_config_flag
{
    defined $_pkg_config_prog or croak("pkg_config_prog required");
    my @pkg_config_args = @_;
    my ( $stdout, $stderr, $exit ) =
      capture { system( $_pkg_config_prog, @pkg_config_args ); };
    chomp $stdout;
    0 == $exit and return $stdout;
    return;
}

sub pkg_config_package_flags
{
    my ( $self, $package, $action_if_found, $action_if_not_found ) = @_;
    $self = $self->_get_instance();
    (my $pkgpfx = $package) =~ s/^(\w+).*?$/$1/;
    my $cache_name = $self->_cache_name( "pkg", $pkgpfx );
    defined $_pkg_config_prog or $_pkg_config_prog = $self->check_prog_pkg_config;
    my $check_sub = sub {
	my ( @pkg_cflags, @pkg_libs );

        (my $ENV_CFLAGS = $package) =~ s/^(\w+).*?$/$1_CFLAGS/;
	my $CFLAGS = defined $ENV{$ENV_CFLAGS} ? $ENV{$ENV_CFLAGS}
	                                       : _pkg_config_flag($package, "--cflags");
        $CFLAGS and @pkg_cflags = (
            map {
                $_ =~ s/^\s+//;
                $_ =~ s/\s+$//;
                Text::ParseWords::shellwords $_;
              } split( m/\n/, $CFLAGS )
          )
	  and push @{ $self->{extra_preprocess_flags} }, @pkg_cflags;
	  # and push @{ $self->{extra_compile_flags}->{"C"} }, @pkg_cflags;
# XXX extra_preprocess_flags

        (my $ENV_LIBS = $package) =~ s/^(\w+).*?$/$1_LIBS/;
        # do not separate between libs and extra (for now) - they come with -l prepended
	my $LIBS = defined $ENV{$ENV_LIBS} ? $ENV{$ENV_LIBS}
	                                   : _pkg_config_flag($package, "--libs");
        $LIBS and @pkg_libs = (
            map {
                $_ =~ s/^\s+//;
                $_ =~ s/\s+$//;
                Text::ParseWords::shellwords $_;
              } split( m/\n/, $LIBS )
          )
	  and push @{ $self->{extra_link_flags} }, @pkg_libs;

	return join(" ", @pkg_cflags, @pkg_libs);
    };

    return $self->check_cached( $cache_name, "for pkg-config package of $package", $check_sub );
}

#
#
# Auxiliary funcs
#

sub _sanitize {
  # This is hard coded, and maybe a little stupid...
  my $x = shift;
  $x =~ s/ //g;
  $x =~ s/\///g;
  $x =~ s/\\//g;
  return $x;
}

sub _get_instance {
  my $class = shift;
  ref $class and return $class;
  defined( $glob_instance ) and ref( $glob_instance ) and return $glob_instance;
  $glob_instance = $class->new();
  return $glob_instance;
}

sub _get_builder {
  my $self = $_[0]->_get_instance();
  defined( $self->{lang_supported}->{ $self->{lang} } ) or croak( "Unsupported compile language \"" . $self->{lang} . "\"" );

  my $builder = $self->{lang_supported}->{ $self->{lang} }->new( quiet => 1 );

  ## XXX - Temporarily. Will try to send upstream
  if ($self->{lang} eq "C") {
      $builder->{config}{ccflags} =~ s/-arch \S+//g;
      $builder->{config}{lddlflags} =~ s/-arch \S+//g;
      $builder->{config}{ldflags} =~ s/-arch \S+//g;
  }
  return $builder;

}

sub _set_language {
  my $self = shift->_get_instance();
  my ($lang, $impl) = @_;

  defined( $lang ) or croak( "Missing language" );

  defined( $impl ) and defined( $self->{lang_supported}->{$lang} )
    and $impl ne $self->{lang_supported}->{$lang}
    and croak( "Language implementor ($impl) doesn't match exisiting one (" . $self->{lang_supported}->{$lang} . ")" );

  defined( $impl ) and !defined( $self->{lang_supported}->{$lang} )
    and $self->{lang_supported}->{$lang} = $impl;

  defined( $self->{lang_supported}->{$lang} ) or croak( "Unsupported language \"$lang\"" );

  defined( $self->{extra_compile_flags}->{$lang} ) or $self->{extra_compile_flags}->{$lang} = [];

  $self->{lang} = $lang;

  return;
}

sub _fill_defines {
  my ($self, $src, $action_if_true, $action_if_false) = @_;
  ref $self or $self = $self->_get_instance();

  my $conftest = "";
  while( my ($defname, $defcnt) = each( %{ $self->{defines} } ) ) {
    $defcnt->[0] or next;
    defined $defcnt->[1] and $conftest .= "/* " . $defcnt->[1] . " */\n";
    $conftest .= join( " ", "#define", $defname, $defcnt->[0] ) . "\n";
  }
  $conftest .= "/* end of conftest.h */\n";

  return $conftest;
}

#
# default includes taken from autoconf/headers.m4
#

=head2 _default_includes

returns a string containing default includes for program prologue taken
from autoconf/headers.m4:

  #include <stdio.h>
  #ifdef HAVE_SYS_TYPES_H
  # include <sys/types.h>
  #endif
  #ifdef HAVE_SYS_STAT_H
  # include <sys/stat.h>
  #endif
  #ifdef STDC_HEADERS
  # include <stdlib.h>
  # include <stddef.h>
  #else
  # ifdef HAVE_STDLIB_H
  #  include <stdlib.h>
  # endif
  #endif
  #ifdef HAVE_STRING_H
  # if !defined STDC_HEADERS && defined HAVE_MEMORY_H
  #  include <memory.h>
  # endif
  # include <string.h>
  #endif
  #ifdef HAVE_STRINGS_H
  # include <strings.h>
  #endif
  #ifdef HAVE_INTTYPES_H
  # include <inttypes.h>
  #endif
  #ifdef HAVE_STDINT_H
  # include <stdint.h>
  #endif
  #ifdef HAVE_UNISTD_H
  # include <unistd.h>
  #endif

=cut

sub _default_includes {
  my $conftest .= <<"_ACEOF";
#include <stdio.h>
#ifdef HAVE_SYS_TYPES_H
# include <sys/types.h>
#endif
#ifdef HAVE_SYS_STAT_H
# include <sys/stat.h>
#endif
#ifdef STDC_HEADERS
# include <stdlib.h>
# include <stddef.h>
#else
# ifdef HAVE_STDLIB_H
#  include <stdlib.h>
# endif
#endif
#ifdef HAVE_STRING_H
# if !defined STDC_HEADERS && defined HAVE_MEMORY_H
#  include <memory.h>
# endif
# include <string.h>
#endif
#ifdef HAVE_STRINGS_H
# include <strings.h>
#endif
#ifdef HAVE_INTTYPES_H
# include <inttypes.h>
#endif
#ifdef HAVE_STDINT_H
# include <stdint.h>
#endif
#ifdef HAVE_UNISTD_H
# include <unistd.h>
#endif
_ACEOF

  return $conftest;
}

sub _default_main {
  return $_[0]->_build_main("");
}

sub _build_main {
  my $self = shift->_get_instance();
  my $body = shift || "";

  my $conftest .= <<"_ACEOF";
  int
  main ()
  {
    $body;
    return 0;
  }
_ACEOF

  return $conftest;
}

sub _cache_prefix {
  return "ac";
}

sub _cache_name {
  my ($self, @names) = @_;
  my $cache_name = join( "_", $self->_cache_prefix(), "cv", @names );
     $cache_name =~ tr/_A-Za-z0-9/_/c;
  return $cache_name;
}

sub _get_log_fh {
  my $self = $_[0]->_get_instance();
  unless( defined( $self->{logfh} ) ) {
    open( $self->{logfh}, ">", $self->{logfile} ) or croak "Could not open file $self->{logfile}: $!";
  }

  return $self->{logfh};
}

sub _add2log {
  my ($self, @logentries) = @_;
  ref($self) or $self = $self->_get_instance();
  $self->_get_log_fh();
  foreach my $logentry (@logentries) {
    print {$self->{logfh}} "$logentry\n";
  }

  return;
}

sub _cache_type_name  {
  my ($self, @names) = @_;
  return $self->_cache_name( map { $_ =~ tr/*/p/; $_ } @names );
}

sub _get_extra_compiler_flags {
  my $self = shift->_get_instance();
  my @ppflags = @{$self->{extra_preprocess_flags}};
  my @cflags = @{$self->{extra_compile_flags}->{ $self->{lang} }};
  return join( " ", @ppflags, @cflags );
}

sub _get_extra_linker_flags {
  my $self = shift->_get_instance();
  my @libs = @{$self->{extra_libs}};
  my @ldflags = @{$self->{extra_link_flags}};
  return join( " ", @ldflags, map { "-l$_" } @libs );
}

=head1 AUTHOR

Alberto Simões, C<< <ambs@cpan.org> >>

Jens Rehsack, C<< <rehsack@cpan.org> >>

=head1 NEXT STEPS

Although a lot of work needs to be done, this is the next steps I
intent to take.

  - detect flex/lex
  - detect yacc/bison/byacc
  - detect ranlib (not sure about its importance)

These are the ones I think not too much important, and will be
addressed later, or by request.

  - detect an 'install' command
  - detect a 'ln -s' command -- there should be a module doing
    this kind of task.

=head1 BUGS

A lot. Portability is a pain. B<<Patches welcome!>>.

Please report any bugs or feature requests to
C<bug-extutils-autoconf@rt.cpan.org>, or through the web interface at
L<http://rt.cpan.org>.  I will be notified, and then you'll automatically
be notified of progress on your bug as I make changes.

=head1 ACKNOWLEDGEMENTS

Michael Schwern for kind MacOS X help.

Ken Williams for ExtUtils::CBuilder

=head1 COPYRIGHT & LICENSE

Copyright 2004-2011 by the Authors

This program is free software; you can redistribute it and/or modify it
under the same terms as Perl itself.

=head1 SEE ALSO

ExtUtils::CBuilder(3)

=cut

1; # End of Config::AutoConf

END_OF_EXTUTILS_BUNDLE_MAKER_MARKER

    $@ and die $@;
    defined $INC{'Config/AutoConf.pm'} or $INC{'Config/AutoConf.pm'} = 'Bundled';
};


1;
