#!/usr/bin/perl

# ABSTRACT: save osm boundary to .poly file

# $Id$


use 5.010;
use strict;
use warnings;
use utf8;
use autodie;

use Carp;

use FindBin qw{ $Bin };

use LWP::UserAgent;
use Getopt::Long;
use List::Util qw{ min max sum };
use List::MoreUtils qw{ first_index none };
use IO::Uncompress::Gunzip qw{ gunzip $GunzipError };
use File::Slurp;

use YAML::Any qw/ Dump LoadFile /;

use Math::Polygon;
use Math::Polygon::Tree qw/ :all /;


####    Settings

my $api             = 'http://www.openstreetmap.org/api/0.6';
my $alias_config    = "$Bin/aliases.yml";
my $http_timeout    = 300;

my $save_mode = 'poly';
my %save_sub = (
    poly => \&_save_poly,
    shp  => \&_save_shp,
);



####    Command-line

GetOptions (
    'file=s'    => \my $filename,
    'o=s'       => \my $outfile,
    'onering!'  => \my $onering,
    'noinner!'  => \my $noinner,
    'proxy=s'   => \my $proxy,
    'aliases=s' => \$alias_config,
    'om=s'      => \$save_mode,
    'offset|buffer=f' => \my $offset,
);

unless ( @ARGV ) {
    print "Usage:  getbound.pl [options] <relation> [<relation> ...]\n\n";
    print "relation - id or alias\n\n";
    print "Available options:\n";
    print "     -o <file>       - output filename (default: STDOUT)\n";
    print "     -proxy <host>   - use proxy\n";
    print "     -onering        - merge rings\n\n";
    exit 1;
}


####    Aliases

my ($rename) = eval{ LoadFile $alias_config };
unless ( $rename ) {
    carp "Unable to load aliases from $alias_config: $@" if $alias_config;
    $rename = {};
}



####    Process

my @rel_ids = map { $rename->{$_} // $_ } @ARGV;

my %valid_role = (
    ''          => 'outer',
    'outer'     => 'outer',
    'border'    => 'outer',
    'exclave'   => 'outer',
    'inner'     => 'inner',
    'enclave'   => 'inner',
);


# getting and parsing
my $osm = OSM->new();
if ( $filename ) {
    my $xml = read_file $filename;
    $osm->load( $xml );
}
else {
    download_relation($_)  for @rel_ids;
}


# connecting rings: outers are counterclockwise!
logg( "Creating polygons" );

# contours are arrays [ \@chain, $is_inner ]
my @contours;

for my $rel_id ( @rel_ids ) {
    my $relation = $osm->{relations}->{$rel_id};
    my %ring;

    for my $member ( @{ $relation->{member} } ) {
        next unless $member->{type} eq 'way';
        my $role = $valid_role{ $member->{role} }  or next;
    
        my $way_id = $member->{ref};
        if ( !exists $osm->{chains}->{$way_id} ) {
            logg( "Incomplete data: way $way_id is missing" );
            next;
        }

        push @{ $ring{$role} },  [ @{ $osm->{chains}->{$way_id} } ];
    }

    while ( my ( $type, $list_ref ) = each %ring ) {
        while ( @$list_ref ) {
            my @chain = @{ shift @$list_ref };
        
            if ( $chain[0] eq $chain[-1] ) {
                my @contour = map { $osm->{nodes}->{$_} } @chain;
                my $order = 0 + Math::Polygon::Calc::polygon_is_clockwise(@contour);
                state $desired_order = { outer => 0, inner => 1 };
                push @contours, [
                    $order == $desired_order->{$type} ? \@contour : [reverse @contour],
                    $type ~~ 'inner',
                ];
                next;
            }

            my $pos = first_index { $chain[0] eq $_->[0] } @$list_ref;
            if ( $pos > -1 ) {
                shift @chain;
                $list_ref->[$pos] = [ (reverse @chain), @{$list_ref->[$pos]} ];
                next;
            }
            $pos = first_index { $chain[0] eq $_->[-1] } @$list_ref;
            if ( $pos > -1 ) {
                shift @chain;
                $list_ref->[$pos] = [ @{$list_ref->[$pos]}, @chain ];
                next;
            }
            $pos = first_index { $chain[-1] eq $_->[0] } @$list_ref;
            if ( $pos > -1 ) {
                pop @chain;
                $list_ref->[$pos] = [ @chain, @{$list_ref->[$pos]} ];
                next;
            }
            $pos = first_index { $chain[-1] eq $_->[-1] } @$list_ref;
            if ( $pos > -1 ) {
                pop @chain;
                $list_ref->[$pos] = [ @{$list_ref->[$pos]}, reverse @chain ];
                next;
            }
            logg( "Invalid data: ring is not closed" );

            logg( "Non-connecting chain:\n" . Dump( \@chain ) );
            exit 1;
        }
    }
}


if ( !@contours || none { !$_->[1] } @contours ) {
    logg( "Invalid data: no outer rings" );
    exit 1;
}


# outers first
# todo: second sort by area
@contours = sort { $a->[1] <=> $b->[1] || $#{$b->[0]} <=> $#{$a->[0]} } @contours;


##  Offset
if ( defined $offset ) {
    require Math::Clipper;

    my $ofs_contours = Math::Clipper::offset( [ map { $_->[0] } @contours ], $offset, 1000/$offset );

    @contours =
        sort { $a->[1] <=> $b->[1] || $#{$b->[0]} <=> $#{$a->[0]} }
        map {[ $_, Math::Polygon::Calc::polygon_is_clockwise(@$_) ]}
        map {[@$_, $_->[0]]}
        @$ofs_contours;
}



##  Merge rings
if ( $onering ) {
    logg( "Merging rings" );

    my $first_item = shift @contours;
    my @ring = @{ $first_item->[0] };

    while ( @contours ) {
        my ($add_contour, $is_inner) = @{ shift @contours };
        next if $noinner && $is_inner;

        # find close[st] points
        my $ring_center = polygon_centroid( \@ring );

        if ( $is_inner ) {
            my ( $index_i, $dist ) = ( 0, metric( $ring_center, $ring[0] ) );
            for my $i ( 1 .. $#ring ) {
                my $tdist = metric( $ring_center, $ring[$i] );
                next if $tdist >= $dist;
                ( $index_i, $dist ) = ( $i, $tdist );
            }
            $ring_center = $ring[$index_i];
        }

        @contours = sort { 
                    metric( $ring_center, polygon_centroid($a->[0]) ) <=>
                    metric( $ring_center, polygon_centroid($b->[0]) )
                } @contours;

        my $add_center = polygon_centroid( $add_contour );

        my ( $index_r, $dist ) = ( 0, metric( $add_center, $ring[0] ) );
        for my $i ( 1 .. $#ring ) {
            my $tdist = metric( $add_center, $ring[$i] );
            next if $tdist >= $dist;
            ( $index_r, $dist ) = ( $i, $tdist );
        }

        ( my $index_a, $dist ) = ( 0, metric( $ring[$index_r], $add_contour->[0] ) );
        for my $i ( 1 .. $#$add_contour ) {
            my $tdist = metric( $ring[$index_r], $add_contour->[$i] );
            next if $tdist >= $dist;
            ( $index_a, $dist ) = ( $i, $tdist );
        }

        # merge
        splice @ring, $index_r, 0, (
            $ring[$index_r],
            @$add_contour[ $index_a .. $#$add_contour-1 ],
            @$add_contour[ 0 .. $index_a-1 ],
            $add_contour->[$index_a],
        );
    }

    @contours = ( [ \@ring, 0 ] );
}




##  Output
logg( "Writing" );

$save_sub{$save_mode}->();

logg( "All ok" );
exit;


sub _save_poly {
    my $out = $outfile && $outfile ne q{-}
        ? do { open my $fh, '>', $outfile; $fh }
        : *STDOUT;

    my $rel = join q{+}, @rel_ids;
    print {$out} "Relation $rel\n\n";

    my $num = 1;

    for my $item ( @contours ) {
        my ($ring, $is_inner) = @$item;
        
        print {$out} ( $is_inner ? q{-} : q{}) . $num++ . "\n";
        for my $point ( @$ring ) {
            printf {$out} "   %-11s  %-11s\n", @$point;
        }
        print {$out} "END\n\n";
    }

    print {$out} "END\n";
    close $out;

    return;
}


sub _save_shp {
    require Geo::Shapefile::Writer;

    my $name = $outfile || join( q{-}, @ARGV );
    my $shp = Geo::Shapefile::Writer->new( $name, 'POLYGON', qw/ NAME GRMN_TYPE / );

    # !!! todo: rearrange contours
    my @shp_contours =
        map {[ reverse @{$_->[0]} ]}
#        grep { !$_->[1] }  # skip inners?
        @contours;

    $shp->add_shape( \@shp_contours, { GRMN_TYPE => 'DATA_BOUNDS' } );
    $shp->finalize();

    return;
}




sub metric {
    my ($p1, $p2) = @_;

    my ($x1, $y1, $x2, $y2) = map {@$_} map { ref $_ ? $_ : $osm->{nodes}->{$_} } ($p1, $p2);
    confess Dump \@_ if !defined $y2;
    return (($x2-$x1)*cos( ($y2+$y1)/2/180*3.14159 ))**2 + ($y2-$y1)**2;
}


sub logg {
    say STDERR @_;
    return;
}


##  HTTP functions

sub _init_ua {
    my $ua = LWP::UserAgent->new();
    $ua->proxy( 'http', $proxy )    if $proxy;
    $ua->default_header('Accept-Encoding' => 'gzip');
    $ua->timeout( $http_timeout );

    return $ua;
}


sub http_get {
    my ($url, %opt) = @_;
    state $ua = _init_ua();
    
    logg ". $url";
    my $req = HTTP::Request->new( GET => $url );

    my $res;
    for my $attempt ( 1 .. $opt{retry} || 1 ) {
        # logg ". attempt $attempt";
        $res = $ua->request($req);
        last if $res->is_success;
    }

    if ( !$res->is_success ) {
        logg 'Failed';
        return;
    }

    gunzip \($res->content) => \my $data;
    return $data;
}


sub download_relation {
    my ($rel_id) = @_;

    logg "Downloading RelID=$rel_id";

    my $data = http_get( "$api/relation/$rel_id/full", retry => 2 );

    if ( $data ) {
        $osm->load( $data );
    }
    else {
        logg "Unable to get full relation, trying by parts";
        my $rel_data = http_get( "$api/relation/$rel_id", retry => 3 );
        exit 1  if !$rel_data;

        $osm->load( $rel_data );
        my $relation = $osm->{relations}->{$rel_id};
        my @ways_to_load =
            grep { !exists $osm->{chains}->{$_} }
            map { $_->{ref} }
            grep { $_->{type} ~~ 'way' && $valid_role{$_->{role}} }
            @{ $relation->{member} };

        logg sprintf "%d ways to load", scalar @ways_to_load;
        for my $way_id ( @ways_to_load ) {
            my $way_data = http_get( "$api/way/$way_id/full", retry => 3 );
            exit 1  if !$way_data;
            $osm->load( $way_data );
        }
    }

    return;
}


##  OSM parser

package OSM;

use Geo::Openstreetmap::Parser;

sub new {
    my ($class, %opt) = @_;
    my $self = bless {}, $class;

    $self->{parser} = Geo::Openstreetmap::Parser->new(
        node => sub {
            my $attr = shift()->{attr};
            $self->{nodes}->{ $attr->{id} } = [ $attr->{lon}, $attr->{lat} ];
            return;
        },
        way => sub {
            my $obj = shift();
            my $id = $obj->{attr}->{id};
            $self->{chains}->{$id} = $obj->{nd};
            return;
        },
        relation => sub {
            my $obj = shift();
            my $id = $obj->{attr}->{id};
            $self->{relations}->{$id} = $obj;
            return;
        },
    );

    return $self;
}

sub load {
    my ($self, $xml) = @_;
    open my $fh, '<', \$xml;
    $self->{parser}->parse($fh);
    close $fh;
    return;
}


