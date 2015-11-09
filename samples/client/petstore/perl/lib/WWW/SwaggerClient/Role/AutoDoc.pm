package WWW::SwaggerClient::Role::AutoDoc;
use List::MoreUtils qw(uniq);

use Moose::Role;

sub autodoc {
	my $self = shift;
	
	$self->_printisa;
	$self->_printmethods;
	$self->_printattrs;
	print "\n";
}

sub _printisa {
	my $self = shift;	
	my $meta = $self->meta;
	
	my $myclass = ref $self;
	
	my $super = join ', ', $meta->superclasses;
	my @roles = $meta->calculate_all_roles;
	shift(@roles); # the first is a composite, the rest are the roles

	my $isa =   join ', ', $meta->linearized_isa;
	my $sub =   join ', ', $meta->subclasses;
	my $dsub =  join ', ', $meta->direct_subclasses;
	
	my ($rolepkg, $role_reqs);
	
	$~ = 'INHERIT';
	write;

	foreach my $role (@roles) {
		$rolepkg = $role->{package};
		$role_reqs = join ', ', keys %{$role->{required_methods}};
		$~ = 'ROLES';
		write;
	}
	
# ----- format specs -----
	format INHERIT = 

@* -
$myclass
                  ISA: @*
                       $isa
    Direct subclasses: @*
                       $dsub
       All subclasses: @*
                       $sub
.
	format ROLES =   
             Composes: @*
                       $rolepkg                                     
                       requires: ^<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<< ~~
                       $role_reqs
.
# ----- / format specs -----
}

sub _printmethods {
	my $self = shift;
	$~ = 'METHODHEAD';
	write;
	$self->_printmethod($_) for uniq sort $self->meta->get_method_list, $self->meta->get_all_method_names;
}

sub _printmethod {
	my ($self, $methodname) = @_;
	return if $methodname =~ /^_/;
	return if $self->meta->has_attribute($methodname);
	my %internal = map {$_ => 1} qw(BUILD BUILDARGS meta can new DEMOLISHALL DESTROY 
		DOES isa BUILDALL does VERSION dump
		);
	return if $internal{$methodname};
	my $method = $self->meta->get_method($methodname) or return; # symbols imported into namespaces i.e. not known by Moose
	
	return if $method->original_package_name eq __PACKAGE__;
	
	my $delegation = '';
	my $delegate_to = '';
	my $via = '';
	my $on = '';
	if ($method->can('associated_attribute')) {
		$delegate_to = $method->delegate_to_method;
		my $aa = $method->associated_attribute;
		$on = $aa->{isa};
		$via = $aa->{name};
	}
	
	$~ = 'METHOD';
	write;

# ----- format specs -----
	format METHODHEAD =

METHODS
-------
Name                                                  delegate to                on                            via
===================================================================================================================================
.
	format METHOD = 
@<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<< @<<<<<<<<<<<<<<<<<<<<<<... @<<<<<<<<<<<<<<<<<<<<<<<<<... @<<<<<<<<<<<<<<<<...
$methodname,                                          $delegate_to,              $on,                          $via
.
# ----- / format specs -----
}

sub _printattrs {
	my $self = shift;
	$~ = 'ATTRHEAD';
	write;
	$self->_printattr($_) for sort $self->meta->get_attribute_list;
}

sub _printattr {
	my ($self, $attrname) = @_;
	return if $attrname =~ /^_/;
	my $attr = $self->meta->get_attribute($attrname) or die "No attr for $attrname";
	
	my $is;
	$is = 'rw' if $attr->get_read_method && $attr->get_write_method;
	$is = 'ro' if $attr->get_read_method && ! $attr->get_write_method;
	$is = 'wo' if $attr->get_write_method && ! $attr->get_read_method;
	$is = '--' if ! $attr->get_write_method && ! $attr->get_read_method;
	$is or die "No \$is for $attrname";
	
	my $tc = $attr->type_constraint || '';
	my $from = $attr->associated_class->name || '';
	my $reqd = $attr->is_required ? 'reqd' : 'opt';
	my $lazy = $attr->is_lazy ? 'lazy' : '';
	my $doc = $attr->has_documentation ? 'yes' : '';
	my $handles = join ', ', sort @{$attr->handles || []};
	
	$~ = 'ATTR';
	write;

# ----- format specs -----
	format ATTRHEAD =

ATTRIBUTES
----------
Name                is isa                       reqd lazy doc handles
==============================================================================================================
.	
	format ATTR = 
@<<<<<<<<<<<<<<<<<  @< @<<<<<<<<<<<<<<<<<<<<<<<< @<<< @<<< @<< ^<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
$attrname,          $is, $tc,                    $reqd, $lazy, $doc, $handles
                                                               ^<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<< ~~
                                                               $handles
.
# ----- / format specs -----
}

1;