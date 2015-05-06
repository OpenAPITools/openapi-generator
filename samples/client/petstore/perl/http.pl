use LWP::UserAgent;
 
my $ua = LWP::UserAgent->new;
 
my $server_endpoint = "http://petstore.swagger.io/v2/pet/10002";
 
# set custom HTTP request header fields
my $req = HTTP::Request->new(GET => $server_endpoint);
$req->header('content-type' => 'application/json');

use Data::Dumper;
print Dumper($req);
 
my $resp = $ua->request($req);
if ($resp->is_success) {
    my $message = $resp->decoded_content;
    print "Received reply: $message\n";
}
else {
    print "HTTP GET error code: ", $resp->code, "\n";
    print "HTTP GET error message: ", $resp->message, "\n";
}



