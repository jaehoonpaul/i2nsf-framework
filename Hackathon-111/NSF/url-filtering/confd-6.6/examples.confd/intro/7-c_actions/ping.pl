#!/usr/bin/perl

while ($#ARGV >= 0) {
    if ($ARGV[0] eq "-c") {
        $context = $ARGV[1];
        shift; shift;
    } elsif ($ARGV[0] eq "-p") {
        @path = split(' ', $ARGV[1]);
        shift; shift;
    } elsif ($ARGV[0] eq "count") {
        $count = $ARGV[1];
        shift; shift;
    } else {
        &fail("Unknown argument " . $ARGV[0]);
    }
}
$host = $path[$#path];

$ENV{'PATH'} = "/bin:/usr/bin:/sbin:/usr/sbin:" . $ENV{'PATH'};
if (`uname -s` eq "SunOS\n") {
    $cmd = "ping -s $host 56 $count";
} else {
    $cmd = "ping -c $count $host";
}
$out = `$cmd 2>&1`;
if ($? != 0) {
    &fail($out);
}

@result = split('\n', $out);
print "header 'Invoked from " . $context . ": " . $result[0] . "'\n";
for ($i = 0; $i < $count; $i++) {
    print "response __BEGIN data '" . $result[$i+1] . "' response __END\n";
}
$packets = $result[$#result-1];
$times = $result[$#result];
print "statistics __BEGIN\n";
print "packet '" . $packets . "' time '" . $times . "'\n";
print "statistics __END\n";

exit 0;

sub fail {
    chomp($msg = join(' ', @_));
    print $msg;
    exit 1;
}
