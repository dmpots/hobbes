#!/usr/bin/perl
use File::Basename;
$EnableMail = 0;
$SharedLibs = ""; #or "--no-shared-libs"
@tools    = qw(opcodemix trace);
@commands = qw(commands/fibon.commands);
if( -e "tools.conf") {
  print "USING tools.conf\n";
  open TOOLS, "tools.conf" or die "error opening tools.conf file";
  @tools = ();
  while(<TOOLS>) {
    next if /^#/;
    chomp;
    my ($tool) = split;
    push @tools, $tool;
  }
  close TOOLS;
}
if( -e "commands.conf") {
  print "USING commands.conf\n";
  open COMMANDS, "commands.conf" or die "error opening commands.conf file";
  @commands = ();
  while(<COMMANDS>) {
    next if /^#/;
    chomp;
    my ($commandFile) = split;
    push @commands, $commandFile;
  }
  close COMMANDS;
}
print "Commands: @commands\n";
print "Tools:    @tools\n";

for my $PinTool (@tools)    {
for my $cmdFile (@commands) {
  print "Running command file:          $cmdFile\n";
  if (! -e $cmdFile) {
    print "Missing command file $cmdFile\n";
    exit 1;
  }
  open RUN, "perl runPin.pl --$PinTool $SharedLibs $cmdFile|" 
    or die "error running runPin.pl\n";
  while (<RUN>) {
    print;
  }
  close RUN; if ($? != 0){die "Error running pin tool\n";}
  $destDir = basename($cmdFile);
  $destDir =~ s/\.commands//;
  $destDir.= ".$PinTool";
  $destDir = "RESULTS/$destDir";
  if(! -e $destDir) {
    print "Creating dest dir     $destDir\n";
    system("mkdir $destDir");
  }
  print "Moving results to dir $destDir\n";
  system("mv RESULTS/*.LOG $destDir");
  if ($? != 0) {print "ERROR running pin tool\n"; exit 1;}
  print "Finished running command file: $cmdFile\n";
}
}

print "Pin loop all done\n";
if($EnableMail) {
  $mailCmd = "mail -s \"[PIN-RUN] finished\" dmp\@rice.edu < /dev/null";
  system($mailCmd);
}

