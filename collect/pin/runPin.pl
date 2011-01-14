#!/usr/bin/perl
use Cwd;
use File::Basename;
use File::Copy;
use File::Path qw(remove_tree);

$PinDir="$ENV{HOME}/pin";
$ToolsDir="$PinDir/source/tools/pin-tools";
($Arch, $ArchDir)=GetArch();
$InstMixPinTool="$ToolsDir/opcodemix/$ArchDir/opcodemix.so";
$BBLenMixPinTool="$ToolsDir/bblengthmix/$ArchDir/bblengthmix.so";
$JumpMixPinTool="$ToolsDir/jumpmix/$ArchDir/jumpmix.so";
$RegMixPinTool="$ToolsDir/regmix/$ArchDir/regmix.so";
$TracePinTool="$ToolsDir/trace/$ArchDir/trace.so";
$Syms="ruby $ToolsDir/trace/syms.rb";
$DieOnNofibFailure=1;
$SanityCheckOnly=0;
$SharedLibsFlag="";
$Mode="pin";

$FullPathPinTool = $InstMixPinTool;
if(grep(/--opcodemix/i, @ARGV)) {
  $FullPathPinTool = $InstMixPinTool;
}
elsif(grep(/--bblengthmix|--bblenghtmix/i, @ARGV)) {
  $FullPathPinTool = $BBLenMixPinTool;
}
elsif(grep(/--jumpmix/i, @ARGV)) {
  $FullPathPinTool = $JumpMixPinTool;
}
elsif(grep(/--regmix/i, @ARGV)) {
  $FullPathPinTool = $RegMixPinTool;
}
elsif(grep(/--trace/i, @ARGV)) {
  $FullPathPinTool = $TracePinTool;
}
if(grep(/--no-shared-libs/i, @ARGV)) {
  $SharedLibsFlag="-no-shared-libs";
}
if(grep(/--papi/i, @ARGV)) {
  $Mode="papi";
}
@ARGV = grep(!/^--/, @ARGV); #remove switches
  

$Pintool=basename($FullPathPinTool, (".so"));
$PinPrefix="setarch $Arch -R $PinDir/pin -t $FullPathPinTool $SharedLibsFlag";

print "runPin in mode: $Mode\n";
while(<>) {
    next if /^#/;
    chomp;
    my ($name,$dir,$cmd) = split(/\|/);
    if($Mode eq "pin") {
      runPin($name, $dir, $cmd);
    }
    else {
      runPapi($name, $dir, $cmd);
    }
}
print "runPin finished running successfully\n";

sub runPin {
    my ($name, $dir, $cmd) = @_;
    my $cwd = getcwd();
    my $pinCmd = "$PinPrefix -o $cwd/RESULTS/$name.$Pintool.$$.LOG -- $cmd\n";
    runCommand($pinCmd, $dir, $cmd);

    if ($Pintool eq "trace") {
      my ($exe) = split(/\s+/, $cmd);
      my $symCmd = "$Syms $exe > $cwd/RESULTS/$name.$Pintool.$$.symtab.LOG";
      runCommand($symCmd, $dir, $cmd);
    }
}

sub runPapi {
    my ($name, $dir, $cmd) = @_;
    my $cwd = getcwd();
    my $papiCmd = "$cwd/runPapi.rb $name $dir \"$cmd\" xml";
    runCommand($papiCmd, $dir, $cmd);
}

sub runCommand {
    my ($cmd, $dir, $bmCmd) = @_;

    print "COMMAND: $cmd\n";
    my $cwd = getcwd();
    chdir $dir or die "Can't cd to $dir: $!\n";
    SanityCheck($bmCmd);
    unless ($SanityCheckOnly) {
        system($cmd);
        if($? == -1) {
            die "Failed to execute: $!";
        }
        elsif($? & 127) {
            my $sig =  $? & 127;
            die "child died with signal $sig";
        }
        else {
            my $exit_val  = $? >> 8;

            if ($exit_val != 0 && $DieOnNofibFailure) {
              printf STDERR "child '$cmd' exited with value %d.\n", $exit_val;
              die "Nonzero exit status: $exit_val";
            }
        }
    }

    chdir $cwd or die "Can't cd to $cwd: $!\n";
  
}

sub SanityCheck {
    my ($cmd) = @_;
    my ($exe) = split(/\s+/, $cmd);
    if (not (-e $exe && -x $exe) ) {
        die "File $exe not executable\n";
    }
    else {
        print STDERR "Checking $exe...ok\n";
    }
}

sub GetArch {
  my $arch = `uname -m`; chomp $arch;
  my $dir = "OOPS";
  if ($arch =~ /x86_64/) {
    $dir = "obj-intel64";
  }
  elsif ($arch =~ /i[234567x]86/) {
    $dir = "obj-ia32";
  }
  else {
    die "Unknown architecture $arch\n";
  }
  return ($arch, $dir);
}
