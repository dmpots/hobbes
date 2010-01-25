
$ProgramClassRE  = "HaskellProgram|CProgram|NofibGhc|SpecGcc|SpecIcc";
$ProgramClassRE .= "|ShootoutGhc|ShootoutGcc|NofibparGhc|DphGhc|ParallelGhc";
@ProgramClasses = split(/\|/, $ProgramClassRE);
$ProgramClassStrings = "  " . join("\n  ", @ProgramClasses);

sub checkClass {
    my ($label) = @_;

    if ($label !~ /$ProgramClassRE/) {
        return 0;
    }

    return 1;
}
