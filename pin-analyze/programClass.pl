
$ProgramClassRE  =  "HaskellProgram|CProgram|NofibGhc|SpecGcc|SpecIcc";
$ProgramClassRE .= "|ShootoutGhc|ShootoutGcc|NofibparGhc|DphGhc|ParallelGhc";
$ProgramClassRE .= "|NofibGhc_Llvm|ShootoutGhc_Llvm|ShootoutLlvm";
$ProgramClassRE .= "|NofibparGhc_Llvm|DphGhc_Llvm|ParallelGhc_Llvm";
$ProgramClassRE .= "|SpecLlvm";

@ProgramClasses = split(/\|/, $ProgramClassRE);
$ProgramClassStrings = "  " . join("\n  ", @ProgramClasses);

sub checkClass {
    my ($label) = @_;

    if ($label !~ /$ProgramClassRE/) {
        return 0;
    }

    return 1;
}
