#!/usr/bin/ruby
require 'pp'
require 'fileutils'

$iters = 100
$outfile = $stdout
$eventsFile = DATA

PapiexTool=ENV["HOME"]+"/local/papiex/bin/papiex "
$dir  = ARGV[0]
$prog = ARGV[1]

if File.exists?("papi-events.conf") then 
  $eventsFile = File.open("papi-events.conf", "r")
end
$events = $eventsFile.read.split(/\s+/).reject{|e| e =~ /^#/}

$resDir  = File.join(FileUtils.pwd, "RESULTS")
$papiOut = File.join($resDir, "PAPI_RUN")
$out    = File.open(File.join($resDir, "#{$prog}.papi.#{Process.pid}.LOG"), "w")

if $dir.nil? || $prog.nil? || $events.nil? || $events.empty? then
  errorOut("usage: runPapi.rb <dir> <cmd> <events>")
end

if not File.directory?($dir) then
  errorOut("Directory #{$dir} does not exist")
end
if not File.exists?($dir + "/" + $prog) then
  errorOut("Program #{$dir}/#{$prog} does not exist")
end


class Array
  def mean
    inject(0){ |sum, n| sum + n } / length.to_f
  end
  def geomean
    sort.inject(1){ |product, n| product * n } ** (1.0/length)
  end
end

def outFileName(dir,event,seqNum)
    outFile = File.join(dir, "#{$prog}.papi.#{event}.#{Process.pid}.#{seqNum}.LOG")
end
def runCommand(cmd)
  log "COMMAND: #{cmd}"
  if not system(cmd) then
    errorOut("RUNNING COMMAND: #{$?}")
  end
end
def errorOut(msg)
  $stderr.puts("ERROR: "+msg)
  exit 1
end
def log(msg)
  puts msg
end
def out(msg)
  $out.puts(msg)
end

FileUtils.cd($dir) do 
  $events.each do |event|
    results = []
    cmd = PapiexTool + " -q -w -e #{event} -o #{$papiOut} #{$prog}"
    (1..$iters).each do |seqNum|
      # clean up
      FileUtils.rm_rf($papiOut)

      # run papiex
      runCommand(cmd)
      destFile = outFileName($resDir, event, seqNum)

      #check results
      if    File.directory?($papiOut) then
        summaryFile = $papiOut + "/process_summary.txt"
        if not File.exists?(summaryFile) then
          errorOut("Missing process summary file")
        end
        FileUtils.cp(summaryFile, destFile)
      elsif File.file?($papiOut) 
        FileUtils.cp($papiOut, destFile) 
      else
        errorOut("Papi results file not created")
      end

      File.read(destFile).each do |line|
        if line =~ /#{event}$/ then
          results << line.split(/\s+/, 2).first.to_f
        end
      end

     # clean up
     FileUtils.rm_rf($papiOut)
    end

    out "(#{event}, #{results.mean.round})"
    killFiles = Dir[outFileName($resDir, event, "*")]
    FileUtils.rm(killFiles)
  end
end

__END__
PAPI_L1_DCM 
PAPI_L1_DCH 
PAPI_L1_DCA 
PAPI_L1_ICM 
PAPI_L1_ICH 
PAPI_L1_ICA 
PAPI_L1_TCA 
PAPI_L1_TCM 
PAPI_L1_LDM 
PAPI_L1_STM 

PAPI_L2_DCM 
PAPI_L2_DCA 
PAPI_L2_DCR 
PAPI_L2_DCW 
PAPI_L2_ICM 
PAPI_L2_ICH 
PAPI_L2_ICA 
PAPI_L2_TCM 
PAPI_L2_TCH 
PAPI_L2_TCA 
PAPI_L2_TCR 
PAPI_L2_TCW 
PAPI_L2_LDM 
PAPI_L2_STM 

PAPI_TLB_DM 
PAPI_TLB_IM 

PAPI_BR_CN  
PAPI_BR_TKN 
PAPI_BR_NTK 
PAPI_BR_MSP 
PAPI_BR_PRC 

PAPI_TOT_INS
PAPI_TOT_CYC

