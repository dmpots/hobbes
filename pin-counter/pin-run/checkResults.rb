#!/usr/bin/ruby
require 'pp'

$errors = 0
allTools = %w(bblengthmix opcodemix papi)
stdSets  = %w(
              nofib
              spec.gcc
              spec.icc
              shootout.ghc
              shootout.gcc
           )
outOfFavorSets = %w(
              nofibpar
              dph
              parallel.ghc

              nofibpar-llvm
              dph-llvm
              parallel.ghc-llvm

              nofibpar-viaC
              dph-viaC
              parallel.ghc-viaC
)

llvmSets = %w(
              nofib-llvm
              spec.llvm
              shootout.ghc-llvm
              shootout.llvm
          )
viaCSets = %w(
              nofib-viaC
              shootout.ghc-viaC
          )
specSets = %w(
              spec.int.gcc
              spec.int.icc
              spec.int.llvm
              spec.fp.gcc
              spec.fp.icc
              spec.fp.llvm
          )
allSets  = stdSets + llvmSets + viaCSets + specSets
papiSets = (stdSets - ["spec.icc"]) + llvmSets

# specific tools
tools = allTools
sets  = allSets

expectedFiles = {
  "nofib"              => 91,
  "nofib-llvm"         => 91,
  "nofib-viaC"         => 91,
  "nofibpar"           => 8,
  "nofibpar-llvm"      => 8,
  "nofibpar-viaC"      => 8,
  "dph"                => 5,
  "dph-llvm"           => 5,
  "dph-viaC"           => 5,
  "spec.gcc"           => 27,
  "spec.icc"           => 18,
  "spec.llvm"          => 26,
  "spec.int.gcc"       => 11,
  "spec.int.icc"       => 11,
  "spec.int.llvm"      => 11,
  "spec.fp.gcc"        => 16,
  "spec.fp.icc"        =>  7,
  "spec.fp.llvm"       => 15,
  "shootout.gcc"       => 11,
  "shootout.llvm"      => 11,
  "shootout.ghc"       => 11,
  "shootout.ghc-llvm"  => 11,
  "shootout.ghc-viaC"  => 11
}

expectedFiles["parallel.ghc"] = expectedFiles["dph"] + expectedFiles["nofibpar"]
expectedFiles["parallel.ghc-llvm"] = expectedFiles["dph-llvm"] + expectedFiles["nofibpar-llvm"]
expectedFiles["parallel.ghc-viaC"] = expectedFiles["dph-viaC"] + expectedFiles["nofibpar-viaC"]

def testFile(file)
  fileType = "UNKNOWN"
  File.open(file).each do |line|
    if (line =~ /^#(\s)*opcode/)      then fileType = "opcodemix"   end
    if (line =~ /^#(\s)*JUMPMIX/)     then fileType = "jumpmix"     end
    if (line =~ /^#(\s)*num(\s)+reg/) then fileType = "regmix"      end
    if (line =~ /^#(\s)*block-length/)then fileType = "bblengthmix" end
    if (line =~ /^\(PAPI_TOT_CYC/)    then fileType = "papi"        end
  end
  fileType
end

def error(msg)
  $errors = $errors + 1
  puts "ERROR: #{msg}"
end

tools.each do |tool|
  toolSets, expectedCount = 
  if tool == "papi" then
    newExpected = expectedFiles.clone
    expectedFiles.each {|k,v| if k =~ /shootout\.ghc/ then newExpected[k] = v - 1 end}
    #pp newExpected
    [sets - ["spec.icc", "spec.int.icc", "spec.fp.icc"], newExpected]
  else
    [sets, expectedFiles]
  end

  toolSets.each do |bench|
    dir = File.join("RESULTS", "#{bench}.#{tool}")
    if (! File.exists?(dir) ) then
      error("#{dir} does not exist")
      next
    end
    files = Dir["#{dir}/*"]
    expected = expectedCount[bench]
    if files.length !=  expected then
      error("#{dir} missing files. Has #{files.length}. Expected #{expected}.")
    end
    files.each do |file|
      type = testFile(file)
      if type != tool then
        error("#{file} is the wrong type. Expected #{tool}. Found #{type}.")
      end
    end
  end
end
 
if $errors == 0 then
  puts "No errors found."
end

