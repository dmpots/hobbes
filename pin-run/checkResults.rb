#!/usr/bin/ruby
$errors = 0
allTools = %w(bblengthmix opcodemix papi)
stdSets  = %w(
              nofib
              nofibpar
              spec.gcc
              spec.icc
              dph
              parallel.ghc
              shootout.ghc
              shootout.gcc
           )

llvmSets = %w(
              nofib-llvm
              nofibpar-llvm
              spec.llvm
              dph-llvm
              parallel.ghc-llvm
              shootout.ghc-llvm
              shootout.llvm
          )
allSets  = stdSets + llvmSets
papiSets = (stdSets - ["spec.icc"]) + llvmSets

# specific tools
tools = %w(papi)
sets  = papiSets

expectedFiles = {
  "nofib"              => 91,
  "nofib-llvm"         => 91,
  "nofibpar"           => 8,
  "nofibpar-llvm"      => 8,
  "dph"                => 5,
  "dph-llvm"           => 5,
  "spec.gcc"           => 27,
  "spec.icc"           => 18,
  "spec.llvm"          => 26,
  "shootout.gcc"       => 11,
  "shootout.llvm"      => 11,
  "shootout.ghc"       => 11,
  "shootout.ghc-llvm"  => 11
}
expectedFiles["parallel.ghc"] = expectedFiles["dph"] + expectedFiles["nofibpar"]
expectedFiles["parallel.ghc-llvm"] = expectedFiles["dph-llvm"] + expectedFiles["nofibpar-llvm"]

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
  sets.each do |bench|
    dir = File.join("RESULTS", "#{bench}.#{tool}")
    if (! File.exists?(dir) ) then
      error("#{dir} does not exist")
      next
    end
    files = Dir["#{dir}/*"]
    expected = expectedFiles[bench]
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

