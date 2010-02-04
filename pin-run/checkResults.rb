#!/usr/bin/ruby
$errors = 0
tools = %w(bblengthmix opcodemix)
sets  = %w(
              nofib
              nofibpar
              spec.gcc
              spec.icc
              dph
              parallel.ghc
              shootout.ghc
              shootout.gcc
           )

expectedFiles = {
  "nofib"        => 91,
  "nofibpar"     => 8,
  "dph"          => 5,
  "spec.gcc"     => 27,
  "spec.icc"     => 18,
  "shootout.gcc"  => 11,
  "shootout.ghc"  => 11
}
expectedFiles["parallel.ghc"] = expectedFiles["dph"] + expectedFiles["nofibpar"]

def testFile(file)
  fileType = "UNKNOWN"
  File.open(file).each do |line|
    if (line =~ /^#(\s)*opcode/)      then fileType = "opcodemix"   end
    if (line =~ /^#(\s)*JUMPMIX/)     then fileType = "jumpmix"     end
    if (line =~ /^#(\s)*num(\s)+reg/) then fileType = "regmix"      end
    if (line =~ /^#(\s)*block-length/)then fileType = "bblengthmix" end
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

