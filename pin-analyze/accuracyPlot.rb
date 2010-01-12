#!/usr/bin/ruby
require 'pp'

class Array
  def mean
    inject(0){ |sum, n| sum + n } / length.to_f
  end
  def geomean
    sort.inject(1){ |product, n| product * n } ** (1.0/length)
  end
end


def processFiles(files)
  min   = 100.0
  max   = 0.0
  means = []
  files.each do |file|
    File.open(file).lines.each do |line|
      if line =~ /^STATS\(.*?\) = (\d+\.?\d+)\/(\d+\.?\d+)\/(\d+\.?\d+)/ then
        n,g,x = [$1.to_f, $2.to_f, $3.to_f]
        if g != 0.0 then
          means << g
          min = [min, n].min
          max = [max, x].max
        end
      end
    end
  end
  #puts "#{min}, #{means.geomean}, #{max}"
  [min, means.geomean, max]
end

if __FILE__ == $0 then
files = {}
ARGV.each do |file|
  ext = File.extname(file).gsub(/^\./, "")
  if ext =~ /SPEC|HICC|HGCC|SHOOTOUT/ then
    pinTool =
    case file 
      when /jumpmix/   
        "jumpmix" 
      when /opcodemix/ 
        "opcodemix"
      when /regmix/    
        "regmix"
    end

    key = "#{ext}-#{pinTool}"
    files[key] ||= []
    files[key] << file
  end
end

files.sort_by{|n,_|n.split("-",1)[0]}.each do |name,resultFiles|
  min,mean,max = processFiles(resultFiles)
  puts "#{name}, #{min}, #{mean}, #{max}"
end

end
