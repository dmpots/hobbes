
class Command
  AWK_DIR = File.dirname(File.expand_path(__FILE__)) + "/../awk-scripts"
  GT_EXE  = File.dirname(File.expand_path(__FILE__)) + "/../dist/build/graph-trace/graph-trace"
  CALVIN_EXE = File.dirname(File.expand_path(__FILE__)) + "/../calvin"

  attr_reader :name, :args, :help, :short_help, :long_help
  def parse(args)
    raise "Override me"
  end

  def usage
    "#{name} - #{short_help}"
    "usage: #{name} #{args}"
  end

  def run(outf)
    raise "Override me"
  end

  def help
    "#{short_help}\n\n#{long_help}"
  end
end

class UsageError < RuntimeError
end

def exe_name(log_file)
 File.basename(log_file).split(".", 2).shift
end
