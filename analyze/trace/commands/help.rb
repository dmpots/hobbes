
require (File.dirname(File.expand_path(__FILE__)) + '/_base')
class HelpCommand < Command
  def initialize(commands)
    @name       = "help"
    @args       = "<command>"
    @short_help = "print the help for a command"
    @cmds       = commands
  end

  def parse(a)
    @cmd_name = a.shift
    raise UsageError if @cmd_name.nil?
  end

  def run(outh)
    @cmd = @cmds.find {|c| c.name == @cmd_name}
    if @cmd.nil? then
      outh.puts "Invalid command: #{@cmd_name}"
    else
      outh.puts @cmd.help
    end
  end
end

(@commands ||= [])
@commands << HelpCommand.new(@commands)
