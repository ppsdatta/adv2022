class File
  attr_reader :parent, :size

  def initialize(parent, name, size)
    @parent = parent
    @name = name
    @size = size
  end

  def to_s
    "file(#@name(#@size))"
  end

  def calculate_size(coll)
    # Nothing to be done
  end
end

class Dir
  attr_reader :parent, :children, :name

  def initialize(parent, name)
    @name = name
    @parent = parent
    @children = Hash.new
  end

  def add_dir(dirname)
    @children[dirname] = Dir.new(self, dirname)
  end

  def add_file(filename, size)
    @children[filename] = File.new(self, filename, size)
  end

  def size
    @children.map { |_, v| v.size }.sum
  end

  def cd(name)
    if @children[name] and @children[name].is_a?(Dir)
      @children[name]
    else
      raise Exception.new('Invalid child directory name ' + name)
    end
  end

  def to_s
    "dir(#@name){ " +
        @children.map { |k, v| v.to_s }.join("; ") +
        '}'
  end

  def calculate_size(coll)
    coll << [self.name, self.size]
    @children.keys.each { |key| @children[key].calculate_size(coll) }
  end
end

class FileSystem
  attr_reader :current, :root

  def initialize
    @root = Dir.new(nil, '/')
    @current = @root
  end

  def cd(name)
    case name
      when ".."
        @current = @current.parent
        if @current.nil?
          @current = @root
        end
      when "/"
        @current = @root
      else
        @current = @current.cd(name)
    end
  end

  def dir(name)
    @current.add_dir(name)
  end

  def file(name, size)
    @current.add_file(name, size)
  end

  def directory_sizes
    s = []
    @root.calculate_size(s)
    s
  end
end


class Solution
  attr_accessor :fs

  def initialize
    @fs = FileSystem.new
  end

  def parse_input(input)
    input.split(/\n/).map do |line|
      parts = line.split(/\s+/)
      if parts[0] == '$'
        if parts[1] == 'cd'
          @fs.cd(parts[2])
        end
      elsif parts[0] == 'dir'
        @fs.dir(parts[1])
      elsif parts[0] =~ /\d+/
        @fs.file(parts[1], parts[0].to_i)
      end
    end
  end

  def solve1(input)
    self.parse_input(input)
    sizes = self.fs.directory_sizes
    sizes.filter { |x| x[1] <= 100000 }.inject(0) { |accm, x| accm + x[1] }
  end

  def solve2(input)
    self.parse_input(input)
    sizes = self.fs.directory_sizes.sort_by { |x| x[1] }
    root_size = self.fs.root.size
    free_space = 70000000 - root_size
    req_space = 30000000 - free_space
    index = sizes.find_index { |x| x[1] >= req_space }
    sizes[index][1]
  end
end

sol = Solution.new
lines = File.read(ARGV[0]).chomp
p sol.solve1(lines)
p sol.solve2(lines)


