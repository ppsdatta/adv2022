class Solution
  def initialize
    @prio_list = ('a'..'z').to_a + ('A'..'Z').to_a
  end
  
  def partition(s)
    mid = s.length / 2
    [s[0...mid].split(//), s[mid...s.length].split(//)]
  end
  
  def calculate(s)
    f, s = self.partition(s)
    commons = f.intersection(s)
    commons.inject(0) {|accm, c| accm + @prio_list.index(c) + 1}
  end
end

s = Solution.new
sum = 0
File.open(ARGV[0]).each do |line|
  sum += s.calculate(line.chomp)
end

p sum

  
    