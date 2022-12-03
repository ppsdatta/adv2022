class Solution
  def initialize
    @prio_list = ('a'..'z').to_a + ('A'..'Z').to_a
  end
  
  def partition(s)
    mid = s.length / 2
    [s[0...mid].split(//), s[mid...s.length].split(//)]
  end
  
  def calculate(slice)
    a, b, c = slice.map {|x| x.split(//)}
    commons = a.intersection(b).intersection(c)
    commons.inject(0) {|accm, c| accm + @prio_list.index(c) + 1}
  end
end

s = Solution.new
sum = 0
File.readlines(ARGV[0]).map {|line| line.chomp}.each_slice(3) {|slice| sum += s.calculate(slice)}

p sum

  
    
