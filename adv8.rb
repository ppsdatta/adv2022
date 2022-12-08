class Grid
  attr_reader :grid

  def initialize(content)
    @grid = content.chomp.split(/\n/).map { |r| r.split(//) }.map { |a| a.map(&:to_i) }
    @row = @grid.count
    @col = @grid[0].count
  end

  def outer_count
    (@grid.count - 2) * 2 + @grid[0].count * 2
  end

  def right_of(r, c)
    (c+1...@col).map { |x| @grid[r][x] }
  end

  def left_of(r, c)
    (0...c).map { |x| @grid[r][x] }
  end

  def above_of(r, c)
    (0...r).map { |x| @grid[x][c] }
  end

  def down_of(r, c)
    (r+1...@row).map { |x| @grid[x][c] }
  end

  def left_score(r, c)
    trees = self.left_of(r, c)
    return 0 if trees.empty?
    ix = trees.reverse.find_index { |x| x >= @grid[r][c] }
    if ix.nil?
      trees.count
    else
      (0..ix).count
    end
  end

  def right_score(r, c)
    trees = self.right_of(r, c)
    return 0 if trees.empty?
    ix = trees.find_index { |x| x >= @grid[r][c] }
    if ix.nil?
      trees.count
    else
      (0..ix).count
    end
  end

  def above_score(r, c)
    trees = self.above_of(r, c)
    return 0 if trees.empty?
    ix = trees.reverse.find_index { |x| x >= @grid[r][c] }
    if ix.nil?
      trees.count
    else
      (0..ix).count
    end
  end

  def down_score(r, c)
    trees = self.down_of(r, c)
    return 0 if trees.empty?
    ix = trees.find_index { |x| x >= @grid[r][c] }
    if ix.nil?
      trees.count
    else
      (0..ix).count
    end
  end

  def total_score(r, c)
    self.left_score(r, c) * self.right_score(r, c) * self.above_score(r, c) * self.down_score(r, c)
  end

  def max_score
    scores = []
    (0...@row).each do |r|
      (0...@col).each do |c|
        scores << self.total_score(r, c)
      end
    end
    scores.max
  end

  def inner_count
    sum = 0
    (1...@row-1).each do |r|
      (1...@col-1).each do |c|
        item = @grid[r][c]
        left_items = self.left_of(r, c)
        right_items = self.right_of(r, c)
        above_items = self.above_of(r, c)
        down_items = self.down_of(r, c)
        visible = [left_items, right_items, above_items, down_items].any? { |arr| arr.all? { |x| item > x } }
        sum += 1 if visible
      end
    end
    sum
  end

  def visible_count
    self.outer_count + self.inner_count
  end
end

# mini_input = '30373
# 25512
# 65332
# 33549
# 35390'
#
# g = Grid.new(mini_input)
# p g.visible_count
# p g.left_score(1, 2)
# p g.right_score(1, 2)
# p g.above_score(1, 2)
# p g.down_score(1, 2)
# p g.total_score(3, 2)
# p g.max_score

content = File.read('ii8.txt').chomp
g = Grid.new(content)

# Part1
p g.visible_count

# Part 2
p g.max_score
