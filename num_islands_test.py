# https://leetcode.com/problems/number-of-islands/description/
def numIslands(self, grid):
    """
    :type grid: List[List[str]]
    :rtype: int
    """
    isl = 0
    
    for i in range(len(grid)):
        for j in range(len(grid[0])):
            if grid[i][j] == "1":
                isl += 1
                q = [(i, j)]
                while q:
                    x, y = q.pop()
                    if x >= 0 and x < len(grid) and y >= 0 and y < len(grid[0]) and grid[x][y] == "1":
                        grid[x][y] = isl + 2
                        q.append((x-1,y))
                        q.append((x+1, y))
                        q.append((x, y+1))
                        q.append((x, y-1))
                        
    return isl

test1 = [[1,1,0,0,0], [1,1,0,0,0],[0,0,1,0,0],[0,0,0,1,1]]
assert(numIslands(test1) == 3)

test2 = [[1,1,1,1,0], [1,1,0,1,0],[1,1,0,0,0],[0,0,0,0,0]]
assert(numIslands(test2) == 1)