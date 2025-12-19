class TreeNode:
    def __init__(self, val=0, left=None, right=None):
        self.val = val
        self.left = left
        self.right = right

def dfs(root: TreeNode, stack = [], res = []):
    if root is None:
        if stack == []:
            return res
        else:
            dfs(stack.pop(), stack = stack)
    else:
        res.append(root.val)
        stack.append(root)
        dfs(root.left)
        dfs(root.right)