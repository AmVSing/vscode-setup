class TreeNode:
    def __init__(self, val=0, left=None, right=None):
        self.val = val
        self.left = left
        self.right = right

def inOrderTraversal(self, root):
    res =[]
    if root is None:
        return []
    res.extend(self.inOrderTraversal(root.left))
    res.append(root.val)
    res.extend(self.inOrderTraversal(root.right))
    return res





root = TreeNode(5)
root.left = TreeNode(4)
root.right = TreeNode(6)
root.right.left = TreeNode(3)
root.right.right = TreeNode(7)


dfs(root)